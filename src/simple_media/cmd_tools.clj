(ns simple-media.cmd-tools
  (:require [taoensso.timbre :as log]
            [clojure.java.shell :as shell]
            [clojure.data.json :as json]))

(defn- check-command-result [exit out err]
  (if (zero? exit)
    (log/debug "command success" out)
    (throw (ex-info "command error" {:out out
                                     :err err}))))

;;==================Probe===============
(defn- apply-crop [args {:keys [x y width height rotate]}]
  ;; crop uses NorthWest gravity regardless of rotation
  (let [args (conj args "-gravity" "NorthWest")]
    (if (and rotate (not= 0 rotate))
      ;; with rotations, the distort ScaleRotateTranslate (SRT) operates
      ;; straight into a viewport, which results in an automatic crop,
      ;; and will rotate in pixels outside the region.  It also
      ;; handles edges by duplicating the edge pixel.  The tempting
      ;; alternative of "-crop" and "-rotate" does not handle the
      ;; boundary conditions as well.
      (conj args "-define" (format "distort:viewport=%dx%d%+d%+d" width height x y)
                 "-distort" "SRT" (str (+ x (* width 0.5)) \, (+ y (* height 0.5)) \, rotate))
      ;; If there is no rotation, then crop is more efficient.
      (conj args "-crop" (format "%dx%d%+d%+d" width height x y)))))

(defn- apply-filter [args filter]
  (case (:tag filter)
    "crop" (apply-crop args filter)
    (do
      (log/warn "unknown filter ignored:" (:tag filter))
      args)))

(defn- get-stream [probe type]
  (first (filter #(= (:codec_type %) type) (:streams probe))))

;; Use original audio if it is 1 or 2 channels (i.e., mono or stereo),
;; in "AAC" or "MP3" format, and has a bitrate less than or equal to
;; 128kbps.
(defn- use-original-audio [audio]
  (and (contains? #{1 2} (:channels audio))
       (contains? #{"aac" "mp3"} (:codec_name audio))
       (<= (Double/parseDouble (:bit_rate audio)) 128000)))

(defn- output-format [video audio]
  (if video
    ["mp4" "video/mp4"]
    (if audio
      (if (and (use-original-audio audio) (= "mp3" (:codec_name audio)))
        ["mp3" "audio/mpeg"]
        ["mp4" "audio/mp4"])
      [nil nil])))

(defn ffprobe
  [source-url]
  (let [cmd ["/app/ffprobe" "-show_format" "-show_streams" "-print_format" "json" source-url]
        _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
        r (apply shell/sh cmd)
        _ (log/debug "Done exec command.")
        {:keys [exit out err]} r]
    (check-command-result exit out err)
    (json/read-str out :key-fn keyword)))

;;==================Thumbnails===========
(defn image-thumbnailize
  [source-url filters outputs]
  ;; filters are an array of filter map, e.g. {:tag "crop"}
  ;; outputs are an array of output map, e.g. {:file "XXX.png" :height 128} TODO(yangye): use clojure spec
  (if (every? (comp string? :file) outputs)
    (let [args ["convert" (str source-url "[0]") "-auto-orient"]
          args (reduce apply-filter args filters)
          args (conj args "+write" "mpr:IN" "-quality" "80" "-background" "#ffffff")
          args (reduce #(conj %1 "(" "mpr:IN" "-thumbnail" (str "x" (:height %2)) "-strip" "-write" (:file %2) ")") args outputs)
          args (conj args "null:white")
          _ (log/debugf "Start exec command... %s" (clojure.string/join " " args))
          {:keys [exit out err]} (apply shell/sh args)
          _ (log/debug "Done exec command.")]
      (check-command-result exit out err))
    (throw (ex-info "image-thumbnailize invalid outputs param: file is not string" {:outputs outputs}))))

(defn video-thumbnailize
  [source-url time outputs]
  ;; time should base on probed duration
  ;; outputs are an array of output map, e.g. {:file "XXX.png" :height 128} TODO(yangye): use clojure spec
  (if (every? (comp string? :file) outputs) 
    (let [args ["/app/ffmpeg" "-y" "-ss" (str time) "-i" source-url]
          args (reduce #(conj %1 "-vframes" "1" "-filter:v" (str "scale=-1:" (:height %2)) (:file %2)) args outputs)
          _ (log/debugf "Start exec command... %s" (clojure.string/join " " args))
          {:keys [exit out err]} (apply shell/sh args)
          _ (log/debug "Done exec command.")]
      (check-command-result exit out err))
    (throw (ex-info "video-thumbnailize invalid outputs param: file is not string" {:outputs outputs}))))

;;==================Transcode===========
(defn transcode
  [source-url output-file height]
  (let [probe (ffprobe source-url)
        video-stream (get-stream probe "video")
        audio-stream (get-stream probe "audio")
        audio-opts (if (use-original-audio audio-stream)
                     ["-c:a" "copy"]
                     (cond-> ["-c:a" "libfdk_aac" "-vbr" "4"]
                       (> (:channels audio-stream) 2) ;; downmix to 2 channels if there were more
                       (concat ["-ac" "2"])))
        [fmt mime] (output-format video-stream audio-stream)
        cmd (cond-> ["/app/ffmpeg"
                     "-hide_banner"
                     "-loglevel" "warning"
                     "-y" "-i" source-url]
                    (not (nil? video-stream)) (concat ["-map" (str "0:" (:index video-stream))
                                                       "-vf" (str "scale=-2:" height)
                                                       "-sws_flags" "bilinear"
                                                       "-force_key_frames" (str "expr:gte(t,n_forced)")
                                                       "-c:v" "libx264"
                                                       "-profile:v" "high"
                                                       "-level" "4.0"
                                                       "-preset" "ultrafast"
                                                       "-threads" "1"])
                    (not (nil? audio-stream)) (concat ["-map" (str "0:" (:index audio-stream))] audio-opts)
                    true (concat ["-movflags" "+faststart" "-f" fmt output-file]))
        _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
        {:keys [exit out err]} (apply shell/sh cmd)
        _ (log/debug "Done exec command.")]
    (check-command-result exit out err)))


(defn -main [& args]
  (println "Running transcode tools")
  (if (< (count args) 2)
    (throw (IllegalArgumentException. "Missing parameters"))
    (println "args: " args))
  (let [tool (first args)]
    (println (format "running tool(%s)" tool))
    (case tool
      "probe" (case (count args)
                2 (ffprobe (nth args 1))
                (throw (IllegalArgumentException. "Missing parameters: source-url")))
      "transcode" (case (count args)
                    4 (transcode (nth args 1) (nth args 2) (last args))
                    (throw (IllegalArgumentException. "Missing parameters: source-url output-file height")))
      (throw (IllegalArgumentException. (format "Illegal tool name: %s%n" tool)))))
  (println "Done transcode tools"))
