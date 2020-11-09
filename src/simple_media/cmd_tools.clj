(ns simple-media.cmd-tools
  (:require [taoensso.timbre :as log]
            [clojure.java.shell :as shell]
            [clojure.data.json :as json]
            [config.core :refer [env]]
            ))

(def ffprobe-location (or (:ffprobe-location env) "ffprobe"))
(def ffmpeg-location (or (:ffmpeg-location env) "ffmpeg"))
(def identify-location (or (:identify-location env) "identify"))
(def imagemagick-location (or (:imagemagick-location env) "convert"))

(defn- check-command-result [exit out err]
  (if (zero? exit)
    (do 
      (log/debug "command success" out)
      true)
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

(defn get-stream [probe type]
  (first (filter #(= (:codec_type %) type) (:streams probe))))

;; Use original audio if it is 1 or 2 channels (i.e., mono or stereo),
;; in "AAC" or "MP3" format, and has a bitrate less than or equal to
;; 128kbps.
(defn- use-original-audio? [audio]
  (and (contains? #{1 2} (:channels audio))
       (contains? #{"aac" "mp3"} (:codec_name audio))
       (<= (Double/parseDouble (:bit_rate audio)) 128000)))

(defn output-format [video audio]
  (if video
    ["mp4" "video/mp4"]
    (if audio
      (if (and (use-original-audio? audio) (= "mp3" (:codec_name audio)))
        ["mp3" "audio/mpeg"]
        ["mp4" "audio/mp4"])
      [nil nil])))

(defn ffprobe
  [source-url]
  (let [cmd [ffprobe-location "-show_format" "-show_streams" "-print_format" "json" source-url]
        _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
        r (apply shell/sh cmd)
        _ (log/debug "Done exec command.")
        {:keys [exit out err]} r]
    (if (zero? exit) 
      (json/read-str out :key-fn keyword)
      (throw (ex-info (format "ffprobe command error: %s" err) {:exit exit
                                                                :out out
                                                                :err err})))))

(defn identify
  [source-url]
  (let [cmd [identify-location source-url]
        result (apply shell/sh cmd)]
    result))

(defn image-convert-json
  [source-url]
  (let [cmd [imagemagick-location source-url "json:"]
        _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
        r (apply shell/sh cmd)
        _ (log/debug "Done exec command.")
        {:keys [exit out err]} r]
    (if (zero? exit)
      (json/read-str out :key-fn keyword)
      (throw (ex-info (format "convert json: command error: %s" err) {:exit exit
                                                                      :out out
                                                                      :err err})))))

;;==================Thumbnails===========
(defn image-thumbnailize
  [source-url filters outputs]
  ;; filters are an array of filter map, e.g. {:tag "crop"}
  ;; outputs are an array of output map, e.g. {:file "XXX.png" :height 128} TODO(yangye): use clojure spec
  (if (every? (comp string? :file) outputs)
    (let [args [imagemagick-location (str source-url "[0]") "-auto-orient"]
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
  ([source-url time outputs]
   ;; time (in seconds) should base on probed duration
   ;; outputs are an array of output map, e.g. {:file "XXX.png" :height 128} TODO(yangye): use clojure spec
   (if (every? (comp string? :file) outputs) 
     (let [args [ffmpeg-location "-y" "-ss" (str time) "-i" source-url]
           args (reduce #(conj %1 "-vframes" "1" "-filter:v" (str "scale=-1:" (:height %2)) (:file %2)) args outputs)
           _ (log/debugf "Start exec command... %s" (clojure.string/join " " args))
           {:keys [exit out err]} (apply shell/sh args)
           _ (log/debug "Done exec command.")]
       (check-command-result exit out err))
     (throw (ex-info "video-thumbnailize invalid outputs param: file is not string" {:outputs outputs}))))
  ;; first-frame as thumbnail
  ([source-url outputs]
   (if (every? (comp string? :file) outputs) 
     (let [args [ffmpeg-location "-y" "-i" source-url]
           args (reduce #(conj %1 "-vframes" "1" "-filter:v" (str "scale=-1:" (:height %2)) (:file %2)) args outputs)
           _ (log/debugf "Start exec command... %s" (clojure.string/join " " args))
           {:keys [exit out err]} (apply shell/sh args)
           _ (log/debug "Done exec command.")]
       (check-command-result exit out err))
     (throw (ex-info "video-thumbnailize invalid outputs param: file is not string" {:outputs outputs}))))
  ([source-url interval height output-prefix]
   ;; Create a thumbnail with height, every interval seconds of the video
   ;; https://trac.ffmpeg.org/wiki/Create%20a%20thumbnail%20image%20every%20X%20seconds%20of%20the%20video
   ;; -vf fps=1/60 img%03d.jpg
   (let [fps-arg (format "fps=1/%d" interval)
         height-arg (format "scale=-1:%d" height)
         filter-arg (format "%s,%s" fps-arg height-arg)
         output-arg (str output-prefix "%03d.jpg")
         cmd [ffmpeg-location "-y" "-i" source-url
              "-vf" filter-arg
              output-arg]
         _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
         {:keys [exit out err]} (apply shell/sh cmd)
         _ (log/debug "Done exec command.")]
     (check-command-result exit out err))))

;;==================Transcode===========
(defn transcode-streams
  ([source-url video-stream audio-stream output-file height watermark]
   (let [[fmt mime] (output-format video-stream audio-stream)
         cmd (cond-> [ffmpeg-location
                      "-hide_banner"
                      "-loglevel" "warning"
                      "-y" "-i" source-url]
                     (and (not (nil? (:codec_name video-stream)))
                          height)
                     (-> (#(if (not (nil? watermark))
                             (concat % ["-i" watermark
                                        "-filter_complex"
                                        (str "[0:v]" (str "scale=-2:" height) "[scaled];"
                                             "[scaled][1:v]" "overlay=x=(main_w-overlay_w-10):y=(main_h-overlay_h-10)" "[out]")
                                        "-map" "[out]"])
                             (concat % ["-map" (str "0:" (:index video-stream))
                                        "-vf" (str "scale=-2:" height)])))
                         (concat ["-sws_flags" "bilinear"
                                  "-force_key_frames" (str "expr:gte(t,n_forced)")
                                  "-c:v" "libx264"
                                  "-pix_fmt" "yuv420p"
                                  "-profile:v" "high"
                                  "-level" "4.0"
                                  "-preset" "ultrafast"
                                  "-threads" "1"]))

                     (not (nil? (:codec_name audio-stream)))
                     (concat ["-map" (str "0:" (:index audio-stream))]
                             (if (use-original-audio? audio-stream)
                               ["-c:a" "copy"]
                               (cond-> ["-c:a" "aac"]
                                       (not (nil? (:codec_name video-stream))) (concat ["-vbr" "4"])
                                       (nil? (:codec_name video-stream)) (concat ["-b:a" "128k"])
                                       (> (:channels audio-stream) 2) (concat ["-ac" "2"]))))

                     true (concat ["-movflags" "+faststart" "-f" fmt output-file]))
         _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
         {:keys [exit out err]} (apply shell/sh cmd)
         _ (log/debug "Done exec command.")]
     (check-command-result exit out err)))
  ([source-url video-stream audio-stream output-file height]
   (transcode-streams source-url video-stream audio-stream output-file height nil)))

(defn transcode
  ([source-url output-file height watermark]
   (let [probe (ffprobe source-url)
         video-stream (get-stream probe "video")
         audio-stream (get-stream probe "audio")]
     (transcode-streams source-url video-stream audio-stream output-file height watermark)))
  ([source-url output-file height]
   (transcode source-url output-file height nil)))


(defn m3u8->mp4
  [source-url output-file]
  (let [cmd [ffmpeg-location "-i" source-url "-vcodec" "copy" "-acodec" "copy" "-absf" "aac_adtstoasc" output-file]
        _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
        {:keys [exit out err]} (apply shell/sh cmd)
        _ (log/debug "Done exec command.")]
    (check-command-result exit out err)))

(defn produce-live-stream
  [source-url output-file protocol]
  (case protocol
    "hls" (let [cmd [ffmpeg-location "-i" source-url
                     "-c:v" "libx264" "-crf" "21" "-preset" "veryfast"
                     "-force_key_frames" (str "expr:gte(t,n_forced*2.000)") ;https://www.reddit.com/r/ffmpeg/comments/cjnqgz/deafult_gop_size_ffmpeg/
                     "-sc_threshold" "0"
                     "-c:a" "aac" "-b:a" "128k" "-ac" "2"
                     "-f" "hls" "-hls_time" "10" "-hls_playlist_type" "event"
                     output-file]
                _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
                {:keys [exit out err]} (apply shell/sh cmd)
                _ (log/debug "Done exec command.")]
            (check-command-result exit out err))
    nil))

;; https://www.bogotobogo.com/FFMpeg/ffmpeg_select_scene_change_keyframes_tile_Creating_a_mosaic_of_screenshots_from_a_movie.php
;; $ ffmpeg -i YosemiteHDII.webm -vf select='gt(scene\,0.4)',scale=160:120,tile -frames:v 1 Yosemite_preview.png
(defn tile-video-by-scene
  [source-url output
   {:keys [scene-diff scale-h]
    :or {scene-diff "0.4"
         scale-h 64}
    :as params}]
  (let [scene-select-arg (str "select=gt(scene" \\ "," scene-diff ")") ;;NOTE: 0.4 might be too high, output file is empty
        scale-arg (format "scale=-1:%d" scale-h)
        tile-arg "tile"
        filter-arg (format "%s,%s,%s" scene-select-arg scale-arg tile-arg)
        cmd ["ffmpeg" "-y" "-i" source-url
             "-vf" filter-arg
             "-frames:v" "1"
             "-vsync" "vfr"
             output]
        _ (log/debugf "Start exec command... %s" (clojure.string/join " " cmd))
        {:keys [exit out err]} (apply shell/sh cmd)
        _ (log/debug "Done exec command.")]
    (check-command-result exit out err)))

;; https://www.bogotobogo.com/FFMpeg/ffmpeg_select_scene_change_keyframes_tile_Creating_a_mosaic_of_screenshots_from_a_movie.php
;; $ ffmpeg -ss 00:00:05 -i YosemiteHDII.webm -frames 1 -vf "select=not(mod(n\,400)),scale=160:120,tile=4x3" tile.png
(defn tile-video-by-frame
  [source-url output
   {:keys [seek-second frame-interval scale-h tile-x tile-y]
    :or {seek-second "00:00:02"
         frame-interval 400
         scale-h 64
         tile-x 4
         tile-y 3}
    :as params}]
  (let [frame-select-arg (str "select=not(mod(n" \\ "," frame-interval "))")
        scale-arg (format "scale=-1:%d" scale-h)
        tile-arg (format "tile=%dx%d" tile-x tile-y)
        filter-arg (format "%s,%s,%s" frame-select-arg scale-arg tile-arg)
        cmd ["ffmpeg" "-ss" seek-second
             "-y" "-i" source-url
             "-frames" "1"
             "-vf" filter-arg
             output]
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
                    5 (transcode (nth args 1) (nth args 2) (nth args 3) (last args))
                    (throw (IllegalArgumentException. "Missing parameters: source-url output-file height watermark(optional)")))
      (throw (IllegalArgumentException. (format "Illegal tool name: %s%n" tool)))))
  (println "Done transcode tools"))
