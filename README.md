# simple-media-prcoessor

A Clojure library designed to ... well, that part is up to you.

## Usage

### Convert

  $ convert "IMG_0080.HEIC[0]" -auto-orient +write mpr:IN -quality 80 -background "#ffffff" "(" mpr:IN -thumbnail x128 -strip -write IMG_0080_128.png ")" "null:white"

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
