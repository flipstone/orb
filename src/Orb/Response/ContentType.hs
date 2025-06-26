module Orb.Response.ContentType
  ( ContentType
  , applicationEpubZip
  , applicationJavaArchive
  , applicationJson
  , applicationMsword
  , applicationOctetStream
  , applicationPdf
  , applicationRtf
  , applicationVndMsExcel
  , applicationVndMsPowerpoint
  , applicationX7zCompressed
  , applicationXRarCompressed
  , applicationXSh
  , applicationXTar
  , applicationXWwwFormUrlencoded
  , applicationXhtmlXml
  , applicationXml
  , applicationZip
  , audioAac
  , audioMidi
  , audioWebm
  , audioXWav
  , fontTtf
  , fontWoff
  , fontWoff2
  , imageGif
  , imageJpeg
  , imagePng
  , imageSvgXml
  , imageTiff
  , imageWebp
  , imageXIcon
  , multipartFormData
  , textCalendar
  , textCss
  , textCsv
  , textHtml
  , textJavascript
  , textPlain
  , videoMpeg
  , videoWebm
  , videoXMsvideo
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8

-- | A simple 'BS.ByteString' type alias representing a MIME type.
type ContentType = BS.ByteString

{- | A 'BS.ByteString' representation of the /application\/epub+zip/ MIME type
(commonly .epub eBooks).
-}
applicationEpubZip :: ContentType
applicationEpubZip = BS8.pack "application/epub+zip"

{- | A 'BS.ByteString' representation of the /application\/java-archive/ MIME
type (e.g., .jar files).
-}
applicationJavaArchive :: ContentType
applicationJavaArchive = BS8.pack "application/java-archive"

{- | A 'BS.ByteString' representation of the /application\/json/ MIME type
(JSON data).
-}
applicationJson :: ContentType
applicationJson = BS8.pack "application/json"

{- | A 'BS.ByteString' representation of the /application\/msword/ MIME type
(classic .doc files).
-}
applicationMsword :: ContentType
applicationMsword = BS8.pack "application/msword"

{- | A 'BS.ByteString' representation of the /application\/octet-stream/ MIME
type (generic binary data).
-}
applicationOctetStream :: ContentType
applicationOctetStream = BS8.pack "application/octet-stream"

{- | A 'BS.ByteString' representation of the /application\/pdf/ MIME type (PDF
documents).
-}
applicationPdf :: ContentType
applicationPdf = BS8.pack "application/pdf"

{- | A 'BS.ByteString' representation of the /application\/rtf/ MIME type (RTF
documents).
-}
applicationRtf :: ContentType
applicationRtf = BS8.pack "application/rtf"

{- | A 'BS.ByteString' representation of the /application\/vnd.ms-excel/ MIME
type (older .xls files).
-}
applicationVndMsExcel :: ContentType
applicationVndMsExcel = BS8.pack "application/vnd.ms-excel"

{- | A 'BS.ByteString' representation of the /application\/vnd.ms-powerpoint/
MIME type (older .ppt files).
-}
applicationVndMsPowerpoint :: ContentType
applicationVndMsPowerpoint = BS8.pack "application/vnd.ms-powerpoint"

{- | A 'BS.ByteString' representation of the /application\/x-7z-compressed/
MIME type (.7z archives).
-}
applicationX7zCompressed :: ContentType
applicationX7zCompressed = BS8.pack "application/x-7z-compressed"

{- | A 'BS.ByteString' representation of the /application\/x-rar-compressed/
MIME type (.rar archives).
-}
applicationXRarCompressed :: ContentType
applicationXRarCompressed = BS8.pack "application/x-rar-compressed"

{- | A 'BS.ByteString' representation of the /application\/x-sh/ MIME type (.sh
shell scripts).
-}
applicationXSh :: ContentType
applicationXSh = BS8.pack "application/x-sh"

{- | A 'BS.ByteString' representation of the /application\/x-tar/ MIME type
(.tar archives).
-}
applicationXTar :: ContentType
applicationXTar = BS8.pack "application/x-tar"

{- | A 'BS.ByteString' representation of the
    /application\/x-www-form-urlencoded/ MIME type (HTML form data).
-}
applicationXWwwFormUrlencoded :: ContentType
applicationXWwwFormUrlencoded = BS8.pack "application/x-www-form-urlencoded"

{- | A 'BS.ByteString' representation of the /application\/xhtml+xml/ MIME type
(XHTML documents).
-}
applicationXhtmlXml :: ContentType
applicationXhtmlXml = BS8.pack "application/xhtml+xml"

{- | A 'BS.ByteString' representation of the /application\/xml/ MIME type (.xml
documents).
-}
applicationXml :: ContentType
applicationXml = BS8.pack "application/xml"

{- | A 'BS.ByteString' representation of the /application\/zip/ MIME type (.zip
archives).
-}
applicationZip :: ContentType
applicationZip = BS8.pack "application/zip"

{- | A 'BS.ByteString' representation of the /audio\/aac/ MIME type (.aac audio
files).
-}
audioAac :: ContentType
audioAac = BS8.pack "audio/aac"

{- | A 'BS.ByteString' representation of the /audio\/midi/ MIME type (.midi
music files).
-}
audioMidi :: ContentType
audioMidi = BS8.pack "audio/midi"

{- | A 'BS.ByteString' representation of the /audio\/webm/ MIME type (.weba or
webm audio).
-}
audioWebm :: ContentType
audioWebm = BS8.pack "audio/webm"

{- | A 'BS.ByteString' representation of the /audio\/x-wav/ MIME type (.wav
files).
-}
audioXWav :: ContentType
audioXWav = BS8.pack "audio/x-wav"

{- | A 'BS.ByteString' representation of the /font\/ttf/ MIME type (.ttf font
files).
-}
fontTtf :: ContentType
fontTtf = BS8.pack "font/ttf"

{- | A 'BS.ByteString' representation of the /font\/woff/ MIME type (.woff font
files).
-}
fontWoff :: ContentType
fontWoff = BS8.pack "font/woff"

{- | A 'BS.ByteString' representation of the /font\/woff2/ MIME type (.woff2
font files).
-}
fontWoff2 :: ContentType
fontWoff2 = BS8.pack "font/woff2"

{- | A 'BS.ByteString' representation of the /image\/gif/ MIME type (.gif
images).
-}
imageGif :: ContentType
imageGif = BS8.pack "image/gif"

{- | A 'BS.ByteString' representation of the /image\/jpeg/ MIME type (.jpg or
.jpeg images).
-}
imageJpeg :: ContentType
imageJpeg = BS8.pack "image/jpeg"

-- | A 'BS.ByteString' representation of the /image\/png/ MIME type (.png images).
imagePng :: ContentType
imagePng = BS8.pack "image/png"

{- | A 'BS.ByteString' representation of the /image\/svg+xml/ MIME type (.svg
images).
-}
imageSvgXml :: ContentType
imageSvgXml = BS8.pack "image/svg+xml"

{- | A 'BS.ByteString' representation of the /image\/tiff/ MIME type (.tif or
.tiff images).
-}
imageTiff :: ContentType
imageTiff = BS8.pack "image/tiff"

{- | A 'BS.ByteString' representation of the /image\/webp/ MIME type (.webp
images).
-}
imageWebp :: ContentType
imageWebp = BS8.pack "image/webp"

{- | A 'BS.ByteString' representation of the /image\/x-icon/ MIME type (.ico
icons).
-}
imageXIcon :: ContentType
imageXIcon = BS8.pack "image/x-icon"

{- | A 'BS.ByteString' representation of the /multipart\/form-data/ MIME type
    (HTML form data).
-}
multipartFormData :: ContentType
multipartFormData = BS8.pack "multipart/form-data"

{- | A 'BS.ByteString' representation of the /text\/calendar/ MIME type (.ics
calendar files).
-}
textCalendar :: ContentType
textCalendar = BS8.pack "text/calendar"

{- | A 'BS.ByteString' representation of the /text\/css/ MIME type (.css
stylesheets).
-}
textCss :: ContentType
textCss = BS8.pack "text/css"

{- | A 'BS.ByteString' representation of the /text\/csv/ MIME type (.csv
spreadsheets).
-}
textCsv :: ContentType
textCsv = BS8.pack "text/csv"

{- | A 'BS.ByteString' representation of the /text\/html/ MIME type (HTML
documents).
-}
textHtml :: ContentType
textHtml = BS8.pack "text/html"

{- | A 'BS.ByteString' representation of the /text\/javascript/ MIME type (.js
scripts).
-}
textJavascript :: ContentType
textJavascript = BS8.pack "text/javascript"

{- | A 'BS.ByteString' representation of the /text\/plain/ MIME type (plain
text).
-}
textPlain :: ContentType
textPlain = BS8.pack "text/plain"

{- | A 'BS.ByteString' representation of the /video\/mpeg/ MIME type (.mpeg or
.mpg videos).
-}
videoMpeg :: ContentType
videoMpeg = BS8.pack "video/mpeg"

{- | A 'BS.ByteString' representation of the /video\/webm/ MIME type (.webm
videos).
-}
videoWebm :: ContentType
videoWebm = BS8.pack "video/webm"

{- | A 'BS.ByteString' representation of the /video\/x-msvideo/ MIME type (.avi
videos).
-}
videoXMsvideo :: ContentType
videoXMsvideo = BS8.pack "video/x-msvideo"
