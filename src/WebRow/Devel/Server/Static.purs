module WebRow.Devel.Server.Static where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Array (last)
import Data.Map (Map, lookup)
import Data.Map (fromFoldable) as Map
import Data.Maybe (maybe)
import Data.String (Pattern(..), split)
import Data.String (joinWith) as String
import Data.Tuple (Tuple(..))
import Data.Undefined.NoProblem (Opt, (!))
import Data.Undefined.NoProblem.Closed (class Coerce, coerce)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import HTTPure (header, notFound, ok', serve) as HTTPure
import HTTPure.Response (Response) as HTTPure.Response
import Node.Buffer (Buffer)
import Node.FS.Aff (readFile) as FS
import Unsafe.Coerce (unsafeCoerce)

htaccess ∷ Map String String
htaccess = Map.fromFoldable $
  [ Tuple "aab" "application/x-authorware-bin"
  , Tuple "aam" "application/x-authorware-map"
  , Tuple "aas" "application/x-authorware-seg"
  , Tuple "asc" "text/plain"
  , Tuple "asf" "video/x-ms-asf"
  , Tuple "asp" "text/html"
  , Tuple "asx" "video/x-ms-asf"
  , Tuple "avi" "application/octet-stream"
  , Tuple "awk" "text/plain"
  , Tuple "bash" "text/plain"
  , Tuple "bsh" "text/plain"
  , Tuple "bz2" "application/octet-stream"
  , Tuple "c" "text/plain"
  , Tuple "cgi" "text/plain"
  , Tuple "chm" "application/octet-stream"
  , Tuple "class" "application/x-java-applet"
  , Tuple "csh" "text/plain"
  , Tuple "css" "text/css"
  , Tuple "csv" "application/vnd.ms-excel"
  , Tuple "dcr" "application/x-director"
  , Tuple "dir" "application/x-director"
  , Tuple "dmg" "application/octet-stream"
  , Tuple "dxr" "application/x-director"
  , Tuple "exe" "application/octet-stream"
  , Tuple "fgd" "application/x-director"
  , Tuple "fh" "image/x-freehand"
  , Tuple "fh4" "image/x-freehand"
  , Tuple "fh5" "image/x-freehand"
  , Tuple "fh7" "image/x-freehand"
  , Tuple "fhc" "image/x-freehand"
  , Tuple "flv" "video/x-flv"
  , Tuple "gawk" "text/plain"
  , Tuple "gtar" "application/x-gtar"
  , Tuple "gz" "application/x-gzip"
  , Tuple "h" "text/plain"
  , Tuple "html" "text/html"
  , Tuple "ico" "image/vnd.microsoft.icon"
  , Tuple "in" "text/plain"
  , Tuple "ini" "text/plain"
  , Tuple "m3u" "audio/x-mpegurl"
  , Tuple "md5" "text/plain"
  , Tuple "mov" "application/octet-stream"
  , Tuple "mov" "video/quicktime"
  , Tuple "mp4" "application/octet-stream"
  , Tuple "mpg" "application/octet-stream"
  , Tuple "msi" "application/octet-stream"
  , Tuple "nawk" "text/plain"
  , Tuple "pdb" "application/x-pilot"
  , Tuple "pdf" "application/pdf"
  , Tuple "phps" "application/x-httpd-php-source"
  , Tuple "pl" "text/plain"
  , Tuple "prc" "application/x-pilot"
  , Tuple "py" "text/plain"
  , Tuple "qt" "video/quicktime"
  , Tuple "ra" "audio/vnd.rn-realaudio"
  , Tuple "ram" "audio/vnd.rn-realaudio"
  , Tuple "rar" "application/x-rar-compressed"
  , Tuple "rm" "application/vnd.rn-realmedia"
  , Tuple "rpm" "audio/x-pn-realaudio-plugin"
  , Tuple "rv" "video/vnd.rn-realvideo"
  , Tuple "sh" "text/plain"
  , Tuple "sha" "text/plain"
  , Tuple "sha1" "text/plain"
  , Tuple "shtml" "text/html"
  , Tuple "svg" "image/svg+xml"
  , Tuple "svgz" "image/svg+xml"
  , Tuple "swf" "application/x-shockwave-flash"
  , Tuple "tgz" "application/octet-stream"
  , Tuple "torrent" "application/x-bittorrent"
  , Tuple "var" "text/plain"
  , Tuple "wav" "audio/x-wav"
  , Tuple "wax" "audio/x-ms-wax"
  , Tuple "wm" "video/x-ms-wm"
  , Tuple "wma" "audio/x-ms-wma"
  , Tuple "wmd" "application/x-ms-wmd"
  , Tuple "wmv" "video/x-ms-wmv"
  , Tuple "wmx" "video/x-ms-wmx"
  , Tuple "wmz" "application/x-ms-wmz"
  , Tuple "wvx" "video/x-ms-wvx"
  , Tuple "xbm" "image/x-xbitmap"
  , Tuple "xhtml" "application/xhtml+xml"
  , Tuple "xls" "application/octet-stream"
  , Tuple "xml" "text/xml"
  , Tuple "xrdf" "application/xrds+xml"
  , Tuple "zip" "application/zip"
  ]

serveFile ∷ ∀ m. MonadAff m ⇒ String → m HTTPure.Response.Response
serveFile fileName = do
  let
    ext = last $ split (Pattern ".") fileName
    contentType = maybe "*/*" identity (ext >>= flip lookup htaccess)
    headers = HTTPure.header "Content-Type" contentType
  let
    notFoundHandler = const $ do
      log $ "File not found: " <> fileName
      HTTPure.notFound
  -- | TODO: make this efficient
  liftAff
    $ ((FS.readFile fileName ∷ Aff Buffer) >>= HTTPure.ok' headers)
    `catchError` notFoundHandler

static ∷ ∀ m. MonadAff m ⇒ String → Array String → m HTTPure.Response.Response
static dir subpath =
  serveFile $ String.joinWith "/" ([dir] <> subpath)

type Options =
  { dir ∷ String
  , port ∷ Opt Int
  }

-- | Use this version when calling from PS
safe ∷ ∀ opts. Coerce opts Options ⇒ opts → Effect (Effect Unit)
safe opts = do
  let
    opts' = coerce opts ∷ Options
    port = opts'.port ! 8000
    msg = "Serving static files from " <> opts'.dir <> " on: http://0.0.0.0:" <> show port

  close ← HTTPure.serve port (\req → static opts'.dir req.path) (log msg)
  pure (close $ pure unit)

-- | Use this version when calling directly from JS
unsafe ∷ ∀ opts. opts → Effect (Effect Unit)
unsafe opts = safe (unsafeCoerce opts ∷ Options)

