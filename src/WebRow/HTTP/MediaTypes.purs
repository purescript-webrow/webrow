module WebRow.HTTP.MediaTypes
  ( known
  , module MediaType
  , parse
  )
  where

import Data.Array (elem) as Array
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.MediaType.Common as Common
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON, applicationJavascript, applicationOctetStream, applicationXML, imageGIF, imageJPEG, imagePNG, multipartFormData, textCSV, textHTML, textPlain, textXML, textCSS) as MediaType

known ∷ Array MediaType
known =
  [ Common.applicationFormURLEncoded
  , Common.applicationJSON
  , Common.applicationJavascript
  , Common.applicationOctetStream
  , Common.applicationXML
  , Common.imageGIF
  , Common.imageJPEG
  , Common.imagePNG
  , Common.multipartFormData
  , Common.textCSV
  , Common.textHTML
  , Common.textPlain
  , Common.textXML
  , Common.textCSS
  ]

parse ∷ String → Maybe MediaType
parse name =
  let
    possible = MediaType name
  in if possible `Array.elem` known
    then Just possible
    else Nothing
