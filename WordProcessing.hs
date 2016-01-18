module WordProcessing where

import Text.XML.HXT.Core

wordNS::String
wordNS = "http://schemas.microsoft.com/office/word/2003/wordml"

type Process a = a XmlTree XmlTree

class ArrowXml a => WordProcessing a where

  w::String->[Process a]->[Process a]->Process a
  w = mkqelem . flip (mkQName "w") wordNS

  we::String->Process a->Process a
  we name child = w name [] [child] 

  wa::String->String->Process a
  wa = sqattr . flip (mkQName "w") wordNS

  result::Process a->Process a
  result content = w "wordDocument" [sattr "xmlns:w" wordNS] [content]
    -- where
    --   children = getChildren >>> processChild
    --   body = we "body" 


