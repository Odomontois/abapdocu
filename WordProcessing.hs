module WordProcessing where

import Text.XML.HXT.Core

wordNS::String
wordNS = "http://schemas.microsoft.com/office/word/2003/wordml"

class ArrowXml a => WordProcessing a where
  w::String->[a XmlTree XmlTree]->[a XmlTree XmlTree]->a XmlTree XmlTree
  w = mkqelem . flip (mkQName "w") wordNS

  we::String->a XmlTree XmlTree->a XmlTree XmlTree
  we name child = w name [] [child] 

  wa::String->String->a XmlTree XmlTree
  wa = sqattr . flip (mkQName "w") wordNS

  result::a XmlTree XmlTree
  result = 
    w "wordDocument" [sattr "xmlns:w" wordNS] [body children]
    where
      children = getChildren >>> placeName
      body = we "body" 

  placeName::a XmlTree XmlTree
  placeName =
    isElem
    >>> (hasName "class" <+> hasName "interface")
    >>> paragraph name
    where 
      paragraph =  we "p" . we "r" . we "t" 
      name = getAttrValue "name" >>>  mkText


instance WordProcessing (IOSLA s)