module Main where

import Text.XML.HXT.Core
import System.Environment

readParams::IO (String, String)
readParams = do 
  args <- getArgs
  let defaultSrc = "methods.xml"
      defaultDst = "output.docx" 
  return $ case args of    
    [src]      -> (src, defaultDst)
    [src, dst] -> (src, dst)
    _other     -> (defaultSrc, defaultDst)

w::ArrowXml a=>String->[a XmlTree XmlTree]->[a XmlTree XmlTree]->a XmlTree XmlTree
w = mkqelem . flip (mkQName "w") wordNS

we::ArrowXml a=>String->a XmlTree XmlTree->a XmlTree XmlTree
we name child = w name [] [child]  

wordNS::String
wordNS = "http://schemas.microsoft.com/office/word/2003/wordml"

result::ArrowXml a=>a XmlTree XmlTree
result = 
  w "wordDocument" [] []
    >>> attachNsEnv env
    += body
  where
    env = toNsEnv [("w", wordNS)]
    children = getChildren >>> placeName
    body = we "body" . we "p" . we "r" $ children
    

placeName::ArrowXml a=>a XmlTree XmlTree
placeName =
  isElem 
  >>>
  w "t" [] [
    getAttrValue "name"
    >>>  mkText
    ]


main::IO ()
main = do
  (src, dst) <- readParams
  _ <- runX $
    readDocument [withValidate no] src
    >>> root [] [ deep ( isElem >>> hasName "types" >>> result) ]
    >>> writeDocument [withIndent yes] dst 
  return ()

