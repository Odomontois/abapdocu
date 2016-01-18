{-#LANGUAGE TypeSynonymInstances#-}

module Main where

import Text.XML.HXT.Core
import WordProcessing
import System.Environment

readParams::IO (String, String)
readParams = do 
  args <- getArgs
  let defaultSrc = "methods.xml"
      defaultDst = "output.docx.xml" 
  return $ case args of    
    [src]      -> (src, defaultDst)
    [src, dst] -> (src, dst)
    _other     -> (defaultSrc, defaultDst)

instance WordProcessing (IOSLA s) where
  processChild =
    isElem
    >>> (hasName "class" <+> hasName "interface")
    >>> paragraph name
    where 
      paragraph =  we "p" . we "r" . we "t" 
      name = getAttrValue "name" >>>  mkText

main::IO ()
main = do
  (src, dst) <- readParams
  _ <- runX $
    readDocument [withValidate no] src
    >>> root [] [ getChildren >>> isElem >>> hasName "types" >>> result]
    >>> writeDocument [withIndent yes] dst 
  return ()

