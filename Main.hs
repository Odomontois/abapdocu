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


main::IO ()
main = do
  (src, dst) <- readParams
  _ <- runX $
    readDocument [withValidate no] src
    >>> root [] [ getChildren >>> isElem >>> hasName "types" >>> result]
    >>> writeDocument [withIndent yes] dst 
  return ()

