{-#LANGUAGE ViewPatterns#-}
module Main where

import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XmlNode
import System.Environment

readParams::IO (String, String)
readParams = do 
  args <- getArgs
  let defaultSrc = "methods.xml"
      defaultDst = "output.xml" 
  return $ case args of    
    [src]      -> (src, defaultDst)
    [src, dst] -> (src, dst)
    _other     -> (defaultSrc, defaultDst)

-- nice::NTree->Bool
-- nice node = True

main::IO ()
main = do
  (src, dst) <- readParams
  _ <- runX $
    readDocument [withValidate no] src
    >>>
    root [] [ deep ( isElem >>> hasName "types" >>> process) ]
    >>>
    writeDocument[withIndent yes] dst 
  return ()
  where
    types = mkName "rororo"
    process = eelem "rororo"

