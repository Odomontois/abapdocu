{-#LANGUAGE TypeSynonymInstances, DeriveDataTypeable, RecordWildCards #-}

module Main where

import Text.XML.HXT.Core
import System.Console.CmdArgs

import WordProcessing
import Content

data Params = Params {src::FilePath, dst::FilePath, noIndent::Bool}
  deriving (Data,Typeable,Show,Eq)

params::Params
params = Params {
  src      = "methods.xml"   &= typFile &= help "source XML"    ,
  dst      = "docu.docx.xml" &= typFile &= help "result DOCX"   ,
  noIndent = def                        &= help "no indentation"
}

main::IO ()
main = do
  Params{..} <- cmdArgs params
  let indent = not noIndent
  _ <- runX $
    readDocument [withValidate no] src
    >>> root [] [content]
    >>> writeDocument [withIndent indent] dst
  return ()
