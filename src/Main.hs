{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import           Data.List
import           System.Console.CmdArgs
import           Text.XML.HXT.Core

import           Content
import           WordProcessing

data Params = Params {src::FilePath, dst::FilePath, noIndent::Bool}
  deriving (Data,Typeable,Show,Eq)

params::Params
params = Params {
  src      = "methods.xml"   &= typFile &= help "source XML"    ,
  dst      = "docu.docx.xml" &= typFile &= help "result DOCX"   ,
  noIndent = def                        &= help "no indentation"
}

styles :: IOStateArrow s a XmlTree
styles = readDocument [] "styles.xml" >>> getChildren

main::IO ()
main = do
  Params{..} <- cmdArgs params
  let indent = not noIndent
  runX $
    readDocument [withValidate no] src
    >>> root [] [content styles]
    >>> writeDocument [withIndent indent] dst
  return ()
