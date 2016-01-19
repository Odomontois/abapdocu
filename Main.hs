{-#LANGUAGE TypeSynonymInstances#-}

module Main where

import Text.XML.HXT.Core
import WordProcessing
import System.Environment 

content::Process
content = styles <+> body where
  styles = readDocument [] "styles.xml" >>> getChildren

  body = we "body" (getChildren >>> clifDocu)

  normal = paragraph "Normal"
  bold   = paragraph "Bold"

  clifDocu =
    isElem
    >>> (hasName "class" <+> hasName "interface")
    >>> clifHeading <+> descr <+> methods

  clifHeading = paragraph "Heading1" text where 
    text          = name >>> mkText 
    name          = kind <+> getAttrValue "name"
    kind          = kindClass <+> kindInterface
    kindClass     = hasName "class"     >>> constA "Класс "
    kindInterface = hasName "interface" >>> constA "Интерфейс "

  methods = 
    getChildren
    >>> isElem
    >>> hasName "method"
    >>> methodHeading <+> descr <+> params

  methodHeading = paragraph "Heading2" text where
    text = constA "метод " <+> getAttrValue "name" >>> mkText

  fieldVal name = 
    getChildren 
    >>> isElem
    >>> hasName name 
    >>> getChildren 
    >>> isText

  descr  = normal  $ fieldVal "description"

  params = wtbl "TableNormal" borders rows where
    borders = wtblBorders 4
    rows = 
      getChildren 
      >>> isElem
      >>> hasName "param"
      >>> we "tr" cells
    cells = catA $ map (we "tc") [name, kind, descr]
    name   = bold $ getAttrValue "name" >>> mkText
    kind   = normal $
      fieldVal "kind"
      >>> getText
      >>> arr kindNm
      >>> mkText

  kindNm "importing" = "Импорт"
  kindNm "exporting" = "Экспорт"
  kindNm "changing"  = "Изменение"
  kindNm "returning" = "Возврат"
  kindNm _           = ""


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
    >>> root [] [ getChildren >>> isElem >>> hasName "types" >>> result content]
    >>> writeDocument [withIndent yes] dst 
  return ()

