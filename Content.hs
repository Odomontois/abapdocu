module Content where

import Text.XML.HXT.Core
import WordProcessing

content::Process XmlTree
-- @The content of document
content = styles <+> body where
  fieldVal name =
    getChildren
    >>> isElem
    >>> hasName name
    >>> getChildren
    >>> isText

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
    >>>  methodHeading
      <+> static
      <+> exposure
      <+> handling
      <+> descr
      <+> params

  methodHeading = paragraph "Heading2" text where
    text = name  >>> mkText
    name = constA "метод " <+> getAttrValue "name"

  static = paragraph "Bold" text where
    text = hasAttr "static" >>> constA " (статический)" >>> mkText

  exposure = getAttrValue "exposure" >>> exposures where
    exposures = public <+> protected <+> private
    expTxt val style text = isA (== val) >>> paragraph style (txt text)
    public    = expTxt "public"    "Green"  "общий"
    protected = expTxt "protected" "Yellow" "защищённый"
    private   = expTxt "private"   "Red"    "личный"

  handling = hasAttr "event_handler" >>> paragraph "Italic" text where
    text = txt "обработчик события "
      <+> fieldVal "refName"
      <+> txt " класса "
      <+> fieldVal "refClass"

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
