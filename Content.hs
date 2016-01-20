{-#LANGUAGE RankNTypes #-}

module Content where

import Text.XML.HXT.Core
import WordProcessing
import qualified Message

type XmlProc= forall a.ArrowXml a=>a XmlTree XmlTree

fieldText::String->XmlProc
fieldText name =
  getChildren
  >>> isElem
  >>> hasName name
  >>> getChildren
  >>> isText

fieldVal::ArrowXml a =>String -> a XmlTree String
fieldVal name = fieldText name >>> getText

content::Process XmlTree
-- The content of document
content = top where

  top = getChildren
    >>> isElem
    >>> hasName "types"
    >>> wordDocument sections

  sections = styles <+> body

  styles = readDocument [] "styles.xml" >>> getChildren

  body = we "body" (getChildren >>> clifDocu)

  normal = paragraph "Normal"
  bold   = paragraph "Bold"

  clifDocu =
    isElem >>>
    (hasName "class" <+> hasName "interface") >>>
    clifHeading <+> descr <+> methods

  clifHeading = paragraph "Heading1" text where
    text          = kind >>> mkText
    name          = getAttrValue "name"
    kind          = kindClass <+> kindInterface
    kindClass     = hasName "class"     >>> name >>^ Message.className
    kindInterface = hasName "interface" >>> name >>^ Message.intfName

  methods =
    getChildren       >>>
    isElem            >>>
    hasName "method"  >>>
    methodHeading
      <+> static
      <+> exposure
      <+> handling
      <+> descr
      <+> params

  methodHeading = paragraph "Heading2" text where
    text = name  >>> mkText
    name = getAttrValue "name" >>^ Message.mtdName

  static = paragraph "Bold" text where
    text = hasAttr "static" >>> txt Message.static

  exposure = getAttrValue "exposure" >>> exposures where
    exposures = public <+> protected <+> private
    expTxt val style text = isA (== val) >>> paragraph style (txt text)
    public    = expTxt "public"    "Green"  Message.public
    protected = expTxt "protected" "Yellow" Message.protected
    private   = expTxt "private"   "Red"    Message.private

  handling = hasAttr "event_handler" >>> paragraph "Italic" text where
    text   = evtCls >>> arr2 Message.eventHandler >>> mkText
    evtCls = fieldVal "refName" &&& fieldVal "refClass"

  descr  = normal  $ fieldText "description"

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
      fieldVal "kind" >>>
      Message.paramKind ^>>
      mkText
