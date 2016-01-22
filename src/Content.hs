{-# LANGUAGE Arrows     #-}
{-# LANGUAGE RankNTypes #-}

module Content (content) where

import qualified Message
import           Text.XML.HXT.Core
import           WordProcessing

fieldText::ArrowXml a =>String->a XmlTree XmlTree
fieldText name =
  getChildren
  >>> isElem
  >>> hasName name
  >>> getChildren
  >>> isText

fieldVal::ArrowXml a =>String->a XmlTree String
fieldVal name = fieldText name >>> getText

normal, bold::ArrowXml a=>a b XmlTree->a b XmlTree
normal = paragraph "Normal"
bold   = paragraph "Bold"

descr, params, handling, exposure, static, methodHeading, methods, clifHeading, clifDocu::ArrowXml a=>a XmlTree XmlTree

descr  = normal  $ fieldText "description"

eqA::(ArrowList a, Eq b)=>b->a b b
eqA = isA . (==)

p::ArrowXml a=>String->a b String->a b XmlTree
p name = paragraph name . (>>> mkText)

content::ArrowXml a=>a XmlTree XmlTree->a XmlTree XmlTree
content styles = top where

  top = getChildren
    >>> isElem
    >>> hasName "types"
    >>> wordDocument sections

  sections = styles <+> body

  body = we "body" (getChildren >>> clifDocu)

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

methodHeading = p "Heading2" name where
  name = getAttrValue "name" >>^ Message.mtdName

static = p "Bold" text where
  text = hasAttr "static" >>> constA Message.static

exposure = getAttrValue "exposure" >>> exposures where
  exposures = public <+> protected <+> private
  expTxt val style text = eqA val >>> p style (constA text)
  public    = expTxt "public"    "Green"  Message.public
  protected = expTxt "protected" "Yellow" Message.protected
  private   = expTxt "private"   "Red"    Message.private

handling = block where
  text = proc method -> do
    cls <- fieldVal "refClass" -< method
    mtd <- fieldVal "refName"  -< method
    returnA -< Message.eventHandler mtd cls
  block = proc xml -> do
    handler <- hasAttr "event_handler" -< xml
    p "Italic" text -< handler

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
