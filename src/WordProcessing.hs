{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}

module WordProcessing where

import           Text.XML.HXT.Core

wordNS::String
wordNS = "http://schemas.microsoft.com/office/word/2003/wordml"

w::ArrowXml a=>String->[a b XmlTree]->[a b XmlTree]->a b XmlTree
w = mkqelem . flip (mkQName "w") wordNS

we::ArrowXml a=>String->a b XmlTree->a b XmlTree
we name child = w name [] [child]

wa::ArrowXml a=>String->String->a b XmlTree
wa = sqattr . flip (mkQName "w") wordNS

wordDocument::ArrowXml a=>a b XmlTree->a b XmlTree
wordDocument content = w "wordDocument" [sattr "xmlns:w" wordNS] [content]

paragraph::ArrowXml a=>String->a b XmlTree->a b XmlTree
paragraph style text =
  w "p" [] [
    we "pPr" $ w "pStyle" [wa "val" style] [],
    we  "r"  $ we "t"  text ]

wtbl::ArrowXml a=>String->a b XmlTree->a b XmlTree->a b XmlTree
wtbl style props rows =
  w "tbl" [] [
    w "tblPr" [] [
      w "tblStyle" [wa "val" style] [],
      props] ,
    rows ]

wtblBorders::ArrowXml a=>Integer->a b XmlTree
wtblBorders size = w "tblBorders" [] $ map border sides where
  sides = ["top", "left", "bottom", "right", "insideH", "insideV"]
  border side =
    w side [
      wa "val"   "single",
      wa "sz"    $ show size,
      wa "space" "0",
      wa "color" "auto"
      ] []
