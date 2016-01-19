module WordProcessing where

import Text.XML.HXT.Core

wordNS::String
wordNS = "http://schemas.microsoft.com/office/word/2003/wordml"

type Process a = IOStateArrow () a XmlTree

w::String->[Process a]->[Process a]->Process a
w = mkqelem . flip (mkQName "w") wordNS

we::String->Process a->Process a
we name child = w name [] [child]

wa::String->String->Process a
wa = sqattr . flip (mkQName "w") wordNS

result::Process a->Process a
result content = w "wordDocument" [sattr "xmlns:w" wordNS] [content]

paragraph::String->Process a->Process a
paragraph style text = w "p" [] [
  we "pPr" $ w "pStyle" [wa "val" style] [],
  we  "r"  $ we "t"  text ]

wtbl::String->Process a->Process a->Process a
wtbl style props rows = table where
  table = w "tbl" [] [
    w "tblPr" [] [
      w "tblStyle" [wa "val" style] [],
      props] ,
    rows ]

wtblBorders::Integer->Process a
wtblBorders size = w "tblBorders" [] $ map border sides where
  sides = ["top", "left", "bottom", "right", "insideH", "insideV"]
  border side =
    w side [
      wa "val"   "single",
      wa "sz"    $ show size,
      wa "space" "0",
      wa "color" "auto"
      ] []
