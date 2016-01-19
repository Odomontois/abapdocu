module WordProcessing where

import Text.XML.HXT.Core

wordNS::String
wordNS = "http://schemas.microsoft.com/office/word/2003/wordml"

type Process = IOStateArrow () XmlTree XmlTree


w::String->[Process]->[Process]->Process
w = mkqelem . flip (mkQName "w") wordNS

we::String->Process->Process
we name child = w name [] [child] 

wa::String->String->Process
wa = sqattr . flip (mkQName "w") wordNS

result::Process->Process
result content = w "wordDocument" [sattr "xmlns:w" wordNS] [content]

paragraph::String->Process->Process
paragraph style text = w "p" [] [
  we "pPr" $ w "pStyle" [wa "val" style] [],
  we  "r"  $ we "t"  text 
  ]  

wtbl::String->Process->Process->Process
wtbl style props rows = table where
  table = w "tbl" [] [
    w "tblPr" [] [
      w "tblStyle" [wa "val" style] [],
      props], 
    rows
    ]

wtblBorders::Integer->Process
wtblBorders size = w "tblBorders" [] $ map border sides where
  sides = ["top", "left", "bottom", "right", "insideH", "insideV"]
  border side = 
    w side [
      wa "val"   "single",
      wa "sz"    $ show size,
      wa "space" "0",
      wa "color" "auto"
      ] []







