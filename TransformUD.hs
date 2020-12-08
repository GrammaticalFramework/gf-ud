module TransformUD where

import UDConcepts

-- build a string transformer from a transform function
transUD :: ([UDSentence] -> [UDSentence]) -> String -> String
transUD trans = prt . trans . parseUDText

-- transform functions on different levels

-- change sentences individually, as lists of words
transByUDWordLines :: ([UDWord] -> [UDWord]) -> ([UDSentence] -> [UDSentence])
transByUDWordLines tws = map (\uds -> uds{udWordLines = tws (udWordLines uds)})

-- change sentences individually, first analysing them to trees
transByUDTree :: (UDTree -> UDTree) -> ([UDSentence] -> [UDSentence])
transByUDTree tt = map apptt
  where
    apptt = udTree2sentence . tt . udSentence2tree

-- change words individually, e.g. their POS tag
transByUDWord :: (UDWord -> UDWord) -> ([UDSentence] -> [UDSentence])
transByUDWord tw = transByUDWordLines (map tw)

