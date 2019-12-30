import Data.List
import Data.Char

main = do
  let parts = words . map (\c -> if c=='_' then ' ' else c)

  let wordparts = takeWhile (all Data.Char.isAlpha) . init . parts

  fs <- readFile "gf-wordnet/WordNet.gf" >>= return . map words . lines

  let bcat c = case c of 'V':_ -> "V" ; _ -> takeWhile Data.Char.isAlpha c

  let cwafs = [((takeWhile Data.Char.isLower (concat (take 1 (wordparts f))),bcat c), (f,c)) | "fun":f:":":c:_ <- fs ]

  let gcwafs = [fcw | fcw:_ <- Data.List.groupBy (\ x y -> fst x == fst y) (Data.List.sortOn fst cwafs)]
  let gfwafs = [(f++"_"++c,c,w) | ((f,c),(w,_)) <- gcwafs, not (null f) ]


  writeFile "JustWordsWordNet.tmp" $ unlines [unwords ["fun",f,":",c,";"] | (f,c,_) <- gfwafs]

  writeFile "JustWordsWordNetEng.tmp" $
    unlines [unwords ["lin",f,"=", wrap ++ "WN."++w,";"] |
      (f,c,w) <- gfwafs, let wps = parts w, let wrap = if last wps == "VV" then "vvV " else ""]

