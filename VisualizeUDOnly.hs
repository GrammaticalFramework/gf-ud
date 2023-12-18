----------------------------------------------------------------------
-- Based on
-- https://github.com/GrammaticalFramework/gf-ud/blob/master/VisualizeUDOnly.hs
-- as pruned from
-- https://github.com/GrammaticalFramework/gf-core/blob/master/src/runtime/haskell/PGF/VisualizeTree.hs
-- Work by Aarne Ranta, Björn Bringert, Peter Ljunglöf, Thomas Hallgren, and others
-- 
-----------------------------------------------------------------------------

module VisualizeUDOnly
             ( 
                conlls2latexDoc
              , conlls2svgHTMLDoc
             ) where
import Prelude hiding ((<>)) -- GHC 8.4.1 clash with Text.PrettyPrint

import qualified Data.Map as Map
--import qualified Data.IntMap as IntMap
import Data.List (intersperse,nub,mapAccumL,find,groupBy,sortBy,partition)
import Data.Ord (comparing)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Text.PrettyPrint
import System.Environment (getArgs)

---------------------- should be a separate module?

-- visualization with SVG and LaTeX output. AR Nov 2015 - Dec 2023

main = do
  tgt <- getArgs
  interact (wrap (mk tgt))
  where
    mk tgt = case tgt of
      ["latex"] -> conlls2latexDoc
      ["svg"] -> conlls2svgHTMLDoc
      _ -> error ("usage: VisualizeUD (latex | svg)")
    wrap mk = mk . map unlines . stanzas . lines
    stanzas ls = case break null ls of
      (l1, _:l2) -> l1 : stanzas l2
      _ -> [ls]


conlls2latexDoc :: [String] -> String
conlls2latexDoc =
  render .
  latexDoc .
  vcat .
  intersperse (text "" $+$ app "vspace" (text "4mm")) .
  map conll2latex .
  filter (not . null)

conlls2svgHTMLDoc :: [String] -> String
conlls2svgHTMLDoc =
  render .
  embedInHTML .
  map conll2svg .
  filter (not . null)

-- toSVG :: [LaTeX] -> [SVG]

conll2svg :: String -> Doc
conll2svg = ppSVG . toSVG . conll2latex' . parseCoNLL

conll2latex :: String -> Doc
conll2latex = ppLaTeX . conll2latex' . parseCoNLL

conll2latex' :: CoNLL -> [LaTeX]
conll2latex' = dep2latex . conll2dep'

data Dep = Dep {
    wordLength  :: Int -> Double        -- length of word at position int       -- was: fixed width, millimetres (>= 20.0)
  , tokens      :: [(String,(String,String))]    -- word, (pos,features) (0..)
  , deps        :: [((Int,Int),String)] -- from, to, label
  , root        :: Int                  -- root word position
  }

-- some general measures
defaultWordLength = 20.0  -- the default fixed width word length, making word 100 units
defaultUnit       = 0.2   -- unit in latex pictures, 0.2 millimetres
spaceLength       = 10.0
charWidth = 1.8

wsize rwld  w  = 100 * rwld w + spaceLength                   -- word length, units
wpos rwld i    = sum [wsize rwld j | j <- [0..i-1]]           -- start position of the i'th word
wdist rwld x y = sum [wsize rwld i | i <- [min x y .. max x y - 1]]    -- distance between words x and y
labelheight h  = h + arcbase + 3    -- label just above arc; 25 would put it just below
labelstart c   = c - 15.0           -- label starts 15u left of arc centre
arcbase        = 30.0               -- arcs start and end 40u above the bottom
arcfactor r    = r * 600            -- reduction of arc size from word distance
xyratio        = 3                  -- width/height ratio of arcs

putArc :: (Int -> Double) -> Int -> Int -> Int -> String -> [DrawingCommand]
putArc frwld height x y label = [oval,arrowhead,labelling] where
  oval = Put (ctr,arcbase) (OvalTop (wdth,hght))
  arrowhead = Put (endp,arcbase + 5) (ArrowDown 5)   -- downgoing arrow 5u above the arc base
  labelling = Put (labelstart ctr,labelheight (hght/2)) (TinyText label)
  dxy  = wdist frwld x y             -- distance between words, >>= 20.0
  ndxy = 100 * rwld * fromIntegral height  -- distance that is indep of word length
  hdxy = dxy / 2                     -- half the distance
  wdth = dxy - (arcfactor rwld)/dxy  -- longer arcs are wider in proportion
  hght = ndxy / (xyratio * rwld)      -- arc height is independent of word length
  begp = min x y                     -- begin position of oval
  ctr  = wpos frwld begp + hdxy + (if x < y then 20 else  10)  -- LR arcs are farther right from center of oval
  endp = (if x < y then (+) else (-)) ctr (wdth/2)            -- the point of the arrow
  rwld = 0.5 ----

dep2latex :: Dep -> [LaTeX]
dep2latex d =
  [Comment (unwords (map fst (tokens d))),
   Picture defaultUnit (width,height) (
     [Put (wpos rwld i,0) (Text w) | (i,w) <- zip [0..] (map fst (tokens d))]   -- words
  ++ [Put (wpos rwld i,15) (TinyText w) | (i,(w,_)) <- zip [0..] (map snd (tokens d))]   -- pos tags 15u above bottom
---  ++ [Put (wpos rwld i,-15) (TinyText w) | (i,(_,w)) <- zip [0..] (map snd (tokens d))]   -- features 15u below bottom -> DON'T SHOW
  ++ concat [putArc rwld (aheight x y) x y label | ((x,y),label) <- deps d]    -- arcs and labels
  ++ [Put (wpos rwld (root d) + 15,height) (ArrowDown (height-arcbase))]
  ++ [Put (wpos rwld (root d) + 20,height - 10) (TinyText "root")]
  )]
 where
   wld i  = wordLength d i  -- >= 20.0
   rwld i = (wld i) / defaultWordLength       -- >= 1.0
   aheight x y = depth (min x y) (max x y) + 1    ---- abs (x-y)
   arcs = [(min u v, max u v) | ((u,v),_) <- deps d]
   depth x y = case [(u,v) | (u,v) <- arcs, (x < u && v <= y) || (x == u && v < y)] of ---- only projective arcs counted
     [] -> 0
     uvs -> 1 + maximum (0:[depth u v | (u,v) <- uvs])
   width = {-round-} (sum [wsize rwld w | (w,_) <- zip [0..] (tokens d)]) + {-round-} spaceLength * fromIntegral ((length (tokens d)) - 1)
   height = 50 + 20 * {-round-} (maximum (0:[aheight x y | ((x,y),_) <- deps d]))

type CoNLL = [[String]]
parseCoNLL :: String -> CoNLL
parseCoNLL = map words . filter ((/="#") . take 1) . lines

--conll2dep :: String -> Dep
--conll2dep = conll2dep' . parseCoNLL

readInt :: String -> Int
readInt s = if all isDigit s then read s else error ("not int " ++ s)

conll2dep' :: CoNLL -> Dep
conll2dep' ls = Dep {
    wordLength = wld 
  , tokens = toks
  , deps = dps
  , root = head $ [readInt x-1 | x:_:_:_:_:_:"0":_ <- ls] ++ [1]
  }
 where
   wld i = maximum (0:[charWidth * fromIntegral (length w) | w <- let (tok,(pos,feat)) = toks !! i in [tok,pos {-,feat-}]]) --- feat not shown
   toks = [(w,(c,m)) | _:w:_:c:_:m:_ <- ls]
   dps = [((readInt y-1, readInt x-1),lab) | x:_:_:_:_:_:y:lab:_ <- ls, y /="0"]
   --maxdist = maximum [abs (x-y) | ((x,y),_) <- dps]


-- * LaTeX Pictures (see https://en.wikibooks.org/wiki/LaTeX/Picture)

-- We render both LaTeX and SVG from this intermediate representation of
-- LaTeX pictures.

data LaTeX = Comment String | Picture UnitLengthMM Size [DrawingCommand]
data DrawingCommand = Put Position Object
data Object = Text String | TinyText String | OvalTop Size | ArrowDown Length

type UnitLengthMM = Double
type Size = (Double,Double)
type Position = (Double,Double)
type Length = Double


-- * latex formatting
ppLaTeX = vcat . map ppLaTeX1
  where
    ppLaTeX1 el =
      case el of
        Comment s -> comment s
        Picture unit size cmds ->
          app "setlength{\\unitlength}" (text (show unit ++ "mm"))
          $$ hang (app "begin" (text "picture")<>text (show size)) 2
                  (vcat (map ppDrawingCommand cmds))
          $$ app "end" (text "picture")
          $$ text ""

    ppDrawingCommand (Put pos obj) = put pos (ppObject obj)

    ppObject obj =
      case obj of
        Text s -> text s
        TinyText s -> small (text s)
        OvalTop size -> text "\\oval" <> text (show size) <> text "[t]"
        ArrowDown len -> app "vector(0,-1)" (text (show len))

    put p@(_,_) = app ("put" ++ show p)
    small w = text "{\\tiny" <+> w <> text "}"
    comment s = text "%%" <+> text s -- line break show follow
    
app macro arg = text "\\" <> text macro <> text "{" <> arg <> text "}"


latexDoc :: Doc -> Doc
latexDoc body =
  vcat [text "\\documentclass{article}",
        text "\\usepackage[a4paper,margin=0.5in,landscape]{geometry}",
        text "\\usepackage[utf8]{inputenc}",
        text "\\begin{document}",
        body,
        text "\\end{document}"]

-- * SVG (see https://www.w3.org/Graphics/SVG/IG/resources/svgprimer.html)

-- | Render LaTeX pictures as SVG
toSVG :: [LaTeX] -> [SVG]
toSVG = concatMap toSVG1
  where
    toSVG1 el =
      case el of
        Comment s -> []
        Picture unit size@(w,h) cmds ->
          [Elem "svg" ["width".=x1,"height".=y0+5,
                       ("viewBox",unwords (map show [0,0,x1,y0+5])),
                       ("version","1.1"),
                       ("xmlns","http://www.w3.org/2000/svg")]
                       (white_bg:concatMap draw cmds)]
          where
            white_bg =
              Elem "rect" ["x".=0,"y".=0,"width".=x1,"height".=y0+5,
                           ("fill","white")] []

            draw (Put pos obj) = objectSVG pos obj

            objectSVG pos obj =
              case obj of
                Text s -> [text 16 pos s]
                TinyText s -> [text 10 pos s]
                OvalTop size -> [ovalTop pos size]
                ArrowDown len -> arrowDown pos len

            text h (x,y) s =
              Elem "text" ["x".=xc x,"y".=yc y-2,"font-size".=h]
                          [CharData s]

            ovalTop (x,y) (w,h) =
              Elem "path" [("d",path),("stroke","black"),("fill","none")] []
              where
                x1 = x-w/2
                x2 = min x (x1+r)
                x3 = max x (x4-r)
                x4 = x+w/2
                y1 = y
                y2 = y+r
                r = h/2
                sx = show . xc
                sy = show . yc
                path = unwords (["M",sx x1,sy y1,"Q",sx x1,sy y2,sx x2,sy y2,
                                 "L",sx x3,sy y2,"Q",sx x4,sy y2,sx x4,sy y1])

            arrowDown (x,y) len =
                [Elem "line" ["x1".=xc x,"y1".=yc y,"x2".=xc x,"y2".=y2,
                              ("stroke","black")] [],
                 Elem "path" [("d",unwords arrowhead)] []]
               where
                 x2 = xc x
                 y2 = yc (y-len)
                 arrowhead = "M":map show [x2,y2,x2-3,y2-6,x2+3,y2-6]

            xc x = num x+5
            yc y = y0-num y
            x1 = num w+10
            y0 = num h+20
            num x = round (scale*x)
            scale = unit*5

            infix 0 .=
            n.=v = (n,show v)

-- * SVG is XML

data SVG = CharData String | Elem TagName Attrs [SVG]
type TagName = String
type Attrs = [(String,String)]

embedInHTML svgs =
  vcat $
     [
      text "<!DOCTYPE html>",
      text "<html>",
      text "<body>",
      vcat svgs,
      text "</body>",
      text "</html>"
     ] 

addHeaderSVG svgs =
  vcat $
       [text "<?xml version=\"1.0\" standalone=\"no\"?>",
        text "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"",
        text "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">",
        text ""] ++
        svgs -- It should be a single <svg> element...


ppSVG svg =
  vcat (map ppSVG1 svg) -- It should be a single <svg> element...
  where
    ppSVG1 svg1 =
      case svg1 of
        CharData s -> text (encode s)
        Elem tag attrs [] ->
            text "<"<>text tag<>cat (map attr attrs) <> text "/>"
        Elem tag attrs svg ->
            cat [text "<"<>text tag<>cat (map attr attrs) <> text ">",
                 nest 2 (cat (map ppSVG1 svg)),
                 text "</"<>text tag<>text ">"]

    attr (n,v) = text " "<>text n<>text "=\""<>text (encode v)<>text "\""

    encode s = foldr encodeEntity "" s

    encodeEntity = encodeEntity' (const False)
    encodeEntity' esc c r =
      case c of
        '&' -> "&amp;"++r
        '<' -> "&lt;"++r
        '>' -> "&gt;"++r
        _ -> c:r
