module CSE230.Shapes where

import CSE230.List
import CSE230.Graphics
import Htdp

-------------------------------------------------------------------------------
main :: IO ()
-------------------------------------------------------------------------------
main = do
  mkRainbow
  mkChess1
  mkChess2
  mkTriangle1
  mkTriangle2
  mkCarpet

-------------------------------------------------------------------------------
-- | Rainbow with 'map'
-------------------------------------------------------------------------------
mkRainbow :: IO ()
mkRainbow = save "img/rainbow.png" rainbow

-- overlay - overlay the shape upon the original image
-- list!!x - indices access like list[x]
-- circle r t c - draw a circle with radius r, type t and color c

rainbow :: Image
rainbow = foldr1 f xs
  where
    xs  = map g [1..7]
    f   = overlay
    g x = circle (x*20) solid (rainbow_colors!!(round (x-1)))
    rainbow_colors = [violet, blue, cyan, green, yellow, orange, red]

-------------------------------------------------------------------------------
-- | ChessBoard with 'clone'
-------------------------------------------------------------------------------
mkChess1 :: IO ()
mkChess1   = save "img/chess1.png"   chessBoard1

chessBoard1 :: Image
chessBoard1 = aboves (clone 4 row)
  where
    row     = besides (clone 4 gridSquare)

gridSquare :: Image
gridSquare = aboves [ besides [ whSq, blSq ]
                    , besides [ blSq, whSq ] ]
  where
    whSq   = square 50 solid bgCol
    blSq   = square 50 solid fgCol

-------------------------------------------------------------------------------
-- | ChessBoard with `iter`
-------------------------------------------------------------------------------
mkChess2 :: IO ()
mkChess2   = save "img/chess2.png"   chessBoard2

chessBoard2 :: Image
chessBoard2 = iter 2 f base
  where
    f     x = aboves [ besides [ x, x ]
                     , besides [ x, x ] ]
    base    = gridSquare


-------------------------------------------------------------------------------
-- | Sierpinski Triangle with recursion
-------------------------------------------------------------------------------
mkTriangle1 :: IO ()
mkTriangle1 = save "img/triangle1.png" sierpinskiTriangle1

sierpinskiTriangle1 :: Image
sierpinskiTriangle1 = triRec 8

triRec :: Int -> Image
triRec 0 = blueTriangle
triRec n = aboves [ besides [ triRec (n-1) ]
                   ,besides [ triRec (n-1), triRec (n-1) ] ]

blueTriangle :: Image
blueTriangle = triangle 5 solid fgCol

-------------------------------------------------------------------------------
-- | Sierpinski Triangle with `iter`
-------------------------------------------------------------------------------
mkTriangle2 :: IO ()
mkTriangle2 = save "img/triangle2.png" sierpinskiTriangle2

sierpinskiTriangle2 :: Image
sierpinskiTriangle2 = iter 8 f base
 where
   f             x = aboves [ besides [ x ]
                             ,besides [ x, x ] ]
   base            = blueTriangle


-------------------------------------------------------------------------------
-- | Sierpinski Carpet with `iter`
-------------------------------------------------------------------------------
mkCarpet :: IO ()
mkCarpet   = save "img/carpet.png" sierpinskiCarpet

sierpinskiCarpet :: Image
sierpinskiCarpet = iter 4 f base
  where
    f          x = overlay (square ((width x) * 1.1) solid bgCol)
                            (aboves [ besides [ whiteFramedSquare, whiteFramedSquare, whiteFramedSquare ],
                            besides [ whiteFramedSquare, whiteFramedSquare, whiteFramedSquare ],
                            besides [ whiteFramedSquare, whiteFramedSquare, whiteFramedSquare ] ])
      where
        whiteFramedSquare = overlay x (square ((width x) * 1.1) solid bgCol)
    base         = blueSquare

blueSquare :: Image
blueSquare =  square 10 solid fgCol
