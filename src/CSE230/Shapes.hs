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

rainbow :: Image
rainbow = foldr1 f xs
  where
    xs  = map g [1..7]
    f   = overlay
    g   = drawCircle

drawCircle :: Int -> Image
drawCircle x
        | x == 1 = circle 20 solid violet
        | x == 2 = circle 40 solid blue
        | x == 3 = circle 60 solid cyan
        | x == 4 = circle 80 solid green
        | x == 5 = circle 100 solid yellow
        | x == 6 = circle 120 solid orange
        | otherwise = circle 140 solid red

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
    f       = \x -> aboves [ besides [ x, x ]
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
triRec n = aboves [ triRec (n - 1)
                  , besides [ triRec (n - 1), triRec (n - 1) ] ]

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
   f               = \x -> aboves [ x, besides [ x, x ] ]
   base            = blueTriangle


-------------------------------------------------------------------------------
-- | Sierpinski Carpet with `iter`
-------------------------------------------------------------------------------
mkCarpet :: IO ()
mkCarpet   = save "img/carpet.png" sierpinskiCarpet

sierpinskiCarpet :: Image
sierpinskiCarpet = iter 4 f base
  where
    f            = \x -> overlay (aboves [
                                          besides [
                                                    overlay x (square (width x + 1) solid bgCol)
                                                  , overlay x (square (width x + 1) solid bgCol)
                                                  , overlay x (square (width x + 1) solid bgCol)
                                                  ]
                                        , besides [ 
                                                    overlay x (square (width x + 1) solid bgCol)
                                                  , square (width x + 1) solid bgCol
                                                  , overlay x (square (width x + 1) solid bgCol)
                                                  ]
                                        , besides [ 
                                                    overlay x (square (width x + 1) solid bgCol)
                                                  , overlay x (square (width x + 1) solid bgCol)
                                                  , overlay x (square (width x + 1) solid bgCol)
                                                  ]
                                        ]) (square ((width x + 1) * 3 + 1) solid bgCol)
    base         = blueSquare

blueSquare :: Image
blueSquare =  square 4 solid fgCol
