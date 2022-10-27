{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module CSE230.Doc
  (
    -- * A document type 
    Doc

    -- * Constructors
  , empty, doc

    -- * Accessors
  , width, height

    -- * Example Doc
  , aDoc, bDoc, lineDoc, animals, triangles

    -- * Combinators
  , vcatL, vcatR, hcatB, hcatT

    -- * Properties
  , prop_hcatT_width
  , prop_hcatT_height
  , prop_hcatB_width
  , prop_hcatB_height
  , prop_vcatL_width
  , prop_vcatR_width
  , prop_vcatL_height
  , prop_vcatR_height

  ) where

import qualified Test.QuickCheck as QC
import           Prelude hiding (maximum)
import           CSE230.List

-------------------------------------------------------------------------------
-- | A 'Doc' is a 'String' list
-------------------------------------------------------------------------------
data Doc = D { docLines :: [String] }
  deriving (Eq, Ord)

empty :: Doc
empty = D []

doc :: String -> Doc
doc s = D (lines s)

aDoc :: Doc
aDoc = D [ "a"
         , "aaa"
         , "aaaaa"]

bDoc :: Doc
bDoc = D [ "b"
         , "bbb"]

lineDoc :: Doc
lineDoc = doc "<----- HERE"

animals :: [Doc]
animals = [ doc "cat"
          , doc "horse"
          , doc "mongoose"
          ]

-------------------------------------------------------------------------------
-- | Printing a Doc 
-------------------------------------------------------------------------------
-- >>> aDoc
-- a
-- aaa
-- aaaaa
-- <BLANKLINE>
--

instance Show Doc where
  show (D ls) = unlines ls

-------------------------------------------------------------------------------
-- | Generating Random Docs
-------------------------------------------------------------------------------
instance QC.Arbitrary Doc where
  arbitrary = fmap D QC.arbitrary

-------------------------------------------------------------------------------
-- | Dimensions of a 'Doc'
-------------------------------------------------------------------------------
-- >>> width aDoc
-- 5

width :: Doc -> Int
width (D ls) = maximum 0 (map length ls)

-- >>> height aDoc
-- 3

height :: Doc -> Int
height (D ls) = length ls

-------------------------------------------------------------------------------
-- | Vertical Concatenation aligned at Left
-------------------------------------------------------------------------------

-- >>> (doc "cat") `vcatL` (doc "horse") `vcatL` (doc "mongoose")
-- cat
-- horse
-- mongoose
-- <BLANKLINE>
--

vcatL :: Doc -> Doc -> Doc
vcatL d1 d2 = doc (show d1 ++ show d2)

-------------------------------------------------------------------------------
-- | Vertical Concatenation aligned at Right
-------------------------------------------------------------------------------

-- >>> (doc "cat") `vcatR` (doc "horse") `vcatR` (doc "mongoose")
--      cat
--    horse
-- mongoose
-- <BLANKLINE>
--

vcatR :: Doc -> Doc -> Doc
vcatR d1 d2 = D (foldr f base xs ++ foldr g base ys)
  where
    f    = \x xs -> (clone (width d2 - width d1) ' ' ++ x) : xs
    xs   = lines (show d1)
    g    = \y ys -> (clone (width d1 - width d2) ' ' ++ y) : ys
    ys   = lines (show d2)
    base = []

-------------------------------------------------------------------------------
-- | Horizontal Concatenation aligned at Top
--   HINT: use `zip` or `zipWith`
-------------------------------------------------------------------------------

-- >>> hcatT aDoc lineDoc
-- a    <----- HERE
-- aaa  
-- aaaaa
-- <BLANKLINE>
--
-- >>> hcatT aDoc bDoc
-- a    b
-- aaa  bbb
-- aaaaa
-- <BLANKLINE>
--
hcatT :: Doc -> Doc -> Doc
hcatT d1 d2 = D (zipWith f xs ys)
  where
    f    = (++)
    xs   = foldr g base (lines (show (elongate DirR (height d2) d1)))
    g    = \x xs -> pad DirR (width d1) ' ' x : xs
    ys   = lines (show (elongate DirR (height d1) d2))
    base = []

-------------------------------------------------------------------------------
-- | elongate pad with "" which is actually adding new lines
-------------------------------------------------------------------------------
elongate :: Dir -> Int -> Doc -> Doc
elongate dir h (D ls) = D (pad dir h "" ls)

-------------------------------------------------------------------------------
-- | Horizontal Concatenation aligned at Bottom
--   HINT: use `zip` or `zipWith`
-------------------------------------------------------------------------------
-- >>> hcatB aDoc lineDoc
-- a    
-- aaa  
-- aaaaa<----- HERE
-- <BLANKLINE>
--
-- >>> hcatB aDoc bDoc
-- a    
-- aaa  b
-- aaaaabbb
-- <BLANKLINE>
--
hcatB :: Doc -> Doc -> Doc
hcatB d1 d2 = D (zipWith f xs ys)
  where
    f    = (++)
    xs   = foldr g base (lines (show (elongate DirL (height d2) d1)))
    g    = \x xs -> pad DirR (width d1) ' ' x : xs
    ys   = lines (show (elongate DirL (height d1) d2))
    base = []

triangle :: Doc
triangle = D
  [ "*"
  , "***"
  , "*****" ]

-- >>> foldr vcatL empty triangles
-- *
-- ***
-- *****
-- *    *
-- ***  ***
-- **********
-- *    *    *
-- ***  ***  ***
-- ***************
-- <BLANKLINE>
--

-- >>> foldr vcatR empty triangles
--           *
--           ***
--           *****
--      *    *
--      ***  ***
--      **********
-- *    *    *
-- ***  ***  ***
-- ***************
-- <BLANKLINE>
--
triangles :: [Doc]
triangles = [ triangle
            , triangle `hcatT` triangle
            , triangle `hcatT` triangle `hcatT` triangle ]


-------------------------------------------------------------------------------
-- | Properties of `Doc` combinators ------------------------------------------
-------------------------------------------------------------------------------

prop_hcatT_width  d1 d2 = width  (d1 `hcatT` d2) == width d1 + width d2
prop_hcatT_height d1 d2 = height (d1 `hcatT` d2) == max (height d1) (height d2)
prop_hcatB_width  d1 d2 = width  (d1 `hcatB` d2) == width d1 + width d2
prop_hcatB_height d1 d2 = height (d1 `hcatB` d2) == max (height d1) (height d2)
prop_vcatL_width  d1 d2 = width  (d1 `vcatL` d2) == max (width d1) (width d2)
prop_vcatR_width  d1 d2 = width  (d1 `vcatR` d2) == max (width d1) (width d2)
prop_vcatL_height d1 d2 = height (d1 `vcatL` d2) == height d1 + height d2
prop_vcatR_height d1 d2 = height (d1 `vcatR` d2) == height d1 + height d2
