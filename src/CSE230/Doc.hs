{-# LANGUAGE InstanceSigs #-}
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
--

instance Show Doc where
  show :: Doc -> String
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
--

vcatL :: Doc -> Doc -> Doc
vcatL (D l1) (D l2) = (D (l1 ++ l2))

-------------------------------------------------------------------------------
-- | Vertical Concatenation aligned at Right
-------------------------------------------------------------------------------

-- >>> (doc "cat") `vcatR` (doc "horse") `vcatR` (doc "mongoose")
--      cat
--    horse
-- mongoose
--

vcatR :: Doc -> Doc -> Doc
vcatR (D l1) (D l2)
  | w1 <= w2 = D ((map (padX DirL (w2 - w1) space) l1) ++ l2)
  | otherwise = D (l1 ++ (map (padX DirL (w1 - w2) space) l2))
    where
      w1 = width (D l1)
      w2 = width (D l2)
      space = ' '

-------------------------------------------------------------------------------
-- | Horizontal Concatenation aligned at Top
--   HINT: use `zip` or `zipWith`
-------------------------------------------------------------------------------

-- >>> hcatT aDoc lineDoc
-- a    <----- HERE
-- aaa  
-- aaaaa
--
-- >>> hcatT aDoc bDoc
-- a    b
-- aaa  bbb
-- aaaaa
--
hcatT :: Doc -> Doc -> Doc
hcatT (D []) d2 = d2
hcatT d1 (D []) = d1
hcatT (D l1) (D l2) 
  | h2 <= h1 = D (zipWith (++) (correctList l1) (elongateList DirR h1 l2))
  | otherwise = D (zipWith (++) (correctList (elongateList DirR h1 l1)) l2)
    where
      h1 = height (D l1)
      h2 = height (D l2)
      correctList = map (pad DirR w1 space)
      w1 = width (D l1)
      space = ' '

elongateList :: Dir -> Int -> [String] -> [String]
elongateList dir h l = pad dir h "" l

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
--
-- >>> hcatB aDoc bDoc
-- a    
-- aaa  b
-- aaaaabbb
--
hcatB :: Doc -> Doc -> Doc
hcatB (D []) d2 = d2
hcatB d1 (D []) = d1
hcatB (D l1) (D l2) 
  | h2 <= h1 = D (zipWith (++) (correctList l1) (elongateList DirL h1 l2))
  | otherwise = D (zipWith (++) (correctList (elongateList DirL h1 l1)) l2)
    where
      h1 = height (D l1)
      h2 = height (D l2)
      correctList = map (pad DirR w1 space)
      w1 = width (D l1)
      space = ' '

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
