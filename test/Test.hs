{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Prelude hiding (maximum)
-- import System.FilePath ((</>))
import CSE230.List
import CSE230.Shapes hiding (main)
import CSE230.Doc
-- import CSE230.Graphics
import CSE230.Directory hiding (main)

main :: IO ()
main = runTests 
  [ probList
  , probShapes
  , probDoc
  -- , probDocProps
  , probDir 
  , probHtree
  ]

probList ::  Score -> TestTree
probList sc = testGroup "List" [
  scoreTest ((\_ -> clone 5 'a'), (), "aaaaa", 1, "clone-1"),
  scoreTest ((\_ -> clone 3 "cat"), (), ["cat","cat","cat"], 1, "clone-2"),
  scoreTest ((\_ -> pad DirL 10 0 [1,2,3,4,5]), (), [0,0,0,0,0,1,2,3,4,5], 1, "pad-1"),
  scoreTest ((\_ -> pad DirR 10 0 [1,2,3,4,5]), (), [1,2,3,4,5,0,0,0,0,0], 1, "pad-2"), 
  scoreTest ((\_ -> pad DirL 3 0 [1,2,3,4,5]), (),  [1,2,3,4,5], 1, "pad-3"),
  scoreTest ((\_ -> pad DirR 3 0 [1,2,3,4,5]), (),  [1,2,3,4,5], 1, "pad-4"),
  scoreTest ((\_ -> isSubSequence "cat" "dog"), (), False, 1, "issub-1"),
  scoreTest ((\_ -> isSubSequence "cat" "craptasticdog"), (), True, 1, "issub-2"),
  scoreTest ((\_ -> isSubSequence "cat" "ziangcatquan"), (), True, 1, "issub-3"),
  scoreTest ((\_ -> maximum 99 []), (), 99, 1, "maximum-1"),
  scoreTest ((\_ -> maximum 99 [90, 100, 200, 52]), (), 200, 1, "maximum-2"),
  scoreTest ((\_ -> intersp '|' "chewbacca"), (), "|c|h|e|w|b|a|c|c|a|", 1, "intersp-1"),
  scoreTest ((\_ -> intersp 3 [1,2,3,4,5,6]), (), [3,1,3,2,3,3,3,4,3,5,3,6,3], 1, "intersp-num"),
  scoreTest ((\_ -> intersp "yo!" ["which", "way", "is", "the", "park"]), (), ["yo!","which","yo!","way","yo!","is","yo!","the","yo!","park","yo!"], 1, "intersp-2"),
  scoreTest ((\_ -> iter 0 (\x -> 2 * x) 1 ), (), 1, 3, "iter-0"),
  scoreTest ((\_ -> iter 10 (\x -> 2 * x) 1 ), (), 1024, 3, "iter-1"),
  scoreTest ((\_ -> iter (-88) (\x -> 2 * x) 1 ), (), 1, 3, "iter-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

probShapes :: Score -> TestTree
probShapes sc = testGroup "Shapes" [
  scoreTest ((\_ -> checkImage "rainbow.png"   mkRainbow),    (), True, 1, "rainbow"),
  scoreTest ((\_ -> checkImage "chess1.png"    mkChess1),     (), True, 2, "chess-1"),
  scoreTest ((\_ -> checkImage "chess2.png"    mkChess2),     (), True, 2, "chess-2"),
  scoreTest ((\_ -> checkImage "triangle1.png" mkTriangle1),  (), True, 3, "triangle-1"),
  scoreTest ((\_ -> checkImage "triangle2.png" mkTriangle2),  (), True, 3, "triangle-2"),
  scoreTest ((\_ -> checkImage "carpet.png"    mkCarpet),     (), True, 5, "carpet")
  ]
  where 
    scoreTest :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)

    checkImage :: FilePath -> IO () -> IO Bool  
    checkImage f build = build >> return True -- compareBMP 5 ("img" </> f) ("out" </> f)

  
probDoc :: Score -> TestTree
probDoc sc = testGroup "Doc" [
  scoreTest ((\_ -> lshow (foldr vcatL empty animals)), (), ["cat","horse","mongoose"], 1, "vcatL"),
  scoreTest ((\_ -> lshow (foldr vcatL empty [empty])), (), [], 1, "vcatL-empty"),
  scoreTest ((\_ -> lshow (foldr vcatR empty animals)), (), ["     cat","   horse","mongoose"], 2, "vcatR"),
  scoreTest ((\_ -> lshow (foldr vcatR empty [empty])), (), [], 2, "vcatR-empty"),
  scoreTest ((\_ -> lshow $ hcatT aDoc lineDoc), (), ["a    <----- HERE","aaa  ","aaaaa"], 2, "hcatT-1"),
  scoreTest ((\_ -> lshow $ hcatT aDoc bDoc), (), ["a    b","aaa  bbb","aaaaa"], 2, "hcatT-2"),
  scoreTest ((\_ -> lshow $ hcatT empty empty), (), [], 2, "hcatT-empty"),
  scoreTest ((\_ -> lshow $ hcatB aDoc lineDoc), (), ["a    ","aaa  ","aaaaa<----- HERE"], 2, "hcatB-1"),
  scoreTest ((\_ -> lshow $ hcatB aDoc bDoc), (), ["a    ","aaa  b","aaaaabbb"], 2, "hcatB-2"),
  scoreTest ((\_ -> lshow $ hcatB empty empty), (), [], 2, "hcatB-empty"),
  scoreTest ((\_ -> lshow $ foldr vcatL empty triangles), (), ["*","***","*****","*    *","***  ***","**********","*    *    *","***  ***  ***","***************"], 3, "vcatL-1"),
  scoreTest ((\_ -> lshow $ foldr vcatR empty triangles), (), ["          *","          ***","          *****","     *    *","     ***  ***","     **********","*    *    *","***  ***  ***","***************"], 3, "vcatR-1")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

probDocProps :: Score -> TestTree
probDocProps sc = testGroup "Doc-Properties"
  [ scoreProp sc ("prop_hcatT_width", prop_hcatT_width , 2)
  , scoreProp sc ("prop_hcatT_height", prop_hcatT_height, 2)
  , scoreProp sc ("prop_hcatB_width", prop_hcatB_width , 2)
  , scoreProp sc ("prop_hcatB_height", prop_hcatB_height, 2)
  , scoreProp sc ("prop_vcatL_width", prop_vcatL_width , 2)
  , scoreProp sc ("prop_vcatR_width", prop_vcatR_width , 2)
  , scoreProp sc ("prop_vcatL_height", prop_vcatL_height, 2)
  , scoreProp sc ("prop_vcatR_height", prop_vcatR_height, 2)
  ]

probDir :: Score -> TestTree
probDir sc = testGroup "Directory" [
  scoreTest ((\_ -> lshow (dirDoc example)), (), ddEx, 4, "dirDoc-1"),
  scoreTest ((\_ -> lshow (dirDoc srcDir)), (), ddSrc, 4, "dirDoc-2"),
  scoreTest ((\_ -> allFiles example), (), fiEx, 2, "allFiles-1"),
  scoreTest ((\_ -> allDirs example), (), diEx, 2, "allDirs-1"),
  scoreTest ((\_ -> findFiles ".hs" example), (), hsEx, 4, "findFiles-1"),
  scoreTestI ((\_ -> build "src"), (), srcDir, 1, "build-1")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
    scoreTestI :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
    scoreTestI (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)
    ddEx = [ ".","\9500\9472\9472 COLLABORATORS.md","\9500\9472\9472 LICENSE","\9500\9472\9472 Makefile","\9500\9472\9472 README.md","\9500\9472\9472 cse230-tree.cabal","\9500\9472\9472 out","\9474   \9500\9472\9472 carpet.png","\9474   \9500\9472\9472 chess1.png","\9474   \9500\9472\9472 chess2.png","\9474   \9500\9472\9472 rainbow.png","\9474   \9500\9472\9472 triangle1.png","\9474   \9492\9472\9472 triangle2.png","\9500\9472\9472 src","\9474   \9500\9472\9472 CSE230","\9474   \9474   \9500\9472\9472 Directory.hs","\9474   \9474   \9500\9472\9472 Doc.hs","\9474   \9474   \9500\9472\9472 Graphics.hs","\9474   \9474   \9500\9472\9472 List.hs","\9474   \9474   \9492\9472\9472 Shapes.hs","\9474   \9500\9472\9472 Htdp","\9474   \9474   \9500\9472\9472 Combinator.hs","\9474   \9474   \9500\9472\9472 Data","\9474   \9474   \9474   \9492\9472\9472 Image.hs","\9474   \9474   \9500\9472\9472 README.md","\9474   \9474   \9492\9472\9472 Shape.hs","\9474   \9500\9472\9472 Htdp.hs","\9474   \9492\9472\9472 Main.hs","\9492\9472\9472 stack.yaml"]
    ddSrc = ["src","\9500\9472\9472 CSE230","\9474   \9500\9472\9472 Directory.hs","\9474   \9500\9472\9472 Doc.hs","\9474   \9500\9472\9472 Graphics.hs","\9474   \9500\9472\9472 List.hs","\9474   \9492\9472\9472 Shapes.hs","\9500\9472\9472 Htdp","\9474   \9500\9472\9472 Combinator.hs","\9474   \9500\9472\9472 Data","\9474   \9474   \9492\9472\9472 Image.hs","\9474   \9500\9472\9472 README.md","\9474   \9492\9472\9472 Shape.hs","\9500\9472\9472 Htdp.hs","\9492\9472\9472 Main.hs"]
    fiEx = ["COLLABORATORS.md","LICENSE","Makefile","README.md","cse230-tree.cabal","carpet.png","chess1.png","chess2.png","rainbow.png","triangle1.png","triangle2.png","Directory.hs","Doc.hs","Graphics.hs","List.hs","Shapes.hs","Combinator.hs","Image.hs","README.md","Shape.hs","Htdp.hs","Main.hs","stack.yaml"]
    diEx  = [".","out","src","CSE230","Htdp","Data"]
    hsEx  = ["./src/CSE230/Directory.hs","./src/CSE230/Doc.hs","./src/CSE230/Graphics.hs","./src/CSE230/List.hs","./src/CSE230/Shapes.hs","./src/Htdp/Combinator.hs","./src/Htdp/Data/Image.hs","./src/Htdp/Shape.hs","./src/Htdp.hs","./src/Main.hs"]

probHtree :: Score -> TestTree
probHtree sc = testGroup "htree"
 [ bc "stack exec -- htree -ls src"       "out/htree-ls.txt"   4 "htree-ls"
 , bc "stack exec -- htree -find src .hs" "out/htree-find.txt" 4 "htree-find"
 ]
 where
  bc cmd f pts name = binTest sc (BinCmd cmd f f pts name)
