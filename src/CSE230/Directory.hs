module CSE230.Directory where

import CSE230.Doc
import CSE230.List hiding (Dir)
import qualified Data.List as L
import System.FilePath      (takeDirectory, takeFileName, (</>))
import System.Directory     (doesFileExist, listDirectory)
import Data.Foldable (Foldable(foldr'))
import Data.List (sortBy)
import Data.Char (toLower)

-------------------------------------------------------------------------------
-- | Top-level "main" function
-------------------------------------------------------------------------------
main :: [String] -> IO ()
main ["-ls", p]        = showDirectory (normalize p)
main ["-find", p, sub] = showMatchingFiles (normalize p) sub
main args              = putStrLn (errorMsg args)

normalize :: FilePath -> FilePath
normalize p = takeDirectory (p </> ".")

errorMsg :: [String] -> String
errorMsg args = unlines
  [ "Sorry, don't understand: " ++ unwords args ++ ". Try one of"
  , " *  'htree -ls <path> ' to render the directory of all files under <path>"
  , " *  'htree -find <path> sub' to find all files matching 'sub' under <path>"
  ]

-------------------------------------------------------------------------------
-- | 'showDirectory path' builds and displays the 'Dir' rooted at 'path'
-------------------------------------------------------------------------------
-- >>> showDirectory "src"
-- src
-- ├── CSE230
-- │   ├── Directory.hs
-- │   ├── Doc.hs
-- │   ├── Graphics.hs
-- │   ├── List.hs
-- │   └── Shapes.hs
-- ├── Htdp
-- │   ├── Combinator.hs
-- │   ├── Data
-- │   │   └── Image.hs
-- │   ├── README.md
-- │   └── Shape.hs
-- ├── Htdp.hs
-- └── Main.hs
-- <BLANKLINE>
--

showDirectory :: FilePath -> IO ()
showDirectory path = do
  dir <- build path
  print (dirDoc dir)

-------------------------------------------------------------------------------
-- | 'showMatchingFiles path sub' finds the files matching 'sub' (deep) in the
--   the directory rooted at 'path' and prints them out.
-------------------------------------------------------------------------------
-- >>> showMatchingFiles "src" ".hs"
-- src/CSE230/Directory.hs
-- src/CSE230/Doc.hs
-- src/CSE230/Graphics.hs
-- src/CSE230/List.hs
-- src/CSE230/Shapes.hs
-- src/Htdp/Combinator.hs
-- src/Htdp/Data/Image.hs
-- src/Htdp/Shape.hs
-- src/Htdp.hs
-- src/Main.hs
--

showMatchingFiles :: FilePath -> String -> IO ()
showMatchingFiles path sub = do
  dir   <- build path
  mapM_ putStrLn (findFiles sub dir)


-------------------------------------------------------------------------------
-- | The 'Directory' data type
-------------------------------------------------------------------------------
data Dir a
    = Fil a             -- ^ A single file named `a`
    | Sub a [Dir a]     -- ^ A sub-directory name `a` with contents `[Dir a]`
    deriving (Eq, Show)

example :: Dir FilePath
example = Sub "."
            [ Fil "COLLABORATORS.md"
            , Fil "LICENSE"
            , Fil "Makefile"
            , Fil "README.md"
            , Fil "cse230-tree.cabal"
            , Sub "out" [ Fil "carpet.png"
                        , Fil "chess1.png"
                        , Fil "chess2.png"
                        , Fil "rainbow.png"
                        , Fil "triangle1.png"
                        , Fil "triangle2.png"
                        ]
            , Sub "src" [ Sub "CSE230" [ Fil "Directory.hs"
                                       , Fil "Doc.hs"
                                       , Fil "Graphics.hs"
                                       , Fil "List.hs"
                                       , Fil "Shapes.hs"
                                       ]
                        , Sub "Htdp" [ Fil "Combinator.hs"
                                     , Sub "Data" [ Fil "Image.hs" ]
                                     , Fil "README.md"
                                     , Fil "Shape.hs"
                                     ]
                        , Fil "Htdp.hs"
                        , Fil "Main.hs"
                        ]
            , Fil "stack.yaml"
            ]

srcDir :: Dir FilePath
srcDir = Sub "src"
         [ Sub "CSE230" [ Fil "Directory.hs"
                        , Fil "Doc.hs"
                        , Fil "Graphics.hs"
                        , Fil "List.hs"
                        , Fil "Shapes.hs"
                        ]
         , Sub "Htdp" [ Fil "Combinator.hs"
                      , Sub "Data" [ Fil "Image.hs" ]
                      , Fil "README.md"
                      , Fil "Shape.hs"
                      ]
         , Fil "Htdp.hs"
         , Fil "Main.hs"
         ]


-------------------------------------------------------------------------------
-- | Printing Directories
-------------------------------------------------------------------------------

-- >>> dirDoc example
-- .
-- ├── COLLABORATORS.md
-- ├── LICENSE
-- ├── Makefile
-- ├── README.md
-- ├── cse230-tree.cabal
-- ├── out
-- │   ├── carpet.png
-- │   ├── chess1.png
-- │   ├── chess2.png
-- │   ├── rainbow.png
-- │   ├── triangle1.png
-- │   └── triangle2.png
-- ├── src
-- │   ├── CSE230
-- │   │   ├── Directory.hs
-- │   │   ├── Doc.hs
-- │   │   ├── Graphics.hs
-- │   │   ├── List.hs
-- │   │   └── Shapes.hs
-- │   ├── Htdp
-- │   │   ├── Combinator.hs
-- │   │   ├── Data
-- │   │   │   └── Image.hs
-- │   │   ├── README.md
-- │   │   └── Shape.hs
-- │   ├── Htdp.hs
-- │   └── Main.hs
-- └── stack.yaml
--

tDir :: Dir FilePath
tDir = Sub "src"
         [ Sub "CSE230" [ Fil "Directory.hs"]]
-- >>> dirDoc tDir
-- src
-- └── CSE230
--     └── Directory.hs
dirDoc :: Dir FilePath -> Doc 
dirDoc (Fil f)    = doc f
dirDoc (Sub f ds) = vcatL (doc f) (dirsDoc ds (doc ""))
-- vcatL (doc f) (foldr vcatL empty (map dirDoc ds))

-- >>> dirDoc srcDir
-- src
-- ├── CSE230
-- │   ├── Directory.hs
-- │   ├── Doc.hs
-- │   ├── Graphics.hs
-- │   ├── List.hs
-- │   └── Shapes.hs
-- ├── Htdp
-- │   ├── Combinator.hs
-- │   ├── Data
-- │   │   └── Image.hs
-- │   ├── README.md
-- │   └── Shape.hs
-- ├── Htdp.hs
-- └── Main.hs

dirsDoc :: [Dir FilePath] -> Doc -> Doc
dirsDoc [] _ = empty
dirsDoc ((Fil f):xs) p
  | null xs = vcatL (hcatB prefixSingle (doc f)) (dirsDoc xs p)
  | otherwise = vcatL (hcatB prefixMulti (doc f)) (dirsDoc xs p)
    where
      prefixSingle = (hcatB p singleFileDash)
      prefixMulti = (hcatB p multiFileDash)
dirsDoc ((Sub f ds):xs) p
  | null xs = vcatL (vcatL (hcatB prefixSingle (doc f)) (dirsDoc ds (hcatB p (doc "    ")))) (dirsDoc xs p)
  | otherwise = vcatL (vcatL (hcatB prefixMulti (doc f)) (dirsDoc ds (hcatB p (doc "│   ")))) (dirsDoc xs p)
    where
      prefixSingle = (hcatT p singleFileDash)
      prefixMulti = (hcatT p multiFileDash)

-------------------------------------------------------------------------------
-- | Some useful 'Doc's--------------------------------------------------------
-------------------------------------------------------------------------------
dash :: Doc
dash = doc "── "

stile :: Doc
stile = doc "├"

angle :: Doc
angle = doc "└"

bar :: Doc
bar = doc "│"

singleFileDash :: Doc
singleFileDash = hcatB angle dash

multiFileDash :: Doc
multiFileDash = hcatB stile dash

-------------------------------------------------------------------------------
-- | A 'fold' for directories
-------------------------------------------------------------------------------
data DirElem a = SubDir a | File a

foldDir :: ([a] -> r -> DirElem a -> r) -> r -> Dir a -> r
foldDir f = go []
  where
      go stk r (Fil a)    = f stk r (File a)
      go stk r (Sub a ds) = L.foldl' (go stk') r' ds
        where
            r'   = f stk r (SubDir a)
            stk' = a:stk

-------------------------------------------------------------------------------
-- | 'allFiles dir' returns a list of all the Files in 'dir'
-------------------------------------------------------------------------------
-- >>> allFiles example
-- ["COLLABORATORS.md","LICENSE","Makefile","README.md","cse230-tree.cabal","carpet.png","chess1.png","chess2.png","rainbow.png","triangle1.png","triangle2.png","Directory.hs","Doc.hs","Graphics.hs","List.hs","Shapes.hs","Combinator.hs","Image.hs","README.md","Shape.hs","Htdp.hs","Main.hs","stack.yaml"]
--

allFiles :: Dir FilePath -> [FilePath]
allFiles dir = reverse (foldDir f [] dir)
  where
      f _ r (File a) = a:r
      f _ r _ = r


-------------------------------------------------------------------------------
-- | 'allDirs dir' returns a list of all the sub-directories in 'dir'
-------------------------------------------------------------------------------
--
-- >>> allDirs example
-- [".","out","src","CSE230","Htdp","Data"]
-- >>> allDirs srcDir
-- ["src","CSE230","Htdp","Data"]

allDirs :: Dir FilePath -> [FilePath]
allDirs dir = reverse (foldDir f [] dir)
  where
      f _ r (SubDir a) = a:r
      f _ r _ = r


-------------------------------------------------------------------------------
-- | 'findFiles sub dir' returns a list of all the Files-with-paths in 'dir'
--   such that 'sub' is a substring of the Files' names.
-------------------------------------------------------------------------------
--
-- >>> findFiles ".hs" example
-- ["./src/CSE230/Directory.hs","./src/CSE230/Doc.hs","./src/CSE230/Graphics.hs","./src/CSE230/List.hs","./src/CSE230/Shapes.hs","./src/Htdp/Combinator.hs","./src/Htdp/Data/Image.hs","./src/Htdp/Shape.hs","./src/Htdp.hs","./src/Main.hs"]
-- >>> findFiles ".png" example
-- ["./out/carpet.png","./out/chess1.png","./out/chess2.png","./out/rainbow.png","./out/triangle1.png","./out/triangle2.png"]

findFiles :: String -> Dir FilePath -> [FilePath]
findFiles sub dir = reverse (foldDir f [] dir)
   where
      f stk r (File a)
        | L.isInfixOf sub a = ((L.foldl' (</>) "" (reverse stk)) ++ "/" ++ a):r
        | otherwise = r
      f _ r _ = r

-------------------------------------------------------------------------------
-- | 'build path' constructing the Directory on the filesystem rooted at 'path'
-------------------------------------------------------------------------------
--
-- >>> build "src"
-- Sub "src" [Sub "CSE230" [Fil "Directory.hs",Fil "Doc.hs",Fil "Graphics.hs",Fil "List.hs",Fil "Shapes.hs"],Sub "Htdp" [Fil "Combinator.hs",Sub "Data" [Fil "Image.hs"],Fil "README.md",Fil "Shape.hs"],Fil "Htdp.hs",Fil "Main.hs"]

build :: FilePath -> IO (Dir FilePath)
build path = buildPath "" path

buildPath :: FilePath -> FilePath -> IO (Dir FilePath)
buildPath prefix path = do
  let curPath = prefix ++ path
  filenames <- listDirectory curPath
  let filepaths = map ((curPath ++ "/") ++) filenames
  isFiles <- mapM doesFileExist filepaths
  let files = [Fil filename | (filename, isFile) <- (zip filenames isFiles), isFile]
  dirs <- sequence [buildPath (curPath ++ "/") dirname | (dirname, isFile) <- (zip filenames isFiles), not isFile]
  return (Sub path (L.sortOn getPath (dirs ++ files)))

getPath :: (Dir FilePath) -> FilePath
getPath (Fil f) = f
getPath (Sub f _) = f

lshow :: Doc -> [String]
lshow = lines . show
