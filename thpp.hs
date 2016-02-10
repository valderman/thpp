-- | Template Haskell as a preprocessor; a proof of concept.
--   TODO: this would be a lot less awful if it actually parsed and modified
--         Haskell syntax trees for the splices...
module Main where
import Control.Shell hiding (ls)
import Data.List
import Data.Char

main
  | null cmdline = do
    putStrLn "thpp - Template Haskell as a preprocessor"
    putStrLn "usage: thpp [haskell files]"
  | otherwise = do
    shell_ $ mapM_ thpp cmdline

-- | Output file name: original file name with extension thpp instead of hs.
outfile :: FilePath -> FilePath
outfile f = replaceExtension f "thpp"

-- | Perform thpp magic on a single file.
thpp :: FilePath -> Shell ()
thpp f = do
  lns <- dumpSplices f `orElse` (rebuild f >> dumpSplices f)
  withFile f ReadMode $ \hin -> do
    src <- hGetContents hin
    withFile (outfile f) WriteMode $ \hout -> do
      let splices = [ splice
                    | splice <- breakSplices lns
                    , locFile (spliceLoc splice) == f
                    ]
      hPutStr hout (insertSplices splices (lines src))

ghcArgs :: FilePath -> [String]
ghcArgs f =
  [ "-v0"
  , "--make"
  , "-hisuf", "thpp_hi"
  , "-osuf", "thpp_o"
  , "-c", f
  ]

rebuild :: FilePath -> Shell ()
rebuild f = void $ run "ghc" (ghcArgs f) ""

dumpSplices :: FilePath -> Shell [String]
dumpSplices f =
    lines <$> run "ghc" args ""
  where
    args = "-fforce-recomp" : "-ddump-splices" : ghcArgs f

-- | Input is assumed to be sorted on line/column in ascending order.
groupSplices :: [Splice] -> [[Splice]]
groupSplices = groupBy (\a b -> locLine (spliceLoc a) == locLine (spliceLoc b))

-- | Inserts a list of splices. Input is assumed to be sorted in ascending
--   order.
insertSplices :: [Splice] -> [String] -> String
insertSplices splices = intercalate "\n" . go 1 (groupSplices splices)
  where
    go ln sss@(s:ss) (l:ls)
      | ln == locLine (spliceLoc (head s)) =
        insertSplicesLn s l : go (ln+1) ss ls
      | otherwise =
        l : go (ln+1) sss ls
    go _ _ ls =
        ls

-- | Insert a list of splices into a line of text.
insertSplicesLn :: [Splice] -> String -> String
insertSplicesLn splices =
    go 1 splices
  where
    go col (s:ss) cols =
        concat [pre', spliceToString s, go (colTo+extraCol) ss post']
      where
        -- from/to column as reported by -ddump-splices
        colFrom = locColFrom (spliceLoc s)
        colTo = locColTo (spliceLoc s)
        (pre, post) = splitAt (colFrom - col) cols

        -- distance from the latest $ sign to colFrom, since -ddump-splices
        -- doesn't include the leading $( or any whitespace following it when
        -- calculating the column numbers
        reversePre = reverse pre
        hasDollar = hasDollarParen pre
        usdDist
          | hasDollar = length (takeWhile (/= '$') reversePre)+1
          | otherwise = 0
        extraDrop
          | hasDollar = 1
          | otherwise = 0
        extraCol
          | hasDollar = 2
          | otherwise = 0
        pre'
          | hasDollar = reverse (drop usdDist reversePre)
          | otherwise = pre

        -- similarly, whitespace between the end of a TH expression and the
        -- closing parenthesis is not included in the column count either.
        -- jesus...
        post' = drop 1 $ dropWhile isSpace $ drop (colTo+extraDrop-colFrom) post
    go _ _ cs =
      cs

-- | Does the given string contain the TH @$(@ combo?
hasDollarParen :: String -> Bool
hasDollarParen ('$':'(':_) = True
hasDollarParen (_:xs)      = hasDollarParen xs
hasDollarParen _           = False

-- | Source location of a splice.
data Loc = Loc
  { locFile    :: FilePath
  , locLine    :: Int
  , locColFrom :: Int
  , locColTo   :: Int
  } deriving Show

data SpliceType
  = SpliceDecl
  | SplicePat
  | SpliceExpr
    deriving Show

-- | A splice consists of a location and the text to be spliced.
data Splice = Splice
  { spliceLoc     :: Loc
  , spliceType    :: SpliceType
  , spliceContent :: [String]
  } deriving Show

spliceToString :: Splice -> String
spliceToString s =
  case spliceType s of
    SpliceDecl -> intercalate "\n" (spliceContent s)
    _          -> concat ["(", intercalate " " (spliceContent s), ")"]

-- | Find and parse all splices in a list of lines.
breakSplices :: [String] -> [Splice]
breakSplices (x:xs) =
    Splice loc type_ splice' : breakSplices rest'
  where
    splicepart = drop 1 $ dropWhile (/= multiLineMarker) xs
    (splice, rest) = break (not . all (== ' ') . take 4) splicepart
    (splice', rest')
      | multiLineMarker `elem` xs = (map (drop 4) splice, rest)
      | otherwise                 = ([singleLineSplice (head xs)], tail xs)
    (loc, type_) = parseHeading x
breakSplices _ =
  []

-- | Separates LHS and RHS of a multiline splice.
multiLineMarker :: String
multiLineMarker = "  ======>"

-- | Separates LHS and RHS of a single line splice.
singleLineMarker :: String
singleLineMarker = "======>"

-- | Read a single line splice out 
singleLineSplice :: String -> String
singleLineSplice ln = go True singleLineMarker ln
  where
    go restarted (m:ms) ccs@(c:cs)
      | m == c    = go False ms cs
      | restarted = go True singleLineMarker cs
      | otherwise = go True singleLineMarker ccs
    go _ [] cs    = dropWhile isSpace cs
    go _ _ _      = error "not a single line splice: `" ++ ln ++ "'"

-- | Parse a splice heading.
parseHeading :: String -> (Loc, SpliceType)
parseHeading s =
    (Loc file (read line) (read from) (read to), type_)
  where
    (file, _:s')   = break (== ':') s
    (line, _:s'')  = break (== ':') s'
    (from, _:s''') = break (== '-') s''
    (to,   _:rest) = break (== ':') s'''
    type_ =
      case words rest !! 1 of
        "declarations" -> SpliceDecl
        "expression"   -> SpliceExpr
        "pattern"      -> SplicePat
        _              -> error $ "unknown splice type: " ++ rest
