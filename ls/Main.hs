module Main where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List
import System.Console.GetOpt
import qualified System.Console.Terminal.Size as TS
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Files
import Text.Read

data Flag
  = FlagA
  | FlagL
  deriving (Eq)

getOptions :: IO ([Flag], [String])
getOptions = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, filepaths, []) -> return (flags, filepaths)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: cat [OPTION]... [FILE]..."
    options =
      [ Option "l" ["long"] (NoArg FlagL) "List in long format."
      , Option
          "a"
          ["all"]
          (NoArg FlagA)
          "Include directory entries whose names begin with a dot (.)."
      ]

cmpFunc ::
     [Flag] -> (FilePath, FileStatus) -> (FilePath, FileStatus) -> Ordering
cmpFunc flags (name1, status1) (name2, status2) = compare name1 name2

listToMatrix :: Int -> [String] -> [[String]]
listToMatrix rowSize list = addRow list
  where
    addRow [] = []
    addRow list =
      let (row, rest) = splitAt rowSize list
       in row : addRow rest

printTable :: Int -> Int -> [String] -> IO ()
printTable nColumns columnWidth strings =
  let nRows = (length strings + nColumns - 1) `quot` nColumns
      matrix = transpose $ listToMatrix nRows strings
   in forM_ matrix printRow
  where
    printRow [] = putChar '\n'
    printRow (head:[]) = do
      putStr head
      putChar '\n'
    printRow (head:tail) = do
      putStr head
      putStr $ replicate (columnWidth - length head) ' '
      printRow tail

tableParams :: [String] -> IO (Int, Int)
tableParams strings = do
  size <- TS.hSize stdout
  case size of
    Just w -> do
      columns <-
        lookupEnv "COLUMNS" <&> maybe (TS.width w) (maybe 80 id . readMaybe)
      let maxLength = maximum [length str | str <- strings]
          tabWidth = 8
          columnWidth = (maxLength + tabWidth) `quot` tabWidth * tabWidth
          nColumns = columns `quot` columnWidth
       in return (nColumns, columnWidth)
    Nothing -> return (1, 0)

lsItems :: [Flag] -> [(FilePath, FileStatus)] -> IO ()
lsItems flags items' = do
  let items = filter itemFilter items'
      sortedItems = sortBy (cmpFunc flags) items
      names = [na | (na, _) <- sortedItems]
   in do (nColumns, columnWidth) <- tableParams names
         printTable nColumns columnWidth names
  where
    itemFilter =
      if FlagA `elem` flags
        then const True
        else (\(name, _) -> head name /= '.')

--  forM_ sortedItems (\(na, st) -> printf "%s\n" na)
lsDir :: [Flag] -> Bool -> FilePath -> IO ()
lsDir flags withName dirpath = do
  names <- getDirectoryContents dirpath
  statuses <- sequence [getFileStatus (dirpath </> name) | name <- names]
  let items = zip names statuses
  when withName $ do
    putStr dirpath
    putStr ":\n"
  lsItems flags items

main :: IO ()
main = do
  (flags, filepaths) <- getOptions
  let names =
        if null filepaths
          then ["."]
          else filepaths
  statuses <- sequence [getFileStatus name | name <- names]
  let nast = sortBy (cmpFunc flags) $ zip names statuses
      (dirs, files) = partition (\(na, st) -> isDirectory st) nast
      dirWithName = length nast > 1
      chunks =
        [lsItems flags files | not (null files)] ++
        [lsDir flags dirWithName na | (na, st) <- dirs]
  sequence_ $ intersperse (putChar '\n') chunks
