{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Int
import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.Printf

data Flag =
  N
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
    options = [Option "n" ["number"] (NoArg N) "number all output lines"]

catNumbered :: L.ByteString -> IO ()
catNumbered s =
  (\(_, builder) -> B.hPutBuilder stdout builder) $
  L.foldl
    (\((n, newline), builder) ch ->
       ( case ch of
           '\n' -> (n + 1, True)
           _ -> (n, False)
       , if newline
           then let spaces = L.replicate (max 0 $ 6 - intLog10 n) ' '
                 in foldl
                      (<>)
                      builder
                      [ B.lazyByteString spaces
                      , B.int64Dec n
                      , B.char8 '\t'
                      , B.char8 ch
                      ]
           else builder <> B.char8 ch))
    ((1 :: Int64, True), mempty)
    s
  where
    intLog10 x
      | x < 10 = 1
      | otherwise = 1 + intLog10 (x `quot` 10)

main :: IO ()
main = do
  (flags, filepaths') <- getOptions
  let filepaths =
        if null filepaths'
          then ["-"]
          else filepaths'
--  let filters = [filterN']
--  let cat = \s -> putStr $ foldl (\s f -> f s) s filters
  let cat =
        if N `elem` flags
          then catNumbered
          else L.putStr
  forM_
    filepaths
    (\case
       "-" -> cat =<< L.getContents
       filename -> cat =<< L.readFile filename)
