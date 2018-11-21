module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Functor
import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import Numeric
import System.Console.GetOpt
import qualified System.Console.Terminal.Size as TS
import System.Directory hiding (isSymbolicLink)
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error
import System.Posix.Files
import System.Posix.Types
import System.Posix.User
import Text.Printf
import Text.Read

data Opts = Opts
    { flag_a :: Bool
    , flag_l :: Bool
    , flag_L :: Bool
    , flag_n :: Bool
    }

defaultOpts =
    Opts {flag_a = False, flag_l = False, flag_L = False, flag_n = False}

getOpts :: IO (Opts, [String])
getOpts = do
    args <- getArgs
    case getOpt RequireOrder opts args of
        (o, filepaths, []) -> return (foldl (flip id) defaultOpts o, filepaths)
        (_, _, errs) ->
            ioError (userError (concat errs ++ usageInfo header opts))
  where
    header = "Usage: cat [OPTION]... [FILE]..."
    opts =
        [ Option "l" [] (NoArg (\o -> o {flag_l = True})) "List in long format."
        , Option
              "a"
              []
              (NoArg (\o -> o {flag_a = True}))
              "Include directory entries whose names begin with a dot (.)."
        , Option
              "L"
              []
              (NoArg (\o -> o {flag_L = True}))
              "Follow all symbolic links to final target and list the file or directory the link references rather than the link itself.  This option cancels the -P option."
        , Option
              "n"
              []
              (NoArg (\o -> o {flag_n = True}))
              "Display user and group IDs numerically, rather than converting to a user or group name in a long (-l) output.  This option turns on the -l option."
              -- TODO: turn on -l
        ]

getUserOrGroupName ::
       (Show a) => Opts -> a -> (a -> IO b) -> (b -> String) -> IO (String)
getUserOrGroupName opts id entryGetter nameGetter =
    if flag_n opts
        then return $ show id
        else do
            entry <- try (entryGetter id)
            case entry of
                Right entry -> return $ nameGetter entry
                Left e
                    | isDoesNotExistError e -> return $ show id
                    | otherwise -> ioError e

getUserName :: Opts -> UserID -> IO (String)
getUserName opts userID =
    getUserOrGroupName opts userID getUserEntryForID userName

getGroupName :: Opts -> GroupID -> IO (String)
getGroupName opts groupID =
    getUserOrGroupName opts groupID getGroupEntryForID groupName

listToMatrix :: Int -> [String] -> [[String]]
listToMatrix rowSize list = addRow list
  where
    addRow [] = []
    addRow list =
        let (row, rest) = splitAt rowSize list
         in row : addRow rest

data PadSide
    = PadLeft
    | PadRight
    | NoPad

printLong :: Opts -> FilePath -> [(FilePath, FileStatus)] -> IO ()
printLong opts dirpath items = do
    (total, table) <- buildTable
    if length table > 0
        then do
            putStr "total "
            putStrLn $ show total
        else return ()
    forM_ table (printRow $ calcWidths table)
  where
    entryType status
        | isBlockDevice status = 'b'
        | isCharacterDevice status = 'c'
        | isDirectory status = 'd'
        | isSymbolicLink status = 'l'
        | isSocket status = 's'
        | isNamedPipe status = 'p'
        | otherwise = '-'
    hasMode mode filemode = intersectFileModes mode filemode == mode
    hasStickyBit (CMode modeWord) = testBit modeWord 9 -- TODO: should be in Posix lib
    fileModeStr status =
        let filemode = fileMode status
            qif a b c =
                if a filemode
                    then b
                    else c
         in [ (entryType status)
            , (qif (hasMode ownerReadMode) 'r' '-')
            , (qif (hasMode ownerWriteMode) 'w' '-')
            , (qif (hasMode ownerExecuteMode)
                   (qif (hasMode setUserIDMode) 's' 'x')
                   (qif (hasMode setUserIDMode) 'S' '-'))
            , (qif (hasMode groupReadMode) 'r' '-')
            , (qif (hasMode groupWriteMode) 'w' '-')
            , (qif (hasMode groupExecuteMode)
                   (qif (hasMode setGroupIDMode) 's' 'x')
                   (qif (hasMode setGroupIDMode) 'S' '-'))
            , (qif (hasMode otherReadMode) 'r' '-')
            , (qif (hasMode otherWriteMode) 'w' '-')
            , (qif (hasMode otherExecuteMode)
                   (qif hasStickyBit 't' 'x')
                   (qif hasStickyBit 'T' '-'))
          -- TODO: add '@' and '+' chars for XNU
            ]
    fileGroupName status =
        let groupID = fileGroup status
         in if flag_n opts
                then return $ show groupID
                else do
                    groupEntry <- try (getGroupEntryForID groupID)
                    case groupEntry of
                        Right GroupEntry {groupName = name} -> return name
                        Left e
                            | isDoesNotExistError e -> return $ show groupID
                            | otherwise -> ioError e
    getTimeStr currentTime status =
        let mtime = posixSecondsToUTCTime $ modificationTimeHiRes status
            age = diffUTCTime currentTime mtime
            -- taken from freebsd/bin/print.c
            sixMonths = fromInteger ((365 `quot` 2) * 86400) :: NominalDiffTime
            format =
                if age < sixMonths && (negate age) < sixMonths
                    then "%b %e %H:%M"
                    else "%b %e  %Y"
         in formatTime defaultTimeLocale format <$> utcToLocalZonedTime mtime
    addRow (total, rows) (path, status) = do
        ownerName <- getUserName opts (fileOwner status)
        groupName <- getGroupName opts (fileGroup status)
        currentTime <- getCurrentTime
        timeStr <- getTimeStr currentTime status
        lastCol <-
            if isSymbolicLink status
                then do
                    linkTarget <- getSymbolicLinkTarget (dirpath </> path)
                    return $ path ++ " -> " ++ linkTarget
                else return path
        let size = fileSize status
         in return
                ( total + (blockCount status)
                , [ (PadRight, fileModeStr status)
                  , (PadLeft, ' ' : (show $ linkCount status))
                  , (PadRight, ownerName)
                  , (PadRight, ' ' : groupName)
                  , (PadLeft, ' ' : (show $ size))
                  , (PadLeft, timeStr)
                  , (NoPad, lastCol)
                  ] :
                  rows)
    buildTable = do
        (total, reversedTable) <- foldM addRow (0, []) items
        return $ (total, reverse reversedTable)
    calcWidths table =
        [maximum [length s | (_, s) <- row] | row <- transpose table]
    printRow widths row = do
        sequence_ $
            intersperse
                (putChar ' ')
                [ let padding = replicate (width - length str) ' '
                   in case padSide of
                          PadLeft -> do
                              putStr padding
                              putStr str
                          PadRight -> do
                              putStr str
                              putStr padding
                          NoPad -> putStr str
                | ((padSide, str), width) <- zip row widths
                ]
        putChar '\n'

printShort :: [String] -> IO ()
printShort names = do
    (nColumns, columnWidth) <- tableParams names
    printTable nColumns columnWidth names
  where
    tableParams strings = do
        size <- TS.hSize stdout
        case size of
            Just w -> do
                columns <-
                    lookupEnv "COLUMNS" <&>
                    maybe (TS.width w) (maybe 80 id . readMaybe)
                let maxLength = maximum [length str | str <- strings]
                    tabWidth = 8
                    columnWidth =
                        (maxLength + tabWidth) `quot` tabWidth * tabWidth
                    nColumns = columns `quot` columnWidth
                 in return (nColumns, columnWidth)
            Nothing -> return (1, 0)
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

lsItems :: Opts -> FilePath -> [(FilePath, FileStatus)] -> IO ()
lsItems opts dirpath items = do
    let items' = sortBy (cmpFunc opts) $ filter itemFilter items
     in if flag_l opts
            then printLong opts dirpath items'
            else printShort [na | (na, _) <- items']
  where
    itemFilter =
        if flag_a opts
            then const True
            else (\(name, _) -> head name /= '.')

fileStatus :: Opts -> String -> IO FileStatus
fileStatus opts name = do
    if flag_L opts
        then getFileStatus name
        else do
            getSymbolicLinkStatus name

lsDir :: Opts -> Bool -> FilePath -> IO ()
lsDir opts withName dirpath = do
    names <- getDirectoryContents dirpath
    statuses <- sequence [fileStatus opts (dirpath </> name) | name <- names]
    let items = zip names statuses
    when withName $ do
        putStr dirpath
        putStr ":\n"
    lsItems opts dirpath items

cmpFunc :: Opts -> (FilePath, FileStatus) -> (FilePath, FileStatus) -> Ordering
cmpFunc opts (name1, status1) (name2, status2) = compare name1 name2

main :: IO ()
main = do
    (opts, filepaths) <- getOpts
    let names =
            if null filepaths
                then ["."]
                else filepaths
    statuses <- sequence [fileStatus opts name | name <- names]
    let nast = sortBy (cmpFunc opts) $ zip names statuses
        (dirs, files) = partition (\(na, st) -> isDirectory st) nast
        dirWithName = length nast > 1
     in do when (length files > 0) (lsItems opts "." files)
           when (length files > 0 && length dirs > 0) $ putChar '\n'
           sequence_ $
               intersperse
                   (putChar '\n')
                   [lsDir opts dirWithName dirName | (dirName, _) <- dirs]
