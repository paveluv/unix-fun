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

cmpFunc :: Opts -> (FilePath, FileStatus) -> (FilePath, FileStatus) -> Ordering
cmpFunc opts (name1, status1) (name2, status2) = compare name1 name2

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
                lookupEnv "COLUMNS" <&>
                maybe (TS.width w) (maybe 80 id . readMaybe)
            let maxLength = maximum [length str | str <- strings]
                tabWidth = 8
                columnWidth = (maxLength + tabWidth) `quot` tabWidth * tabWidth
                nColumns = columns `quot` columnWidth
             in return (nColumns, columnWidth)
        Nothing -> return (1, 0)

printLong :: Opts -> [(FilePath, FileStatus)] -> IO ()
printLong opts items = printTable
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
    hasStickyBit (CMode modeWord) = testBit modeWord 9 -- TODO: should be in XNU lib
    fileModeStr status =
        let filemode = fileMode status
         in [ (entryType status)
            , (if hasMode ownerReadMode filemode
                   then 'r'
                   else '-')
            , (if hasMode ownerWriteMode filemode
                   then 'w'
                   else '-')
            , (let suid = hasMode setUserIDMode filemode
                in if hasMode ownerExecuteMode filemode
                       then if suid
                                then 's'
                                else 'x'
                       else if suid
                                then 'S'
                                else '-')
            , (if hasMode groupReadMode filemode
                   then 'r'
                   else '-')
            , (if hasMode groupWriteMode filemode
                   then 'w'
                   else '-')
            , (let guid = hasMode setGroupIDMode filemode
                in if hasMode groupExecuteMode filemode
                       then if guid
                                then 's'
                                else 'x'
                       else if guid
                                then 'S'
                                else '-')
            , (if hasMode otherReadMode filemode
                   then 'r'
                   else '-')
            , (if hasMode otherWriteMode filemode
                   then 'w'
                   else '-')
            , (let sticky = hasStickyBit filemode
                in if hasMode otherExecuteMode filemode
                       then if sticky
                                then 't'
                                else 'x'
                       else if sticky
                                then 'T'
                                else '-')
          -- TODO: add '@' and '+' chars for XNU
            ]
    fileOwnerName status =
        let userID = fileOwner status
         in if flag_n opts
                then return $ show userID
                -- TODO: fetching entries is very slow, it should be cached
                else do
                    userEntry <- try (getUserEntryForID userID)
                    case userEntry of
                        Right UserEntry {userName = name} -> return name
                        Left e
                            | isDoesNotExistError e -> return $ show userID
                            | otherwise -> ioError e
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
    timeStr tz currentTime status =
        let mtime = posixSecondsToUTCTime $ modificationTimeHiRes status
            age = diffUTCTime currentTime mtime
            -- taken from freebsd/bin/print.c
            sixMonths = fromInteger ((365 `quot` 2) * 86400) :: NominalDiffTime
            format =
                if age < sixMonths && (negate age) < sixMonths
                    then "%b %d %H:%M"
                    else "%b %d  %Y"
         in return $
            formatTime defaultTimeLocale format $ utcToLocalTime tz mtime
    table = do
        tz <- getCurrentTimeZone
        currentTime <- getCurrentTime
        sequence
            [ sequence
                [ return $ fileModeStr status
                , return $ show $ linkCount status
                , (++ " ") <$> fileOwnerName status {- extra space as per XNU -}
                , (++ " ") <$> fileGroupName status
                , return $ show $ fileSize status
                , timeStr tz currentTime status
                , return path
                ]
            | (path, status) <- items
            ]
    widths table = [maximum [length s | s <- row] | row <- transpose table]
    padLefts = [True, False, True, True, False, False, True]
    printRow row = do
        widths <- widths <$> table
        putStr $
            intercalate
                " "
                [ let padding = replicate (width - length str) ' '
                   in if padLeft
                          then str ++ padding
                          else padding ++ str
                | (str, width, padLeft) <- (zip3 row widths padLefts)
                ]
        putChar '\n'
    printTable = do
        table <- table
        forM_ table printRow

lsItems :: Opts -> [(FilePath, FileStatus)] -> IO ()
lsItems opts items' = do
    let items = filter itemFilter items'
        sortedItems = sortBy (cmpFunc opts) items
     in if flag_l opts
            then printLong opts sortedItems
            else let names = [na | (na, _) <- sortedItems]
                  in do (nColumns, columnWidth) <- tableParams names
                        printTable nColumns columnWidth names
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
            isLink <- pathIsSymbolicLink name
            if isLink
                then do
                    getSymbolicLinkStatus name
                else getFileStatus name

lsDir :: Opts -> Bool -> FilePath -> IO ()
lsDir opts withName dirpath = do
    names <- getDirectoryContents dirpath
    statuses <- sequence [fileStatus opts (dirpath </> name) | name <- names]
    let items = zip names statuses
    when withName $ do
        putStr dirpath
        putStr ":\n"
    lsItems opts items

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
        chunks =
            [lsItems opts files | not (null files)] ++
            [lsDir opts dirWithName na | (na, st) <- dirs]
    sequence_ $ intersperse (putChar '\n') chunks
