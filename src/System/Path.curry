------------------------------------------------------------------------------
--- Library to support operations related to the program execution path,
--- i.e., environment variable `PATH`.
---
--- @author Michael Hanus
--- @version November 2018
------------------------------------------------------------------------------

module System.Path
  ( dirsInPath, fileInPath, getFileInPath )
 where

import Directory ( doesFileExist, getAbsolutePath )
import FilePath  ( (</>), searchPathSeparator )
import List      ( splitOn )
import System    ( getEnviron )

--- Returns the list of the directories of the environment variable `PATH`.
dirsInPath :: IO [String]
dirsInPath = do
  path <- getEnviron "PATH"
  return $ splitOn [searchPathSeparator] path

--- Checks whether a file exists in one of the directories
--- of the environment variable `PATH`.
fileInPath :: String -> IO Bool
fileInPath file = do
  dirs <- dirsInPath
  (liftIO (any id)) $ mapIO (doesFileExist . (</> file)) dirs

--- Checks whether a file exists in one of the directories
--- of the environment variable `PATH` and returns its absolute path,
--- otherwise returns `Nothing`.
getFileInPath :: String -> IO (Maybe String)
getFileInPath file = dirsInPath >>= checkPath
 where
  checkPath [] = return Nothing
  checkPath (dir:dirs) = do
    let dirfile = dir </> file
    direx <- doesFileExist dirfile
    if direx then getAbsolutePath dirfile >>= return . Just
             else checkPath dirs

------------------------------------------------------------------------------
