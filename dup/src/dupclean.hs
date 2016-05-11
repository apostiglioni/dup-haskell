import Control.Monad (forM)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, getDirectoryContents)

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topDir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

main = do
  contents <- getRecursiveContents "."
  print contents
