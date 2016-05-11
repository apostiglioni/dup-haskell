module Main where

import Lib
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PB
import Control.Monad (forM_, unless, when)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(ReadMode), isEOF)
import Crypto.Hash (Digest, hashInitWith, hashUpdate, hashFinalize, Context)
import Crypto.Hash.Algorithms --(MD5)

import Data.List (mapAccumL)

walk :: FilePath -> Producer FilePath IO ()
walk topPath = do
  names <- lift $ getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- lift $ doesDirectoryExist path
    if isDirectory
      then walk path
      else yield path


--hashFile :: FilePath -> Producer (Digest MD5) IO ()
hashFileWorking file = withFile file ReadMode $ \h ->
  runEffect $
    for (PB.fromHandle h) $ \b ->
      lift $ print b

hashBytes ctx = do
  bytes <- await

--  lift $ print "Hola Mundo"
--  lift $ print bytes
--  lift $ print "Hola Mundo"

--  hashBytes (hashUpdate (ctx :: Context MD5) bytes)
--  hashBytes (hashUpdate ctx bytes)
  hashBytes (hashUpdate ctx bytes)


hashFile file = withFile file ReadMode $ \h ->
  runEffect $ PB.fromHandle h >-> hashBytes (hashInitWith MD5) >-> PP.stdoutLn

main :: IO ()
main = do
  [path] <- getArgs
  runEffect $
    walk path
    >-> PP.map ("Found file " ++)
    >-> PP.stdoutLn
