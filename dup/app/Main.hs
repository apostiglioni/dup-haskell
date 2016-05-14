{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
--import Pipes
--import qualified Pipes.Prelude as PP
--import qualified Pipes.ByteString as PB
import Control.Monad (forM_, unless, when)
import System.Directory (doesDirectoryExist, getDirectoryContents, canonicalizePath)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(ReadMode), isEOF)
import Crypto.Hash (Digest, hashInitWith, hashUpdate, hashFinalize, Context, hash)
import Crypto.Hash.Algorithms --(MD5)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.Posix


-- type Source m a = ConduitM () a m () -- no meaningful input or return value
-- type Conduit a m b = ConduitM a b m () -- no meaningful return value
-- type Sink a m b = ConduitM a Void m b -- no meaningful output value


import Data.Conduit
import Data.List (mapAccumL)

import qualified Data.Conduit.List as CL

import Text.Printf (printf)


--walk :: FilePath -> Source IO FilePath
--walk path = sourceDirectoryDeep False


walk2 :: FilePath -> Source IO FilePath
walk2 topPath = do
  names <- liftIO $ getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- liftIO $ doesDirectoryExist path
    if isDirectory
      then walk2 path
      else yield path

data FileData = FileData {
                  path   :: FilePath
                , status :: FileStatus
                }

instance Show FileData where
  --show fileData = "{ path='" ++ (path fileData) ++ "' }"
  show fileData = printf "{ path='%s' }" (path fileData)

--type FileSize = FileOffset


-- TODO: Partitioner could be a class defining the methods name and partition,
-- and FileSize and Digest would be instances uf the Partitioner class
data Partitioner = FileSize FileOffset --FileSize
                 | Digest -- TODO: There must be a way to generalize MD5
    deriving (Show)

--data Partitioner = Partitioner {
--                   name      :: String
--                 , partition :: PartitionMethod
--                 } deriving (Show)

data Cluster = Cluster { 
                 key :: [Partitioner] 
               , value :: [FileData]
               }
    deriving (Show)

getStatus :: Conduit FilePath IO FileData
getStatus = do
  -- TODO : awaitForever???
  maybePath <- await
  case maybePath of
    Nothing -> return ()
    Just path -> do
      status <- liftIO $ getFileStatus path
      yield $ FileData path status
      getStatus

gSize :: Conduit FileData IO Cluster
gSize = conduit Map.empty
  where
    conduit :: Map.Map FileOffset Cluster -> Conduit FileData IO Cluster
    conduit map = do
      maybeData <- await
      case maybeData of
        Just fileData -> do
          let size = fileSize (status fileData)
          let maybeDups = Map.lookup size map
          case maybeDups of
            -- In both cases I'm doing conduit $ Map.insert size <something>
            -- there must be a way to abstract that
            -- Map.insert size (fromMaybe (...) (...))
            Nothing -> conduit $ Map.insert size (Cluster [FileSize size] [fileData]) map
            Just cluster -> do
              let newCluster = Cluster (key cluster) (fileData : (value cluster))
              conduit $ Map.insert size newCluster map
        Nothing -> do
--        TODO:  traverse yield map
          Map.traverseWithKey (\k v -> yield v) map
          return ()



differentCanonicalPath :: Conduit FilePath IO FilePath
differentCanonicalPath = conduit Set.empty
  where
    conduit :: Set.Set FilePath -> Conduit FilePath IO FilePath
    conduit set = do
      maybePath <- await
      case maybePath of
        Nothing -> return ()
        Just path -> do
          canonicalPath <- liftIO $ canonicalizePath path
          if (Set.member canonicalPath set)
            then conduit set
            else do
              yield path
              conduit $ Set.insert canonicalPath set


--
-- walk2 "test/.test-data" $= differentCanonicalPath $= hashPipe $= conduitPrint
-- $$ CL.consume
--

-- sinkPrint :: Sink (FileHash MD5) IO ()
-- sinkPrint = CL.mapM_ $ print

sinkPutStrLn :: Sink String IO () -- consumes a stream of Strings, no result
sinkPutStrLn = CL.mapM_ putStrLn

--TODO Refactor hashPipe
--sink :: Sink String IO ()
--sink = do
--    mstr <- await
--    case mstr of
--        Nothing -> return ()
--        Just str -> do
--            liftIO $ putStrLn str
--            sink
--sink = awaitForever $ liftIO . putStrLn

--sizePipe :: Conduit FilePath IO Map
--

-- hashPipe :: Conduit FilePath IO (FileHash MD5)
-- hashPipe = do
--   maybePath <- await
--   case maybePath of
--     Nothing -> return ()
--     Just path -> do
--       md5 <- hashFile path
--       yield $ FileHash path md5
--       hashPipe

main :: IO ()
main = do
  [path] <- getArgs

  md5 <- hashFile "dup.cabal" :: IO (Digest MD5)
  print $ "dup.cabal " ++ show md5

--  hashFile "dup.cabal" >> print
----  Couldn't match expected type ‘IO ()’ with actual type ‘a0 -> IO ()’
----  Probable cause: ‘print’ is applied to too few arguments
----  In the second argument of ‘(>>)’, namely ‘print’
----  In a stmt of a 'do' block: hashFile "dup.cabal" >> print
