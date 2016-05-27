{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
import Category
import Control.Monad (forM_, unless, when, liftM2, liftM, (<=<))
import System.Directory (doesDirectoryExist, getDirectoryContents, canonicalizePath)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), hIsEOF, withBinaryFile)
import Crypto.Hash (Digest, hashInitWith, hashUpdate, hashFinalize, Context, hash)
import Crypto.Hash.Algorithms --(MD5)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.Posix
import Data.Maybe (fromMaybe, fromJust)
import Data.Function (fix)

-- type Source m a = ConduitM () a m () -- no meaningful input or return value
-- type Conduit a m b = ConduitM a b m () -- no meaningful return value
-- type Sink a m b = ConduitM a Void m b -- no meaningful output value



import Data.Conduit
import Data.List (mapAccumL)

import qualified Data.Conduit.List as CL

import Text.Printf (printf)

import Data.ByteString.Lazy (hGet, readFile)

data FileSummary = FileSummary {
                  path   :: FilePath
                , status :: FileStatus
                }
instance Show FileSummary where
  --show fileData = "{ path='" ++ (path fileData) ++ "' }"
  show fileData = printf "{ path='%s' }" (path fileData)


data Partitioner = FileSize FileOffset --FileSize
                 | D (Digest MD5) -- TODO: There must be a way to generalize MD5
                 | Content
                 deriving (Show, Ord, Eq)


data Cluster a b = Cluster {
                     key   :: [a]
                   , content :: [b]
                   }
                   deriving (Show)


type ClusterKey = [Partitioner]

type FileSummaryCluster = Cluster Partitioner FileSummary

-- walk :: FilePath -> Source IO FilePath
-- walk path = sourceDirectoryDeep False


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

-- 
-- type CategoryClassifierM a m k = a -> m k   -- TODO (Monad m, Ord k) =>
-- classifyM :: (Monad m, Ord k) => CategoryClassifierM a m k -> [a] -> m (Map.Map k [a])
-- classifyM :: (Monad m, Ord k) => CategoryClassifierM a m k -> [a] -> m [(k, [a])]
-- 
-- type ComparingClassifierM a m = a -> a -> m Bool
-- classifyContent :: Monad m => ComparingClassifierM a m -> [a] -> m [[a]]
-- 
-- 
-- class Dig t where
--   classify :: (Traversable t, Monad m) => [a] -> m t [c]  -- >--- esto reemplaza a classifyM y classifyContent
--   map :: c -> b -> Cluster a b   -- ---> esto reemplaza a la creacion del cluster
-- 

-- class Dig t where
--   classify :: (Traversable t, Monad m) => [a] -> m t [b]  -- >--- esto reemplaza a classifyM y classifyContent
--   map :: a -> b -> Cluster a b   -- ---> esto reemplaza a la creacion del cluster
-- 


dig :: CategoryClassifierM FileSummary IO Partitioner -> Conduit (Cluster Partitioner FileSummary) IO (Cluster Partitioner FileSummary) 
dig classifier =
  await >>= \case
    Nothing -> return ()
    Just ( Cluster clusterKey clusterValue ) -> do
      categories <- liftIO $ classifyM classifier clusterValue
      if (length clusterValue == Map.size categories)
        then do
          yield $ Cluster clusterKey clusterValue
          dig classifier
        else do
          (`Map.traverseWithKey` categories) $ \key -> yield . Cluster (key : clusterKey)
          dig classifier

digContent :: ComparingClassifierM FileSummary IO -> Conduit (Cluster Partitioner FileSummary) IO (Cluster Partitioner FileSummary)
digContent classifier =
  await >>= \case
    Nothing -> return ()
    Just ( Cluster clusterKey clusterValue ) -> do
      categories <- liftIO $ classifyContent classifier clusterValue
      if (length clusterValue == length categories)
        then do
          yield $ Cluster clusterKey clusterValue
          digContent classifier       -- TODO: Why can't this be put out of the if?
        else do
          mapM_ (yield . Cluster (Content : clusterKey)) categories    -- yield categories as a new clusters
          digContent classifier

cmpFilesData :: FileSummary -> FileSummary -> IO Bool
cmpFilesData a b = cmpFiles (path a) (path b)

cmpFiles :: FilePath -> FilePath -> IO Bool
cmpFiles a b = liftM2 (==) (Data.ByteString.Lazy.readFile a) (Data.ByteString.Lazy.readFile b)


mapSummary :: Conduit FilePath IO FileSummary
-- ^Maps a stream of FilePath to a stream of FileSummary
-- awaitForEver $ \path -> do
--   status <- liftIO $ getFileStatus path
--   yield $ FileSummary path status
--   TODO: investigate `map :: Monad m => (a -> b) -> Conduit a m b`
mapSummary = awaitForever $ \path ->
  liftIO (getFileStatus path) >>= yield . FileSummary path


gSize :: Conduit FileSummary IO FileSummaryCluster
gSize = conduit Map.empty
  where
    conduit :: Map.Map FileOffset FileSummaryCluster -> Conduit FileSummary IO FileSummaryCluster
    conduit map =
      await >>= \case
        Just fileData ->
          let size = fileSize (status fileData) in
          case (Map.lookup size map) of
            -- In both cases I'm doing conduit $ Map.insert size <something>
            -- there must be a way to abstract that
            -- Map.insert size (fromMaybe (...) (...))
            Nothing -> conduit $ Map.insert size (Cluster [FileSize size] [fileData]) map
            Just cluster ->
              let newCluster = Cluster (key cluster) (fileData : (content cluster)) in
              conduit $ Map.insert size newCluster map
        Nothing -> do
          traverse yield map
          --Map.traverseWithKey (\k v -> yield v) map
          return ()


prune :: Conduit FilePath IO FilePath
-- ^Prunes files with the same cannonical path.
prune = conduit Set.empty
  -- TODO: Se puede usar un monad en vez de inventar una funcion que recibe un
  -- parametro mas para mantener el estado???
  where
    conduit :: Set.Set FilePath -> Conduit FilePath IO FilePath
    conduit set =
      await >>= \case
        Nothing -> return ()
        Just path -> do
          canonicalPath <- liftIO $ canonicalizePath path
          if (canonicalPath `Set.member` set)
            then conduit set
            else do
              yield path
              conduit $ Set.insert canonicalPath set

md5 :: CategoryClassifierM FileSummary IO Partitioner
-- ^md5 :: FileSummary -> IO Partitioner
--
-- TODO: Por que son iguales? Por que en uno return y en otro fmap
--  md5 p = do
--    hash <- hashFile $ path p
--    return $ D hash
md5 = fmap D . hashFile . path


-- main :: IO ()
-- main = walk2 "test" $= prune $= mapSummary $= gSize $= (dig md5) $= (digContent cmpFilesData)$$ CL.consume
