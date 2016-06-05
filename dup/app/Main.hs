{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Lib
import Category
import Control.Monad (forM_, unless, when, liftM2, liftM, (<=<), guard)
import System.Directory (doesDirectoryExist, getDirectoryContents, canonicalizePath)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), hIsEOF, withBinaryFile)
import Crypto.Hash (Digest, hashInitWith, hashUpdate, hashFinalize, Context, hash)
import Crypto.Hash.Algorithms --(MD5)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative (liftA2)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.Posix
import Data.Maybe (fromMaybe, fromJust)
import Data.Function (fix, on)

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
                 | MD5Digest (Digest MD5) -- TODO: There must be a way to generalize MD5
                 | Content
                 deriving (Show, Ord, Eq)


data Cluster a b = Cluster {
                     key   :: [a]
                   , content :: [b]
                   }
                   deriving (Show)


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

class (Monad m) => Class m cl k a | cl -> m k a where
  cluster :: [k] -> cl -> Cluster k a

instance Class IO [FileSummary] Partitioner FileSummary where
  cluster = Cluster . (Content : )

instance Class IO (Partitioner, [FileSummary]) Partitioner FileSummary where
  cluster x (key, val) = Cluster (key : x) val

class Classifier classifier a m c | classifier -> m c where
  classify :: (Monad m) => classifier -> [a] -> m [c]

instance Classifier (FileSummary -> IO Partitioner) FileSummary IO (Partitioner, [FileSummary]) where
  classify = classifyM

instance Classifier (FileSummary -> FileSummary -> IO Bool) FileSummary IO [FileSummary] where
  classify = classifyBinary


--instance Classifier (FileSummary -> FileSummary -> IO Bool) FileSummary IO (Partitioner, [FileSummary]) where
--  classify classifier = (fmap $ map (Content,)) . (classifyBinary classifier)
--
--newtype ConstantClassifier = ConstantClassifier (FileSummary -> FileSummary -> IO Bool)
--instance Classifier ConstantClassifier FileSummary IO (Partitioner, [FileSummary]) where
--  classify (ConstantClassifier classifier) = (fmap $ map (Content,)) . (classifyBinary classifier)

--dig :: (MonadIO m, Classifier classifier a IO (k, [a])) => classifier -> Conduit (Cluster k a) m (Cluster k a)
dig classifier =
  await >>= \case
    Nothing -> return ()
    Just ( Cluster clusterKey clusterValue ) -> do
      categories <- liftIO $ classify classifier clusterValue

      when (length clusterValue == length categories) $
        yield $ Cluster clusterKey clusterValue

      when (length clusterValue /= length categories) $
        mapM_ (yield . (cluster clusterKey)) categories

      dig classifier
--  where
--    cluster :: _
--    cluster x (key, val) = Cluster (key : x) val

cmpFilesData :: FileSummary -> FileSummary -> IO Bool
--cmpFilesData a b = cmpFiles (path a) (path b)
cmpFilesData = cmpFiles `on` path

cmpFiles :: FilePath -> FilePath -> IO Bool
--cmpFiles a b = liftM2 (==) (Data.ByteString.Lazy.readFile a) (Data.ByteString.Lazy.readFile b)
cmpFiles = liftA2 (==) `on` Data.ByteString.Lazy.readFile


mapSummary :: Conduit FilePath IO FileSummary
-- ^Maps a stream of FilePath to a stream of FileSummary
-- awaitForEver $ \path -> do
--   status <- liftIO $ getFileStatus path
--   yield $ FileSummary path status
--   TODO: investigate `map :: Monad m => (a -> b) -> Conduit a m b`
mapSummary = awaitForever $ \path ->
  liftIO (getFileStatus path) >>= yield . FileSummary path


gSize :: Conduit FileSummary IO (Cluster Partitioner FileSummary)
gSize = conduit Map.empty
  where
    conduit :: Map.Map FileOffset (Cluster Partitioner FileSummary) -> Conduit FileSummary IO (Cluster Partitioner FileSummary)
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

md5 :: FileSummary -> IO Partitioner
-- ^md5 :: FileSummary -> IO Partitioner
--
-- TODO: Por que son iguales? Por que en uno return y en otro fmap
--  md5 p = do
--    hash <- hashFile $ path p
--    return $ MD5Digest hash
md5 = fmap MD5Digest . hashFile . path



main :: IO [Cluster Partitioner FileSummary]
--main = walk2 "test" $= prune $= mapSummary $= gSize $= (dig md5) $= (dig cmpFilesData) $= (dig.ConstantClassifier $ cmpFilesData) $$ CL.consume
main = walk2 "test" $= prune $= mapSummary $= gSize $= (dig md5) $= (dig cmpFilesData) $$ CL.consume
