-- walk2 "test" $= prune $= mapSummary $= gSize $= (dig md5) $= (digContent cmpFilesData)$$ CL.consume

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Lib
--import Pipes
--import qualified Pipes.Prelude as PP
--import qualified Pipes.ByteString as PB
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

type ClusterKey = [Partitioner]

data Cluster a b = Cluster {
                        key   :: [a]
                      , value :: [b]
                      }
    deriving (Show)


dig :: (FileSummary -> IO Partitioner) -> ConduitM (Cluster Partitioner FileSummary) (Cluster Partitioner FileSummary) IO ()
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
          (`Map.traverseWithKey` categories) $ \key -> 
            yield . Cluster (key : clusterKey)
          dig classifier


digContent classifier =
  await >>= \case
    Nothing -> return ()
    Just ( Cluster clusterKey clusterValue ) -> do
      categories <- liftIO $ classifyContent classifier clusterValue
      if (length clusterValue == length categories)
        then do
          -- (yield . Cluster clusterKey) clusterValue
          yield $ Cluster clusterKey clusterValue
          digContent classifier       -- TODO: Why can't this be put out of the if?
        else do
          let newKey = Content : clusterKey
          -- yield categories as a new clusters
          mapM_ (yield . Cluster newKey) categories
          digContent classifier

cmpFilesData :: FileSummary -> FileSummary -> IO Bool
cmpFilesData a b = cmpFiles (path a) (path b)

cmpFiles :: FilePath -> FilePath -> IO Bool
cmpFiles a b = liftM2 (==) (Data.ByteString.Lazy.readFile a) (Data.ByteString.Lazy.readFile b)

classifyContent :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
classifyContent classifier list = fmap (fromMaybe []) (classifyContent' classifier list)

classifyContent' :: Monad m => (a -> a -> m Bool) -> [a] -> m (Maybe [[a]])
--classifyContent' :: (FilePath -> FilePath -> IO Bool) -> [FilePath] -> IO (Maybe [[FilePath]])
-- TODO: Investigate Monad vs Applicative
classifyContent'          _ []       = return Nothing
classifyContent'          _ [e]      = return $ Just [[e]]
classifyContent' classifier elements = do
  (equal, different) <- accumulate (head elements) elements ([], [])

  classifyContent' classifier different >>= \case
    Nothing     -> return $ Just [equal]
    Just more -> return $ Just (equal : more)

  where
--    accumulate :: FilePath -> [FilePath] -> ([FilePath], [FilePath]) -> IO ([FilePath], [FilePath])
    accumulate file [] (eq, df)           = return (eq, df)
    accumulate file (other:rest) (eq, df) = do
      isEqual <- classifier file other
      if isEqual
        then accumulate file rest (other:eq, df)
        else accumulate file rest (eq, other:df)


classifyM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m (Map.Map k [a])
-- TODO: Investigate Monad vs Applicative
classifyM classifier = loop Map.empty
  where
    loop acc [] = return acc
    loop acc (x : xs) = do
      category <- classifier x
      let members = fromMaybe [] (Map.lookup category acc)
      loop (Map.insert category (x : members) acc) xs


classify :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
classify classifier = loop Map.empty
  where
    loop acc [] = acc
    loop acc (x : xs) = do
      let category = classifier x
      let members = fromMaybe [] (Map.lookup category acc)
      loop (Map.insert category (x : members) acc) xs


type FileSummaryCluster = Cluster Partitioner FileSummary


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



md5 :: FileSummary -> IO Partitioner
-- TODO: Por que son iguales? Por que en uno return y en otro fmap
--  md5 p = do
--    hash <- hashFile $ path p
--    return $ D hash
md5 = fmap D . hashFile . path
  
