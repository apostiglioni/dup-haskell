-- walk2 "test" $= differentCanonicalPath $= getStatus $= gSize $= (dig md5) $$ CL.consume


-- <srhb> :t fmap
-- [11:57] <lambdabot> Functor f => (a -> b) -> f a -> f b
-- [11:58] == jfb_ [~jfb_@107.179.158.248] has joined #haskell-beginners
-- [11:58] <srhb> Here, a ~ [Digest MD5], f ~ IO and b ~ Map FilePath (Digest MD5)



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
import Data.Maybe (fromMaybe)


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
                 | D (Digest MD5) -- TODO: There must be a way to generalize MD5
    deriving (Show, Ord, Eq)

--data Partitioner = Partitioner {
--                   name      :: String
--                 , partition :: PartitionMethod
--                 } deriving (Show)

data Cluster a b = Cluster {
                        key   :: [a]
                      , value :: [b]
                      }
    deriving (Show)


--dig :: (Monad m, Partitioner k) => (a -> m k) -> Cluster k a -> [Cluster k a]
--dig :: (Monad m) => (a -> m k) -> Conduit (Cluster k a) IO (Cluster k a)
-- TODO: Review type
dig categorizer = do
  maybeCluster <- await
  case maybeCluster of
    Nothing -> return ()
    Just ( Cluster k v ) -> do
      categories <- liftIO $ categorizeM (categorizer . path) v
      Map.traverseWithKey 
        (\categoryKey categoryValue -> yield $ Cluster (D categoryKey : k) categoryValue) 
        categories
      dig categorizer

  
  

categorizeM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m (Map.Map k [a])
categorizeM categorizer = loop Map.empty
-- TODO: Investigate Monad vs Applicative
-- TODO: Generalize list to Traversable
  where
    loop acc [] = return acc
    loop acc (x : xs) = do
      category <- categorizer x
      let members = fromMaybe [] (Map.lookup category acc)
      loop (Map.insert category (x : members) acc) xs


categorize :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
categorize categorizer = loop Map.empty
  where
    loop acc [] = acc
    loop acc (x : xs) = do
      let category = categorizer x
      let members = fromMaybe [] (Map.lookup category acc)
      loop (Map.insert category (x : members) acc) xs


type FileDataCluster = Cluster Partitioner FileData

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

gMD5 :: Conduit FileDataCluster IO FileDataCluster
gMD5 = conduit Map.empty
  where
    conduit :: Map.Map (Digest MD5) FileDataCluster -> Conduit FileDataCluster IO FileDataCluster
    conduit map = do
      maybeCluster <- await
      case maybeCluster of
        Just ( Cluster k v ) -> do
          
          liftIO $ print v
--
--          md5 <- liftIO (hashFile "dup.cabal" :: IO (Digest MD5))
--          liftIO $ print md5
          yield $ Cluster k v
          conduit map
        Nothing -> return ()

gSize :: Conduit FileData IO FileDataCluster
gSize = conduit Map.empty
  where
    conduit :: Map.Map FileOffset FileDataCluster -> Conduit FileData IO FileDataCluster
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

md5 :: FilePath -> IO (Digest MD5)
md5 = hashFile

