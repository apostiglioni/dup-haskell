module Lib
    ( -- * Cryptographic hash functions
      sinkHash
    , hashFile
    ) where

import Crypto.Hash
import qualified Data.ByteString as B

import Data.Conduit
import Data.Conduit.Binary (sourceFile)
--
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)

-- | A 'Sink' that hashes a stream of 'B.ByteString'@s@ and
-- creates a digest @d@.
sinkHash :: (Monad m, HashAlgorithm hash) => Consumer B.ByteString m (Digest hash)
sinkHash = sink hashInit
  where sink ctx = do
            b <- await
            case b of
                Nothing -> return $! hashFinalize ctx
                Just bs -> sink $! hashUpdate ctx bs

-- | Hashes the whole contents of the given file in constant
-- memory.  This function is just a convenient wrapper around
-- 'sinkHash' defined as:
--
-- @
-- hashFile fp = 'liftIO' $ 'runResourceT' ('sourceFile' fp '$$' 'sinkHash')
-- @
hashFile :: (MonadIO m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashFile fp = liftIO $ runResourceT (sourceFile fp $$ sinkHash)
