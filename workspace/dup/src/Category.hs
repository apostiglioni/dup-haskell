{-# LANGUAGE LambdaCase #-}

module Category
       ( classifyM
       , classify'
       , classifyBinary
       , classifyBinary'
       ) where

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as Map


classifyBinary :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
classifyBinary classifier list = fmap (fromMaybe []) (classifyBinary' classifier list)

classifyBinary' :: Monad m => (a -> a -> m Bool) -> [a] -> m (Maybe [[a]])
-- TODO: Investigate Monad vs Applicative
classifyBinary'          _ []       = return Nothing
classifyBinary'          _ [e]      = return $ Just [[e]]
classifyBinary' classifier elements = do
  (equal, different) <- accumulate (head elements) elements ([], [])

  classifyBinary' classifier different >>= \case
    Nothing     -> return $ Just [equal]
    Just more -> return $ Just (equal : more)

  where
    accumulate file [] (eq, df)           = return (eq, df)
    accumulate file (other:rest) (eq, df) = do
      isEqual <- classifier file other
      if isEqual
        then accumulate file rest (other:eq, df)
        else accumulate file rest (eq, other:df)


classifyM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [(k, [a])]
-- TODO: Investigate Monad vs Applicative
classifyM classifier = loop Map.empty
  where
    loop acc [] = return $ Map.toList acc
    loop acc (x : xs) = do
      category <- classifier x
      let members = fromMaybe [] (Map.lookup category acc)
      loop (Map.insert category (x : members) acc) xs


classify' :: Ord k => (a -> k) -> [a] -> [(k, [a])]
classify' classifier = loop Map.empty
  where
    loop acc [] = Map.toList acc
    loop acc (x : xs) = do
      let category = classifier x
      let members = fromMaybe [] (Map.lookup category acc)
      loop (Map.insert category (x : members) acc) xs
