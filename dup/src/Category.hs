{-# LANGUAGE LambdaCase #-}

module Category
       ( CategoryClassifier
       , ComparingClassifierM
       , CategoryClassifierM
       , classifyM
       , classifyContent
       ) where

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as Map

type CategoryClassifier a k = a -> k        -- TODO: Ord k => ...
type CategoryClassifierM a m k = a -> m k   -- TODO (Monad m, Ord k) =>

type ComparingClassifierM a m = a -> a -> m Bool


classifyContent :: Monad m => ComparingClassifierM a m -> [a] -> m [[a]]
classifyContent classifier list = fmap (fromMaybe []) (classifyContent' classifier list)


classifyContent' :: Monad m => ComparingClassifierM a m -> [a] -> m (Maybe [[a]])
-- TODO: Investigate Monad vs Applicative
classifyContent'          _ []       = return Nothing
classifyContent'          _ [e]      = return $ Just [[e]]
classifyContent' classifier elements = do
  (equal, different) <- accumulate (head elements) elements ([], [])

  classifyContent' classifier different >>= \case
    Nothing     -> return $ Just [equal]
    Just more -> return $ Just (equal : more)

  where
    accumulate file [] (eq, df)           = return (eq, df)
    accumulate file (other:rest) (eq, df) = do
      isEqual <- classifier file other
      if isEqual
        then accumulate file rest (other:eq, df)
        else accumulate file rest (eq, other:df)


classifyM :: (Monad m, Ord k) => CategoryClassifierM a m k -> [a] -> m (Map.Map k [a])
-- TODO: Investigate Monad vs Applicative
classifyM classifier = loop Map.empty
  where
    loop acc [] = return acc
    loop acc (x : xs) = do
      category <- classifier x
      let members = fromMaybe [] (Map.lookup category acc)
      loop (Map.insert category (x : members) acc) xs


classify :: Ord k => CategoryClassifier a k -> [a] -> Map.Map k [a]
classify classifier = loop Map.empty
  where
    loop acc [] = acc
    loop acc (x : xs) = do
      let category = classifier x
      let members = fromMaybe [] (Map.lookup category acc)
      loop (Map.insert category (x : members) acc) xs
