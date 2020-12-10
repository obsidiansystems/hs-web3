{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      :  Data.Solidity.Abi.Generic
-- Copyright   :  Alexander Krupenkin 2017-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- This module is internal, the purpose is to define helper classes and functions
-- to assist in encoding and decoding Solidity types for function calls and events.
-- The user of this library should have no need to use this directly in application code.
--

module Data.Solidity.Abi.Generic where

import qualified Data.ByteString.Lazy   as LBS
import           Data.Int               (Int64)
import           Data.Proxy             (Proxy (..))
import           Data.Serialize         (Get, Put)
import           Data.Serialize.Get     (bytesRead, lookAheadE, skip)
import           Data.Serialize.Put     (runPutLazy)
import           Generics.SOP           (I (..), NP (..), NS (..), SOP (..))

import           Data.Solidity.Abi      (AbiGet (..), AbiPut (..), AbiType (..),
                                         GenericAbiGet (..), GenericAbiPut (..))
import           Data.Solidity.Prim.Int (getWord256, putWord256)

data Encoding = Encoding
  { encoding :: Put
  , len :: Int64
  , isDyn :: Bool
  }

makeEncoding :: forall b. (AbiType b, AbiPut b) => b -> Encoding
makeEncoding b = Encoding
  { encoding = p
  , len = LBS.length (runPutLazy p)
  , isDyn = isDynamic (Proxy :: Proxy b)
  }
  where
    p = abiPut b

combineEncodedValues :: [Encoding] -> Put
combineEncodedValues encodings = putHeads tailsStart [] encodings
  where
    tailsStart :: Int64
    tailsStart = sum $ flip fmap encodings $ \e ->
      if isDyn e
      then 32
      else len e

    putHeads tailOffset tails = \case
      [] -> sequence_ $ reverse tails
      (x:xs) -> case isDyn x of
        False -> do
          encoding x
          putHeads tailOffset tails xs
        True ->  do
          putWord256 (fromIntegral tailOffset)
          putHeads (tailOffset + len x) (encoding x : tails) xs

class AbiData a where
    _serialize :: a -> [Encoding]

instance AbiData (NP f '[]) where
    _serialize Nil = []

instance (AbiType b, AbiPut b, AbiData (NP I as)) => AbiData (NP I (b :as)) where
    _serialize (I b :* a) = makeEncoding b : _serialize a

instance AbiData (NP f as) => GenericAbiPut (SOP f '[as]) where
    gAbiPut (SOP (Z a)) = combineEncodedValues $ _serialize a
    gAbiPut _           = error "Impossible branch"

instance GenericAbiGet (NP f '[]) where
    gAbiGet = return Nil

instance (AbiGet a, GenericAbiGet (NP I as)) => GenericAbiGet (NP I (a : as)) where
    gAbiGet = (:*) <$> (I <$> factorParser) <*> gAbiGet

instance GenericAbiGet (NP f as) => GenericAbiGet (SOP f '[as]) where
    gAbiGet = SOP . Z <$> gAbiGet

factorParser :: forall a . AbiGet a => Get a
factorParser
  | not $ isDynamic (Proxy :: Proxy a) = abiGet
  | otherwise = do
        dataOffset <- fromIntegral <$> getWord256
        currentOffset <- bytesRead
        Left x <- lookAheadE $ do
            skip (dataOffset - currentOffset)
            Left <$> abiGet
        return x

{-
factorParser' :: forall a. AbiGet a => Int -> Get [a]
factorParser' = go [] True
  where
    getHeads = do


    go isHead
      | not isHead || not (isDynamic (Proxy :: Proxy a)) = abiGet
      | otherwise = do
          dataOffset <- fromIntegral <$> getWord256
          currentOffset <- bytesRead
          Left x <- lookAheadE $ do
            skip (dataOffset - currentOffset)
            Left <$> go False
          return x
-}
