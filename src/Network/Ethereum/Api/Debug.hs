{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Web3.Eth
-- Copyright   :  Obsidian Systems 2020
-- License     :  BSD3
--
-- Maintainer  :  maintainer@obsidian.systems
-- Stability   :  experimental
-- Portability :  unknown
--
-- Go Ethereum node JSON-RPC API methods with `debug_` prefix.
--

module Network.Ethereum.Api.Debug where

import           Data.Text                  (Text)
import           Data.Word
import           Network.JsonRpc.TinyClient (JsonRpc (..))

getBlockRlp :: JsonRpc m => Word64 -> m Text
{-# INLINE getBlockRlp #-}
getBlockRlp = remote "debug_getBlockRlp"
