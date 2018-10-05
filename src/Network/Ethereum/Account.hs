-- |
-- Module      :  Network.Ethereum.Account
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Network.Ethereum.Account (
      Account(..)

    , DefaultAccount

    , PersonalAccount
    , Personal(..)

    , withParam
    , to
    , value
    , gasLimit
    , gasPrice
    , block
    , account
    , (&)
    , (.~)

    ) where

import           Lens.Micro                        ((&), (.~))

import           Network.Ethereum.Account.Class    (Account (..))
import           Network.Ethereum.Account.Default  (DefaultAccount)
import           Network.Ethereum.Account.Internal (account, block, gasLimit,
                                                    gasPrice, to, value,
                                                    withParam)
import           Network.Ethereum.Account.Personal (Personal (..),
                                                    PersonalAccount)
