{-# LANGUAGE NoImplicitPrelude #-}

module Origin where

import Data.Eq(Eq(..))
import Data.Functor ((<$>))
import Data.Function ((.), ($))
import Data.Either (either, Either(..))
import Data.Map.Strict (Map, lookup)
import Data.Maybe (Maybe(..))
import Data.Ord(Ord(..))
import Control.Applicative((<*>))
import Control.Monad ((>>), (>>=), return)
import Text.Show (Show(..))
