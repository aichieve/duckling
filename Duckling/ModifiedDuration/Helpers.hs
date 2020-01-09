-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.ModifiedDuration.Helpers
  ( empty
  , isPositive
  , withInterval
  , withApproximate
  , withMin
  , withMax
  ) where

import Duckling.Dimensions.Types
import Duckling.ModifiedDuration.Types (ModifiedDurationData(..))
import Duckling.Types hiding (Entity(value))
import Duckling.TimeGrain.Types (Grain(..), inSeconds)

-- -----------------------------------------------------------------
-- Patterns
isPositive :: Predicate
isPositive (Token ModifiedDuration ModifiedDurationData{ value = Just v, minValue = Nothing, maxValue = Nothing }) = v >= 0
isPositive (Token ModifiedDuration ModifiedDurationData{ value = Nothing, minValue = Just v, maxValue = Nothing }) = v >= 0
isPositive (Token ModifiedDuration ModifiedDurationData{ value = Nothing, minValue = Nothing, maxValue = Just v }) = v > 0
isPositive (Token ModifiedDuration ModifiedDurationData{ value = Nothing, minValue = Just min, maxValue = Just max }) = min >= 0 && min < max
isPositive _ = False

-- -----------------------------------------------------------------
-- Production
empty :: Grain -> ModifiedDurationData
empty g = ModifiedDurationData
  { value = Nothing
  , minValue = Nothing
  , maxValue = Nothing
  , grain = g
  }

withInterval :: (Int, Int) -> ModifiedDurationData -> ModifiedDurationData
withInterval (from, to) fd = fd
  { minValue = Just from
  , maxValue = Just to
  , value = Nothing }

withApproximate :: Int -> ModifiedDurationData -> ModifiedDurationData
withApproximate x fd = fd { value = Just x }

withMin :: Int -> ModifiedDurationData -> ModifiedDurationData
withMin x fd = fd { minValue = Just x}

withMax :: Int -> ModifiedDurationData -> ModifiedDurationData
withMax x fd = fd { maxValue = Just x }



