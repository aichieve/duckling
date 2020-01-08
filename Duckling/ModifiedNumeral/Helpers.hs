-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.ModifiedNumeral.Helpers
  ( empty
  , isPositive
  , withInterval
  , withApproximate
  , withMin
  , withMax
  ) where

import Duckling.Dimensions.Types
import Duckling.ModifiedNumeral.Types
import Duckling.Types hiding (Entity(value))

-- -----------------------------------------------------------------
-- Patterns
isPositive :: Predicate
isPositive (Token ModifiedNumeral ModifiedNumeralData{value = Just v, minValue = Nothing, maxValue = Nothing}) = v >= 0
isPositive (Token ModifiedNumeral ModifiedNumeralData{value = Nothing, minValue = Just v, maxValue = Nothing}) = v >= 0
isPositive (Token ModifiedNumeral ModifiedNumeralData{value = Nothing, minValue = Nothing, maxValue = Just v}) = v > 0
isPositive (Token ModifiedNumeral ModifiedNumeralData{value = Nothing, minValue = Just min, maxValue = Just max}) = min >=0 && min < max
isPositive _ = False
-- -----------------------------------------------------------------
-- Production
empty :: ModifiedNumeralData
empty = ModifiedNumeralData
  { value = Nothing
  , minValue = Nothing
  , maxValue = Nothing
  , grain = Nothing
  }

withInterval :: (Double, Double) -> ModifiedNumeralData -> ModifiedNumeralData
withInterval (from, to) fd = fd
  {minValue = Just from
  , maxValue = Just to
  , value = Nothing
  , grain = Nothing}

withApproximate :: Double -> ModifiedNumeralData -> ModifiedNumeralData
withApproximate x fd = fd {value = Just x}

withMin :: Double -> ModifiedNumeralData -> ModifiedNumeralData
withMin x fd = fd {minValue = Just x}

withMax :: Double -> ModifiedNumeralData -> ModifiedNumeralData
withMax x fd = fd {maxValue = Just x}



