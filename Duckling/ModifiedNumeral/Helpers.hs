-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.ModifiedNumeral.Helpers
  ( empty
  , withInterval
  , withApproximate
  , withMin
  , withMax
  ) where

import Duckling.Dimensions.Types
import Duckling.ModifiedNumeral.Types
import Duckling.Types hiding (Entity(value))

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



