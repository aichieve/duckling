-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.EstimatedNumeral.Helpers
  ( empty
  , withInterval
  , withApproximate
  , withMin
  , withMax
  ) where

import Duckling.Dimensions.Types
import Duckling.EstimatedNumeral.Types
import Duckling.Types hiding (Entity(value))

-- -----------------------------------------------------------------
-- Production
empty :: EstimatedNumeralData
empty = EstimatedNumeralData
  { value = Nothing
  , minValue = Nothing
  , maxValue = Nothing
  , grain = Nothing
  }

withInterval :: (Double, Double) -> EstimatedNumeralData -> EstimatedNumeralData
withInterval (from, to) fd = fd
  {minValue = Just from
  , maxValue = Just to
  , value = Nothing
  , grain = Nothing}

withApproximate :: Double -> EstimatedNumeralData -> EstimatedNumeralData
withApproximate x fd = fd {value = Just x}

withMin :: Double -> EstimatedNumeralData -> EstimatedNumeralData
withMin x fd = fd {minValue = Just x}

withMax :: Double -> EstimatedNumeralData -> EstimatedNumeralData
withMax x fd = fd {maxValue = Just x}



