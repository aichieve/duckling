-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.ModifiedNumeral.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import GHC.Generics
import Prelude

import Duckling.Resolve

data ModifiedNumeralData = ModifiedNumeralData
  { value        :: Maybe Double
  , grain        :: Maybe Int
  , minValue     :: Maybe Double
  , maxValue     :: Maybe Double
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve ModifiedNumeralData where
  type ResolvedValue ModifiedNumeralData = ModifiedNumeralValue
  resolve _ _ ModifiedNumeralData {value = Just val} =
    Just (approximate val, False)
  resolve _ _ ModifiedNumeralData {value = Nothing
                         , minValue = Just from, maxValue = Just to} =
    Just (between (from, to), False)
  resolve _ _ ModifiedNumeralData {value = Nothing
                         , minValue = Just from, maxValue = Nothing} =
    Just (above from, False)
  resolve _ _ ModifiedNumeralData {value = Nothing
                         , minValue = Nothing, maxValue = Just to} =
    Just (under to, False)
  resolve _ _ _ = Nothing

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
  { vValue :: Double }
  deriving (Eq, Ord, Show)

instance ToJSON SingleValue where
  toJSON (SingleValue value) = object
    [ "value" .= value]

data ModifiedNumeralValue
  = ApproximateValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Eq, Ord, Show)


instance ToJSON ModifiedNumeralValue where
  toJSON (ApproximateValue value) = object
      [ "type" .= ("approximate" :: Text)
      , "approximate" .= toJSON value
      ]
  toJSON (IntervalValue (from, to)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    , "to" .= toJSON to
    ]
  toJSON (OpenIntervalValue (from, Above)) = object
    [ "type" .= ("interval" :: Text)
    , "from" .= toJSON from
    ]
  toJSON (OpenIntervalValue (to, Under)) = object
    [ "type" .= ("interval" :: Text)
    , "to" .= toJSON to
    ]

-- -----------------------------------------------------------------
-- Value helpers
single :: Double -> SingleValue
single v = SingleValue {vValue = v}

approximate :: Double -> ModifiedNumeralValue
approximate v = ApproximateValue $ single v

between :: (Double, Double) -> ModifiedNumeralValue
between (from, to) = IntervalValue (single from, single to)

above :: Double -> ModifiedNumeralValue
above = openInterval Above

under :: Double -> ModifiedNumeralValue
under = openInterval Under

openInterval :: IntervalDirection -> Double -> ModifiedNumeralValue
openInterval direction v = OpenIntervalValue (single v, direction)
