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

module Duckling.ModifiedDuration.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Semigroup
import Data.Text (Text)
import Data.Tuple.Extra (both)
import GHC.Generics
import TextShow (showt)
import Prelude

import Duckling.Resolve (Resolve(..))
import Duckling.TimeGrain.Types (Grain(..), inSeconds)

data ModifiedDurationData = ModifiedDurationData
  { value :: Maybe Int
  , minValue  :: Maybe Int
  , maxValue  :: Maybe Int
  , grain :: Grain
  }
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance Resolve ModifiedDurationData where
  type ResolvedValue ModifiedDurationData = ModifiedDurationValue
  resolve _ _ ModifiedDurationData { value = Just val
                                   , grain = g } =
    Just (approximate val g, False)
  resolve _ _ ModifiedDurationData { value = Nothing
                                   , grain = g
                                   , minValue = Just from
                                   , maxValue = Just to } =
    Just (between (from, to) g, False)
  resolve _ _ ModifiedDurationData { value = Nothing
                                   , grain = g
                                   , minValue = Just from
                                   , maxValue = Nothing } =
    Just (above from g, False)
  resolve _ _ ModifiedDurationData { value = Nothing
                                   , grain = g
                                   , minValue = Nothing
                                   , maxValue = Just to } =
    Just (under to g, False)
  resolve _ _ _ = Nothing

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
  { vValue :: Int
  , vGrain :: Grain
  }
  deriving (Eq, Ord, Show)

instance ToJSON SingleValue where
  toJSON SingleValue {vValue, vGrain} = object
    [ "value"      .= vValue
    , "unit"       .= vGrain
    , showt vGrain  .= vValue
    , "normalized" .= object
      [ "unit"  .= ("second" :: Text)
      , "value" .= inSeconds vGrain vValue
      ]
    ]

data ModifiedDurationValue
  = ApproximateValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Eq, Ord, Show)

instance ToJSON ModifiedDurationValue where
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
  toJSON (ApproximateValue value) = object
    [ "type" .= ("approximate" :: Text)
    , "approximate" .= toJSON value
    ]

-- -----------------------------------------------------------------
-- Value helpers
single :: Int -> Grain -> SingleValue
single v g = SingleValue { vValue = v, vGrain = g }

approximate :: Int -> Grain -> ModifiedDurationValue
approximate v g = ApproximateValue $ single v g

between :: (Int, Int) -> Grain  -> ModifiedDurationValue
between (from, to) g = IntervalValue (single from g, single to g)

above :: Int -> Grain -> ModifiedDurationValue
above = openInterval Above

under :: Int -> Grain -> ModifiedDurationValue
under = openInterval Under

openInterval :: IntervalDirection -> Int -> Grain -> ModifiedDurationValue
openInterval direction v g  = OpenIntervalValue (single v g, direction)
