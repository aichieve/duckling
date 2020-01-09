-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.ModifiedDuration.ZH.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import qualified Duckling.Numeral.Helpers as NHelpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Duration.Helpers as DHelpers
import Duckling.Duration.Types (DurationData (..))
import qualified Duckling.ModifiedDuration.Helpers as MHelpers
import Duckling.ModifiedDuration.Types (ModifiedDurationData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Duration.Types as DDuration
import qualified Duckling.ModifiedDuration.Types as MDuration
import qualified Duckling.Numeral.ZH.Rules as NRules
import qualified Duckling.TimeGrain.ZH.Rules as TGRules

ruleIntervalDuration :: Rule
ruleIntervalDuration = Rule
  { name = "<duration> - <duration>"
  , pattern =
    [ Predicate DHelpers.isPositive
    , regex "-|~|,|，|、|到"
    , Predicate DHelpers.isPositive
    ]
  , prod = \case
      (Token Duration DurationData{DDuration.value = from, DDuration.grain = gr1}:
       _:
       Token Duration DurationData{DDuration.value = to, DDuration.grain = gr2}:
       _) | from < to -> case gr1 == gr2 of
        True -> Just . Token ModifiedDuration . MHelpers.withInterval (from, to) $ MHelpers.empty gr1
        False -> Nothing
      _ -> Nothing
  }

ruleIntervalDuration2 :: Rule
ruleIntervalDuration2 = Rule
  { name = "<duration> - <duration>"
  , pattern =
    [ Predicate NHelpers.isPositive
    , regex "-|~|,|，|、|到"
    , Predicate DHelpers.isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from, TNumeral.grain = Nothing}:
       _:
       Token Duration DurationData{DDuration.value = to, DDuration.grain = g}:
       _) | floor from < to
         -> Just . Token ModifiedDuration . MHelpers.withInterval (floor from, to) $ MHelpers.empty g
      _ -> Nothing
  }

ruleIntervalDuration3 :: Rule
ruleIntervalDuration3 = Rule
  { name = "<number><number><CN time grain>"
  , pattern =
    [ regex $ "(" ++ NRules.digitZHRegex ++ ")(" ++ NRules.digitZHRegex ++ ")(" ++ NRules.suffixZHRegex ++ ")(个|個)" ++ "(" ++ TGRules.timeGrainZHRegex ++ ")"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dFrom:dTo:dUnit:dGrain:_)):
       _) -> do
         vFrom <- HashMap.lookup dFrom NRules.integerMap
         vTo <- HashMap.lookup dTo NRules.integerMap
         vUnit <- HashMap.lookup dUnit NRules.suffixUnitValueMap
         vGrain <- HashMap.lookup dGrain TGRules.timeGrainMap
         let from = vUnit * fromIntegral vFrom
             to = vUnit * fromIntegral vTo
         case from < to of
           True -> Just . Token ModifiedDuration . MHelpers.withInterval (floor from, floor to) $ MHelpers.empty vGrain
           False -> Nothing
      _ -> Nothing
  }

ruleIntervalDuration4 :: Rule
ruleIntervalDuration4 = Rule
  { name = "<number><number><CN time grain>"
  , pattern =
    [ regex $ "(" ++ NRules.digitZHRegex ++ ")(" ++ NRules.digitZHRegex ++ ")(" ++ NRules.suffixZHRegex ++ ")(" ++ TGRules.timeGrainZHRegex ++ ")"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dFrom:dTo:dUnit:dGrain:_)):
       _) -> do
         vFrom <- HashMap.lookup dFrom NRules.integerMap
         vTo <- HashMap.lookup dTo NRules.integerMap
         vUnit <- HashMap.lookup dUnit NRules.suffixUnitValueMap
         vGrain <- HashMap.lookup dGrain TGRules.timeGrainMap
         let from = vUnit * fromIntegral vFrom
             to = vUnit * fromIntegral vTo
         case from < to of
           True -> Just . Token ModifiedDuration . MHelpers.withInterval (floor from, floor to) $ MHelpers.empty vGrain
           False -> Nothing
      _ -> Nothing
  }

ruleIntervalDuration5 :: Rule
ruleIntervalDuration5 = Rule
  { name = "<number><number><CN time grain>"
  , pattern =
    [ regex $ "(" ++ NRules.digitZHRegex ++ ")(" ++ NRules.digitZHRegex ++ ")(个|個)(" ++ TGRules.timeGrainZHRegex ++ ")"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dFrom:dTo:dGrain:_)):
       _) -> do
         vFrom <- HashMap.lookup dFrom NRules.integerMap
         vTo <- HashMap.lookup dTo NRules.integerMap
         vGrain <- HashMap.lookup dGrain TGRules.timeGrainMap
         let from = vFrom
             to = vTo
         case from < to of
           True -> Just . Token ModifiedDuration . MHelpers.withInterval (fromInteger from, fromInteger to) $ MHelpers.empty vGrain
           False -> Nothing
      _ -> Nothing
  }

ruleIntervalDuration6 :: Rule
ruleIntervalDuration6 = Rule
  { name = "<number><number><CN time grain>"
  , pattern =
    [ regex $ "(" ++ NRules.digitZHRegex ++ ")(" ++ NRules.digitZHRegex ++ ")(" ++ TGRules.timeGrainZHRegex ++ ")"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dFrom:dTo:dGrain:_)):
       _) -> do
         vFrom <- HashMap.lookup dFrom NRules.integerMap
         vTo <- HashMap.lookup dTo NRules.integerMap
         vGrain <- HashMap.lookup dGrain TGRules.timeGrainMap
         let from = vFrom
             to = vTo
         case from < to of
           True -> Just . Token ModifiedDuration . MHelpers.withInterval (fromInteger from, fromInteger to) $ MHelpers.empty vGrain
           False -> Nothing
      _ -> Nothing
  }

ruleIntervalBound :: Rule
ruleIntervalBound = Rule
  { name = "under/less/lower/no more than <number> (最多|至少|最少)"
  , pattern =
    [ regex "(最多|不到|小于|至少|最少|最小|大于)"
    , Predicate DHelpers.isPositive
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration DurationData{DDuration.value = to, DDuration.grain = g}:
       _) -> case match of
         "最多" -> Just . Token ModifiedDuration . MHelpers.withMax to $ MHelpers.empty g
         "不到" -> Just . Token ModifiedDuration . MHelpers.withMax to $ MHelpers.empty g
         "小于" -> Just . Token ModifiedDuration . MHelpers.withMax to $ MHelpers.empty g
         "最少" -> Just . Token ModifiedDuration . MHelpers.withMin to $ MHelpers.empty g
         "最小" -> Just . Token ModifiedDuration . MHelpers.withMin to $ MHelpers.empty g
         "至少" -> Just . Token ModifiedDuration . MHelpers.withMin to $ MHelpers.empty g
         "大于" -> Just . Token ModifiedDuration . MHelpers.withMin to $ MHelpers.empty g
         _ -> Nothing
      _ -> Nothing
  }

ruleIntervalBound2 :: Rule
ruleIntervalBound2 = Rule
  { name = "under/less/lower/no more than <number> (以下|以上)"
  , pattern =
    [ Predicate DHelpers.isPositive
    , regex "(以下|以上)"
    ]
  , prod = \case
      (Token Duration DurationData{DDuration.value = to, DDuration.grain = g}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
         "以下" -> Just . Token ModifiedDuration . MHelpers.withMax to $ MHelpers.empty g
         "以上" -> Just . Token ModifiedDuration . MHelpers.withMin to $ MHelpers.empty g
         _ -> Nothing
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "exactly <number> (刚好|恰好)"
  , pattern =
    [ regex "(刚好|恰好)"
    , Predicate DHelpers.isPositive
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleApproximate :: Rule
ruleApproximate = Rule
  { name = "about <number> (大约|差不多|大概)"
  , pattern =
    [ regex "(大约|差不多|大概)"
    , Predicate DHelpers.isPositive
    ]
  , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         Token Duration DurationData{DDuration.value = to, DDuration.grain = g}:
         _) -> Just . Token ModifiedDuration . MHelpers.withApproximate to $ MHelpers.empty g
        _ -> Nothing
  }

ruleApproximate2 :: Rule
ruleApproximate2 = Rule
  { name = "about <number> (左右|上下)"
  , pattern =
    [ Predicate DHelpers.isPositive
    , regex "(左右|上下)"
    ]
  , prod = \case
        (Token Duration DurationData{DDuration.value = to, DDuration.grain = g}:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token ModifiedDuration . MHelpers.withApproximate to $ MHelpers.empty g
        _ -> Nothing
  }

ruleApproximate3 :: Rule
ruleApproximate3 = Rule
  { name = "about <number> (大约|差不多|大概)"
  , pattern =
    [ regex "(大约|差不多|大概)"
    , Predicate MHelpers.isPositive
    ]
  , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):token:_) -> Just token
        _ -> Nothing
  }

ruleApproximate4 :: Rule
ruleApproximate4 = Rule
  { name = "about <number> (左右|上下)"
  , pattern =
    [ Predicate MHelpers.isPositive
    , regex "(左右|上下)"
    ]
  , prod = \case
        (token:Token RegexMatch (GroupMatch (match:_)):_) -> Just token
        _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleIntervalDuration
  , ruleIntervalDuration2
  , ruleIntervalDuration3
  , ruleIntervalDuration4
  , ruleIntervalDuration5
  , ruleIntervalDuration6
  , ruleIntervalBound
  , ruleIntervalBound2
  , rulePrecision
  , ruleApproximate
  , ruleApproximate2
  , ruleApproximate3
  , ruleApproximate4
  ]