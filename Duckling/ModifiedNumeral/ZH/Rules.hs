-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.ModifiedNumeral.ZH.Rules
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
import qualified Duckling.ModifiedNumeral.Helpers as EHelpers
import Duckling.ModifiedNumeral.Types (ModifiedNumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.ModifiedNumeral.Types as ENumeral
import qualified Duckling.Numeral.ZH.Rules as NRules

ruleIntervalNumeral :: Rule
ruleIntervalNumeral = Rule
  { name = "<number> - <number>"
  , pattern =
    [ Predicate NHelpers.isPositive
    , regex "-|~|,|，|、|到"
    , Predicate NHelpers.isPositive
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from, TNumeral.grain = gr1}:
       _:
       Token Numeral NumeralData{TNumeral.value = to, TNumeral.grain = gr2}:
       _) | from < to ->
         case (gr1, gr2) of
           (Nothing, Just g2) -> Just . Token ModifiedNumeral . EHelpers.withInterval ((10 ** fromIntegral g2) * from, to) $ EHelpers.empty
           _ -> Just . Token ModifiedNumeral . EHelpers.withInterval (from, to) $ EHelpers.empty
      _ -> Nothing
  }

ruleIntervalNumeral2 :: Rule
ruleIntervalNumeral2 = Rule
  { name = "<number><number><CN unit>"
  , pattern =
    [ regex $ "(" ++ NRules.digitZHRegex ++ ")(" ++ NRules.digitZHRegex ++ ")(" ++ NRules.suffixZHRegex ++ ")"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dFrom:dTo:unit:_)):_) -> do
        vFrom <- HashMap.lookup dFrom NRules.integerMap
        vTo <- HashMap.lookup dTo NRules.integerMap
        vUnit <- HashMap.lookup unit NRules.suffixUnitValueMap
        let from = vUnit * fromIntegral vFrom
            to = vUnit * fromIntegral vTo
        case from < to of
          True -> Just . Token ModifiedNumeral . EHelpers.withInterval (from, to) $ EHelpers.empty
          False -> Nothing
      _ -> Nothing
  }

ruleIntervalBound :: Rule
ruleIntervalBound = Rule
  { name = "under/less/lower/no more than <number> (最多|至少|最少)"
  , pattern =
    [ regex "(最多|不到|小于|至少|最少|大于)"
    , Predicate NHelpers.isPositive
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):
       Token Numeral NumeralData{TNumeral.value = to}:
       _) -> case match of
        "最多" -> Just . Token ModifiedNumeral . EHelpers.withMax to $ EHelpers.empty
        "不到" -> Just . Token ModifiedNumeral . EHelpers.withMax to $ EHelpers.empty
        "小于" -> Just . Token ModifiedNumeral . EHelpers.withMax to $ EHelpers.empty
        "最少" -> Just . Token ModifiedNumeral . EHelpers.withMin to $ EHelpers.empty
        "至少" -> Just . Token ModifiedNumeral . EHelpers.withMin to $ EHelpers.empty
        "大于" -> Just . Token ModifiedNumeral . EHelpers.withMin to $ EHelpers.empty
        _ -> Nothing
      _ -> Nothing
  }

ruleIntervalBound2 :: Rule
ruleIntervalBound2 = Rule
  { name = "under/less/lower/no more than <number> (以下|以上)"
  , pattern =
    [ Predicate NHelpers.isPositive
    , regex "(以下|以上)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = to}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case match of
        "以下" -> Just . Token ModifiedNumeral . EHelpers.withMax to $ EHelpers.empty
        "以上" -> Just . Token ModifiedNumeral . EHelpers.withMin to $ EHelpers.empty
        _ -> Nothing
      _ -> Nothing
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "exactly <number> (刚好|恰好)"
  , pattern =
    [ regex "(刚好|恰好)"
    , Predicate NHelpers.isPositive
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
    , Predicate NHelpers.isPositive
    ]
  , prod = \case
        (Token RegexMatch (GroupMatch (match:_)):
         Token Numeral NumeralData{TNumeral.value = to}:
         _) -> Just . Token ModifiedNumeral . EHelpers.withApproximate to $ EHelpers.empty
        _ -> Nothing
  }

ruleApproximate2 :: Rule
ruleApproximate2 = Rule
  { name = "about <number> (左右|上下)"
  , pattern =
    [ Predicate NHelpers.isPositive
    , regex "(左右|上下)"
    ]
  , prod = \case
        (Token Numeral NumeralData{TNumeral.value = to}:
         Token RegexMatch (GroupMatch (match:_)):
         _) -> Just . Token ModifiedNumeral . EHelpers.withApproximate to $ EHelpers.empty
        _ -> Nothing
  }
rules :: [Rule]
rules =
  [ ruleIntervalNumeral
  , ruleIntervalNumeral2
  , ruleIntervalBound
  , ruleIntervalBound2
  , rulePrecision
  , ruleApproximate
  , ruleApproximate2
  ]
