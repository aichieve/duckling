-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.TimeGrain.ZH.Rules
  ( rules, timeGrainZHRegex, timeGrainMap ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import qualified Duckling.TimeGrain.Types as TG
import Duckling.Types
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

grains :: [(Text, String, TG.Grain)]
grains = [ ("second (grain)", "秒(钟|鐘)?", TG.Second)
         , ("minute (grain)", "分(钟|鐘)?", TG.Minute)
         , ("hour (grain)",
             "小时|小時|鐘(\x982d)?", TG.Hour)
         , ("day (grain)", "天|日", TG.Day)
         , ("week (grain)",
             "周|週|礼拜|禮拜|星期", TG.Week)
         , ("month (grain)", "月", TG.Month)
         , ("year (grain)", "年", TG.Year)
         ]

rules :: [Rule]
rules = map go grains
  where
    go (name, regexPattern, grain) = Rule
      { name = name
      , pattern = [regex regexPattern]
      , prod = \_ -> Just $ Token TimeGrain grain
      }

timeGrainZHRegex :: String
timeGrainZHRegex = "秒|秒钟|秒鐘|分|分钟|分鐘|小时|小時|鐘|鐘\x982d|天|日|周|週|礼拜|星期|月|年"

timeGrainMap :: HashMap.HashMap Text TG.Grain
timeGrainMap = HashMap.fromList
  [ ( "秒", TG.Second )
  , ( "秒钟", TG.Second )
  , ( "秒鐘", TG.Second )
  , ( "分", TG.Minute )
  , ( "分钟", TG.Minute )
  , ( "小时", TG.Hour )
  , ( "小時", TG.Hour )
  , ( "鐘", TG.Hour )
  , ( "鐘\x982d", TG.Hour )
  , ( "天", TG.Day )
  , ( "日", TG.Day )
  , ( "周", TG.Week )
  , ( "週", TG.Week )
  , ( "礼拜", TG.Week )
  , ( "星期", TG.Week )
  , ( "月", TG.Month )
  , ( "年", TG.Year )
  ]