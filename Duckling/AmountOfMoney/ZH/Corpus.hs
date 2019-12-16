-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.ZH.Corpus
  ( corpus
  ) where

import Data.String
import Prelude

import Duckling.AmountOfMoney.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing},
  testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Cent 5)
             [ "五分"
             , "5分"
             ]
  , examples (simple Cent 20)
             [ "20分"
             , "二十分"
             , "2角"
             , "两毛"
             ]
  , examples (simple Cent 25)
             [ "25分"
             , "二十五分"
             , "两角五分"
             , "两毛五"
             ]
  , examples (simple Yuan 7)
             [ "7块"
             , "七元"
             ]
  , examples (simple CNY 3.14)
             [ "3.14人民币"
             , "人民幣3.14"
             ]
  , examples (under Yuan 1.2)
             [ "1.2元以下"
             , "最多一块二角"
             , "最多一块二"
             , "不到一块二角"
             , "不到一块二"
             , "小于一块二角"
             , "小于一块二"
             ]
  , examples (above Yuan 3.04)
             [ "3.04块以上"
             , "至少三块四分"
             , "至少三块零四"
             , "最少三块四分"
             , "最少三块零四"
             , "大于三块四分"
             , "大于三块零四"
             ]
  , examples (between Yuan (5.6, 5.78))
             [ "5.6到5.78元"
             , "五元六角-五元七毛八分"
             , "五块六到五块七毛八"
             ]
  , examples (between CNY (20000, 30000))
             [
             "2万到3万人民币"
             , "2到3万人民幣"
             , "2,3万人民币"
             , "2，3万人民币"
             , "2、3万人民幣"
             , "两三万人民币"
             , "贰叁万人民幣"
             ]
  , examples (approximate CNY 3.14)
             [ "大约3.14人民币"
             , "大约人民幣3.14"
             , "大概3.14人民币"
             , "大概人民幣3.14"
             , "差不多3.14人民币"
             , "差不多人民幣3.14"
             , "3.14人民币左右"
             , "人民幣3.14左右"
             , "3.14人民币上下"
             , "人民幣3.14上下"
             ]
  ]
