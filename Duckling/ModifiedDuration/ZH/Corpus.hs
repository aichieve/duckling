-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.



{-# LANGUAGE OverloadedStrings #-}

module Duckling.ModifiedDuration.ZH.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.ModifiedDuration.Types
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [examples (between (5, 7) Day)
             [ "5到7天"
             , "5,7天"
             , "5，7天"
             , "5、7天"
             , "五七天"
             , "伍柒天"
             , "大约5、7天"
             ]
  , examples (between (1, 2) Month)
             [ "1个月到2个月"
             , "1,2个月"
             ]
  , examples (between (3, 4) Year)
             [ "3年到4年"
             , "3,4年"
             , "3，4年"
             , "三四年"
             ]
  , examples (above 3 Week)
             [ "3周以上"
             ]
  , examples (above 5 Week)
             [ "最少5个星期"
             ]
  , examples (under 2 Second)
             [ "最多两秒钟"
             ]
  , examples (approximate 5 Month)
             ["大约5个月"
             ]
  ]