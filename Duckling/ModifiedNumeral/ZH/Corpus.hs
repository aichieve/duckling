-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.ModifiedNumeral.ZH.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.ModifiedNumeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [examples (between (20000, 30000))
             [ "2万到3万"
             , "2到3万"
             , "2,3万"
             , "2，3万"
             , "2、3万"
             , "两三万"
             , "贰叁万"
             , "大约2、3万"
             ]
  , examples (above 50000)
             [ "5万以上"
             , "50000以上"
             , "伍万以上"
             , "最少50000"
             , "至少50000"
             , "大于50000"
             ]
  , examples (above 500)
             [ "最少500"
             , "最小五百"
             ]
  , examples (under 200)
             [ "最多200"
             , "不到200"
             , "小于200"
             , "200以下"
             , "两百以下"
             , "两佰以下"
             , "贰佰以下"
             ]
  , examples (approximate 1000)
             ["大约1000"
              , "大约1千"
              , "大约1仟"
              , "大约壹千"
              , "大约壹仟"
              , "大概1000"
              , "大概1千"
              , "大概1仟"
              , "大概壹千"
              , "大概壹仟"
              , "差不多1000"
              , "差不多1千"
              , "差不多1仟"
              , "差不多壹千"
              , "差不多壹仟"
              , "1000左右"
              , "1千左右"
              , "1仟左右"
              , "壹千左右"
              , "壹仟左右"
              , "1000上下"
              , "1千上下"
              , "1仟上下"
              , "壹千上下"
              , "壹仟上下"
             ]
  ]
