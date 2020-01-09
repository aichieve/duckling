-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.ModifiedDuration.Tests (tests) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.ModifiedDuration.ZH.Tests as ZH

tests :: TestTree
tests = testGroup "ModifiedDuration Tests"
  [ ZH.tests
  ]
