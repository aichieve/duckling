-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.

-----------------------------------------------------------------
-- Auto-generated by regenClassifiers
--
-- DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
--  @generated
-----------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Duckling.Ranking.Classifiers.GA_XX (classifiers) where
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import Duckling.Ranking.Types

classifiers :: Classifiers
classifiers
  = HashMap.fromList
      [("Thursday",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2}}),
       ("integer (numeric)",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 3}}),
       ("ar\250 am\225rach",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("dd/mm",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("inniu",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("Monday",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -2.0794415416798357,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 6}}),
       ("dd/mm/yyyy",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("fractional number",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1}}),
       ("year",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("integer (numeric)", 0.0)],
                               n = 1}}),
       ("<time> seo chugainn",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -2.833213344056216,
                               likelihoods =
                                 HashMap.fromList
                                   [("Monday", -1.3862943611198906), ("day", -0.8266785731844679),
                                    ("d\233 named-day", -2.0794415416798357),
                                    ("an named-day", -1.6739764335716716)],
                               n = 6}}),
       ("d\233 named-day",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.9459101490553135,
                               likelihoods =
                                 HashMap.fromList
                                   [("Monday", -0.6931471805599453), ("day", -0.6931471805599453)],
                               n = 2}}),
       ("anois",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("an named-day",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -2.3978952727983707,
                               likelihoods =
                                 HashMap.fromList
                                   [("Monday", -0.6931471805599453), ("day", -0.6931471805599453)],
                               n = 4}}),
       ("am\225rach",
        Classifier{okData =
                     ClassData{prior = -0.6931471805599453,
                               unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -0.6931471805599453,
                               unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1}}),
       ("inn\233",
        Classifier{okData =
                     ClassData{prior = -0.6931471805599453,
                               unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -0.6931471805599453,
                               unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1}}),
       ("<time> seo",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -3.044522437723423,
                               likelihoods =
                                 HashMap.fromList
                                   [("Monday", -1.3862943611198906), ("day", -0.7985076962177716),
                                    ("d\233 named-day", -2.3025850929940455),
                                    ("an named-day", -1.6094379124341003)],
                               n = 8}}),
       ("ar\250 inn\233",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}})]