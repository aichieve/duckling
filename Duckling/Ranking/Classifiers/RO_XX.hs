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
module Duckling.Ranking.Classifiers.RO_XX (classifiers) where
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import Duckling.Ranking.Types

classifiers :: Classifiers
classifiers
  = HashMap.fromList
      [("integer (numeric)",
        Classifier{okData =
                     ClassData{prior = -0.2876820724517809,
                               unseen = -2.0794415416798357,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 6},
                   koData =
                     ClassData{prior = -1.3862943611198906,
                               unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2}}),
       ("absorption of , after named day",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.9459101490553135,
                               likelihoods =
                                 HashMap.fromList
                                   [("luni", -0.6931471805599453), ("day", -0.6931471805599453)],
                               n = 2},
                   koData =
                     ClassData{prior = -infinity, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("acum",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("intersect",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -3.044522437723423,
                               likelihoods =
                                 HashMap.fromList
                                   [("daymonth", -1.8971199848858813),
                                    ("martithe <day-of-month> (number)", -2.3025850929940455),
                                    ("dayday", -1.2039728043259361),
                                    ("absorption of , after named day<day-of-month>(number) <named-month>",
                                     -1.8971199848858813),
                                    ("absorption of , after named day<day-of-month> (non ordinal) <named-month>",
                                     -1.8971199848858813),
                                    ("<named-day> <day-of-month> (number)februarie",
                                     -1.8971199848858813)],
                               n = 7},
                   koData =
                     ClassData{prior = -infinity, unseen = -1.9459101490553135,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("<named-day> <day-of-month> (number)",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -2.4849066497880004,
                               likelihoods =
                                 HashMap.fromList
                                   [("day", -0.7884573603642702),
                                    ("martiinteger (numeric)", -1.2992829841302609),
                                    ("absorption of , after named dayinteger (numeric)",
                                     -1.2992829841302609)],
                               n = 4},
                   koData =
                     ClassData{prior = -infinity, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("duminica",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.791759469228055,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 4},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("marti",
        Classifier{okData =
                     ClassData{prior = -0.5108256237659907,
                               unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 3},
                   koData =
                     ClassData{prior = -0.916290731874155, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2}}),
       ("iunie",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("numbers prefix with - or minus",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("integer (numeric)", 0.0)],
                               n = 1}}),
       ("luni",
        Classifier{okData =
                     ClassData{prior = -0.916290731874155, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2},
                   koData =
                     ClassData{prior = -0.5108256237659907,
                               unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 3}}),
       ("februarie",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("craciun",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("sambata",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 3},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("the <day-of-month> (number)",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("integer (numeric)", 0.0)],
                               n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("joi",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("vineri",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("ieri",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("intersect by \",\"",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -2.4849066497880004,
                               likelihoods =
                                 HashMap.fromList
                                   [("luni<day-of-month>(number) <named-month>",
                                     -1.2992829841302609),
                                    ("dayday", -0.7884573603642702),
                                    ("luni<day-of-month> (non ordinal) <named-month>",
                                     -1.2992829841302609)],
                               n = 4},
                   koData =
                     ClassData{prior = -infinity, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("azi",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 3},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("<named-day> pe <day-of-month> (number)",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.6094379124341003,
                               likelihoods =
                                 HashMap.fromList
                                   [("day", -0.6931471805599453),
                                    ("martiinteger (numeric)", -0.6931471805599453)],
                               n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("integer (0..10)",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("<time> (aceasta|acesta|[a\259]sta)",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.9459101490553135,
                               likelihoods =
                                 HashMap.fromList
                                   [("luni", -0.6931471805599453), ("day", -0.6931471805599453)],
                               n = 2}}),
       ("<named-month> <day-of-month> (non ordinal)",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.6094379124341003,
                               likelihoods =
                                 HashMap.fromList
                                   [("iunieinteger (numeric)", -0.6931471805599453),
                                    ("month", -0.6931471805599453)],
                               n = 1}}),
       ("<day-of-month> (non ordinal) <named-month>",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -2.5649493574615367,
                               likelihoods =
                                 HashMap.fromList
                                   [("integer (numeric)martie", -1.791759469228055),
                                    ("integer (numeric)februarie", -1.3862943611198906),
                                    ("integer (0..10)martie", -1.791759469228055),
                                    ("month", -0.8754687373538999)],
                               n = 4},
                   koData =
                     ClassData{prior = -infinity, unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("this|next <day-of-week>",
        Classifier{okData =
                     ClassData{prior = -infinity, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [], n = 0},
                   koData =
                     ClassData{prior = 0.0, unseen = -1.9459101490553135,
                               likelihoods =
                                 HashMap.fromList
                                   [("luni", -0.6931471805599453), ("day", -0.6931471805599453)],
                               n = 2}}),
       ("martie",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("maine",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.3862943611198906,
                               likelihoods = HashMap.fromList [("", 0.0)], n = 2},
                   koData =
                     ClassData{prior = -infinity, unseen = -0.6931471805599453,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("<month> dd-dd (interval)",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -1.6094379124341003,
                               likelihoods =
                                 HashMap.fromList
                                   [("iunie", -0.6931471805599453), ("month", -0.6931471805599453)],
                               n = 1},
                   koData =
                     ClassData{prior = -infinity, unseen = -1.0986122886681098,
                               likelihoods = HashMap.fromList [], n = 0}}),
       ("<day-of-month>(number) <named-month>",
        Classifier{okData =
                     ClassData{prior = 0.0, unseen = -2.5649493574615367,
                               likelihoods =
                                 HashMap.fromList
                                   [("integer (numeric)martie", -1.791759469228055),
                                    ("integer (numeric)februarie", -1.3862943611198906),
                                    ("integer (0..10)martie", -1.791759469228055),
                                    ("month", -0.8754687373538999)],
                               n = 4},
                   koData =
                     ClassData{prior = -infinity, unseen = -1.6094379124341003,
                               likelihoods = HashMap.fromList [], n = 0}})]