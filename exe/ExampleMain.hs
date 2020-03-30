-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.

-- Slightly modified from the original version by huajun, to support batch input.

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString, empty)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Time.LocalTime.TimeZone.Series
import Prelude
import System.Directory
import System.Environment (lookupEnv)
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Snap.Core
import Snap.Http.Server

import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime, TimeResolveStrategy(..))

createIfMissing :: FilePath -> IO ()
createIfMissing f = do
  exists <- doesFileExist f
  unless exists $ writeFile f ""

setupLogs :: IO ()
setupLogs = do
  createDirectoryIfMissing False "log"
  createIfMissing "log/error.log"
  createIfMissing "log/access.log"

main :: IO ()
main = do
  setupLogs
  tzs <- loadTimeZoneSeries "/usr/share/zoneinfo/"
  p <- lookupEnv "PORT"
  conf <- commandLineConfig $
    maybe defaultConfig (`setPort` defaultConfig) (readMaybe =<< p)
  httpServe conf $
    ifTop (writeBS "quack!") <|>
    route
      [ ("targets", method GET targetsHandler)
      , ("parse", method POST $ parseHandler tzs)
      ]

-- | Return which languages have which dimensions
targetsHandler :: Snap ()
targetsHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS $ encode $
    HashMap.fromList . map dimText $ HashMap.toList supportedDimensions
  where
    dimText :: (Lang, [Some Dimension]) -> (Text, [Text])
    dimText = (Text.toLower . showt) *** map (\(This d) -> toName d)


-- | Parse some text into the given dimensions
parseHandler :: HashMap Text TimeZoneSeries -> Snap ()
parseHandler tzs = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  ts <- getPostParam "texts"
  l <- getPostParam "lang"
  ds <- getPostParam "dims"
  tz <- getPostParam "tz"
  loc <- getPostParam "locale"
  ref <- getPostParam "reftime"
  latent <- getPostParam "latent"
  trs <- getPostParam "timeResolveStrategy"

  case ts of
    Nothing -> do
      modifyResponse $ setResponseStatus 422 "Bad Input"
      writeBS "Need a 'texts' parameter to parse"
    Just tx -> do
      let timezone = parseTimeZone tz
      now <- liftIO $ currentReftime tzs timezone
      let
        context = Context
          { referenceTime = maybe now (parseRefTime timezone) ref
          , locale = maybe (makeLocale (parseLang l) Nothing) parseLocale loc
          }
        options = Options {withLatent = parseLatent latent, timeResolveStrategy = parseTimeResolveStrategy trs}

        dimParse = fromMaybe [] $ decode $ LBS.fromStrict $ fromMaybe "" ds
        dims = mapMaybe parseDimension dimParse
        
        texts = map replaceFullWidthNumberWithHalfWidth $ fromMaybe []  $ decode $ LBS.fromStrict tx
        parsedResult = map (\x -> parse x context options dims) texts

      writeLBS $ encode parsedResult
  where
    defaultLang = EN
    defaultLocale = makeLocale defaultLang Nothing
    defaultTimeZone = "America/Los_Angeles"
    defaultLatent = False
    defaultTimeResolveStrategy = TO_FUTURE

    parseDimension :: Text -> Maybe (Some Dimension)
    parseDimension x = fromName x <|> fromCustomName x
      where
        fromCustomName :: Text -> Maybe (Some Dimension)
        fromCustomName name = HashMap.lookup name m
        m = HashMap.fromList
          [ -- ("my-dimension", This (CustomDimension MyDimension))
          ]

    parseTimeZone :: Maybe ByteString -> Text
    parseTimeZone = maybe defaultTimeZone Text.decodeUtf8

    parseLocale :: ByteString -> Locale
    parseLocale x = maybe defaultLocale (`makeLocale` mregion) mlang
      where
        (mlang, mregion) = case chunks of
          [a, b] -> (readMaybe a :: Maybe Lang, readMaybe b :: Maybe Region)
          _      -> (Nothing, Nothing)
        chunks = map Text.unpack . Text.split (== '_') . Text.toUpper
          $ Text.decodeUtf8 x

    parseLang :: Maybe ByteString -> Lang
    parseLang l = fromMaybe defaultLang $ l >>=
      readMaybe . Text.unpack . Text.toUpper . Text.decodeUtf8

    parseRefTime :: Text -> ByteString -> DucklingTime
    parseRefTime timezone refTime = makeReftime tzs timezone utcTime
      where
        msec = read $ Text.unpack $ Text.decodeUtf8 refTime
        utcTime = posixSecondsToUTCTime $ fromInteger msec / 1000

    parseLatent :: Maybe ByteString -> Bool
    parseLatent x = fromMaybe defaultLatent
      (readMaybe (Text.unpack $ Text.toTitle $ Text.decodeUtf8 $ fromMaybe empty x)::Maybe Bool)

    parseTimeResolveStrategy :: Maybe ByteString -> TimeResolveStrategy
    parseTimeResolveStrategy s = fromMaybe defaultTimeResolveStrategy $ s >>=
      readMaybe . Text.unpack . Text.toUpper . Text.decodeUtf8

    replaceFullWidthNumberWithHalfWidth :: [Char] -> Text  
    replaceFullWidthNumberWithHalfWidth tx = 
      let 
        repl '\65297' = '1'
        repl '\65298' = '2'
        repl '\65299' = '3'
        repl '\65300' = '4'
        repl '\65301' = '5'
        repl '\65302' = '6'
        repl '\65303' = '7'
        repl '\65304' = '8'
        repl '\65305' = '9'
        repl '\65306' = '0'
        repl c = c
      in Text.pack $ map repl tx  
