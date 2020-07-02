{-# LANGUAGE OverloadedStrings #-}

module Config (readConfig, SchedConfig (..)) where

import Data.Yaml

data SchedConfig = SchedConfig {
    listen_address :: String
}

instance FromJSON SchedConfig where
    parseJSON (Object m) = SchedConfig <$> m .: "listen_address"
    parseJSON _ = fail ("Builder config has incorrect format")

readConfig :: IO SchedConfig
readConfig = do
    either (error . show) id <$> decodeFileEither "sched.yaml"
