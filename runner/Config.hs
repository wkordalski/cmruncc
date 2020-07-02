{-# LANGUAGE OverloadedStrings #-}

module Config (readConfig, RunnerConfig (..)) where

import Data.Yaml

data RunnerConfig = RunnerConfig {
    devices :: [String],
    heni_cmd :: String,
    temp_dir :: String,
    scheduler_address :: String
}

instance FromJSON RunnerConfig where
    parseJSON (Object m) = RunnerConfig <$> m .: "devices" <*> m .: "heni_cmd" <*> m .: "temp_dir" <*> m .: "scheduler_address"
    parseJSON _ = fail ("Builder config has incorrect format")

readConfig :: IO RunnerConfig
readConfig = do
    either (error . show) id <$> decodeFileEither "runner.yaml"
