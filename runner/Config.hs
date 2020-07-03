{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Config (readConfig, RunnerConfig (..)) where

import Data.Aeson.TH
import Data.Yaml
import System.FilePath

data RunnerConfig = RunnerConfig {
    devices :: [String],
    heni_cmd :: FilePath,
    temp_dir :: FilePath,
    scheduler_address :: String
}

$(deriveJSON defaultOptions ''RunnerConfig)

readConfig :: IO RunnerConfig
readConfig = do
    either (error . show) id <$> decodeFileEither "runner.yaml"
