{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Config (readConfig, SchedConfig (..)) where

import Data.Aeson.TH
import Data.Yaml

data SchedConfig = SchedConfig {
    listen_address :: String
}

$(deriveJSON defaultOptions ''SchedConfig)

readConfig :: IO SchedConfig
readConfig = do
    either (error . show) id <$> decodeFileEither "sched.yaml"
