{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Config (readConfig, BuilderConfig (..)) where

import Data.Aeson.TH
import Data.Yaml
import System.FilePath

data BuilderConfig = BuilderConfig {
    builders :: [FilePath],
    scheduler_address :: String
}

$(deriveJSON defaultOptions ''BuilderConfig)

readConfig :: IO BuilderConfig
readConfig = do
    either (error . show) id <$> decodeFileEither "builder.yaml"
