{-# LANGUAGE OverloadedStrings #-}

module Config (readConfig, BuilderConfig (..)) where

import Data.Yaml
import System.FilePath

data BuilderConfig = BuilderConfig {
    builders :: [FilePath],
    scheduler_address :: String
}

instance FromJSON BuilderConfig where
    parseJSON (Object m) = BuilderConfig <$> m .: "builders" <*> m .: "scheduler_address"
    parseJSON _ = fail ("Builder config has incorrect format")

readConfig :: IO BuilderConfig
readConfig = do
    either (error . show) id <$> decodeFileEither "builder.yaml"
