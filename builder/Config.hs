{-# LANGUAGE OverloadedStrings #-}

module Config (readConfig, BuilderConfig (..)) where

import Data.Yaml
import System.FilePath

data BuilderConfig = BuilderConfig {
    builders :: [FilePath]
}

instance FromJSON BuilderConfig where
    parseJSON (Object m) = BuilderConfig <$> m .: "builders"
    parseJSON _ = fail ("Builder config has incorrect format")

readConfig :: IO BuilderConfig
readConfig = do
    either (error . show) id <$> decodeFileEither "builder.yaml"
