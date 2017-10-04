{-# LANGUAGE DeriveGeneric #-}

module WTB.Config  where

import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy

data Config = Config {
  token :: String
  } deriving (Generic, Show)

bot_token Config {token = t} = "bot" ++ t

instance FromJSON Config

-- note:
--  error messages are not very explicative with aeson
--  ToDo check http://harry.garrood.me/blog/aeson-better-errors/
read_config :: FilePath -> IO (Either String Config)
read_config path = eitherDecode <$> (Data.ByteString.Lazy.readFile path)
