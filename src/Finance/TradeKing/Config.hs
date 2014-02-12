{-# LANGUAGE OverloadedStrings #-}
module Finance.TradeKing.Config
    ( defaultTKConfs,
      readTKConf ) where

import Finance.TradeKing.Types

import Prelude hiding (lookup)

import Control.Applicative

import Data.Configurator

-- | Default locations to look for the TradeKing configuration file.
--
-- > defaultTKConfs = ["/etc/tradeking.conf", "~/.tradeking"]
defaultTKConfs :: [FilePath]
defaultTKConfs = ["/etc/tradeking.conf", "~/.tradeking"]

-- | Reads in TradeKing configuration file and returns the resulting application
readTKConf :: [FilePath] -> IO (Either String TradeKingApp)
readTKConf confs = do
  tkConf <- load (map Optional confs)

  let label l Nothing  = Left l
      label _ (Just x) = Right x

  consumerKey      <- label "Consumer key not present in config" <$> lookup tkConf "consumer-key"
  consumerSecret   <- label "Consumer secret not present in config" <$> lookup tkConf "consumer-secret"
  oAuthToken       <- label "OAuth token not present in config" <$> lookup tkConf "oauth-token"
  oAuthTokenSecret <- label "OAuth token secret not present in config" <$> lookup tkConf "oauth-token-secret"
  return (TradeKingApp <$> pure defaultTk
                       <*> consumerKey
                       <*> consumerSecret
                       <*> oAuthToken
                       <*> oAuthTokenSecret)