module Client.Quote where

import Control.Monad

import Data.String

import Finance.TradeKing

doQuote :: [String] -> TradeKingApp -> IO ()
doQuote args app = do
  quotes <- stockQuotes app (map fromString args)
  mapM_ (putStrLn . show) quotes

doInfo :: [String] -> TradeKingApp -> IO ()
doInfo args app = do
  infos <- stockInfos app (map fromString args)
  mapM_ (putStrLn . show) infos
