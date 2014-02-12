{-# LANGUAGE OverloadedStrings #-}
-- | High-level access to TradeKing APIs
module Finance.TradeKing.Quotes (stockQuotes, stockInfos, streamQuotes) where

import Finance.TradeKing.Types
import Finance.TradeKing.Service (invokeSimple, streamQuotes')

import qualified Control.Exception.Lifted as E
import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe
import Data.Conduit
import Data.Time
import Data.Time.Clock.POSIX
import Data.Monoid
import Data.Aeson ((.:), (.:?), eitherDecode, FromJSON, FromJSON(..), Value(..))
import Data.Aeson.Types (Parser)

import Network.OAuth.Http.Response

import Safe (readMay)

import System.Locale

newtype TKQuoteResponse fields = TKQuoteResponse { unTKQuoteResponse :: TKQuotes fields }
newtype TKQuotes field = TKQuotes { unTKQuotes :: [field] }
newtype TKStockQuote = TKStockQuote { unTKStockQuote :: StockQuote }
newtype TKStockInfo = TKStockInfo { unTKStockInfo :: StockInfo }
newtype TKPrice = TKPrice Fixed4
newtype TKFrequency = TKFrequency { unTKFrequency :: Period}

-- TKPrice always in USD
unTKPrice :: TKPrice -> Price
unTKPrice (TKPrice nominal) = Price USD nominal

instance FromJSON TKFrequency where
  parseJSON (String t)
    | t == "A" = return (TKFrequency Annually)
    | t == "S" = return (TKFrequency SemiAnnually)
    | t == "Q" = return (TKFrequency Quarterly)
    | t == "M" = return (TKFrequency Monthly)
  parseJSON _ = mzero

instance FromJSON TKPrice where
  parseJSON (String t) = return (TKPrice . read . T.unpack $ t)
  parseJSON _ = mzero

instance FromJSON fields => FromJSON (TKQuoteResponse fields) where
  parseJSON (Object v) = do
    response <- v .: "response"
    quotes <- response .: "quotes"
    TKQuoteResponse <$> quotes .: "quote"
  parseJSON _ = mzero

instance FromJSON fields => FromJSON (TKQuotes fields) where
  parseJSON o@(Object _) = do
    quote <- parseJSON o
    return (TKQuotes [quote])
  parseJSON (Array v) = do
    quotes <- V.toList <$> V.mapM parseJSON v
    return (TKQuotes quotes)
  parseJSON _ = mzero

adapt :: Read a => String -> Parser a
adapt = maybe mzero return . readMay

instance FromJSON StreamOutput where
    parseJSON (Object v) = do
      let parseQuote (Object v) =
              do
                timestamp <- (maybe mzero return . parseTime defaultTimeLocale "%FT%T%z") =<< v .: "datetime"
                let day = localDay . zonedTimeToLocalTime $ timestamp
                    timeFormat = "%R"
                    timeZone = zonedTimeZone timestamp
                StreamQuote <$> pure (zonedTimeToUTC timestamp)
                            <*> (Stock <$> v .: "symbol")
                            <*> (unTKPrice <$> v .: "ask")
                            <*> (adapt =<< v .: "asksz")
                            <*> (unTKPrice <$> v .: "bid")
                            <*> (adapt =<< v .: "bidsz")
                            <*> v .:? "qcond"
          parseQuote _ = mzero

          parseTrade (Object v) =
              do
                timestamp <- (maybe mzero return . parseTime defaultTimeLocale "%FT%T%z") =<< v .: "datetime"
                let day = localDay . zonedTimeToLocalTime $ timestamp
                    timeFormat = "%R"
                    timeZone = zonedTimeZone timestamp
                StreamTrade <$> pure (zonedTimeToUTC timestamp)
                            <*> (Stock <$> v .: "symbol")
                            <*> (unTKPrice <$> v .: "last")
                            <*> (adapt =<< v .: "vl")
                            <*> (adapt =<< v .: "cvol")
                            <*> (adapt =<< v .: "vwap")
                            <*> (v .:? "tcond")
                            <*> (Exchange <$> v .: "exch")
          parseTrade _ = mzero
      status <- v .:? "status"
      quote <- v .:? "quote"
      trade <- v .:? "trade"
      case status of
        Nothing -> case quote of
                     Nothing -> case trade of
                                  Nothing -> mzero
                                  Just t -> parseTrade t
                     Just q -> parseQuote q
        Just s -> return (StreamStatus s)
    parseJSON _ = mzero

instance FromJSON TKStockQuote where
  parseJSON (Object v) = do
    timestamp <- (maybe mzero return . parseTime defaultTimeLocale "%FT%T%z") =<< v .: "datetime"
    let day = localDay . zonedTimeToLocalTime $ timestamp
        timeFormat = "%R"
        timeZone = zonedTimeZone timestamp

    TKStockQuote <$> (
      StockQuote <$>
        (Stock <$> v .: "symbol") <*>
        pure (zonedTimeToUTC timestamp) <*>
        (unTKPrice <$> v .: "ask") <*>
        (localTimeToUTC timeZone . LocalTime day <$> (maybe mzero return . parseTime defaultTimeLocale timeFormat =<< v .: "ask_time")) <*>
        (adapt =<< v .: "asksz") <*>
        (unTKPrice <$> v .: "bid") <*>
        (localTimeToUTC timeZone . LocalTime day <$> (maybe mzero return . parseTime defaultTimeLocale timeFormat =<< v .: "bid_time")) <*>
        (adapt =<< v .: "bidsz") <*>
        (unTKPrice <$> v .: "last") <*>
        (adapt =<< v .: "incr_vl") <*>
        (adapt =<< v .: "vl"))
  parseJSON _ = mzero

instance FromJSON TKStockInfo where
  parseJSON o@(Object v) = do
    timestamp <- (maybe mzero return . parseTime defaultTimeLocale "%FT%T%z") =<< v .: "datetime"

    let parseDate = maybe mzero return . parseTime defaultTimeLocale "%Y%m%d"
        timeZone = zonedTimeZone timestamp

    TKStockInfo <$> (
      StockInfo <$>
        (Stock <$> v .: "symbol") <*>
        pure (zonedTimeToUTC timestamp) <*>
        v .: "name" <*>
        (unTKPrice <$> v .: "pcls") <*>
        (unTKPrice <$> v .: "popn") <*>
        (unTKPrice <$> v .: "opn") <*>
        (CUSIP <$> v .: "cusip") <*>
        (maybe Nothing (Just . unTKPrice) <$> (v .:? "div")) <*>
        (maybe (return Nothing) ((Just <$>) . parseDate) =<< (v .:? "divexdate")) <*>
        (maybe Nothing (Just . unTKFrequency) <$> (v .:? "divfreq")) <*>
        (maybe (return Nothing) ((Just <$>) . parseDate) =<< (v .:? "divpaydt")) <*>
        (adapt . filter (/= ',') =<< v .: "sho") <*> -- Tradeking returns the number separated by commas, ew...
        (maybe Nothing (Just . unTKPrice) <$> v .:? "eps") <*>
        (Exchange <$> v .: "exch") <*>
        (HighLow <$> (unTKPrice <$> v .: "phi") <*> (unTKPrice <$> v .: "plo")) <*>
        (HighLow <$> ((,) <$> (parseDate =<< (v .: "wk52lodate")) <*> (unTKPrice <$> v .: "wk52lo"))
                 <*> ((,) <$> (parseDate =<< (v .: "wk52hidate")) <*> (unTKPrice <$> v .: "wk52hi"))) <*>
        (maybe Nothing readMay <$> v .:? "pe") <*>
        (unTKPrice <$> v .: "prbook") <*>
        (unTKStockQuote <$> parseJSON o))
  parseJSON _ = mzero

-- | Retrieve the stock quotes for the stocks specified.
stockQuotes :: TradeKingApp -> [Stock] -> IO [StockQuote]
stockQuotes app stocks = do
  let command = Quote assets ["timestamp", "ask", "ask_time", "asksz",
                               "bid", "bid_time", "bidsz", "last",
                               "date", "incr_vl", "vl"]
      assets = map StockAsset stocks
  response <- invokeSimple app command JSON
  case eitherDecode (rspPayload response) of
    Left e -> fail ("Malformed data returned: " ++ e)
    Right quoteData -> return (map unTKStockQuote . unTKQuotes . unTKQuoteResponse $ quoteData)

-- | Retrieve information on the stock symbols specified.
stockInfos :: TradeKingApp -> [Stock] -> IO [StockInfo]
stockInfos app stocks = do
  let command = Quote assets []
      assets = map StockAsset stocks
  response <- invokeSimple app command JSON
  case eitherDecode (rspPayload response) of
    Left e -> fail ("Malformed data returned: " ++ e)
    Right infoData -> return (map unTKStockInfo . unTKQuotes . unTKQuoteResponse $ infoData)

-- | Run a streaming operation on the stocks specified. This takes a function which will be passed a
--   `Source` from `Data.Conduit` that will yield the streaming quote information.
streamQuotes :: TradeKingApp -> [Stock] -> (Source (ResourceT IO) StreamOutput -> ResourceT IO b) -> IO b
streamQuotes app stocks f = streamQuotes' app stocks doStream
    where doStream bsrc = do
            (bsrc', finalizer) <- unwrapResumable bsrc
            let decodeMessages = await >>= \x ->
                                 case x of
                                   Nothing -> return ()
                                   Just x -> case eitherDecode (LBS8.fromChunks [x]) of
                                               Left e -> fail ("Malformed data returned: " ++ e)
                                               Right d -> do
                                                 yield d
                                                 decodeMessages
            f (bsrc' $= decodeMessages) `E.finally` finalizer