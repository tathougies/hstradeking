module Finance.Asset.Quote where

import Finance.Asset.Types

import qualified Data.Text as T
import Data.Time
import Data.Word

data HighLow a = HighLow {
  high :: a,
  low  :: a
  } deriving (Show, Eq)

data StockQuote = StockQuote {
  sqStock        :: Stock,
  sqTime         :: UTCTime,     -- ^ Time at which the quote was sampled
  sqAsk          :: Price,
  sqAskTime      :: UTCTime,
  sqAskSz        :: Word,        -- ^ Current size of ask queue
  sqBid          :: Price,
  sqBidTime      :: UTCTime,
  sqBidSz        :: Word,        -- ^ Current size of bid queue
  sqLastPrice    :: Price,
  sqLastTradeVol :: Word,        -- ^ Volue of last trade
  sqCumVol       :: Word         -- ^ Number of trades since market open
  } deriving (Show, Eq)

-- | Stock information that changes day-to-day
data StockInfo = StockInfo {
  siStock        :: Stock,
  siTime         :: UTCTime,
  siCompanyName  :: T.Text,
  siPrevClose    :: Price,   -- ^ Previous day close
  siPrevOpen     :: Price,   -- ^ Previous day open
  siOpen         :: Price,   -- ^ Current day open
  siCusip        :: CUSIP,
  siDiv          :: Maybe Price,
  siDivExDate    :: Maybe Day,
  siDivFrequency :: Maybe Period,
  siDivPayDate   :: Maybe Day,
  siShares       :: Integer,
  siEps          :: Maybe Price,
  siExchange     :: Exchange,
  siDailyHiLo    :: HighLow Price,
  siYearlyHiLo   :: HighLow (Day, Price),
  siPE           :: Maybe Double,
  siBook         :: Price,
  siQuote        :: StockQuote -- ^ Embedded stock quote
  } deriving (Show, Eq)