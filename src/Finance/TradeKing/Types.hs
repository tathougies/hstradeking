{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Finance.TradeKing.Types where

import qualified Data.Text as T
import Data.Time
import Data.String
import Data.Int
import Data.Number.Fixed
import Data.Aeson
import Data.Word
import Data.Time.Clock

-- * TradeKing specific

-- | Data type that represents the URLs we need to connect to TradeKing
--   This could be useful if you wanted to host your own TradeKing-compatible testing server.
--   .
--   Most of the time, you'll use `defaultTk` to get the default values.
data TradeKing = TradeKing {
  apiBase :: String,
  streamBase :: String,
  authorizeURL :: String,
  requestTokenResource :: String,
  accessTokenResource :: String }
  deriving (Show, Read, Eq, Ord)

-- | This data type holds the application information provided by TradeKing and is used to make
--   requests.
data TradeKingApp = TradeKingApp {
  tradeKing :: TradeKing, -- ^ TradeKing URIs to use
  consumerKey :: String,
  consumerSecret :: String,
  oAuthToken :: String,
  oAuthTokenSecret :: String }
  deriving (Show, Read, Eq, Ord)

-- | Expected return format for API calls.
data Format = JSON | XML
   deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | TradeKing API commmands that we support.
data Command = Accounts |
               Clock |
               Quote [Asset] [String]
   deriving (Show, Read, Eq, Ord)

-- * Common Financial Asset and Transaction Types

-- ** Assets
-- | The different types of Assets available on TradeKing. Currently on Stocks and Options are supported.
data Asset = StockAsset Stock   |
             OptionAsset Option |
             NoteAsset Note     |
             FutureAsset Future
    deriving (Show, Read, Eq, Ord)

newtype Stock = Stock { unStock :: T.Text }
    deriving (Show, Read, Eq, Ord, IsString)
-- | Type to represent SEC CUSIP identifiers
newtype CUSIP = CUSIP T.Text
    deriving (Show, Read, Eq, Ord, IsString)
newtype Exchange = Exchange { unExchange :: T.Text }
    deriving (Show, Read, Eq, Ord, IsString)

-- ** Prices

-- | Exact way of representing prices, etc.
type Eps4 = EpsDiv10 (EpsDiv10 (EpsDiv10 (EpsDiv10 Eps1)))
type Fixed4 = Fixed Eps4

data Currency = USD |
                GBP |
                CAD |
                Other T.Text
    deriving (Show, Read, Eq, Ord)

-- | A price has both a currency component and a decimal. Currently we only denote prices in USD,
-- but this is here for future compatibility.
data Price = Price Currency Fixed4
    deriving (Show, Read, Eq, Ord)

-- ** Options

-- | Describes an option. Option support is TBD.
data Option = Option {
  optionType        :: OptionType,
  optionUnderlying  :: Stock,
  optionStrikePrice :: Price,
  optionExpiration  :: Day
  }
  deriving (Show, Read, Eq, Ord)

data OptionType = OptionType {
  optionContract :: ContractType,
  optionStyle :: OptionStyle
  }
  deriving (Show, Read, Eq, Ord)

data ContractType = Call | Put
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
data OptionStyle = American | European
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- ** Other Assets

-- | Unimplemented
data Note = Note deriving (Show, Read, Eq, Ord)
-- | Unimplemented
data Future = Future deriving (Show, Read, Eq, Ord)

-- ** Utility types

data Period = Annually | SemiAnnually | Quarterly | Monthly
    deriving (Show, Read, Eq, Ord)

data HighLow a = HighLow {
  high :: a,
  low  :: a
  } deriving (Show, Eq)

-- * TradeKing API-specific data structures

-- | A stock quote
data StockQuote = StockQuote {
  sqStock        :: Stock,       -- ^ Stock which we're quoting
  sqTime         :: UTCTime,     -- ^ Time at which the quote was sampled
  sqAsk          :: Price,
  sqAskTime      :: UTCTime,     -- ^ Time at which the ask quote was last updated
  sqAskSz        :: Word,        -- ^ Current size of ask queue
  sqBid          :: Price,
  sqBidTime      :: UTCTime,     -- ^ Time at which the bid quote was last updated
  sqBidSz        :: Word,        -- ^ Current size of bid queue
  sqLastPrice    :: Price,
  sqLastTradeVol :: Word,        -- ^ Volue of last trade
  sqCumVol       :: Word         -- ^ Number of trades since market open
  } deriving (Show, Eq)

-- | Stock information that may change day-to-day
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

-- | This is the type of data that may be output by the streaming calls. We can't use other data
--   types here, because the streaming data is limited in scope.
data StreamOutput = StreamStatus String
                    -- ^ Stream status. This should be the first thing that comes out of the conduit
                    --   in `streamQuotes` and should have the argument `"connected"` on success.

                    -- | Represents updates to the quote of a symbol
                  | StreamQuote
                    { stqTime         :: UTCTime
                    , stqSymbol       :: Stock
                    , stqAsk          :: Price
                    , stqAskSz        :: Word
                    , stqBid          :: Price
                    , stqBidSz        :: Word
                    , stqQCond        :: Maybe String }

                    -- | Represents a trade that just took place
                  | StreamTrade
                    { tTime           :: UTCTime
                    , tSymbol         :: Stock
                    , tPrice          :: Price
                    , tVol            :: Word
                    , tCumVol         :: Word
                    , tVWAP           :: Maybe Float
                    , tCond           :: Maybe String
                    , tExch           :: Exchange }
                    deriving (Show)

-- | Default TradeKing URIs. Equivalent to
--
-- > TradeKing {
-- >   apiBase = "https://api.tradeking.com/v1/",
-- >   streamBase = "https://stream.tradeking.com/v1/",
-- >   authorizeURL = "https://developers.tradeking.com/oauth/authorize?oauth_token=",
-- >   requestTokenResource = "https://developers.tradeking.com/oauth/request_token",
-- >   accessTokenResource = "https://developers.tradeking.com/oauth/access_token" }
defaultTk :: TradeKing
defaultTk = TradeKing {
  apiBase = "https://api.tradeking.com/v1/",
  streamBase = "https://stream.tradeking.com/v1/",
  authorizeURL = "https://developers.tradeking.com/oauth/authorize?oauth_token=",
  requestTokenResource = "https://developers.tradeking.com/oauth/request_token",
  accessTokenResource = "https://developers.tradeking.com/oauth/access_token" }
