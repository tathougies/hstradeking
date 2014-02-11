module Finance.Market where

import Data.Time

data MarketState = PreHours |
                   Open |
                   AfterHours |
                   Closed
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | For use with the TradeKing `/market/clock` API call.
data MarketClock = MarketClock {
  mcState     :: MarketState,
  mcNextState :: MarketState,
  mcTime      :: UTCTime,
  mcNextTime  :: UTCTime --^ when the market will transition into the next state
  } deriving (Show, Read, Eq, Ord)