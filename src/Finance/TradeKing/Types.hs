module Finance.TradeKing.Types where

data TradeKing = TradeKing {
  apiBase :: String,
  authorizeURL :: String,
  requestTokenResource :: String,
  accessTokenResource :: String }
  deriving (Show, Read, Eq, Ord)

data TradeKingApp = TradeKingApp {
  tradeKing :: TradeKing, -- ^ TradeKing URIs to use
  consumerKey :: String,
  consumerSecret :: String,
  oAuthToken :: String,
  oAuthTokenSecret :: String }
  deriving (Show, Read, Eq, Ord)

data Format = JSON | XML
   deriving (Show, Read, Eq, Ord, Bounded, Enum)

data Command = Accounts
   deriving (Show, Read, Eq, Ord, Bounded, Enum)

defaultTk :: TradeKing
defaultTk = TradeKing {
  apiBase = "https://api.tradeking.com/v1/",
  authorizeURL = "https://developers.tradeking.com/oauth/authorize?oauth_token=",
  requestTokenResource = "https://developers.tradeking.com/oauth/request_token",
  accessTokenResource = "https://developers.tradeking.com/oauth/access_token" }
