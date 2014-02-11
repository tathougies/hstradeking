-- | Low-level access to TradeKing API
module Finance.TradeKing.Service where

import Finance.TradeKing.Types

import Control.Monad.Trans.Resource

import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI
import Data.Maybe
import Data.List
import Data.Conduit
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import Data.String

import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.Request as Req
import Network.OAuth.Http.Response
import qualified Network.HTTP.Conduit as HttpC

-- | Given a set of TradeKing URLs, a `Command`, and a `Format`, returns the API endpoint
apiEndPoint :: TradeKing -> Command -> Format -> String
apiEndPoint tk cmd fmt = apiBase tk ++ endPoint ++ "." ++ apiFmt fmt ++
                         case queryString of
                           Nothing -> ""
                           Just queryString -> "?" ++ queryString
  where (endPoint, queryString) = cmdEndPoint cmd

        cmdEndPoint Accounts = ("accounts", Nothing)
        cmdEndPoint Clock    = ("market/clock", Nothing)
        cmdEndPoint (Quote assets fields) = ("market/ext/quotes", Just queryStr)
          where queryStr = "symbols=" ++ intercalate "," (symbols assets) ++ fieldsQuery
                fieldsQuery = case fields of
                  [] -> ""
                  _ -> "&fids=" ++ intercalate "," fields
                symbols = map assetSymbol

                assetSymbol (StockAsset (Stock s)) = T.unpack s

        apiFmt XML = "xml"
        apiFmt JSON = "json"

-- | Invokes a simple command signed with OAuth in the given format, and returns the `Response`
--   object.
invokeSimple :: TradeKingApp -> Command -> Format -> IO Response
invokeSimple tk cmd format = do
  let uri = fromJust . parseURL $ apiEndPoint (tradeKing tk) cmd format
      app = Application (consumerKey tk) (consumerSecret tk) OOB
      tok = fromApplication app
      tok' = AccessToken {
        application = application tok,
        oauthParams = fromList [("oauth_token", oAuthToken tk), ("oauth_token_secret", oAuthTokenSecret tk)] }
  runOAuthM tok' $ signRq2 HMACSHA1 Nothing uri >>= serviceRequest CurlClient

-- | Low-level quote streaming command. This differs from `streamQuotes` in that the source it
--   provides produces ByteStrings.
streamQuotes' :: TradeKingApp -> [Stock] -> (ResumableSource (ResourceT IO) ByteString -> ResourceT IO b) -> IO b
streamQuotes' tk stocks f = do
  let uri = (streamBase (tradeKing tk)) ++ "market/quotes.json?symbols=" ++ intercalate "," (map (T.unpack . unStock) stocks)
  req <- HttpC.parseUrl uri
  let app = Application (consumerKey tk) (consumerSecret tk) OOB
      tok = fromApplication app
      tok' = AccessToken {
        application = application tok,
        oauthParams = fromList [("oauth_token", oAuthToken tk), ("oauth_token_secret", oAuthTokenSecret tk)] }
  oauthReq <- runOAuthM tok' $ signRq2 HMACSHA1 Nothing (fromJust . parseURL $ uri)
  let oauthReq' = unpackRq oauthReq
      req' = req { HttpC.requestHeaders = map (\(h,b) -> (CI.mk (fromString h), fromString b)) (Req.toList (reqHeaders oauthReq')) }
  HttpC.withManager $ \manager -> do
                           response <- HttpC.http req' manager
                           let bsrc = HttpC.responseBody response
                           f bsrc