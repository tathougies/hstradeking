module Finance.TradeKing.Service where

import Finance.TradeKing.Types
import Finance.Asset

import qualified Data.Text as T
import Data.Maybe
import Data.List

import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response

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

invokeSimple :: TradeKingApp -> Command -> Format -> IO Response
invokeSimple tk cmd format = do
  let uri = fromJust . parseURL $ apiEndPoint (tradeKing tk) cmd format
      app = Application (consumerKey tk) (consumerSecret tk) OOB
      tok = fromApplication app
      tok' = AccessToken {
        application = application tok,
        oauthParams = fromList [("oauth_token", oAuthToken tk), ("oauth_token_secret", oAuthTokenSecret tk)] }
  runOAuthM tok' $ signRq2 HMACSHA1 Nothing uri >>= serviceRequest CurlClient