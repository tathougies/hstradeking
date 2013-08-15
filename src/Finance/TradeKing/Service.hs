module Finance.TradeKing.Service where

import Finance.TradeKing.Types

import Data.Maybe

import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response

apiEndPoint :: TradeKing -> Command -> Format -> String
apiEndPoint tk cmd fmt = apiBase tk ++ cmdEndPoint cmd ++ "." ++ apiFmt fmt
  where cmdEndPoint Accounts = "accounts"

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