import qualified Control.Exception as E
import Client.Quote

import Data.String
import Data.Conduit

import Finance.TradeKing hiding (Option)




import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data AppToken = ConsumerKey String |
                ConsumerSecret String |
                OAuthToken String |
                OAuthTokenSecret String

options = [Option "k" ["consumer-key"] (ReqArg ConsumerKey "<consumer-key>") "Consumer Key",
           Option "s" ["consumer-secret"] (ReqArg ConsumerSecret "<consumer-secret>") "Consumer Secret",
           Option "t" ["oauth-token"] (ReqArg OAuthToken "<oauth-token>") "OAuth Token",
           Option "S" ["oauth-secret"] (ReqArg OAuthTokenSecret "<oauth-secret>") "OAuth Token Secret"]

buildApp :: [String] -> IO TradeKingApp
buildApp args = do
  let (actions, nonOptions, errors) = getOpt RequireOrder options args

  -- Look for the config file in certain locations
  let defEmptyTradeKingApp =  TradeKingApp {
        tradeKing = defaultTk,
        consumerKey = error "Missing consumer key",
        consumerSecret = error "Missing consumer secret",
        oAuthToken = error "Missing oauth token",
        oAuthTokenSecret = error "Missing oauth token secret"
        }

  emptyTradeKingAppR <- readTKConf defaultTKConfs
  let emptyTradeKingApp = case emptyTradeKingAppR of
                             Left err -> defEmptyTradeKingApp
                             Right tkApp -> tkApp

  let modifyApp a (ConsumerKey v) = a { consumerKey = fromString v }
      modifyApp a (ConsumerSecret v) = a { consumerSecret = fromString v }
      modifyApp a (OAuthToken v) = a { oAuthToken = fromString v }
      modifyApp a (OAuthTokenSecret v) = a { oAuthTokenSecret = fromString v }

      finalTradeKingApp = foldl modifyApp emptyTradeKingApp actions
  E.evaluate (consumerKey finalTradeKingApp)
  E.evaluate (consumerSecret finalTradeKingApp)
  E.evaluate (oAuthToken finalTradeKingApp)
  E.evaluate (oAuthTokenSecret finalTradeKingApp)
  return finalTradeKingApp

usage = do
  progName <- getProgName
  let progName' = progName ++ " <COMMAND>"
  hPutStrLn stderr (usageInfo progName' options)
  exitWith ExitSuccess

main :: IO ()
main = getArgs >>= \args ->
       case args of
         [] -> usage
         ["-h"] -> usage
         ["--help"] -> usage
         ('-':_):_ -> usage -- command starts with '-'
         "quote":args -> buildApp args >>= doQuote args
         "info":args -> buildApp args >>= doInfo args
         "stream":args -> buildApp args >>= doStream args
         _ -> usage