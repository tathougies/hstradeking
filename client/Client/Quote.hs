module Client.Quote where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8 as BS
import Data.String
import Data.Conduit.Binary hiding (mapM_)
import Data.Conduit

import Finance.TradeKing

import System.IO

doQuote :: [String] -> TradeKingApp -> IO ()
doQuote args app = do
  quotes <- stockQuotes app (map fromString args)
  mapM_ (putStrLn . show) quotes

doInfo :: [String] -> TradeKingApp -> IO ()
doInfo args app = do
  infos <- stockInfos app (map fromString args)
  mapM_ (putStrLn . show) infos


doStream :: [String] -> TradeKingApp -> IO ()
doStream args app = do
  let mySink = await >>= \x ->
               case x of
                 Nothing -> return ()
                 Just x -> do
                        liftIO (putStrLn (show x))
                        liftIO (hFlush stdout)
                        mySink
  streamQuotes app (map fromString args)  (\s -> s $$ mySink)
