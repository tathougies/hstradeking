{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Finance.Asset.Types where

import qualified Data.Text as T
import Data.Time
import Data.String
import Data.Int
import Data.Number.Fixed

data Asset = StockAsset Stock   |
             OptionAsset Option |
             NoteAsset Note     |
             FutureAsset Future
    deriving (Show, Read, Eq, Ord)

newtype Stock = Stock { unStock :: T.Text }
    deriving (Show, Read, Eq, Ord, IsString)
newtype CUSIP = CUSIP T.Text
    deriving (Show, Read, Eq, Ord, IsString)
newtype Exchange = Exchange T.Text
    deriving (Show, Read, Eq, Ord, IsString)

-- Exact way of representing prices, etc.
type Eps4 = EpsDiv10 (EpsDiv10 (EpsDiv10 (EpsDiv10 Eps1)))
type Fixed4 = Fixed Eps4

data Currency = USD |
                GBP |
                CAD |
                Other T.Text
    deriving (Show, Read, Eq, Ord)

data Price = Price Currency Fixed4
    deriving (Show, Read, Eq, Ord)

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

data Note = Note deriving (Show, Read, Eq, Ord)
data Future = Future deriving (Show, Read, Eq, Ord)

data Period = Annually | SemiAnnually | Quarterly | Monthly
    deriving (Show, Read, Eq, Ord)