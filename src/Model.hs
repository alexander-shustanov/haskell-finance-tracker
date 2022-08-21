{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Text
import Data.Time
import GHC.Generics (Generic)

-- import qualified Text.Read as TR
-- import qualified Text.Show as TS

-- такой подход не позволит быстро добавить новую валюту
-- возможно стоило бы сделать что-то в духе Currency {symbol :: Text, basePrice :: Double {- цена одной единицы в каких-то условных единицах -}}
data Currency = USD | RUB | GBP | BTC | USDT deriving (Show, Read, Eq, Generic)

convertMoney :: Currency -> Currency -> Double -> Double
convertMoney from to value = value * price from to
  where
    price USD RUB = 60.53
    price USD GBP = 0.83
    price USD BTC = 0.000043
    price USD USDT = 1
    price x y
      | x == y = 1
      | otherwise = price USD x / price USD y

instance ToJSON Currency

instance FromJSON Currency

data Account = Account
  { accountId :: Int {- для accountId стоило бы ввести свой тип newtype AccountId = Int -},
    accountName :: Text,
    accountCurrency :: Currency,
    accountSum :: Double
  }
  deriving (Show)

instance FromJSON Account where
  parseJSON (Object o) =
    Account
      <$> o .:? "id" .!= 0
      <*> o .: "name"
      <*> o .: "currency"
      <*> o .: "sum"

instance ToJSON Account where
  toJSON (Account id name currency sum) =
    object
      [ "id" .= id,
        "name" .= name,
        "currency" .= currency,
        "sum" .= sum
      ]

-- наверно не имело смысл вводить этот тип, а хранить данные напрямую в Income, Outcome и Transfer
data TxLeg = TxLeg
  { legAccountId :: Int,
    legSum :: Double
  }
  deriving (Show, Generic)

instance FromJSON TxLeg

instance ToJSON TxLeg

data TxData
  = Income
      { txLeg :: TxLeg
      }
  | Outcome
      { txLeg :: TxLeg
      }
  | Transfer
      { transferFrom :: TxLeg,
        transferTo :: TxLeg
      }
  deriving (Show)

data Tx = Tx
  { txId :: Int {- newtype TxId = Int -},
    txDescription :: Text,
    txDate :: UTCTime,
    txData :: TxData
  }
  deriving (Show)

instance ToJSON Tx where
  toJSON tx =
    object (commonProperties ++ specificProperties)
    where
      commonProperties =
        [ "id" .= txId tx,
          "description" .= txDescription tx,
          "date" .= show (txDate tx)
        ]
      specificProperties = case txData tx of
        Income (TxLeg to amount) ->
          [ "type" .= String "income",
            "toAccount" .= to,
            "amount" .= amount
          ]
        Outcome (TxLeg from amount) ->
          [ "type" .= String "outcome",
            "fromAccount" .= from,
            "amount" .= amount
          ]
        Transfer (TxLeg to toAmount) (TxLeg from fromAmount) ->
          [ "type" .= String "transfer",
            "fromAccount" .= from,
            "toAccount" .= to,
            "fromAmount" .= fromAmount,
            "toAmount" .= toAmount
          ]
