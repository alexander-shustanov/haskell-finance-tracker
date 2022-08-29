{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson
import Data.Text.Lazy
import Data.Time
import GHC.Generics (Generic)

-- такой подход не позволит быстро добавить новую валюту
-- возможно стоило бы сделать что-то в духе Currency {symbol :: Text, basePrice :: Double {- цена одной единицы в каких-то условных единицах -}}
data Currency = USD | RUB | GBP | BTC | USDT deriving (Show, Read, Eq, Generic)

instance ToJSON Currency

instance FromJSON Currency

convertMoney :: Currency -> Currency -> Double -> Double
convertMoney from to value = value * price from to
  where
    price USD RUB = 60.53
    price USD GBP = 0.83
    price USD BTC = 0.000043
    price USD USDT = 1
    price x y
      | x == y = 1
      | otherwise = price USD y / price USD x

class NewId a where
  newId :: a

newtype AccountId = AccountId Int deriving (Show, Generic, Eq)

instance NewId AccountId where
  newId = AccountId 0

instance FromJSON AccountId

instance ToJSON AccountId

data Account = Account
  { accountId :: AccountId,
    accountName :: Text,
    accountCurrency :: Currency,
    accountSum :: Double
  }
  deriving (Show)

instance FromJSON Account where
  parseJSON (Object o) =
    Account
      <$> o .:? "id" .!= (AccountId 0)
      <*> o .: "name"
      <*> o .: "currency"
      <*> o .: "sum"

instance ToJSON Account where
  toJSON (Account accountId name currency sum) =
    object
      [ "id" .= accountId,
        "name" .= name,
        "currency" .= currency,
        "sum" .= sum
      ]

newtype TxId = TxId Int deriving (Show, Generic)

instance NewId TxId where
  newId = TxId 0

instance FromJSON TxId

instance ToJSON TxId

data Tx = Tx
  { txId :: TxId,
    txDescription :: Text,
    txDate :: UTCTime,
    txDetails :: TxDetails
  }
  deriving (Show)

data TxDetails
  = Income
      { toLeg :: TxLeg
      }
  | Outcome
      { fromLeg :: TxLeg
      }
  | Transfer
      { fromLeg :: TxLeg,
        toLeg :: TxLeg
      }
  deriving (Show)

-- наверно не имело смысл вводить этот тип, а хранить данные напрямую в Income {toAccountId, toSum}, Outcome {fromAccountId, fromSum} и Transfer {toAccountId, toSum, fromAccountId, fromSum}
-- но тогда Income и Outcome будут шарить свои поля с Transfer, что создаст не очень безопасный код
-- я не знаю, как поступают в таких ситуациях, поэтому вынес в отдельный тип
-- но надо признать, что проблему это не решило
data TxLeg = TxLeg
  { legAccountId :: AccountId,
    legSum :: Double
  }
  deriving (Show, Generic)

instance ToJSON Tx where
  toJSON tx =
    object (commonProperties ++ specificProperties)
    where
      commonProperties =
        [ "id" .= txId tx,
          "description" .= txDescription tx,
          "date" .= show (txDate tx)
        ]
      specificProperties = case txDetails tx of
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