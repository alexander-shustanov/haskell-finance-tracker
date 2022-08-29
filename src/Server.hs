module Server (runServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (object, (.=))
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Text.Lazy as T
import Db
import Model
import Network.HTTP.Types (mkStatus)
import Text.Read (readMaybe)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import BusinessLogic

runServer = scotty 80 $ do
  get "/" $
    html "Finance Tracker Starter Page"

  get "/accounts" $
    liftIO loadAccounts >>= json

  get "/account/:id" $
    do
      param "id"
      >>= liftIO . loadAccount
      >>= getOr 404 "Account Not Found"
      >>= json

  post "/account" $
    jsonData >>= (liftIO . saveAccount) >>= json

  get "/balance" $ do
    currency <- param "currency" >>= (getOr 400 "Currency does not exists" . readMaybe) :: ActionM Currency
    balance <- calculateBalance currency <$> liftIO loadAccounts

    json $
      object
        [ "balance" .= balance,
          "currency" .= currency
        ]

  post "/transfer" $ do
    fromId <- param "from" :: ActionM AccountId
    toId <- param "to" :: ActionM AccountId
    amount <- param "amount" :: ActionM Double
    description <- param "description" :: ActionM T.Text

    validate (fromId /= toId) 400 "fromAccount and toAccount can't be same account"

    validate (amount > 0) 400 "Ammount must be positive"

    fromAccount <- liftIO (loadAccount fromId) >>= getOr 404 "fromAccount not found"
    toAccount <- liftIO (loadAccount toId) >>= getOr 404 "toAccount not found"

    validate (accountSum fromAccount >= amount) 400 "fromAccount has insufficient balance"

    tx <- liftIO $ transfer description fromAccount toAccount amount
    json tx

  post "/income" $ do
    accountId <- param "account"
    amount <- param "amount" :: ActionM Double
    description <- param "description" :: ActionM T.Text

    validate (amount > 0) 400 "Ammount must be positive"

    account <- liftIO (loadAccount accountId) >>= getOr 404 "account not found"

    liftIO (income description account amount) >>= json

  post "/outcome" $ do
    accountId <- param "account" :: ActionM AccountId
    amount <- param "amount" :: ActionM Double
    description <- param "description" :: ActionM T.Text

    validate (amount > 0) 400 "Ammount must be positive"

    account <- liftIO (loadAccount accountId) >>= getOr 404 "account not found"

    validate (accountSum account >= amount) 400 "account has insufficient balance"

    tx <- liftIO $ outcome description account amount
    json tx

  get "/transactions" $
    liftIO loadTransactions >>= json

calculateBalance :: Traversable t => Currency -> t Account -> Double
calculateBalance targetCurrency = sum . fmap accountBalance
  where
    accountBalance (Account _ _ from accountBalance) = convertMoney from targetCurrency accountBalance

validate :: Bool -> Int -> ByteString -> ActionT T.Text IO ()
validate condition code message =
  if not condition
    then status (mkStatus code message) >> finish
    else pure ()

getOr :: Int -> ByteString -> Maybe a -> ActionT T.Text IO a
getOr code message Nothing = status (mkStatus code message) >> finish
getOr _ _ (Just v) = return v

instance Parsable AccountId where
  parseParam = fmap AccountId . parseParam