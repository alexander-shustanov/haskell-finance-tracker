module Server (runServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (object, (.=))
import Data.Maybe
import qualified Data.Text.Lazy as T
import Db
import Model
import Network.HTTP.Types (mkStatus, Status)
import Text.Read (readMaybe)
import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)

runServer = scotty 80 $ do
  get "/" $ do
    html "Finance Tracker Starter Page"

  get "/accounts" $ do
    accounts <- liftIO loadAccounts
    json accounts

  get "/account/:id" $ do
    id <- param "id" :: ActionM Int
    maybeAccount <- liftIO $ loadAccount id
    case maybeAccount of
      Nothing -> status $ mkStatus 404 "Account Not Found"
      Just account -> json account

  post "/account" $ do
    account <- jsonData :: ActionM Account
    account <- liftIO $ saveAccount account
    json account

  get "/balance" $ do
    maybeCurrency <- (readMaybe <$> param "currency") :: ActionM (Maybe Currency)
    if isNothing maybeCurrency
      then do text "Error"
      else do
        let currency = fromJust maybeCurrency
        accounts <- liftIO loadAccounts
        let balance = calculateBalance accounts currency
        json $ object ["balance" .= balance, "currency" .= currency]

  post "/transfer" $ do
    fromId <- param "from" :: ActionM Int
    toId <- param "to" :: ActionM Int
    amount <- param "amount" :: ActionM Double
    description <- param "description" :: ActionM T.Text

    validate (fromId /= toId) $ mkStatus 400 "fromAccount and toAccount can't be same account"

    validate (amount > 0) $ mkStatus 400 "Ammount must be positive"

    maybeFromAccount <- liftIO $ loadAccount fromId
    maybeToAccount <- liftIO $ loadAccount toId

    validate (isJust maybeFromAccount) $ mkStatus 404 "fromAccount not found"
    validate (isJust maybeToAccount) $ mkStatus 404 "toAccount not found"

    let fromAccount = fromJust maybeFromAccount
    let toAccount = fromJust maybeToAccount

    validate (accountSum fromAccount >= amount) $ mkStatus 400 "fromAccount has insufficient balance"

    tx <- liftIO $ transfer (T.toStrict description) fromAccount toAccount amount
    json tx

  post "/income" $ do
    accountId <- param "account" :: ActionM Int
    amount <- param "amount" :: ActionM Double
    description <- param "description" :: ActionM T.Text

    validate (amount > 0) $ mkStatus 400 "Ammount must be positive"

    maybeAccount <- liftIO $ loadAccount accountId

    validate (isJust maybeAccount) $ mkStatus 404 "account not found"

    let account = fromJust maybeAccount

    tx <- liftIO $ income (T.toStrict description) account amount
    json tx

  post "/outcome" $ do
    accountId <- param "account" :: ActionM Int
    amount <- param "amount" :: ActionM Double
    description <- param "description" :: ActionM T.Text

    validate (amount > 0) $ mkStatus 400 "Ammount must be positive"

    maybeAccount <- liftIO $ loadAccount accountId

    validate (isJust maybeAccount) $ mkStatus 404 "account not found"

    let account = fromJust maybeAccount

    validate (accountSum account >= amount) $ mkStatus 400 "account has insufficient balance"

    tx <- liftIO $ outcome (T.toStrict description) account amount
    json tx

  get "/transactions" $ do
    txs <- liftIO loadTransactions
    json txs

-- text $ T.pack $ show balance

calculateBalance :: Foldable t => t Account -> Currency -> Double
calculateBalance accounts targetCurrency =
  foldl (\acc account -> acc + accountBalance account) 0.0 accounts
  where
    accountBalance (Account _ _ from accountBalance) = convertMoney from targetCurrency accountBalance

validate :: Bool -> Status -> ActionT T.Text IO ()
validate condition orStatus =
  if not condition
    then do
      status orStatus
      finish
    else pure ()