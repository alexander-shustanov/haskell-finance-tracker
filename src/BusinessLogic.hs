module BusinessLogic where

import qualified Data.Text.Lazy as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
import Db
import Model

-- в идеале вынести в конфиг, и получать из монадного трансформера ReaderT
dbConnectionInfo :: ConnectInfo
dbConnectionInfo =
  defaultConnectInfo
    { connectHost = "127.0.0.1",
      connectDatabase = "finances",
      connectUser = "postgres",
      connectPassword = "postgres"
    }

loadAccounts :: IO [Account]
loadAccounts = connect dbConnectionInfo >>= getAll

-- вроде как имя id подошло идеально, но тогда линтер ругается на name shadowing
-- не знаю, как приянто в haskell сообществе, но конкретно тут и далее можно было написать accountId
-- но на самом деле - нет, так как accountId тоже уже занят
loadAccount :: AccountId -> IO (Maybe Account)
loadAccount accountId = do
  conn <- connect dbConnectionInfo
  findById conn accountId

saveAccount :: Account -> IO Account
saveAccount account = do
  conn <- connect dbConnectionInfo
  if accountId account == newId
    then do
      insert conn account
    else do
      update conn account

loadTransactions :: IO [Tx]
loadTransactions = connect dbConnectionInfo >>= getAll

transfer :: T.Text -> Account -> Account -> Double -> IO Tx
transfer description from to fromAmount = do
  now <- getCurrentTime
  conn <- connect dbConnectionInfo

  withTransaction conn $ do
    let toAmount = convertMoney (accountCurrency from) (accountCurrency to) fromAmount
    let details =
          Transfer
            { fromLeg = TxLeg (accountId from) fromAmount,
              toLeg = TxLeg (accountId to) toAmount
            }

    tx <- insert conn $ Tx newId description now details

    _ <- update conn $ from {accountSum = accountSum from - fromAmount}
    _ <- update conn $ to {accountSum = accountSum to + toAmount}

    return tx

income :: T.Text -> Account -> Double -> IO Tx
income description to amount = do
  now <- getCurrentTime
  conn <- connect dbConnectionInfo

  withTransaction conn $ do
    let details = Income $ TxLeg (accountId to) amount

    tx <- insert conn $ Tx newId description now details
    _ <- update conn $ to {accountSum = accountSum to + amount}

    return tx

outcome :: T.Text -> Account -> Double -> IO Tx
outcome description from amount = do
  now <- getCurrentTime
  conn <- connect dbConnectionInfo

  withTransaction conn $ do
    let details = Outcome $ TxLeg (accountId from) amount

    tx <- insert conn $ Tx newId description now details

    _ <- update conn $ from {accountSum = accountSum from - amount}

    return tx