-- без этой опции language service ругается на instance FromRow Tx, якобы это orphan instance
-- возможно, я не до конца понимаю эту концепцию, но этот инсанс явно используется при чтении из базы
{-# OPTIONS_GHC -Wno-orphans #-}

module Db where

import Data.ByteString (ByteString)
import Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField (Action (Plain), ToField (toField))
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified GHC.Exts as T
import Model
import Text.Read (readMaybe)

-- todo move to configs
-- тут есть несколько очевидно плохих моментов в работе с базой,
-- первый из которых - захардкоженные параметры подключения
-- кроме того, постоянно создаются новые коннекшны, хотя, скорее всего стоило бы использовать какой-нибудь пул
dbConnectionInfo :: ConnectInfo
dbConnectionInfo =
  defaultConnectInfo
    { connectHost = "127.0.0.1",
      connectDatabase = "finances",
      connectUser = "postgres",
      connectPassword = "postgres"
    }

loadAccounts :: IO [Account]
loadAccounts = do
  conn <- connect dbConnectionInfo
  query_ conn "SELECT id, name, currency, sum FROM account" :: IO [Account]

loadAccount :: Int -> IO (Maybe Account)
-- вроде как имя id подошло идеально, но тогда линтер ругается на name shadowing
-- не знаю, как приянто в haskell сообществе, но конкретно тут и далее можно было написать accountId
-- но на самом деле - нет, так как accountId тоже уже занят
loadAccount id_ = do
  conn <- connect dbConnectionInfo
  accounts <- query conn "SELECT id, name, currency, sum FROM account where id = ?" $ Only id_ :: IO [Account]
  case accounts of
    account : _ -> return $ Just account
    [] -> return Nothing

saveAccount :: Account -> IO Account
saveAccount (Account id_ name currency sum_) = do
  conn <- connect dbConnectionInfo
  if id_ == 0
    then do
      withTransaction conn $ do
        _ <- execute conn "INSERT INTO Account(name, currency, sum) VALUES (?, '?', ?)" (name, currency, sum_)
        newId <- lastId conn "Account"
        return $ Account newId name currency sum_
    else do
      _ <- execute conn "UPDATE Account set name = ? , currency = '?' , sum = ? where id = ?" (name, currency, sum_, id_)
      return $ Account id_ name currency sum_

loadTransactions :: IO [Tx]
loadTransactions = do
  conn <- connect dbConnectionInfo
  query_ conn "SELECT id, discriminator, description, ts, fromId, fromAmount, toId, toAmount FROM Tx" :: IO [Tx]

saveTransaction :: Tx -> IO Tx
saveTransaction (Tx _ description date txData) = do
  conn <- connect dbConnectionInfo
  withTransaction conn $ do
    case txData of
      Income (TxLeg to amount) -> do
        _ <-
          execute
            conn
            "INSERT INTO Tx(description, ts, discriminator, toId, toAmount) VALUES (?, ?, ?, ?, ?)"
            (description, utcToMillis date, B.pack "I", to, amount)

        newId <- lastId conn "Tx"

        return $ Tx newId description date txData
      Outcome (TxLeg from amount) -> do
        _ <-
          execute
            conn
            "INSERT INTO Tx(description, ts, discriminator, fromId, fromAmount) VALUES (?, ?, ?, ?, ?)"
            (description, utcToMillis date, B.pack "O", from, amount)

        newId <- lastId conn "Tx"

        return $ Tx newId description date txData
      Transfer (TxLeg from fromAmount) (TxLeg to toAmount) -> do
        _ <-
          execute
            conn
            "INSERT INTO Tx(description, ts, discriminator, fromId, fromAmount, toId, toAmount) VALUES (?, ?, ?, ?, ?, ?, ?)"
            (description, utcToMillis date, B.pack "T", from, fromAmount, to, toAmount)

        newId <- lastId conn "Tx"

        return (Tx newId description date txData)

-- на мой взгляд, совершенно неподходяшее место для этой функции
-- она одновременно выполняет и "бизнес-операцию" и запись в базу
-- наверно, стоит вынести в отдельный модуль, выполнять вычисления там
-- а дальше уже дергать функции работы с базой
transfer :: T.Text -> Account -> Account -> Double -> IO Tx
transfer description from to fromAmount = do
  -- todo: run in tx
  now <- getCurrentTime
  let toAmount = convertMoney (accountCurrency from) (accountCurrency to) fromAmount
  let txData =
        Transfer
          { transferFrom = TxLeg (accountId from) fromAmount,
            transferTo = TxLeg (accountId to) toAmount
          }

  tx <- saveTransaction $ Tx 0 description now txData

  _ <- saveAccount from {accountSum = accountSum from - fromAmount}
  _ <- saveAccount to {accountSum = accountSum to + toAmount}

  return tx

income :: T.Text -> Account -> Double -> IO Tx
income description to amount = do
  -- todo: run in tx

  now <- getCurrentTime
  let txData = Income $ TxLeg (accountId to) amount

  tx <- saveTransaction $ Tx 0 description now txData

  _ <- saveAccount to {accountSum = accountSum to + amount}

  return tx

outcome :: T.Text -> Account -> Double -> IO Tx
outcome description from amount = do
  -- todo: run in tx

  now <- getCurrentTime
  let txData = Outcome $ TxLeg (accountId from) amount

  tx <- saveTransaction $ Tx 0 description now txData

  _ <- saveAccount from {accountSum = accountSum from - amount}

  return tx

lastId :: Connection -> ByteString -> IO Int
lastId conn table = do
  let aQuery = Query $ B.concat ["SELECT id FROM ", table, " ORDER BY id DESC LIMIT 1"]
  newId <- query_ conn aQuery :: IO [Only Int]
  return $ fromOnly $ Prelude.head newId

instance FromField Currency where
  fromField f mdata = case B.unpack `fmap` mdata of
    Nothing -> returnError UnexpectedNull f ""
    Just dat -> do
      let maybeCurrency = readMaybe dat :: Maybe Currency
      case maybeCurrency of
        Nothing -> returnError ConversionFailed f dat
        Just currency -> return currency

-- todo change mapping to avoid the need of wrap currency into quotes in queries
instance ToField Currency where
  toField currency = Plain $ byteString $ B.pack $ show currency

instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field <*> field

instance FromRow Tx where
  fromRow = do
    id_ <- field :: RowParser Int
    discriminator <- field :: RowParser T.Char
    description <- field :: RowParser T.Text
    date <- field :: RowParser Integer
    fromId <- field :: RowParser (Maybe Int)
    fromAmount <- field :: RowParser (Maybe Double)
    toId <- field :: RowParser (Maybe Int)
    toAmount <- field :: RowParser (Maybe Double)

    let fromLeg = TxLeg (fromJust fromId) (fromJust fromAmount)
    let toLeg = TxLeg (fromJust toId) (fromJust toAmount)

    let dateMillis = posixSecondsToUTCTime $ fromInteger date / 1000

    return $ case discriminator of
      'I' -> Tx id_ description dateMillis $ Income toLeg
      'O' -> Tx id_ description dateMillis $ Outcome fromLeg
      'T' -> Tx id_ description dateMillis $ Transfer fromLeg toLeg
      _ -> undefined -- todo returnError

millisToUTC :: Integer -> UTCTime
millisToUTC millis = posixSecondsToUTCTime $ fromInteger millis / 1000

utcToMillis :: UTCTime -> Integer
utcToMillis time = (round . (* 1000)) $ utcTimeToPOSIXSeconds time