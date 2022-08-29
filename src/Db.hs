{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- без этой опции language service ругается на instance FromRow Tx, якобы это orphan instance
-- возможно, я не до конца понимаю эту концепцию, но этот инсанс явно используется при чтении из базы
{-# OPTIONS_GHC -Wno-orphans #-}

module Db (insert, update, findById, getAll) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Data.Text.Lazy as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField (Action (Plain), ToField (toField), inQuotes)
import Database.PostgreSQL.Simple.Types (Query (..))
import qualified GHC.Exts as T
import Model
import Text.Read (readMaybe)

-- ######### query constants #########

loadAccountsQuery :: Query
loadAccountsQuery = "SELECT id, name, currency, sum FROM account"

loadAccountQuery :: Query
loadAccountQuery = "SELECT id, name, currency, sum FROM account where id = ?"

insertAccountQuery :: Query
insertAccountQuery = "INSERT INTO Account(name, currency, sum) VALUES (?, ?, ?)"

updateAccountQuery :: Query
updateAccountQuery = "UPDATE Account set name = ? , currency = ? , sum = ? where id = ?"

loadTransactionsQuery :: Query
loadTransactionsQuery = "SELECT id, discriminator, description, ts, fromId, fromAmount, toId, toAmount FROM Tx"

-- ######### repositories

type DbCtxt = Connection

class Repository e id | e -> id where
  insert :: DbCtxt -> e -> IO e
  update :: DbCtxt -> e -> IO e
  findById :: DbCtxt -> id -> IO (Maybe e)
  getAll :: DbCtxt -> IO [e]

instance Repository Account AccountId where
  insert conn (Account _ name currency accountSum) = do
    withTransaction conn $ do
      _ <- execute conn insertAccountQuery (name, currency, accountSum)
      newId' <- lastId conn "Account" AccountId
      return $ Account newId' name currency accountSum

  update conn (Account accountId name currency accountSum) = do
    _ <- execute conn updateAccountQuery (name, currency, accountSum, accountId)
    return $ Account accountId name currency accountSum

  findById conn accountId = do
    accounts <- query conn loadAccountQuery $ Only accountId :: IO [Account]
    case accounts of
      account : _ -> return $ Just account
      [] -> return Nothing

  getAll conn = do
    query_ conn loadAccountsQuery :: IO [Account]

instance Repository Tx TxId where
  insert conn (Tx _ description date txDetails) = do
    withTransaction conn $ do
      case txDetails of
        Income (TxLeg to amount) -> do
          _ <-
            execute
              conn
              "INSERT INTO Tx(description, ts, discriminator, toId, toAmount) VALUES (?, ?, ?, ?, ?)"
              (description, date, B.pack "I", to, amount)

          newId' <- lastId conn "Tx" TxId

          return $ Tx newId' description date txDetails
        Outcome (TxLeg from amount) -> do
          _ <-
            execute
              conn
              "INSERT INTO Tx(description, ts, discriminator, fromId, fromAmount) VALUES (?, ?, ?, ?, ?)"
              (description, date, B.pack "O", from, amount)

          newId' <- lastId conn "Tx" TxId

          return $ Tx newId' description date txDetails
        Transfer (TxLeg from fromAmount) (TxLeg to toAmount) -> do
          _ <-
            execute
              conn
              "INSERT INTO Tx(description, ts, discriminator, fromId, fromAmount, toId, toAmount) VALUES (?, ?, ?, ?, ?, ?, ?)"
              (description, date, B.pack "T", from, fromAmount, to, toAmount)

          newId' <- lastId conn "Tx" TxId

          return (Tx newId' description date txDetails)

  update = undefined
  findById conn txId = undefined
  getAll conn = query_ conn loadTransactionsQuery

lastId :: Connection -> ByteString -> (Int -> m) -> IO m
lastId conn table idBuilder = do
  let aQuery = Query $ B.concat ["SELECT id FROM ", table, " ORDER BY id DESC LIMIT 1"]
  newId <- query_ conn aQuery :: IO [Only Int]
  return $ idBuilder $ fromOnly $ Prelude.head newId

-- ######### parsing types #########

instance FromField Currency where
  fromField f mdata = case B.unpack `fmap` mdata of
    Nothing -> returnError UnexpectedNull f ""
    Just dat -> do
      let maybeCurrency = readMaybe dat :: Maybe Currency
      case maybeCurrency of
        Nothing -> returnError ConversionFailed f dat
        Just currency -> return currency

instance ToField Currency where
  toField currency = Plain $ inQuotes $ byteString $ B.pack $ show currency

instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field <*> field

instance FromField TxId where
  fromField f mdata = TxId <$> fromField f mdata

instance FromField AccountId where
  fromField f mdata = AccountId <$> fromField f mdata

instance ToField TxId where
  toField (TxId txId) = toField txId

instance ToField AccountId where
  toField (AccountId accountId) = toField accountId

instance FromRow Tx where
  fromRow = do
    txId <- field :: RowParser TxId
    discriminator <- field :: RowParser T.Char
    description <- field :: RowParser T.Text
    date <- field :: RowParser UTCTime
    fromId <- field :: RowParser (Maybe AccountId)
    fromAmount <- field :: RowParser (Maybe Double)
    toId <- field :: RowParser (Maybe AccountId)
    toAmount <- field :: RowParser (Maybe Double)

    -- выглядит стремно, но я не знаю, как по-другому такие ситуации разруливать
    -- конечно, желательно писать ошибку неконсистентности данных, если записи нет
    -- другое решение - хранить каждый тип транзакции в своей таблице и делать джоины, но оно выглядит сильно сложнее
    let fromLeg = TxLeg (fromJust fromId) (fromJust fromAmount)
    let toLeg = TxLeg (fromJust toId) (fromJust toAmount)

    -- let dateMillis = posixSecondsToUTCTime $ fromInteger date / 1000

    let txWith = Tx txId description date

    return $ case discriminator of
      'I' -> txWith $ Income toLeg
      'O' -> txWith $ Outcome fromLeg
      'T' -> txWith $ Transfer fromLeg toLeg
      _ -> undefined -- todo returnError
