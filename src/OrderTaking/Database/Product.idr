module OrderTaking.Database.Product

import public Control.Monad.Either

import Data.String
import Control.Monad.Trans
import Control.Monad.Reader

import Rango.DataTransfer.SQL.Syntax
import Service.NodeJS.SQLite
import Service.NodeJS.Promise

import OrderTaking.DTO.PlaceOrder


productTable : Table
productTable = MkTable
  "product"
  [ field "id"          SQL_Integer [PrimaryKey, AutoIncrement]
  , field "code"        SQL_Text    [NotNull]
  , field "description" SQL_Text    [NotNull]
  , field "price"       SQL_Double  [NotNull]
  ]
  [ Unique "code_unique" ["code"] ]
  YesOfCourseValid

public export
data ProductDBError
  = InitializeError String
  | SaveProductError String
  | NonExistingProduct String

export
Show ProductDBError where
  show (InitializeError    e) = "InitializeError: " ++ show e
  show (SaveProductError   e) = "SaveProductError: " ++ show e
  show (NonExistingProduct e) = "NonExistingProduct: " ++ show e

export
ProductDB : Type -> Type
ProductDB a = EitherT ProductDBError (ReaderT Database Promise) a

export
runProductDB : Database -> ProductDB a -> Promise (Either ProductDBError a)
runProductDB db m = runReaderT db (runEitherT m)

throwIfFail : (String -> ProductDBError) -> Promise SomeError -> ProductDB ()
throwIfFail mkError p = do
  NoError <- lift (lift p)
    | HasError err => throwError $ mkError !(toString err)
  pure ()

export
saveProduct : ProductDTO -> ProductDB ()
saveProduct (MkProductDTO (MkProductCodeDTO productCode) price description) = do
  db <- ask
  throwIfFail SaveProductError $ Database.run db $ renderCommand $
    Insert productTable
      [ FieldOf "code"        (SQLText productCode)
      , FieldOf "description" (SQLText description)
      , FieldOf "price"       (SQLDouble price)
      ]

export
productExists : ProductCodeDTO -> ProductDB Bool
productExists _ = pure False
-- TODO

export
productPrice : ProductCodeDTO -> ProductDB Double
productPrice _ = pure 1.0
-- TODO

export
initDB : IO ()
initDB = do
  sqlite <- SQLite.require
  resolve' (\_ => putStrLn "OK.") putStrLn $ do
    db <- either !(SQLite.database sqlite "./db/product.db")
    ignore $ Database.run db $ renderCommand $ CreateTable productTable
    ignore $ Database.close db
