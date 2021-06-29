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

throwIfFailE : (String -> ProductDBError) -> Promise (Either Error a) -> ProductDB a
throwIfFailE mkError p = do
  Right result <- lift (lift p)
    | Left err => throwError $ mkError !(toString err)
  pure result

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
productExists (MkProductCodeDTO productCode) = do
  db <- ask
  row <- throwIfFailE NonExistingProduct
       $ Database.get db "SELECT code, description, price FROM product WHERE code='\{productCode}';"
  pure $ maybe False (const True) !(toNonEmpty row)

export
productPrice : ProductCodeDTO -> ProductDB Double
productPrice (MkProductCodeDTO productCode) = do
  db <- ask
  row0 <- throwIfFailE NonExistingProduct
        $ Database.get db "SELECT code, description, price FROM product WHERE code='\{productCode}';"
  Just row <- toNonEmpty row0
    | Nothing => throwError $ NonExistingProduct $ show productCode
  fieldDouble row "price"

export
initDB : IO ()
initDB = do
  sqlite <- SQLite.require
  resolve' (\_ => putStrLn "OK.") putStrLn $ do
    db <- either !(SQLite.database sqlite "./db/product.db")
    ignore $ Database.run db $ renderCommand $ CreateTable productTable
    ignore $ Database.run db $ renderCommand $ Insert productTable
      [ FieldOf "code"        (SQLText "g21")
      , FieldOf "description" (SQLText "A fluffy gizmo toy")
      , FieldOf "price"       (SQLDouble 24.00)
      ]
    ignore $ Database.close db
