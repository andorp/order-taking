module BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.Product

import public Control.Monad.Either

import Data.Maybe
import Data.String
import Control.Monad.Trans
import Control.Monad.Reader
import Language.JSON.Schema

import Rango.DataTransfer.SQL.Syntax
import Rango.Database.SQLite
import Service.NodeJS.SQLite
import Service.NodeJS.Promise
import Service.NodeJS.JSON

import BoundedContext.OrderTaking.Workflow.PlaceOrder.Database.DTO


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
  | ProductRetrieveError String
  | ConversionError String

export
Show ProductDBError where
  show (InitializeError       e) = "InitializeError: " ++ show e
  show (SaveProductError      e) = "SaveProductError: " ++ show e
  show (ProductRetrieveError  e) = "ProductRetrieveError: " ++ show e
  show (ConversionError       e) = "ConversionError: " ++ show e 

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

throwOnError : (String -> ProductDBError) -> Promise (Database.Safe.Result s) ->  ProductDB (Maybe (Indexed.JSON s))
throwOnError mkError p = do
  Row json <- lift (lift p)
    | EmptyRow      => pure Nothing
    | GetError  err => throwError $ mkError !(toString err)
    | JSONError err => throwError $ mkError err
  pure (Just json)

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
  db  <- ask
  res <- throwOnError ProductRetrieveError
       $ Rango.Database.SQLite.query db
       $ Select ["code", "description", "price"] productTable [("code", "=", "'\{productCode}'")]
  pure $ isJust res

export
productPrice : ProductCodeDTO -> ProductDB Double
productPrice (MkProductCodeDTO productCode) = do
  db <- ask
  Just json  <- throwOnError ProductRetrieveError
              $ Rango.Database.SQLite.query db
              $ Select ["code", "description", "price"] productTable [("code", "=", "'\{productCode}'")]
    | Nothing => throwError $ ProductRetrieveError $ show productCode
  let Just (Number ** JNumber d) = getField json "price"
        | Just  _ => throwError $ ProductRetrieveError "price field should have number type."
        | Nothing => throwError $ ProductRetrieveError "price field was not found in DB."
  pure d

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
