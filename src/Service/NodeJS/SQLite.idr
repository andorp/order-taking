module Service.NodeJS.SQLite

import Language.JSON
import Language.JSON.Schema
import Service.NodeJS.JSON
import Service.NodeJS.Promise


||| SQL statement
public export
Sql : Type
Sql = String

namespace Error

  export
  data Error : Type where [external]

  public export
  data SomeError
    = NoError
    | HasError Error

  export
  toMaybe : SomeError -> Maybe Error
  toMaybe NoError      = Nothing
  toMaybe (HasError e) = Just e

  %foreign "node:lambda: e => {return String(e);}"
  ffi_toString : Error -> PrimIO String

  export
  toString : HasIO io => Error -> io String
  toString e = primIO (ffi_toString e)

  %foreign "node:lambda: e => (String(e))"
  ffi_showError : Error -> String

  export
  Show Error where
    show e = ffi_showError e

%foreign "node:lambda: e => {if(e){return 0;}else{return 1;}}"
ffi_isNull : Error -> PrimIO Bool

isNull : Error -> IO Bool
isNull e = primIO (ffi_isNull e)

||| Checks if the error really occured
occured : HasIO io => Error -> io SomeError
occured e = do
  n <- liftIO $ isNull e
  pure $ if n then NoError else HasError e

namespace Database

  public export
  data Database : Type where [external]

  %name Database db

  %foreign "node:lambda: db => (db.close())"
  ffi_close : Database -> PrimIO ()

  export
  close : HasIO io => Database -> io ()
  close db = primIO (ffi_close db)

  %foreign "node:lambda: (db,s,er) => (db.run(s, (e) => (er(e)())))"
  ffi_run : Database -> Sql -> (Error -> PrimIO ()) -> PrimIO ()

  export
  run : Database -> Sql -> Promise SomeError
  run db sql = promisify
    (\ok, err => ffi_run db sql (\e => toPrim $ do
      putStrLn sql -- TODO: Better logging
      mErr <- occured e
      ok mErr))

  export
  data RawResult : Type where [external]

  %foreign "node:lambda: r => {if(r){return 0;}else{return 1;}}"
  ffi_isEmptyResult : RawResult -> PrimIO Bool

  isEmptyRow : HasIO io => RawResult -> io Bool
  isEmptyRow r = primIO (ffi_isEmptyResult r)

  export
  nonEmpty : HasIO io => RawResult -> io (Maybe NodeJS.JSON.JSON)
  nonEmpty r = pure $ if !(isEmptyRow r)
    then Nothing
    else Just $ believe_me r

  %foreign "node:lambda: (db,s,c) => (db.get(s,(e,r) => c(e)(r)()))"
  ffi_get : Database -> String -> (Error -> RawResult -> PrimIO ()) -> PrimIO ()

  rawGet : Database -> Sql -> Promise (Either Error RawResult)
  rawGet db sql = promisify $ \ok, err => ffi_get db sql $ \e, row => toPrim $ do
    putStrLn sql
    mErr <- occured e
    case mErr of
      NoError     => ok $ Right row
      HasError e2 => ok $ Left e2

  namespace NonSafe

    public export
    data Result
      = GetError  Error
      | JSONError String
      | EmptyRow 
      | Row       Data.JSON

    export
    get : Database -> String -> Promise Result
    get db query = do
      res <- rawGet db query
      case res of
        Left err => pure $ GetError err
        Right raw => do
          json <- nonEmpty raw
          case json of
            Nothing => pure EmptyRow
            Just j0 =>
              case !(convert j0) of
                Nothing => pure $ JSONError "FFI JSON conversion"
                Just j2 => pure $ Row j2

  namespace Safe

    public export
    data Result : Schema -> Type where
      GetError  : Error  -> Result s
      JSONError : String -> Result s
      EmptyRow            : Result s
      Row       : JSON s -> Result s

    export
    get : Database -> (s : Schema) -> String -> Promise (Result s)
    get db schema query = do
      res <- rawGet db query
      case res of
        Left err => pure $ GetError err
        Right raw => do
          json <- nonEmpty raw
          case json of
            Nothing => pure EmptyRow
            Just j0 =>
              case !(convert j0) of
                Nothing => pure $ JSONError "FFI JSON conversion"
                Just j2 => case toIndexed schema j2 of
                  Nothing => pure $ JSONError "Non-schema conforming."
                  Just j3 => pure $ Row j3

namespace SQLite

  export
  data SQLite : Type where [external]

  %foreign "node:lambda: u => (require('sqlite3').verbose())"
  ffi_require : () -> PrimIO SQLite

  export
  require : HasIO io => io SQLite
  require = primIO (ffi_require ())

  %foreign "node:lambda: (s,d,err) => (new s.Database(d,(e) => (err(e)())))"
  ffi_database : SQLite -> String -> (Error -> PrimIO ()) -> PrimIO Database

  export
  database : SQLite -> String -> Promise (Either Error Database)
  database s dbStr = promisify (\ok, err => toPrim $ do
    ok $ Right !(primIO $ ffi_database s dbStr (\e => toPrim $ do
      if !(isNull e)
        then pure ()
        else (ok (Left e)))))
