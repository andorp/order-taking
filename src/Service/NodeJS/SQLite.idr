module Service.NodeJS.SQLite

import Language.JSON
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

%foreign "node:lambda: e => {if(e){return BigInt(0);}else{return BigInt(1);}}"
ffi_isNull : Error -> PrimIO Bool

isNull : Error -> IO Bool
isNull e = primIO (ffi_isNull e)

||| Checks if the error really occured
occured : Error -> IO SomeError
occured e = do
  n <- isNull e
  pure $ if n then NoError else HasError e

namespace Row

  export
  data Row : Type where [external]

  %foreign "node:lambda: r => {if(r){return BigInt(0);}else{return BigInt(1);}}"
  ffi_isNull : Row -> PrimIO Bool

  isNull : Row -> IO Bool
  isNull r = primIO (ffi_isNull r)

  export
  nonEmpty : Row -> IO (Maybe Row)
  nonEmpty r = do
    u <- isNull r
    pure $ if u then Nothing else Just r

  %foreign "node:lambda: r => {return JSON.stringify(r);}"
  ffi_toString : Row -> PrimIO String

  export
  toString : Row -> IO String
  toString r = primIO (ffi_toString r)

  export
  json : Row -> IO (Maybe JSON)
  json r = do
    jsonStr <- toString r
    -- This is slow, we need a better JSON API
    pure $ JSON.parse jsonStr

namespace Database

  public export
  data Database : Type where [external]

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
      mErr <- occured e
      ok mErr))

  -- TODO: Bring it back when needed.
  -- %foreign "node:lambda: (db,s,p) => (db.run(s,[p]))"
  -- ffi_runWith1 : Database -> String -> String -> PrimIO ()

  -- export
  -- runWith1 : Database -> Sql -> String -> IO ()
  -- runWith1 db s p = primIO (ffi_runWith1 db s p)

  %foreign "node:lambda: (db,s,c) => (db.get(s,c))"
  ffi_get : Database -> String -> (Error -> Row -> PrimIO ()) -> PrimIO ()

  export
  get : Database -> Sql -> Promise (Either Error Row)
  get db sql = promisify $ \ok, err => ffi_get db sql $ \e, row => toPrim $ do
    mErr <- occured e
    case mErr of
      NoError     => ok $ Right row
      HasError e2 => ok $ Left e2

  -- TODO: Uncomment when we need this.
  -- %foreign "node:lambda: (db,s,row,comp) => (db.each(s, (e,r) => (row(e)(r)()), (e,c) => (comp(e)()) ))"
  -- ffi_each
  --   : Database -> String -> (Error -> Row -> PrimIO ()) -> (Error -> PrimIO ()) -> PrimIO ()

  -- export
  -- each : Database -> Sql -> (Error -> Row -> IO ()) -> (Error -> IO ()) -> IO ()
  -- each db s onRow onComplete
  --   = primIO
  --   $ ffi_each db s (\e,r => toPrim $ onRow e r)
  --                   (\e   => toPrim $ onComplete e)

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
    ok $ Right !(primIO $ ffi_database s dbStr (\e => toPrim (ok (Left e)))))

