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

  %foreign "node:lambda: e => {if(e){return BigInt(0);}else{return BigInt(1);}}"
  ffi_isNull : Error -> PrimIO Bool

  isNull : Error -> IO Bool
  isNull e = primIO (ffi_isNull e)

  ||| Checks if the error really occured
  export
  occured : Error -> IO (Maybe Error)
  occured e = do
    n <- isNull e
    pure $ if n then Nothing else Just e

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
  run : HasIO io => Database -> Sql -> (Sql -> Error -> IO ()) -> io ()
  run db sql onErr = do
    primIO (ffi_run db sql (\e => toPrim $ onErr sql e))

  -- TODO: Use SomeError
  export
  runP : Database -> Sql -> Promise (Maybe Error)
  runP db sql = promisify
    (\ok, err => ffi_run db sql (\e => toPrim $ do
      mErr <- occured e
      ok mErr))

  export
  ignoreError : Sql -> Error -> IO ()
  ignoreError _ _ = pure ()

  %foreign "node:lambda: (db,s,p) => (db.run(s,[p]))"
  ffi_runWith1 : Database -> String -> String -> PrimIO ()

  export
  runWith1 : Database -> Sql -> String -> IO ()
  runWith1 db s p = primIO (ffi_runWith1 db s p)

  %foreign "node:lambda: (db,s,c) => (db.get(s,c))"
  ffi_get : Database -> String -> (Error -> Row -> PrimIO ()) -> PrimIO ()

  export
  get : Database -> Sql -> (Error -> Row -> IO ()) -> IO ()
  get db sql callback = primIO (ffi_get db sql (\err,row => toPrim $ callback err row))

  export
  getP : Database -> Sql -> Promise Row
  getP db sql = promisify (\ok, err => ffi_get db sql (\e, row => toPrim $ do
    mErr <- occured e
    case mErr of
      Nothing => ok row
      Just e2 => err !(toString e2)))

  export
  getP2 : Database -> Sql -> Promise (Either Error Row)
  -- getP db sql = promisify (\ok, err => ffi_get db sql (\e, row => toPrim $ do
  --   mErr <- occured e
  --   case mErr of
  --     Nothing => ok row
  --     Just e2 => err !(toString e2)))


  %foreign "node:lambda: (db,s,row,comp) => (db.each(s, (e,r) => (row(e)(r)()), (e,c) => (comp(e)()) ))"
  ffi_each
    : Database -> String -> (Error -> Row -> PrimIO ()) -> (Error -> PrimIO ()) -> PrimIO ()

  export
  each : Database -> Sql -> (Error -> Row -> IO ()) -> (Error -> IO ()) -> IO ()
  each db s onRow onComplete
    = primIO
    $ ffi_each db s (\e,r => toPrim $ onRow e r)
                    (\e   => toPrim $ onComplete e)

namespace SQLite

  export
  data SQLite : Type where [external]

  %foreign "node:lambda: u => (require('sqlite3').verbose())"
  ffi_require : () -> PrimIO SQLite

  export
  require : HasIO io => io SQLite
  require = primIO (ffi_require ())

  %foreign "node:lambda: (s,d) => (new s.Database(d,(e) => {if(e){throw e}}))"
  ffi_database : SQLite -> String -> PrimIO Database

  export
  database : HasIO io => SQLite -> String -> io Database
  database s db = primIO (ffi_database s db)
