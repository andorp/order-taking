module Service.NodeJS.Promise

-- Adapted from https://raw.githubusercontent.com/idris-community/inigo/master/Inigo/Async/Promise.idr

export
data Promise : Type -> Type where
  MkPromise : ((a -> IO ()) -> (String -> IO ()) -> IO ()) -> Promise a

export
Functor Promise where
  map f (MkPromise cmd) = MkPromise (\succ => \err => cmd (\x => succ (f x)) err)

mutual
  export
  Applicative Promise where
    pure x = MkPromise (\succ => \err => succ x)
    x <*> y = x >>= (\f => f <$> y)

  export
  Monad Promise where
    (MkPromise cmd) >>= f = MkPromise (\succ =>
                                        \err =>
                                                cmd (\x =>
                                                          let (MkPromise cmd_) = (f x)
                                                          in cmd_ succ err
                                                    ) err
                                      )

export
HasIO Promise where
  liftIO x = MkPromise (\ok => \err => x >>= ok)

export
reject : String -> Promise a
reject msg = MkPromise (\ok, err => err msg)

export
resolve : Promise a -> (a -> IO ()) -> (String -> IO ()) -> IO ()
resolve (MkPromise cmd) ok err =
  cmd ok err

export
resolve' : (a -> IO ()) -> (String -> IO ()) -> Promise a -> IO ()
resolve' ok err (MkPromise cmd) =
  cmd ok err

||| On Left convert it to String and reject the promise, on Right resolve it.
export
either : Show e => Either e a -> Promise a
either (Left x)  = reject $ show x
either (Right x) = pure x

export
run : Promise a -> IO ()
run p =
  resolve p (\_ => pure ()) (\err => putStrLn ("Error: " ++ err))

-- I can fold these, but that's a bit of an issue since
-- they will end up running sequentially, which is really
-- not the intent here, but for now...
export
all : List (Promise a) -> Promise (List a)
all promises =
  doAll promises
  where
    doAll : List (Promise a) -> Promise (List a)
    doAll (p :: ps) =
      do
        x <- p
        rest <- doAll ps
        pure (x :: rest)
    doAll [] = pure []

export
lift : a -> Promise a
lift x = MkPromise (\ok => \err => ok x)

export
parallel : Promise a -> Promise a -> Promise a
parallel (MkPromise s1) (MkPromise s2) = MkPromise $ \err => \cb => do
  s1 err cb
  s2 err cb

public export
PromiseShape : Type -> Type
PromiseShape a = (a -> IO ()) -> (String -> IO ()) -> PrimIO ()

export
promisify : PromiseShape a -> Promise a
promisify prim =
  MkPromise (\ok, err => primIO $ prim ok err)

export
boolToInt : Bool -> Int
boolToInt False = 0
boolToInt True = 1
