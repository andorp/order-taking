module Service.NodeJS.Promise

-- Promise abstraction from NodeJS

-- NodeJS using an event driven approach to represent computations. This approach can
-- be formalized/approximated with a Monad abstraction.

-- See: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
-- Adapted from https://raw.githubusercontent.com/idris-community/inigo/master/Inigo/Async/Promise.idr

||| Promise monad
|||
||| Encapsulates the JavaScript abstraction of promise, which consists of a continuation
||| for successful computation and a continuation for errorneous computation. The promise
||| abstraction bounds these two continuations.
export
data Promise : Type -> Type where
  MkPromise : ((a -> IO ()) -> (String -> IO ()) -> IO ()) -> Promise a

-- Promise is a functor, as we can pre-apply the 'f : a -> b' to the 'a' in the first computation.
-- Hint: This is possible as because the 'a' is in a positive position, because it got twice negated.
export
Functor Promise where
  map f (MkPromise cmd) = MkPromise (\succ => \err => cmd (\x => succ (f x)) err)

mutual

  -- The Applicative instance of the Promise uses the succ
  -- continuation with the value injected by the 'pure' function.
  -- For the '<*>' operator it piggybacks on the monad's (>>=) operator.
  export
  Applicative Promise where
    pure x = MkPromise (\succ => \err => succ x)
    x <*> y = x >>= (\f => f <$> y)

  -- Promise is like the continuation monad, we can create a new command
  -- function applying the result of the success computation in the
  -- 'f' continuation which computes another Promise that can be unwrapped.
  export
  Monad Promise where
    (MkPromise cmd) >>= f = MkPromise (\succ =>
                                        \err =>
                                                cmd (\x =>
                                                          let (MkPromise cmd_) = (f x)
                                                          in cmd_ succ err
                                                    ) err
                                      )

||| The Promise monad under the hood relies on the IO monad, for that
||| reason we can use the IO monad, execute the IO computation and
||| evaluate the success computation with the result.
export
HasIO Promise where
  liftIO x = MkPromise (\ok => \err => x >>= ok)

||| Activate the reject branch of the Promise with the given message.
export
reject : String -> Promise a
reject msg = MkPromise (\ok, err => err msg)

||| Resolve a promise compuation.
|||
||| When the final computations for successful branch and errorneous
||| branch is given, the promise computation can be resolved, and if the
||| successful branch finished then its result is passed to the success
||| 'a -> IO ()' computation. If the errorneus branch finishes, the
||| given 'String -> IO ()' evalautes with the result error String.
export
resolve : Promise a -> (a -> IO ()) -> (String -> IO ()) -> IO ()
resolve (MkPromise cmd) = cmd

||| Alternative version of the resolve function with different argument order.
export
resolve' : (a -> IO ()) -> (String -> IO ()) -> Promise a -> IO ()
resolve' ok err (MkPromise cmd) = cmd ok err

||| On Left convert it to String and reject the promise, on Right resolve it.
export
either : Show e => Either e a -> Promise a
either (Left x)  = reject $ show x
either (Right x) = pure x

||| Helper function definition which encapsulates the essence of
||| the Promise type, PrimIO is needed for the FFI parts to
||| be easily wired in.
public export
PromiseShape : Type -> Type
PromiseShape a = (a -> IO ()) -> (String -> IO ()) -> PrimIO ()

||| Turn an FFI promise like construction into a Promise.
|||
||| The name of this function comes from its JavaScript
||| counterpart, which is also called promisify and accepts
||| two closures as arguments.
export
promisify : PromiseShape a -> Promise a
promisify prim =
  MkPromise (\ok, err => primIO $ prim ok err)
