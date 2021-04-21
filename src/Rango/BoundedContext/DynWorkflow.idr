module Rango.BoundedContext.DynWorkflow

public export
data DynWorkflow
      :  (0 t : Type)
      -> (0 cmd : state -> state -> Type)
      -> (0 obs : state -> state -> Type -> Type)
      -> (0 chk : state -> state -> state -> Type)
      -> state
      -> (t -> state)
      -> Type
  where
    Pure
      :  (r : t) -> DynWorkflow t cmd obs chk (c r) c
    Do
      :  {0 cmd : state -> state -> Type}
      -> {1 pre, post : state}
      -> cmd pre post
      -> DynWorkflow () cmd obs chk pre (const post)
    Observe
      :  {0 obs : state -> state -> Type -> Type}
      -> {1 pre, post : state} -> {t : Type}
      -> obs pre post t
      -> DynWorkflow t cmd obs chk pre (const post)
    Branch
      :  {0 chk : state -> state -> state -> Type}
      -> {1 pre, branch1, branch2, post : state}
      -> chk pre branch1 branch2
      -> DynWorkflow () cmd obs chk branch1 (const post)
      -> DynWorkflow () cmd obs chk branch2 (const post)
      -> DynWorkflow () cmd obs chk pre (const post)
    (>>=)
      :  {0 a,b : Type} -> {0 pre : state}
      -> {1 postFn1 : a -> state} -> {0 postFn2 : b -> state}
      -> DynWorkflow a cmd obs chk pre postFn1
      -> ((r : a) -> (DynWorkflow b cmd obs chk (postFn1 r) postFn2))
      -> DynWorkflow b cmd obs chk pre postFn2

export
(>>) :  {0 b : Type} -> {0 pre : state} -> {0 postFn2 : b -> state} -> {1 postFn1 : () -> state}
     -> DynWorkflow () cmd obs chk pre postFn1
     -> DynWorkflow b  cmd obs chk (postFn1 ()) postFn2
     -> DynWorkflow b  cmd obs chk pre postFn2
(>>) m k = m >>= (\() => k)

public export
record Interpreter
        state
        (0 m : Type -> Type)
        (0 cmd : state -> state -> Type)
        (0 obs : state -> state -> Type -> Type)
        (0 chk : state -> state -> state -> Type)
  where
    constructor MkRunner
    stateType
      : state -> Type
    runCommand
      : {0 s,e : state} -> cmd s e -> (stateType s) -> m (stateType e)
    runObserve
      : {0 t : Type} -> {0 s,e : state} -> obs s e t -> (stateType s) -> m (t, stateType e)
    runCheck
      :  {0 s,b1,b2 : state}
      -> chk s b1 b2
      -> (stateType s) -> m (Either (stateType b1) (stateType b2))

export
run
  :  Functor m
  => Applicative m
  => Monad m
  => (r : Interpreter state m cmd obs chk)
  -> DynWorkflow t cmd obs chk start end
  -> (stateType r start)
  -> m (x : t ** stateType r (end x))
run r (Pure x) i = pure (x ** i)
run r (Do cmd) i = do
  o <- runCommand r cmd i
  pure (() ** o)
run r (Observe obs) i = do
  (x, o) <- runObserve r obs i
  pure (x ** o)
run r (Branch h b1 b2) i = do
  x <- runCheck r h i
  case x of
    Left y => run r b1 y
    Right y => run r b2 y
run r (m >>= k) i = do
  (x ** md) <- run r m i
  run r (k x) md
