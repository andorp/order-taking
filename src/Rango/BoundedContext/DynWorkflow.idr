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
record Morphism
        state
        (0 m : Type -> Type)
        (0 cmd : state -> state -> Type)
        (0 obs : state -> state -> Type -> Type)
        (0 chk : state -> state -> state -> Type)
  where
    constructor MkRunner
    StateType
      : state -> Type
    command
      : {0 s,e : state} -> cmd s e -> (StateType s) -> m (StateType e)
    observe
      : {0 t : Type} -> {0 s,e : state} -> obs s e t -> (StateType s) -> m (t, StateType e)
    check
      :  {0 s,b1,b2 : state}
      -> chk s b1 b2
      -> (StateType s) -> m (Either (StateType b1) (StateType b2))

export
morph
  :  Functor m
  => Applicative m
  => Monad m
  => (r : Morphism state m cmd obs chk)
  -> DynWorkflow t cmd obs chk start end
  -> (StateType r start)
  -> m (x : t ** StateType r (end x))
morph r (Pure x) i = pure (x ** i)
morph r (Do cmd) i = do
  o <- command r cmd i
  pure (() ** o)
morph r (Observe obs) i = do
  (x, o) <- observe r obs i
  pure (x ** o)
morph r (Branch h b1 b2) i = do
  x <- check r h i
  case x of
    Left y => morph r b1 y
    Right y => morph r b2 y
morph r (m >>= k) i = do
  (x ** md) <- morph r m i
  morph r (k x) md
