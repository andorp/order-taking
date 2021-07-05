module OrderTaking.Context

import OrderTaking.Domain.PlaceOrder
import OrderTaking.DTO.PlaceOrder
import OrderTaking.BoundedContext.PlaceOrder
import OrderTaking.Domain.Backend

import Rango.BoundedContext.BoundedContext
import Rango.BoundedContext.Workflow
import Service.NodeJS.Promise
import Language.JSON


public export
data BoundedContextCommand
  = PlaceOrderCmd OrderForm

namespace Error

  public export
  data BoundedContextError
    = PlaceOrderErr PlaceOrderError

  public export
  data BoundedContextErrorDTO
    = PlaceOrderErrDTO PlaceOrderErrorDTO

  export
  toDTO : BoundedContextError -> BoundedContextErrorDTO

  export
  toJSON : BoundedContextErrorDTO -> JSON

namespace Event

  public export
  data BoundedContextEvent
    = PlaceOrderEv (List PlacedOrderEvent)

  public export
  data BoundedContextEventDTO
    = PlaceOrderEvDTO (List PlaceOrderEventDTO)

  export
  toDTO : BoundedContextEvent -> BoundedContextEventDTO
  toDTO = ?w2

  export
  toJSON : BoundedContextEventDTO -> JSON
  -- toJSON (PlaceOrderEvDTO xs) = toJSON xs

namespace Bounded

  data OTCmd
    = POCmd

  data OTWf
    = POWf

  data OTEv
    = POEv

  BCe : OTCmd -> OTWf
  BCe POCmd = POWf

  BEe : OTCmd -> OTEv
  BEe POCmd = POEv

  record BCE where
    constructor MkBCE
    cmdType : Type
    evType  : Type
    wfType  : Type
    c2w     : cmdType -> wfType
    c2e     : cmdType -> evType

  data MkMnd : (f : Type -> Type) -> (e : Type) -> (t : Type -> Type) -> Type where
    MkMnd1 : ((a : Type) -> f a -> t (Either e a)) -> MkMnd f e t

  runMkMnd : {a : Type} -> MkMnd f e t -> f a -> t (Either e a)
  runMkMnd (MkMnd1 f) y = f a y


  public export
  record BCM (monad : Type -> Type) where
    constructor MkBCM
    context  : BCE
    eventRep : Type
    cmdRep   : Type
    errRep   : Type
    wfDef    : context.wfType -> WD
    cmdIntp  : context.cmdType -> Type
    evIntp   : context.evType -> Type
    errIntp  : context.wfType -> Type
    wfMnd    : context.wfType -> (Type -> Type)
    wfMndI   : (w : context.wfType) -> Monad (wfMnd w)
    wfMorp   : (cmd : context.cmdType) ->
               let w = context.c2w cmd
                   d = wfDef w
               in Morphism (state d) (wfMnd w) (command d) (branch d)
    cmdMainM : (cmd : context.cmdType) ->
               let w = context.c2w cmd
               in monad (MkMnd (wfMnd w) (errIntp w) monad)
    cmdEvTy  : (cmd : context.cmdType) ->
               let m = wfMorp cmd
               in m.StateType (WD.end (wfDef (context.c2w cmd))) -> monad (evIntp (context.c2e cmd))
    evFinal  : (cmd : context.cmdType) -> evIntp (context.c2e cmd) -> monad eventRep
    cmdInit  : cmdRep -> monad (cmd : context.cmdType ** cmdIntp cmd)
    wfStart  : (cmd : context.cmdType) ->
               cmdIntp cmd -> 
               let m = wfMorp cmd
               in monad (m.StateType (WD.start (wfDef (context.c2w cmd))))
    errConv  : (wf : context.wfType) -> (errIntp wf) -> monad errRep
  
  export
  handle : (Monad m) => (bc : BCM m) -> bc.cmdRep -> m (Either bc.errRep bc.eventRep)
  handle bc cmdRep = do
    (cmd ** cmdData) <- bc.cmdInit cmdRep
    workflowRunner <- bc.cmdMainM cmd
    let wfmndi = bc.wfMndI (bc.context.c2w cmd) -- For applyWD
    wfInp <- bc.wfStart cmd cmdData
    res <- runMkMnd workflowRunner (applyWD (bc.wfDef (bc.context.c2w cmd)) (bc.wfMorp cmd) wfInp)
    case res of
      Left err  => map Left  (bc.errConv (bc.context.c2w cmd) err)
      Right wfVal => do
        evVal <- bc.cmdEvTy cmd wfVal
        map Right (bc.evFinal cmd evVal)

  record Abstract (m : Type -> Type) where
    constructor MkAbstract
    effect   : Int -> m Bool
 
  testBCE : BCE
  testBCE = MkBCE
    { cmdType = OTCmd
    , wfType  = OTWf
    , evType  = OTEv
    , c2w     = BCe
    , c2e     = BEe
    }

  testCmdIntp : OTCmd -> Type
  testCmdIntp POCmd = OrderForm

  testEvIntp : OTEv -> Type
  testEvIntp POEv = List PlacedOrderEvent

  testWFDef : OTWf -> WD
  testWFDef POWf = mkWD PlaceOrder.workflow

  testWfMnd : OTWf -> (Type -> Type)
  testWfMnd POWf = POM

  testWFErr : OTWf -> Type
  testWFErr POWf = PlaceOrderError

  testMkWfRunner : (cmd : OTCmd) -> Promise (MkMnd (testWfMnd (BCe cmd)) (testWFErr (BCe cmd)) Promise)
  testMkWfRunner POCmd = do
    let orderDBComp       = orderDBSQLite
    let productDBComp     = productDBSQlite
    let emailComp         = noEmail
    let checkAddressComp  = okCheckAddress
    rb <- mkRunBackend
    pure $ MkMnd1 (\type, x => map (the (Either PlaceOrderError type)) (runBackend rb (interpret backend x)))

  testWfMorp : (cmd : OTCmd) -> let w = BCe cmd in Morphism (WD.state (testWFDef (w))) (testWfMnd w) (WD.command (testWFDef w)) (WD.branch (testWFDef w))
  testWfMorp POCmd = withPOMMapping
 
  testCmdEvTy : (cmd : OTCmd) -> let m = testWfMorp cmd in m.StateType (WD.end (testWFDef (BCe cmd))) -> Promise (testEvIntp (BEe cmd))
  testCmdEvTy POCmd x = pure x

  testEvFinal : (cmd : OTCmd) -> testEvIntp (BEe cmd) -> Promise BoundedContextEvent
  testEvFinal POCmd x = pure $ PlaceOrderEv x

  testCmdInit : BoundedContextCommand -> Promise (cmd : OTCmd ** testCmdIntp cmd)
  testCmdInit (PlaceOrderCmd x) = pure (POCmd ** x)

  testErrConv : (wf : OTWf) -> testWFErr wf -> Promise BoundedContextError
  testErrConv POWf x = pure (PlaceOrderErr x)

  testWfStart : (cmd : OTCmd) -> testCmdIntp cmd -> Promise ((testWfMorp cmd) .StateType ((testWFDef (BCe cmd)) .start))
  testWfStart POCmd x = pure x

  testWfMndI : (w : OTWf) -> Monad (testWfMnd w)
  testWfMndI POWf = %search

  public export
  testBCM : BCM Promise
  testBCM = MkBCM
    { context  = testBCE
    , eventRep = BoundedContextEvent
    , cmdRep   = BoundedContextCommand
    , errRep   = BoundedContextError
    , wfDef    = testWFDef
    , cmdIntp  = testCmdIntp
    , evIntp   = testEvIntp
    , errIntp  = testWFErr
    , wfMnd    = testWfMnd
    , wfMndI   = testWfMndI
    , wfMorp   = testWfMorp
    , cmdMainM = testMkWfRunner
    , cmdEvTy  = testCmdEvTy
    , evFinal  = testEvFinal
    , cmdInit  = testCmdInit
    , wfStart  = testWfStart
    , errConv  = testErrConv
    }
