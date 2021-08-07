module Rango.Example.Workflow

import Rango.BoundedContext.Workflow
import System.Random

-- State is the knowledge we know about the health of the lamp, which
-- is going to be refined in every step.
public export
data State
  = DoesntWork
  | NotPluggedIn
  | PluggedIn
  | BulbBurntOut
  | BulbIsOk
  | Works

public export
data Action : State -> State -> Type where
  PlugInLamp  : Action NotPluggedIn Works
  ReplaceBulb : Action BulbBurntOut Works
  RepairLamp  : Action BulbIsOk     Works

public export
data Check : State -> State -> State -> Type where
  IsPluggedIn : Check DoesntWork PluggedIn NotPluggedIn
  IsBulbOut   : Check PluggedIn  BulbIsOk  BulbBurntOut

public export
fixLamp : Workflow Action Check DoesntWork Works
fixLamp
  = Branch IsPluggedIn
      (Branch IsBulbOut
        (Do RepairLamp)
        (Do ReplaceBulb))
      (Do PlugInLamp)

public export data LightsOff  = MkLightsOff
public export data LigthsOn   = MkLightsOn
public export data BrokenLamp = MkBrokenLamp

public export
lampStateRep : State -> Type
lampStateRep DoesntWork   = LightsOff
lampStateRep NotPluggedIn = LightsOff
lampStateRep PluggedIn    = LigthsOn
lampStateRep BulbBurntOut = LightsOff
lampStateRep BulbIsOk     = BrokenLamp
lampStateRep Works        = LigthsOn

lampAction : Action s e -> (lampStateRep s) -> IO (lampStateRep e)
lampAction PlugInLamp  MkLightsOff  = do { putStrLn "Plug in lamp"; pure MkLightsOn }
lampAction ReplaceBulb MkLightsOff  = do { putStrLn "Replace bulb and turn it on"; pure MkLightsOn }
lampAction RepairLamp  MkBrokenLamp = do { putStrLn "Fix the lamp and turn it on"; pure MkLightsOn }

yesNo : IO Bool
yesNo = map (>0.5) randomIO

lampCheck : Check p t e -> (lampStateRep p) -> IO (Either (lampStateRep t) (lampStateRep e))
lampCheck IsPluggedIn MkLightsOff = do
  putStrLn "Is it plugged in?"
  res <- yesNo
  if res
    then do
      putStrLn "Yes."
      pure (Left MkLightsOn)
    else do
      putStrLn "No."
      pure (Right MkLightsOff)
lampCheck IsBulbOut MkLightsOn = do
  putStrLn "Is the bulb out?"
  res <- yesNo
  if res
    then do
      putStrLn "Yes."
      pure (Right MkLightsOff)
    else do
      putStrLn "No."
      pure (Left MkBrokenLamp)

morphism : Morphism IO State Action Check
morphism = MkMorphism
  { StateType = lampStateRep
  , step      = lampAction
  , check     = lampCheck
  }

lampProgram : LightsOff -> IO LigthsOn
lampProgram = morph morphism fixLamp

export
testLamp : IO ()
testLamp = do { MkLightsOn <- lampProgram MkLightsOff; pure () }
