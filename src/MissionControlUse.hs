{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module MissionControlUse where

------------------------------------------------------------------------------
import           GaugeUse
import           MissionControl
------------------------------------------------------------------------------
import qualified Prelude
import           Protolude
------------------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce" :: Prelude.String) #-}
------------------------------------------------------------------------------

countdown :: MissionControl -> Text
countdown mc = case doRocket mc of
  Left  e -> show e
  Right _ -> "Rocket Launched!"

doRocket :: MissionControl -> ERE (MissionControl, Path 'NoProblems 'Launched)
doRocket mc1 = do
  (mc2, p2) <- begin        mc1
  (mc3, p3) <- startFueling mc2       p2
  (mc4, p4) <- addFuel      mc3 [1.0] p3
  --_mc       <- reportProblem mc4 "GAUGES BROKE"
  (mc5, p5) <- addMoreFuel' mc4 [1.0] [2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0] p4
  (mc6, p6) <- okToLaunch   mc5       p5
  launch                   mc6       p6
 where
  addMoreFuel' mc fuelSeen newFuel p = do
    let fuelSeen' = Prelude.head newFuel : fuelSeen
        newFuel'  = Prelude.tail newFuel
    case chkDecrAndMax fuelSeen' of
      Left e -> case reportProblem mc (show e) of
                  Left   e0 -> Left e0
                  Right _mc -> panic "impossible"
      Right rs' -> do
        (mc', ep) <- addMoreFuel mc rs' p
        case ep of
          Left  p' -> addMoreFuel' mc' fuelSeen' newFuel' p'
          Right p' -> Right (mc', p')

{-
countdown mkMissionControl
countdown (mkMissionControl' (RInfo False 10 False False))
-------------------------
mc1 = mkMissionControl
mc1 = mkMissionControl' (RInfo False 10 False False)
Right (mc2 ,       p2)  = begin        mc1
Right (mc3 ,       p3)  = startFueling mc2             p2
Right (mc4 ,       p4)  = addFuel      mc3       [5.0] p3
Right (mc5 , Right p5)  = addMoreFuel  mc4 [10.0, 5.0] p4
Right (mc6 ,       p6)  = okToLaunch   mc5             p5
Right (mc7 ,       p7)  = launch       mc6             p6
import Data.Text
:set -XOverloadedStrings
reportProblem mc7 ("TOO LATE" :: Text)

------------------------------------------------------------------------------
useful papers/articles on using Existentials

Existential Quantification Patterns and Antipatterns
Jonathan Fischoff : May 2017
https://medium.com/@jonathangfischoff/existential-quantification-patterns-and-antipatterns-3b7b683b7d71
- HIDE AN ITERMEDIATE VALUE
  - example : Fold type from https://hackage.haskell.org/package/foldl
- HIDDEN TYPES WHICH CAN UNHIDDEN
  - example : OOP RocketInfo
- TYPE ALIGNED DATA STRUCTURES
  - example : Path RocketState; he gives paths between TCP states

Data.TASequence : heterogeneous sequences where the types enforce the element order
https://hackage.haskell.org/package/type-aligned-0.9.6/docs/Data-TASequence.html
-}
