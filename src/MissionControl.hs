{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoImplicitPrelude         #-}

module MissionControl where

------------------------------------------------------------------------------
import           Data.Kind
import qualified Prelude
import           Protolude
------------------------------------------------------------------------------
{-# ANN module ("HLint: ignore Redundant bracket" :: Prelude.String) #-}
------------------------------------------------------------------------------

data RocketState
  = InHanger
  | MovingToLaunchPad
  | OnLaunchPad
  | BeingFueled
  | FuelFull
  | NoProblems
  | Launched
  deriving (Eq, Show)

-- TYPE-ALIGNED SEQUENCE : types enforce order
data Path :: RocketState -> RocketState -> Type where
  Begin      :: Path 'InHanger          'MovingToLaunchPad
  AtPad      :: Path 'MovingToLaunchPad 'OnLaunchPad
  Fueling    :: Path 'OnLaunchPad       'BeingFueled
  FuelingMore:: Path 'BeingFueled       'BeingFueled
  Fueled     :: Path 'BeingFueled       'FuelFull
  OkToLaunch :: Path 'FuelFull          'NoProblems
  Launch     :: Path 'NoProblems        'Launched
  (:::)      :: Path a b -> Path b c -> Path a c    -- 'b' is existential

------------------------------------------------------------------------------

data MCException = AlreadyBegun | AlreadyFueling | NotReady | Scrub Text deriving Show

type ERE a            = Either MCException a

data MissionControl = forall ri. MC (MC_ ri) -- 'ri' is existential
instance Show MissionControl where show _ = "MissionControl"

begin          ::      MissionControl
               -> ERE (MissionControl,               Path  'InHanger     'OnLaunchPad)
--------------------------------------------------------------------------------------
startFueling   ::      MissionControl ->             Path  'InHanger     'OnLaunchPad
               -> ERE (MissionControl,               Path  'OnLaunchPad  'BeingFueled)
--------------------------------------------------------------------------------------
addFuel        ::      MissionControl -> [Double] -> Path  'OnLaunchPad  'BeingFueled
               -> ERE (MissionControl,               Path  'BeingFueled  'BeingFueled)
--------------------------------------------------------------------------------------
addMoreFuel    ::      MissionControl -> [Double] -> Path  'BeingFueled  'BeingFueled
               -> ERE (MissionControl,       Either (Path  'BeingFueled  'BeingFueled)
                                                    (Path  'BeingFueled  'FuelFull))
--------------------------------------------------------------------------------------
okToLaunch     ::      MissionControl ->             Path  'BeingFueled  'FuelFull
               -> ERE (MissionControl,               Path  'FuelFull     'NoProblems)
--------------------------------------------------------------------------------------
launch         ::      MissionControl ->             Path  'FuelFull     'NoProblems
               -> ERE (MissionControl,               Path  'NoProblems   'Launched)
--------------------------------------------------------------------------------------
reportProblem  ::      MissionControl -> Text
               -> ERE  MissionControl
--------------------------------------------------------------------------------------
begin         (MC r)      = (rBegin         r) (rRi r)      >>= uncurry (ret r)
startFueling  (MC r)    t = (rStartFueling  r) (rRi r)    t >>= uncurry (ret r)
addFuel       (MC r) rs t = (rAddFuel       r) (rRi r) rs t >>= uncurry (ret r)
addMoreFuel   (MC r) rs t = (rAddMoreFuel   r) (rRi r) rs t >>= uncurry (ret r)
okToLaunch    (MC r)    t = (rOkToLaunch    r) (rRi r)    t >>= uncurry (ret r)
launch        (MC r)    t = (rLaunch        r) (rRi r)    t >>= uncurry (ret r)
reportProblem (MC r) p    = (rReportProblem r) (rRi r) p    >>= pure . updateRi r

ret :: MC_ ri -> ri -> a -> Either MCException (MissionControl, a)
ret r ri a = pure (updateRi r ri, a)

updateRi :: MC_ ri -> ri -> MissionControl
updateRi r ri = MC (r { rRi = ri})

data MC_ ri = MC_
  { rRi            ::      ri
  , rBegin         ::      ri
                   -> ERE (ri,               Path  'InHanger     'OnLaunchPad)
  , rStartFueling  ::      ri ->             Path  'InHanger     'OnLaunchPad
                   -> ERE (ri,               Path  'OnLaunchPad  'BeingFueled)
  , rAddFuel       ::      ri -> [Double] -> Path  'OnLaunchPad  'BeingFueled
                   -> ERE (ri,               Path  'BeingFueled  'BeingFueled)
  , rAddMoreFuel   ::      ri -> [Double] -> Path  'BeingFueled  'BeingFueled
                   -> ERE (ri,       Either (Path  'BeingFueled  'BeingFueled)
                                            (Path  'BeingFueled  'FuelFull))
  , rOkToLaunch    ::      ri ->             Path  'BeingFueled  'FuelFull
                   -> ERE (ri,               Path  'FuelFull     'NoProblems)
  , rLaunch        ::      ri ->             Path  'FuelFull     'NoProblems
                   -> ERE (ri,               Path  'NoProblems   'Launched)
  , rReportProblem ::      ri -> Text
                   -> ERE  ri }

------------------------------------------------------------------------------
-- specific kind of MissionControl

data RInfo = RInfo
  { rAtPad     :: Bool
  , rFuelLevel :: Double
  , rProblems  :: Bool
  , rLaunched  :: Bool
  } deriving Show

mkMissionControl :: MissionControl
mkMissionControl  = mkMissionControl' (RInfo False 0 False False)

mkMissionControl' :: RInfo -> MissionControl
mkMissionControl' ri0 = MC $
  MC_
  ri0
  -- begin
  (\ri ->
     if | rAtPad ri             -> Left AlreadyBegun
        | otherwise             -> Right (ri { rAtPad = True}, Begin ::: AtPad))
  -- startFueling
  (\ri _ ->
     if | rFuelLevel ri /= 0.0  -> Left AlreadyFueling
        | rAtPad ri             -> Right (ri, Fueling)
        | otherwise             -> Left NotReady)
  -- addFuel
  (\ri readings _ ->
     Right (ri { rFuelLevel = maximum readings}, FuelingMore))
  -- addMoreFuel
  (\ri readings _ ->
     let m   = maximum readings
         ri' = ri { rFuelLevel = m} in
     if | m == 10.0             -> Right (ri', Right Fueled)
        | otherwise             -> Right (ri', Left  FuelingMore))
  -- okToLaunch
  (\ri _ ->
     if | rFuelLevel ri /= 10.0 -> Left NotReady
        | rProblems  ri         -> Left NotReady
        | otherwise             -> Right (ri, OkToLaunch))
  -- launch
  (\ri _                        -> Right (ri { rLaunched = True}, Launch))
  -- reportProblem
  (\_ri problem ->
     Left (Scrub problem))
