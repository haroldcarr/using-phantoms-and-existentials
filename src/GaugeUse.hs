{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module GaugeUse where

------------------------------------------------------------------------------
import           Gauge
------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Typeable
import qualified Prelude
import           Protolude          hiding (fromRight)
import           Refined            hiding (NonEmpty)
import           Test.Hspec
------------------------------------------------------------------------------
{-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}
------------------------------------------------------------------------------

{-
-- ===========================================================================
OVERVIEW
             IO                |        PURE
                               |
gauge1 ---+                    |
          |                    |
gauge2 ---+              (existential)
          + --> readings ---->OOP--- inputs
gauge3 ---+     (phantom)      | <-- outputs (TA seq/existential)
          |                    |     + state (existential)
gauge4 ---+                    |
                               |

-- ===========================================================================
GAUGES

             IO

gauge1 --->

------------------------------------------------------------------------------
WORKING WITH GAUGES YOU DO NOT OWN

import System.Random

stdgen <- newStdGen

                    start     max-increment   max-value (e.g., full)
                        v     v               v
g <- Gauge.new  stdgen  0.0   1.0             10.0 :: IO (Gauge Double)
readGauge g


stdgen = mkStdGen 137
g <- Gauge.new  stdgen  0.0   1.0             10.0 :: IO (Gauge Double)
readGauge g

------------------------------------------------------------------------------
CHECKING GAUGE READINGS : WORKING WITH CHECKERS YOU DO NOT OWN
-}

-- | Expects input to be in DESCREASING order.
--   Checks that the difference between any consecutive elements does not exceed 1.0
chkMaxFlow :: [Double] -> Either GaugeException [Double]
chkMaxFlow  = checkMaxFlow 1.0

chkMF1 :: Spec
chkMF1  = describe "check max flow with CORRECT input" $ do
  it "ok"  $ chkMaxFlow [3.3, 2.9, 2.0] `shouldBe` Right [3.3, 2.9, 2.0]
  it "not" $ chkMaxFlow [3.3, 2.9, 1.8] `shouldBe` Left ExceedsMaxFlow

{-
import Test.Hspec
hspec chkMF1
-}

chkMF2 :: Spec
chkMF2  = describe "check max flow with INCORRECT input" $ do
  it "false ok1" $ chkMaxFlow [0.0, -0.5, 1.0] `shouldBe` Right [0.0, -0.5, 1.0] -- WRONG
  it "elemDiff1" $ elemDiff   [0.0, -0.5, 1.0] `shouldBe`       [   0.5, -1.5]
  it "lucky ok1" $ chkMaxFlow [0.0, -0.5     ] `shouldBe` Right [0.0, -0.5     ] -- OK, BUT ...
  it "elemDiff2" $ elemDiff   [0.0, -0.5     ] `shouldBe`       [   0.5]
  it "false ok2" $ chkMaxFlow [     -1.5, 1.0] `shouldBe` Right [     -1.5, 1.0] -- WRONG
  it "elemDiff3" $ elemDiff   [     -1.5, 1.0] `shouldBe`       [        -2.5  ]

{-
hspec chkMF2
-}

-- | Checks that input list is in DESCREASING order.
chkDecr :: [Double] -> Either GaugeException [Double]
chkDecr  = checkDecr 0.0

chkD :: Spec
chkD  = describe "check decreasing" $ do
  it "ok"  $ chkDecr [3.3, 2.9, 2.0] `shouldBe` Right [3.3, 2.9, 2.0]
  it "ok"  $ chkDecr [3.3, 2.9, 1.8] `shouldBe` Right [3.3, 2.9, 1.8]
  it "not" $ chkDecr [2.8, 2.9, 1.8] `shouldBe` Left NotDecr
  it "not" $ chkDecr [3.3, 1.7, 1.8] `shouldBe` Left NotDecr

{-
hspec chkD
-}

chkDecrAndMax :: [Double] -> Either GaugeException [Double]
chkDecrAndMax i = chkDecr i >>= chkMaxFlow

chkDMF :: Spec
chkDMF  = describe "check decreasing and max flow" $ do
  it "1" $ chkDecrAndMax [3.3,  2.9, 2.0] `shouldBe` Right [3.3, 2.9, 2.0]
  it "2" $ chkDecrAndMax [3.3,  2.9, 1.8] `shouldBe` Left ExceedsMaxFlow
  it "3" $ chkDecrAndMax [0.0, -0.5, 1.0] `shouldBe` Left NotDecr
  it "4" $ chkDecrAndMax [0.0, -0.5     ] `shouldBe` Right [0.0, -0.5    ] -- TODO below zero
  it "5" $ chkDecrAndMax [     -1.5, 1.0] `shouldBe` Left NotDecr

{-
hspec chkDMF

-- ===========================================================================
HOOKING UP GAUGES TO CHECKS (what can go wrong?)

             IO

gauge1 ---> check

-}

type CHK = [Double] -> Either GaugeException [Double]

l1 :: [Double] -> CHK -> Gauge Double -> IO Double
l1  = fix $ \loop rs chk g -> do
  r <- readGauge g
  print r
  case chk (r:rs) of -- CHECK
    Left  e   -> Prelude.error (show e)
    Right rs' ->
      if r >= 10.0
        then pure r
        else loop rs' chk g

l1g, l1ge :: IO Double

-- descreasing list at correct rate
l1g = Gauge.new137 0.0 1.0 10.0 >>=
      l1 [] chkMaxFlow

-- detects increases faster than possible
--                      v
l1ge = Gauge.new137 0.0 2.0 10.0 >>=
       l1 [] chkMaxFlow

------------------------------------------------------------------------------
-- Undetected non-decreasing

l2 :: [Double] -> CHK -> Gauge Double -> IO Double
l2  = fix $ \loop rs chk g -> do
  r <- (\r -> if r > 3.0 then -r else r) <$> readGauge g -- FAULTY GAUGE
  print r
  case chk (r:rs) of -- CHECK
    Left   e  -> Prelude.error (show e)
    Right rs' ->
      if r >= 10.0
        then pure r
        else loop rs' chk g

-- does NOT detect non-decreasing (causing infinite loop)
l2b :: IO Double
l2b  = Gauge.new137 0.0 1.0 10.0 >>=
       l2 [] chkMaxFlow -- only check max flow

l2ge :: IO Double
l2ge  = Gauge.new137 0.0 1.0 10.0 >>=
        l2 [] (chkDecr >=> chkMaxFlow) -- check decreasing then max flow

{-
-- ===========================================================================
PASSING CHECKED DATA DOWNSTREAM

             IO                |        PURE
                               |
gauge1 -------> checked  ------|----> inputs
                               |
-}

-- DownStream (DS) is a function that takes List input
type DSL = [Double] -> Bool

dsl :: DSL
dsl  = (>= 10) . Prelude.head -- downstream : expects max at head of list

-------------------------

ldsl :: [Double] -> CHK -> DSL -> Gauge Double -> IO Double
ldsl  = fix $ \loop rs chk ds g -> do
  r <- readGauge g
  print r
  case chk (r:rs) of -- CHECK
    Left   e  -> Prelude.error (show e)
    Right rs0 -> do
      let rs' = if length rs0 > 3 then [] else rs0 -- data mucked with AFTER checking
      if ds rs' -- downstream call
        then pure r
        else loop rs' chk ds g

-- downstream needs to check again (or ELSE!)
ldslb :: IO Double
ldslb  = Gauge.new137 0.0 1.0 10.0 >>=
         ldsl [] (chkDecr >=> chkMaxFlow)
                 dsl

------------------------------------------------------------------------------
-- make data correct by construction : - make invalid states unrepresentable

-- import qualified Data.List.NonEmpty

type CHKNE = NonEmpty Double -> Either GaugeException (NonEmpty Double)

chkMaxFlowNE :: CHKNE
chkMaxFlowNE x = checkMaxFlow 1.0 (NE.toList x) >>= pure . NE.fromList

chkDecrNE :: CHKNE
chkDecrNE x = checkDecr 0.0 (NE.toList x) >>= pure . NE.fromList

-- DownStream is a function that takes NonEmpty (list) input
type DSNE  = NonEmpty Double -> Bool

dsne :: DSNE
dsne  = (>= 10) . NE.head -- downstream : expects max at head of list

-------------------------

ldsne :: NonEmpty Double -> CHKNE -> DSNE -> Gauge Double -> IO Double
ldsne  = fix $ \loop rs chk ds g -> do
  r <- readGauge g
  print r
  case chk (NE.cons r rs) of -- CHECK
    Left   e  -> panic (show e)
    Right rs' -> --do
      --let rsX = if NE.length rs' < 3 then rs' else [] -- caught at compile time
      if ds rs' -- downstream call
        then pure r
        else loop rs' chk ds g

ldsneg :: IO Double
ldsneg  = Gauge.new137 0.0 1.0 10.0 >>=
          ldsne (NE.fromList [0.0])
                (chkDecrNE >=> chkMaxFlowNE)
                dsne

------------------------------------------------------------------------------
-- but still have to check already checked data (or ELSE!)

-- downstream given wrong data
-- exception not seen until next time through loop
-- difficult to debug

ldsneb :: NonEmpty Double -> CHKNE -> DSNE -> Gauge Double -> IO Double
ldsneb  = fix $ \loop rs chk ds g -> do
  r <- readGauge g
  print r
  case chk (NE.cons r rs) of -- CHECK
    Left   e  -> Prelude.error (show e)
    Right rs0 -> do
      let rs' = if NE.length rs0 < 3 then rs0 else NE.reverse rs0 -- mucked after check
      if ds rs' -- downstream call
        then pure r
        else loop rs' chk ds g

ldsnebb :: IO Double
ldsnebb  = Gauge.new137 0.0 1.0 10.0 >>=
           ldsneb (NE.fromList [0.0])
                  (chkDecrNE >=> chkMaxFlowNE)
                  dsne -- downstream : expects max at head of list

------------------------------------------------------------------------------

newtype DecrValuesNT = DecrValuesNT (NonEmpty Double)
newtype FlowOkNT     = FlowOkNT     (NonEmpty Double)

chkDecrNT :: NonEmpty Double -> Either GaugeException DecrValuesNT
chkDecrNT                  xs  = chkDecrNE    xs >>= pure . DecrValuesNT

chkMaxFlowNT :: DecrValuesNT -> Either GaugeException FlowOkNT
chkMaxFlowNT (DecrValuesNT xs) = chkMaxFlowNE xs >>= pure . FlowOkNT

type NTCONS = Double -> FlowOkNT -> Either GaugeException FlowOkNT

ntCons :: NTCONS
ntCons r (FlowOkNT rs) = chkDecrNT (r `NE.cons` rs) >>= chkMaxFlowNT

-- DownStream is a function that takes a FlowOkNT
type DSNT  = FlowOkNT -> Bool

dsnt :: DSNT
dsnt (FlowOkNT xs) = NE.head xs >= 10 -- downstream : expects max at head of list

-------------------------

lNT :: FlowOkNT -> NTCONS -> DSNT -> Gauge Double -> IO Double
lNT  = fix $ \loop rs cons ds g -> do
  r <- readGauge g
  print r
  case cons r rs of -- check
    Left   e  -> panic (show e)
    Right rs' ->
      if ds rs' -- downstream call
        then pure r
        else loop rs' cons ds g

lNTg :: IO Double
lNTg  = Gauge.new137 0.0 1.0 10.0 >>=       -- TODO detect non-decreasing example
        lNT (FlowOkNT (NE.fromList [0.0]))
        ntCons
        dsnt

------------------------------------------------------------------------------
-- same thing with PHANTOM TYPES (finally!) : - carry information at the type level

-- use type system as a mechanism for enforcing program invariants at compile time

data DecrValues
data FlowOk
newtype GaugeReadings p = GaugeReadings (NonEmpty Double)

chkDecrPT :: NonEmpty Double -> Either GaugeException (GaugeReadings DecrValues)
chkDecrPT                   xs  = chkDecrNE    xs >>= pure . GaugeReadings

chkMaxFlowPT
  :: GaugeReadings DecrValues -> Either GaugeException (GaugeReadings FlowOk)
chkMaxFlowPT (GaugeReadings xs) = chkMaxFlowNE xs >>= pure . GaugeReadings

type PTCONS = Double -> GaugeReadings FlowOk -> Either GaugeException (GaugeReadings FlowOk)

ptCons :: PTCONS
ptCons r (GaugeReadings rs) = chkDecrPT (r `NE.cons` rs) >>= chkMaxFlowPT

-- DownStream is a function that takes a GaugeReadings
type DSPT  = GaugeReadings FlowOk -> Bool

dspt :: DSPT
dspt (GaugeReadings xs) = NE.head xs >= 10 -- downstream : expects max at head of list

-------------------------

lPT :: GaugeReadings FlowOk -> PTCONS -> DSPT -> Gauge Double -> IO Double
lPT  = fix $ \loop rs cons ds g -> do
  r <- readGauge g
  print r
  case cons r rs of -- check
    Left   e  -> panic (show e)
    Right rs' ->
      if ds rs' -- downstream call
        then pure r
        else loop rs' cons ds g

lPTg :: IO Double
lPTg  = Gauge.new137 0.0 1.0 10.0 >>=       -- TODO detect non-decreasing example
        lPT (GaugeReadings (NE.fromList [0.0]))
        ptCons
        dspt

------------------------------------------------------------------------------
-- REFINEMENT TYPES via Phantom types using 'Refined' library

-- import Refined hiding (NonEmpty (..))

type DecrValuesFlowOk = Refined (And DecrValues FlowOk) (NonEmpty Double)

instance Predicate DecrValuesFlowOk (NonEmpty Double) where
  validate p v = case chkDecrNE v of
    Left  e  -> throwRefineSomeException (typeOf p) (toException e)
    Right v' -> case chkMaxFlowNE v' of
      Left  e -> throwRefineSomeException (typeOf p) (toException e)
      Right _ -> pure ()

psCons
  :: Double
  ->                         Refined DecrValuesFlowOk (NonEmpty Double)
  -> Either RefineException (Refined DecrValuesFlowOk (NonEmpty Double))
psCons r rs = refine (r `NE.cons` unrefine rs)

type DSR = Refined DecrValuesFlowOk (NonEmpty Double) -> Bool

dsr :: DSR
dsr  = (>= 10) . NE.head . unrefine -- downstream : expects max at head of list

-------------------------

lRT :: Refined DecrValuesFlowOk (NonEmpty Double)
    -> DSR
    -> Gauge Double
    -> IO Double
lRT  = fix $ \loop rs ds g -> do
  r <- readGauge g
  print r
  case psCons r rs of -- check
    Left  e  -> panic (show e)
    Right rs' ->
      if ds rs' -- downstream call
        then pure r
        else loop rs' ds g

lRTg :: IO Double
lRTg  = Gauge.new137 0.0 1.0 10.0 >>=  -- TODO detect non-decreasing example
        lRT (fromRight (refine (NE.fromList [0.0])))
            dsr
{-
------------------------------------------------------------------------------
PHANTOM USE CASE : MERGING

             IO                |        PURE
                               |
gauge1 ---+                    |
          |                    |
gauge2 ---+                    |
          + --> readings ----------> inputs : merge all non-exception streams
gauge3 ---+     (phantom)      |
          |                    |
gauge4 ---+                    |
                               |

The merging function expects the lists to be merge are all sorted in same way (descending).
To express that in the type of the merge function, see:

Ghosts of Departed Proofs (Functional Pearl) - GDP --- by Matt Noonan

https://github.com/matt-noonan/gdp-paper/
https://github.com/matt-noonan/gdp
https://github.com/matt-noonan/justified-containers

------------------------------------------------------------------------------
useful phantom library

https://hackage.haskell.org/package/tagged-0.8.6/docs/Data-Tagged.html

-- | A @'Tagged' s b@ value is a value @b@ with an attached phantom type @s@.
newtype Tagged s b = Tagged { unTagged :: b }
  deriving (Eq, Ord, Ix, Bounded, Generic, Generic1, Typeable)

Instances

Bitraversable (Tagged :: * -> * -> *)
Bifoldable    (Tagged :: * -> * -> *)
Bifunctor     (Tagged :: * -> * -> *)
Eq2           (Tagged :: * -> * -> *)
Ord2          (Tagged :: * -> * -> *)
Read2         (Tagged :: * -> * -> *)
Show2         (Tagged :: * -> * -> *)
Generic1      (Tagged s :: * -> *)
Monad         (Tagged s)
Functor       (Tagged s)
Applicative   (Tagged s)
Foldable      (Tagged s)
Traversable   (Tagged s)
Eq1           (Tagged s)
Ord1          (Tagged s)
Read1         (Tagged s)
Show1         (Tagged s)
Bounded    b => Bounded    (Tagged s b)
Enum       a => Enum       (Tagged s a)
Eq         b => Eq         (Tagged s b)
Floating   a => Floating   (Tagged s a)
Fractional a => Fractional (Tagged s a)
Integral   a => Integral   (Tagged s a)
(Data s, Data b) => Data   (Tagged s b)
Num        a => Num        (Tagged s a)
Ord        b => Ord        (Tagged s b)
Read       b => Read       (Tagged s b)
Real       a => Real       (Tagged s a)
RealFloat  a => RealFloat  (Tagged s a)
RealFrac   a => RealFrac   (Tagged s a)
Show       b => Show       (Tagged s b)
Ix         b => Ix         (Tagged s b)
IsString   a => IsString   (Tagged s a)
Generic (Tagged s b)
Semigroup  a => Semigroup  (Tagged s a)
(Semigroup a, Monoid a) => Monoid (Tagged s a)
Storable   a => Storable   (Tagged s a)
Bits       a => Bits       (Tagged s a)
FiniteBits a => FiniteBits (Tagged s a)
NFData     b => NFData     (Tagged s b)
type Rep1  (Tagged s :: * -> *)
type Rep   (Tagged s b)

------------------------------------------------------------------------------
useful papers/articles on using Phantoms

Parse, don’t validate
Alexis King : November 2019
https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
- Not specifically about phantom types.
- Parsing: validate then turn into data structure that carries the validation.
- A parser is a function that consumes less-structured input and produces more-structured output.

Phantom Types for Real Problems
Neil Mitchell : April 2007
https://neilmitchell.blogspot.com/2007/04/phantom-types-for-real-problems.html

Motivation behind Phantom Types
January 2015
https://stackoverflow.com/questions/28247543/motivation-behind-phantom-types
- distances tagged (in phantom type) by length unit

-- ===========================================================================
EXISTENTIALS

------------------------------------------------------------------------------
EXISTENTIAL USE CASE : hide information

             IO                    |        PURE
                                   |
gauge1 ---+                        |
          |                        |
gauge2 ---+                        |
          + --> readings + state --|---------> inputs
gauge3 ---+     (phantom)          |
          |                  <-----|----- output + state
gauge4 ---+                        |
                                   |

------------------------------------------------------------------------------
-}
-- {-# LANGUAGE ExistentialQuantification #-}

newtype PType a = PTypeDouble Double

-- 'a' is existentially quantified
data PureState = forall a. PSD (PType a)
{-
data PureState where PSD :: PType a -> PureState -- GADT version
-}
type DSPS = Refined DecrValuesFlowOk (NonEmpty Double)
         -> PureState
         -> (Bool, Double, PureState)

dsps :: DSPS
dsps rs (PSD (PTypeDouble d)) =
  let r = NE.head (unrefine rs)
   in (r >= 10, r - d, PSD (PTypeDouble r))

-------------------------

lRTwS
  :: Refined DecrValuesFlowOk (NonEmpty Double)
  -> PureState
  -> DSPS
  -> Gauge Double
  -> IO (Double, Double)
lRTwS  = fix $ \loop rs ps ds g -> do
  r <- readGauge g
  putStrT "latest: "; print r
  case psCons r rs of
    Left  e  -> panic (show e)
    Right rs' -> do
      let (d, x, ps') = ds rs' ps -- downstream call
      putStrT "diff  : "; print x
      if d
        then pure (r, x)
        else loop rs' ps' ds g

lRTwSg :: IO (Double, Double)
lRTwSg  = Gauge.new137 0.0 1.0 10.0 >>= -- TODO detect non-decreasing example
          lRTwS (fromRight (refine (NE.fromList [0.0])))
                (PSD (PTypeDouble 0.0))
                dsps

{-
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

Existential quantification
Mark Karpov : November 2018
https://markkarpov.com/post/existential-quantification.html
- how much info about hidden type to be able to recover
  - OPEN: 'a' in "container" (e.g., [a]) existentially quantified: can still compute length
  - OPEN: assume existentially quantified type has certain properties (instances) "ANTIPATTERN"
  _ CLOSED: GADTs to restore exact types of existentially quantified variables

Existential Types
Arnaud Bailly : March 2017
http://abailly.github.io/posts/existential-types.html
- example of quiz made up of different types of questions
- uses "Existential Type Class Antipatthen"
- OPEN

------------------------------------------------------------------------------
EXISTENTIAL USE CASE : Object Orient Programming (OOP)

HERE: CLOSED
Object is a value with a well-defined interface.
The internal state of the object is closed to the outside world (encapsulation).
Different instantiations of the object can have different behaviour.

NOT DONE HERE: OPEN
Object can be modified by redefining functions in the object’s interface
and the internal state of the object can be extended.
-}

type DSPSOOP ps = Refined DecrValuesFlowOk (NonEmpty Double)
               -> ps
               -> (Bool, Double, PureStateOOP)

data PureStateOOP = forall ps. PSOOP -- 'ps' is existential
  { psv :: ps
  , psf :: DSPSOOP ps }

-- Apply DownStream
ads :: DSPSOOP PureStateOOP
ads rs (PSOOP ps f) = f rs ps

ds1 :: PureStateOOP
ds1  = PSOOP 0.0 f
 where
  f rs d =
    let r = NE.head (unrefine rs)
     in (r >= 10, r - d, PSOOP r f)

ds2 :: PureStateOOP
ds2  = PSOOP 0.0 f
 where
  f rs _d = -- _d : different from ds1
    let rs' = unrefine rs
        r   = NE.head rs'
     in (r >= 10, sum rs', PSOOP r f)
--                ^
--                different from ds1

lOOP
  :: Refined DecrValuesFlowOk (NonEmpty Double)
  -> PureStateOOP
  -> Gauge Double
  -> IO (Double, Double)
lOOP  = fix $ \loop rs ps g -> do
  r <- readGauge g
  putStrT "latest: "; print r
  case psCons r rs of -- check
    Left  e  -> panic (show e)
    Right rs' -> do
      let (d, x, ps') = ads rs' ps -- downstream call
      putStrT "xxxx  : "; print x
      if d
        then pure (r, x)
        else loop rs' ps' g

lOOPg1 :: IO (Double, Double)
lOOPg1  = lOOPg' ds1

lOOPg2 :: IO (Double, Double)
lOOPg2  = lOOPg' ds2

lOOPg' :: PureStateOOP -> IO (Double, Double)
lOOPg' ds = Gauge.new137 0.0 1.0 10.0 >>= -- TODO detect non-decreasing example
            lOOP (fromRight (refine (NE.fromList [0.0])))
                 ds
{-
------------------------------------------------------------------------------
useful papers/articles on OOP in Haskell

Object Oriented Programming in Haskell
Edsko de Vries : March 2018
https://www.well-typed.com/blog/2018/03/oop-in-haskell/
- see the CONCLUSIONS section of the paper for more references

------------------------------------------------------------------------------
-- NEXT existential : type-aligned sequences

------------------------------------------------------------------------------
-- utilities
-}

fromRight :: Either l r -> r
fromRight  = \case Right r -> r; _ -> panic "fromRight"

putStrT :: Text -> IO ()
putStrT  = putStr
