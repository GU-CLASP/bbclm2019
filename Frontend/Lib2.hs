{-# LANGUAGE GADTs #-}
module Lib2 where

import qualified Lib
import Lib (Ind,P,Expr,constant,sampleGaussian,Vec)

observe :: S -> P ()
observe (S p) = observeP p

observeP :: Prop -> P ()
observeP (And x y) = observeP x >> observeP y
observeP (x :=: y) = Lib.observeEqual x y
observeP p = Lib.observe (eval p)

eval :: Prop -> Expr Bool
eval (Con x) = x
eval (x :>: y) = x Lib.> y
eval (x :=: y) = 10 Lib.> abs (x - y) -- FIXME
eval (Or x y) = eval x `Lib.or` eval y
eval (And x y) = eval x `Lib.and` eval y
eval (If c x y) = Lib.If c (eval x) (eval y)
eval x = error $ "no eval for: " ++ show x

genQProb :: Bool -> Expr Float -> Pred -> VP -> Prop
genQProb pol prob cn vp = Lib.expectedPortion p :>: prob
  where p = do x <- newInd
               observeP (evalPred cn True x)
               return (eval (evalPred vp pol x))

-- | This version of 'every' measures the angle between the vectors and compares the biases.
universalOptimized :: Pred -> Pred -> Prop
universalOptimized _ Universe = Con (constant True)
universalOptimized (SimplePred (_,x,thx)) (SimplePred (_,y,thy))
  = ((Lib.cosineDistance x y) :>: 0.99) ∧ (thx :>: thy)
universalOptimized x y = genQProb True 0.99 x y


newInd :: Lib.P Ind
newInd = Lib.newInd

data Prop = (Expr Float) :=: (Expr Float)
          | (Expr Float) :/=: (Expr Float)
          | (Expr Float) :>: (Expr Float)
          | If (Expr Bool) Prop Prop
          | Or Prop Prop
          | And Prop Prop
          | Con (Expr Bool)
          deriving Show

(∧) :: Prop -> Prop -> Prop
(∧) = And

(∨) :: Prop -> Prop -> Prop
(∨) = Or

not' :: Prop -> Prop
not' (Con x) = Con (Lib.not' x)
not' (x :=: y) = (x :/=: y)
not' (x :/=: y) = (x :=: y)
not' (x :>: y) = (y :>: x)
not' (x `And` y) = not' x `Or` not' y
not' (x `Or` y) = not' x `And` not' y

is :: Measure -> ShallowPred
is a@(_lo,_,hi) True x = applicability a x :>: hi
is a@(lo,_,_hi) False x = lo :>: applicability a x

newPred :: P CN
newPred = do
  m <- newMeasure
  return (SimplePred m)

positive :: Expr Float -> Prop
positive x = x :>: 0

type Measure = (Expr Float,Vec,Expr Float)

newMeasure :: Lib.P Measure
newMeasure = do
  lowBound <- sampleGaussian 0 1
  highBound <- sampleGaussian 0 1
  observeP (highBound :>: lowBound)
  v <- Lib.newNormedVector
  return (lowBound,v,highBound)


data Scoping = Narrow | Wide

type CN = Pred
type VP = Pred 
type A = Measure
type Card = Int
type RS = Pred
type RCl = VP
type RP = ()

data S where
  S :: Prop -> S
  YouS :: Pred -> S
fromS :: S -> Prop
fromS (S x) = x

data Operator = Equal | More | Less

data NP where
  Percents :: Card -> CN -> NP
  QNP :: Quant -> CN -> NP
  PN :: Ind -> NP
  You :: NP

genericYou :: NP
genericYou = You

type ShallowPred = Bool -> Ind -> Prop
data Pred = SimplePred Measure | EvalledPred ShallowPred | Universe

evalPred :: Pred -> (Bool -> Ind -> Prop)
evalPred (EvalledPred p) = p
evalPred (SimplePred m) = is m
evalPred (Universe) = \pol _x -> Con (constant pol)

type Quant = (Pred -> VP -> Prop, Scoping)
type AVP = VP
type Pol = Bool
type Di = Int
type Digits = [Int]

type ModalAdv = VP -> AVP

modify :: ModalAdv -> VP -> AVP
modify = id

simpleModalAdv :: (Prop -> Prop) -> VP -> AVP
simpleModalAdv f vp = EvalledPred $ \ pol x -> f (evalPred vp pol x)

bare   ::             VP -> AVP
bare = id

(-->) :: Prop -> Prop -> Prop
p --> q = not' p ∨ q

probablyInt :: Expr Float -> Prop -> Prop
probablyInt v x = Con (Lib.sample (Lib.bernouilli v)) --> x

probablyDef ::  Expr Float -> Prop -> Prop
probablyDef v x = If (Lib.sample (Lib.bernouilli v)) x (not' x)

definitely :: ModalAdv
definitely = id -- FIXME: in fact we should rule out.

generally :: ModalAdv
generally = simpleModalAdv (probablyInt 0.8)

always :: ModalAdv
always = id

rarely :: ModalAdv
rarely vp = EvalledPred $ \pol x -> probablyInt 0.7 (evalPred vp (Prelude.not pol) x)

usually :: ModalAdv
usually = simpleModalAdv (probablyInt 0.8)

probably :: ModalAdv
probably = simpleModalAdv (probablyInt 0.75)

often :: ModalAdv
often = simpleModalAdv (probablyInt 0.7)

frequently :: ModalAdv
frequently = simpleModalAdv (probablyInt 0.8)

also :: ModalAdv
also = id

regularly :: ModalAdv
regularly = simpleModalAdv (probablyInt 0.7)

never :: ModalAdv
never vp  = non vp

occasionally :: ModalAdv
occasionally vp = EvalledPred $ \pol x ->
    probablyInt 0.2 (evalPred vp pol x)
  ∧ probablyInt 0.2 (evalPred vp (Prelude.not pol) x)
-- With a factor closer to 0.5, this would be more accurate. However it is sloooow.


isA :: CN -> VP            -- Example: to be a violinist
isA = id

data Cl where
  S1  :: NP -> AVP -> Cl                -- Example: John reads music

s1 :: NP -> AVP -> Cl
s1 = S1

-- If  : S -> S -> S;            -- Example: If John generally reads music, then Mary is a violinist
-- But : S -> S -> S;


composePol :: Bool -> VP -> VP
composePol True = id
composePol False = non

interpQuantifier :: Bool -> NP -> VP -> Prop
interpQuantifier pol q0 vp = case q0 of
  Percents n cn -> genQProb pol (fromIntegral n / 100) cn vp
  PN x  -> evalPred vp pol x
  QNP (q,Wide) cn    -> topPol pol (q cn vp)
  QNP (q,Narrow) cn  -> q cn (composePol pol vp)

topPol :: Pol -> Prop -> Prop
topPol b = if b then id else not'

cltoS :: Pol -> Cl -> S
cltoS pol (S1 You vp) = YouS (composePol pol vp)
cltoS pol (S1 q vp) = S (interpQuantifier pol q vp)



neg :: Pol
neg = False

pos :: Pol
pos = True

tHAT :: RP
tHAT = ()

makeRCL :: RP -> VP -> RCl;
makeRCL _ = id

makepolarRS :: Pol -> RCl -> RS;
makepolarRS True = id
makepolarRS False = non

relativiseCN :: CN -> RS -> CN
relativiseCN (Universe) rs = rs
relativiseCN cn rs = EvalledPred $ \ pol x ->
  topPol pol (evalPred cn True x ∧ evalPred rs True x)

-- All,Most,Few,Every,GenericPl, Many, the, a : Quant;
every :: Quant
every = (universalOptimized,Wide)

all :: Quant
all = (universalOptimized,Narrow)

genericPl :: Quant
genericPl = (genQProb True 0.90,Narrow)

many :: Quant
many = (genQProb True 0.75,Narrow)

most :: Quant
most = (genQProb True 0.9,Narrow)

few :: Quant
few = (\cn vp -> genQProb False 0.8 cn vp,Wide)

aFew :: Quant
aFew = (genQProb True 0.2,Narrow)

some :: Quant
some = (genQProb True 0.01,Narrow)

percentOf :: Card -> CN -> NP
percentOf = Percents
-- Exactly, AtLeast, MoreThan : Card -> Quant;


non :: CN -> CN
non (EvalledPred vp) = EvalledPred $ \pol x -> vp (Prelude.not pol) x
non (SimplePred (lo,v,hi)) = SimplePred (negate hi,negate <$> v,negate lo)
non Universe = EvalledPred $ \pol _x -> Con (constant pol)

-- Qual : A -> CN -> CN;                  -- Example: short musician

qNP :: Quant -> CN -> NP
qNP = QNP

if_ :: S -> S -> S
if_ (S x) (S y) = S (x --> y)
if_ (YouS vp1) (YouS vp2) = S (fst every vp1 vp2)

-- infixr -->
-- (-->) :: S -> S -> S
-- (S x) --> (S y) = S (x Lib.--> y)

but :: S -> S -> S
but (S x) (S y) = S (x ∧ y)

and :: S -> S -> S
and (S x) (S y) = S (x ∧ y)

or :: S -> S -> S
or (S x) (S y) = S (x ∨ y)

parens :: S -> S
parens = id

not :: S -> S
not (S x) = S (not' x)

less :: Operator
less = Less

more :: Operator
more = More

equal :: Operator
equal = Equal

moreVP :: A -> NP -> VP
moreVP a = comparVP More a

more' :: Measure -> Ind -> Ind -> Prop
more' m x y = applicability m x :>: applicability m y

equal' :: Measure -> Ind -> Ind -> Prop
equal' m x y = applicability m x :=: applicability m y

comparVP :: Operator -> A -> NP -> VP
comparVP op a q  = EvalledPred $ \pol x -> interpQuantifier pol q  $ EvalledPred $ \pol' y -> case (pol',op) of
  (True,More) -> more' a x y
  (True,Less) -> more' a y x
  (True,Equal) -> equal' a x y
  (False,More) -> more' a y x -- FIXME
  (False,Less) -> more' a x y -- FIXME
  (False,Equal) -> equal' a x y -- FIXME


qual :: A -> CN -> CN
qual a cn = EvalledPred $ \pol x -> case pol of
  True -> is a True x ∧ evalPred cn True x
  False -> is a False x ∨ evalPred cn False x
-- qual a cn pol x = topPol pol (is a True x ∧ cn True x)

testVP :: A -> VP
testVP a = EvalledPred (is a)

run :: P S -> IO ()
run p = Lib.run (eval <$> fromS <$> p)

person :: CN
person = Universe


mkCanPlayChoords :: P (Operator -> Card -> VP)
mkCanPlayChoords = do
  choordAbility <- newMeasure -- FIXME what to do with bounds?
  factor <- sampleGaussian 100 50
  bias <- sampleGaussian 0 1
  let ability x = factor * (applicability choordAbility x) + bias
  return $ \op n -> EvalledPred $ \pol x ->
    case (pol,op) of
      (True,Equal) -> ability x :=: Lib.constant (fromIntegral n)
      (True,Less) ->  Lib.constant (fromIntegral n) :>: ability x 
      (True,More) ->  ability x :>: Lib.constant (fromIntegral n)
      (False,Equal) -> not' (ability x :=: Lib.constant (fromIntegral n)) -- FIXME
      (False,Less) ->  ability x :>: Lib.constant (fromIntegral n)
      (False,More) ->  Lib.constant (fromIntegral n) :>: ability x

n0_Dig :: Di
n0_Dig = 0
n1_Dig :: Di
n1_Dig = 1
n2_Dig :: Di
n2_Dig = 2
n3_Dig :: Di
n3_Dig = 3
n4_Dig :: Di
n4_Dig = 4
n5_Dig :: Di
n5_Dig = 5
n6_Dig :: Di
n6_Dig = 6
n7_Dig :: Di
n7_Dig = 7
n8_Dig :: Di
n8_Dig = 8
n9_Dig :: Di
n9_Dig = 9

addDigit :: Di -> Digits -> Digits;
addDigit = (:)

oneDigit :: Di -> Digits;
oneDigit = (:[])

digitsToCard :: Digits -> Card;
digitsToCard = foldl (\x y -> 10 * x + y) 0

-- >>> digitsToCard [1,4,5]
-- 145

type Unit = (Expr Float, Expr Float)

newUnit :: P Unit
newUnit = do
  factor <- sampleGaussian 20 5
  bias <- sampleGaussian 175 5
  return (factor,bias)
-- These biases worked somewhat better for example 38
measure :: Card -> Unit -> A -> VP
measure n (factor,bias) a = EvalledPred $ \pol x -> case pol of
  True -> (factor * (applicability a x) + bias) :=: constant (fromIntegral n)
  False -> not' ((factor * (applicability a x) + bias) :=: constant (fromIntegral n)) -- FIXME


applicability :: Measure -> Ind -> Expr Float
applicability (_,a,_) x = Lib.dotProd a x
