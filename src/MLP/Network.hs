{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE QuantifiedConstraints #-}

module MLP.Network (
   AllCon,
   ArrList(..),
   arrLast,
   Weights(..),
   Net(..),
   scalarR,
   scalarL,
   MLP(..),
   Learn(..),
   NetInstance
) where

import GHC.TypeLits ( KnownNat, Nat, natVal )
import Numeric.LinearAlgebra.Static
    ( R,
      L,
      (#>),
      (<·>),
      matrix,
      uniformSample,
      unrow,
      vector,
      tr,
      Domain(outer) )
import Fcf ( Eval )
import Fcf.Data.List (Snoc, Last)
import Data.Singletons
    ( Proxy(Proxy), SingI(sing), SingKind(fromSing) )
import Data.Kind (Constraint, Type)
import Control.Monad.Random.Class (MonadRandom(..))
import Numeric.Natural ( Natural )
import Unsafe.Coerce ( unsafeCoerce )
import Data.Proxy ( Proxy(Proxy) )

type family AllCon (f :: k -> Constraint) (ks :: [k]) :: Constraint where
   AllCon _1 '[] = () :: Constraint
   AllCon f (k ': ks) = (f k, AllCon f ks) :: Constraint

data ArrList (e :: Nat -> Type) (a :: [Nat]) where
   Nil :: ArrList e '[]
   Cons :: (KnownNat o, Show (e o)) => e o -> ArrList e os -> ArrList e (o ': os)

arrLast :: forall (o :: Nat) (e :: Nat -> Type) (a :: [Nat]). (AllCon KnownNat a, KnownNat o) 
           => ArrList e a -> Maybe (e o)
arrLast Nil = Nothing
arrLast (Cons a as@(Cons _ _)) = arrLast as
arrLast (Cons (a :: e o1) Nil) 
   | natVal (Proxy @o1) == natVal (Proxy @o) = Just (unsafeCoerce a)
   | otherwise = Nothing

instance Show (ArrList e a) where
   show Nil = "Nil"
   show (Cons a x) = "Cons " ++ show a ++ show x


data Weights (i :: Nat) (o :: Nat) = Weights {
   inputs :: !(L o i),
   bias :: !(R o)
}

instance (KnownNat i, KnownNat  o) => Show (Weights i o) where
   show (Weights i b) = "\n[ Weights \n" ++ show i ++ "\n, Bias \n" ++ show b ++ "]\n"

rndWeights :: (KnownNat i, KnownNat o, MonadRandom m) => m (Weights i o)
rndWeights = do
   (s1,s2) <- (,) <$> getRandom <*> getRandom

   let w = uniformSample s1 (-1) 1
       b = unrow $ uniformSample s2 (-1) 1
       
   pure (Weights w b)

data Net (i :: Nat) (hs :: [Nat]) (o :: Nat) where
   SLayer :: !(Weights i o) -> Net i '[] o
   MLayers :: !(Weights i h) -> !(Net h hs o) -> Net i (h ': hs) o

instance AllCon KnownNat (i ': o ': hs) => Show (Net i hs o) where
   show (SLayer l) = "SLayer ->" ++ show l ++ "\n"
   show (MLayers l n) = "MLayers ->" ++ show l ++ "\n" ++ show n

scalarR :: forall n. KnownNat n => Double -> R n
scalarR d = vector (replicate numElems d)
   where
      numElems = fromIntegral . natVal $ Proxy @n

scalarL :: forall n m. (KnownNat n, KnownNat m) => Double -> L n m
scalarL d = matrix (replicate numElems d)
   where
      numElems = product . map fromIntegral . fromSing $ sing @'[n, m] 

class MLP a where
   type Topo a :: [Nat]
   type Arr a :: Nat -> Type
   type DimI a :: Nat
   type DimO a :: Nat
   type Layer a :: Nat -> Nat -> Type

   layerOut :: (KnownNat i, KnownNat o) 
            => Layer a i o -> Arr a i -> Arr a o
   
   newLayer :: (KnownNat i, KnownNat o) 
               => Double -> Arr a i -> Arr a o -> Layer a i o -> Layer a i o

   netError :: Arr a (DimO a) -> Arr a (DimO a) -> Double

class MLP a => Learn a where
   create :: MonadRandom m => m a

   topo :: a -> [Natural]

   netOut :: Arr a (DimI a) -> a -> ArrList (Arr a) (Topo a)

   mkDeltas :: ArrList (Arr a) (Topo a) -> Arr a (DimO a) -> a -> ArrList (Arr a) (Topo a)
   
   newNet :: Double -> Arr a (DimI a) -> ArrList (Arr a) (Topo a) -> ArrList (Arr a) (Topo a) -> a -> a

instance AllCon KnownNat (i ': o ': hs)
         => MLP (Net i hs o) where
   
   type Topo  (Net i hs o) = Eval (Snoc hs o)
   type Arr   (Net i hs o) = R
   type DimI  (Net i hs o) = i
   type DimO  (Net i hs o) = o
   type Layer (Net i hs o) = Weights

   layerOut :: (KnownNat w, KnownNat n) => Weights w n -> R w -> R n
   layerOut l i = sigmoid $ (inputs l #> i) + bias l
      where
         sigmoid x = 1 / (1 + exp (-x))

   newLayer :: (KnownNat w, KnownNat n)
               => Double -> R w -> R n -> Weights w n -> Weights w n
   newLayer lr i delta l = Weights (w - errW) (b - errB)
      where
         w = inputs l
         b = bias l
         errW = scalarL lr * (delta `outer` i)
         errB = scalarR lr * delta

   netError :: KnownNat o => R o -> R o -> Double
   netError expOut actOut = 1 <·> ((expOut - actOut) ** 2)

-- this dummy type can be used to help find the correct MLP instance
-- in functions layerOut, newLayer
type NetInstance = Net 0 '[] 0

instance (KnownNat i, KnownNat o)
         => Learn (Net i '[] o) where

   create :: MonadRandom m => m (Net i '[] o)
   create = SLayer <$> rndWeights

   topo :: Net i '[] o -> [Natural]
   topo _ = [fromSing (sing @o)]

   netOut :: R i -> Net i '[] o -> ArrList R '[o]
   netOut i (SLayer l) = Cons (layerOut @NetInstance l i) Nil

   mkDeltas :: ArrList R '[o] -> R o -> Net i '[] o -> ArrList R '[o]
   mkDeltas (Cons lo Nil) expOut (SLayer l) = Cons (dsn * dso) Nil
      where
         dsn = lo * (1 - lo)
         dso = lo - expOut

   newNet :: Double -> R i -> ArrList R '[o] -> ArrList R '[o] -> Net i '[] o -> Net i '[] o
   newNet lr i (Cons delta Nil) _ (SLayer l) = SLayer (newLayer @NetInstance lr i delta l)

instance (AllCon KnownNat (i ': o ': h ': hs),
         Learn (Net h hs o))
         => Learn (Net i (h ': hs) o) where

   create :: forall i h hs o m. (MonadRandom m, KnownNat i, KnownNat h, Learn (Net h hs o)) 
             => m (Net i (h ': hs) o)
   create = MLayers <$> (rndWeights @i @h) <*> create @(Net h hs o)

   topo :: Net i (h ': hs) o -> [Natural]
   topo (MLayers _ n) = fromSing (sing @h) : topo n

   netOut :: hhs ~ (h ': hs) => R i -> Net i hhs o -> ArrList R (Topo (Net i hhs o))
   netOut i (MLayers l n) = Cons lo (netOut lo n)
      where
         lo = layerOut @NetInstance l i

   mkDeltas :: hhs ~ (h ': hs) => 
               ArrList R (Topo (Net i hhs o)) -> R o -> Net i hhs o -> ArrList R (Topo (Net i hhs o))
   mkDeltas (Cons lo lor) expOut (MLayers l nxt) =
      case nxt of
         SLayer lnxt -> let dd@(Cons d _) = mkDeltas lor expOut nxt 
                            dso = tr (inputs lnxt) #> d
                          in Cons (dsn * dso) dd
         
         MLayers lnxt _ -> let dd@(Cons d _) = mkDeltas lor expOut nxt 
                               dso = tr (inputs lnxt) #> d
                             in Cons (dsn * dso) dd
      where
         dsn = lo * (1 - lo)

   newNet :: Double -> 
             R i ->
             ArrList R (Topo (Net i (h ': hs) o)) ->
             ArrList R (Topo (Net i (h ': hs) o)) ->
             Net i (h ': hs) o ->
             Net i (h ': hs) o
   newNet lr i (Cons d ds) (Cons lo los) (MLayers l n) =
      MLayers (newLayer @NetInstance lr i d l) (newNet lr lo ds los n)


layer1 :: Weights 2 3
layer1 = Weights (matrix [1..6]) (vector [1..3])

layer2 :: Weights 3 5
layer2 = Weights (matrix [1..15]) (vector [1..5])

network :: Net 2 '[3] 5
network = MLayers layer1 $ SLayer layer2