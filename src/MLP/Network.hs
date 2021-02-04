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

module MLP.Network (
   AllCon,
   ArrList(..),
   Weights(..),
   Net(..),
   fromScalar,
   MLP(..),
   Learn(..)
) where

import Data.Proxy
import GHC.TypeLits
import Numeric.LinearAlgebra.Static
import Fcf
import Fcf.Data.List (Snoc)
import Data.Singletons
import Data.Kind (Constraint, Type)

type family AllCon (f :: k -> Constraint) (ks :: [k]) :: Constraint where
   AllCon _1 '[] = () :: Constraint
   AllCon f (k ': ks) = (f k, AllCon f ks) :: Constraint

data ArrList (e :: Nat -> Type) (a :: [Nat]) where
   Nil :: ArrList e '[]
   Cons :: e o -> ArrList e os -> ArrList e (o ': os)

data Weights (i :: Nat) (o :: Nat) = Weights {
   inputs :: L o i,
   bias :: R o
} deriving Show

data Net (i :: Nat) (hs :: [Nat]) (o :: Nat) where
   SLayer :: Weights i o -> Net i '[] o
   MLayers :: Weights i h -> Net h hs o -> Net i (h ': hs) o

fromScalar :: forall n m. (KnownNat n, KnownNat m) => Double -> L n m
fromScalar f =
   let numElems = product . map fromIntegral . fromSing $ sing @'[n, m] 
     in matrix (replicate numElems f)

class MLP a where
   type Topo a :: [Nat]
   type Arr a :: Nat -> Type
   type DimI a :: Nat
   type DimO a :: Nat
   type Layer a :: Nat -> Nat -> Type

   topo :: SingI (Topo a) => a -> [Integer]
   topo _ = map fromIntegral . fromSing $ sing @(Topo a)

   mkOut :: (KnownNat i, KnownNat o) 
            => Layer a i o -> Arr a i -> Arr a o
   
   layerUpd :: (KnownNat i, KnownNat o) 
               => Double -> Arr a i -> Arr a o -> Layer a i o -> Layer a i o

class MLP a => Learn a where
   mkDeltas :: Arr a (DimI a) -> Arr a (DimO a) -> a -> 
               ArrList (Arr a) (Topo a)
   netUpd :: Double -> Arr a (DimI a) -> ArrList (Arr a) (Topo a) -> a -> a

instance AllCon KnownNat (i ': o ': hs)
         => MLP (Net i hs o) where
   
   type Topo (Net i hs o) = Eval (Snoc hs o)
   type Arr (Net i hs o) = R
   type DimI (Net i hs o) = i
   type DimO (Net i hs o) = o
   type Layer (Net i hs o) = Weights

   mkOut :: (KnownNat w, KnownNat n) => Weights w n -> R w -> R n
   mkOut l i = inputs l #> i

   layerUpd :: (KnownNat w, KnownNat n) 
               => Double -> R w -> R n -> Weights w n -> Weights w n
   layerUpd lr i err l = Weights (w * delta) (b * err)
      where
         delta = fromScalar lr * (err `outer` i)
         w = inputs l
         b = bias l

instance (KnownNat i, KnownNat o)
         => Learn (Net i '[] o) where

   mkDeltas :: R i -> R o -> Net i '[] o -> ArrList R '[o]
   mkDeltas i o (SLayer l) = Cons (dsn * dso) Nil 
      where
         lo = mkOut @(Net i '[] o) l i
         dsn = lo * (1 - lo)
         dso = lo - o

   netUpd :: Double -> R i -> ArrList R '[o] -> Net i '[] o 
             -> Net i '[] o
   netUpd lr i (Cons delta _) (SLayer l) =
         SLayer (mkLayer lr i delta l)
      where
         mkLayer = layerUpd @(Net i '[] o)

instance (AllCon KnownNat (i ': o ': h ': hs),
         Learn (Net h hs o))
         => Learn (Net i (h ': hs) o) where

   mkDeltas :: R i -> R o -> Net i (h ': hs) o
               -> ArrList R (Topo (Net i (h ': hs) o))
   mkDeltas i o (MLayers l nxt) = 
      case nxt of
         SLayer lnxt -> let dd@(Cons d _) = mkDeltas lo o nxt 
                            dso = tr (inputs lnxt) #> d
                          in Cons (dsn * dso) dd
         
         MLayers lnxt _ -> let dd@(Cons d _) = mkDeltas lo o nxt 
                               dso = tr (inputs lnxt) #> d
                             in Cons (dsn * dso) dd
      where
         lo = mkOut @(Net i '[] o) l i
         dsn = lo * (1 - lo)

   
   netUpd :: Double -> R i -> ArrList R (Topo (Net i (h ': hs) o)) 
             -> Net i (h ': hs) o -> Net i (h ': hs) o
   netUpd lr i (Cons delta dr) (MLayers l n) =
      MLayers (mkLayer lr i delta l) (netUpd lr lo dr n) 

      where
         mkLayer = layerUpd @(Net i '[] o)
         lo = mkOut @(Net i '[] o) l i


layer1 :: Weights 2 3
layer1 = Weights (matrix [1..6]) (vector [1..3])

layer2 :: Weights 3 5
layer2 = Weights (matrix [1..15]) (vector [1..5])

network :: Net 2 '[3] 5
network = MLayers layer1 $ SLayer layer2
