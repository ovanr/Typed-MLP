{-# LANGUAGE GADTs, LambdaCase, InstanceSigs, FlexibleInstances, UndecidableInstances, StandaloneKindSignatures, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, TypeOperators, RankNTypes, FlexibleContexts #-}

module MLP.NatList where

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Kind
import Numeric.Natural
import Data.Constraint
import MLP.Network (AllCon, Net(..), Learn(..))

data Door = Open | Closed

type SDoor :: Door -> Type
data SDoor s where
   SOpen :: SDoor 'Open
   SClosed :: SDoor 'Closed

type instance Sing = SDoor

newtype MyList a = MyList [a]

newtype NatList = NatList [Nat] deriving Show

type SNatList :: NatList -> Type
data SNatList s where
   SNatNil :: SNatList ('NatList '[])
   SNatCons :: (AllCon KnownNat (n ': ns)) => 
               SNat (n :: Nat) -> SNatList ('NatList ns) -> SNatList ('NatList (n ': ns))

type instance Sing = SNatList

instance SingI ('NatList '[]) where
   sing = SNatNil

instance (AllCon KnownNat (a ': as), SingI as, SingI ('NatList as)) 
         => SingI ('NatList ((a :: Nat) ': (as :: [Nat]))) where
   sing = SNatCons (SNat @a) (sing @('NatList as))

instance SingKind NatList where
   type Demote NatList = MyList Natural

   toSing :: Demote NatList -> SomeSing NatList
   toSing (MyList []) = SomeSing SNatNil
   toSing (MyList (x:xs)) = withSomeSing x $
                     \case
                        xn@(SNat :: Sing (n :: Nat)) ->
                           withSomeSing (MyList xs) $
                              \case
                                 all@(SNatCons _ _) ->
                                    SomeSing (SNatCons xn all)
                                 SNatNil -> SomeSing (SNatCons xn SNatNil)
   fromSing SNatNil = MyList []
   fromSing (SNatCons x xs) = let MyList r = fromSing xs
                                 in MyList (fromSing x : r)
