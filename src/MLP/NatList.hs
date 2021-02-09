{-# LANGUAGE GADTs #-} 
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE StandaloneKindSignatures #-} 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module MLP.NatList where

import Data.Singletons
    ( withSomeSing, Sing, SingI(..), SingKind(..), SomeSing(..) )
import Data.Singletons.TypeLits ( KnownNat, Nat, Sing, SNat(..) )
import Data.Kind ( Type )
import Numeric.Natural ( Natural )
import MLP.Network (AllCon, Net(..), Learn(..))

-- Wrap a list to a newtype so that we can define (Sing (a :: List Nat))
-- to hold KnownNat Constraints over all the NATs
newtype List a = List { unList :: [a] } deriving Show

type SNatList :: List Nat -> Type
data SNatList s where
   SNatNil :: SNatList ('List ('[] :: [Nat]))
   SNatCons :: (AllCon KnownNat (n ': ns)) => 
               SNat (n :: Nat) -> SNatList ('List ns) -> SNatList ('List (n ': ns))

type instance Sing = SNatList

instance SingI ('List ('[] :: [Nat])) where
   sing = SNatNil

instance (AllCon KnownNat (a ': as), SingI ('List as)) 
         => SingI ('List ((a :: Nat) ': (as :: [Nat]))) where
   sing = SNatCons (SNat @a) (sing @('List as))

instance SingKind (List Nat) where
   type Demote (List Nat) = List Natural

   toSing :: Demote (List Nat) -> SomeSing (List Nat)
   toSing (List []) = SomeSing SNatNil
   toSing (List (x:xs)) = 
      withSomeSing x $
         \case
            (SNat :: SNat n) ->
               withSomeSing (List xs) $
                  \case
                     all@(SNatCons _ _) -> SomeSing (SNatCons (SNat @n) all)
                     SNatNil -> SomeSing (SNatCons (SNat @n) SNatNil)

   fromSing SNatNil = List []
   fromSing (SNatCons x xs) = List (fromSing x : unList (fromSing xs))
