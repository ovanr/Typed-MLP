{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MLP.DataSet (
   toPat,
   normPat
) where

import Numeric.LinearAlgebra.Data (cols, Matrix, R, takeColumns, dropColumns, toRows)
import Numeric.LinearAlgebra.Static (create, zipWithVector)
import MLP.Types (Pattern(..), Topology(..), zipWithPat)
import qualified Data.Vector.Storable as V
import qualified Data.Text as T
import Control.Monad (when)
import GHC.TypeLits (KnownNat)
import Data.Singletons ( SingI(sing), SingKind(fromSing) )
import Data.Maybe (mapMaybe)


toPat :: forall i o. (KnownNat i, KnownNat o) => Matrix R -> Either String [Pattern i o]
toPat m
   | cols m == (dimI + dimO) = Right $ zipWith Pattern is os
   | otherwise = Left "Mismatch on number of columns"

   where
      [dimI, dimO] = map fromIntegral . fromSing $ sing @'[i,o]
      is = mapMaybe create . toRows . takeColumns dimI $ m
      os = mapMaybe create . toRows . dropColumns dimI $ m


normPat :: forall i o. (KnownNat i, KnownNat o) => [Pattern i o] -> [Pattern i o]
normPat [] = []
normPat xxs@(x:xs) = map (\p -> (p - minp) / (maxp - minp)) xxs
   where
      minp = foldl (zipWithPat min) x xs
      maxp = foldl (zipWithPat max) x xs