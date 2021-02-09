{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module MLP.Config (
   readConf,
   mkDataSet
) where

import Data.Aeson (eitherDecode)
import Numeric.LinearAlgebra.Data (loadMatrix, (><), Matrix)
import Numeric.LinearAlgebra.Static (create, L)
import Control.Lens ((^.), foldlOf, (%~), (&))
import MLP.DataSet (toPat, normPat)
import MLP.Network (AllCon, Net, Learn, MLP)
import MLP.Types (trainFile, 
                  testFile,
                  topology, 
                  normalise, 
                  topoFold,
                  DataSet(..), 
                  Parameters(..),
                  State(..),
                  Topology,
                  MonadFileSystem(..))
import Control.Monad ((=<<))
import System.FilePath (splitFileName, takeFileName, (</>))
import GHC.TypeLits (KnownNat, Nat)
import Control.Monad.Random.Class (MonadRandom)

liftEither :: (MonadFail m) => Either String b -> m b
liftEither = either fail pure

readConf :: (MonadFail m, MonadFileSystem m) => FilePath -> m Parameters
readConf fl = do
   s <- liftEither . eitherDecode =<< readFileM fl
   pure $ s & trainFile %~ absPath
            & testFile %~ absPath

   where
      (base,_) = splitFileName fl
      absPath x = base </> takeFileName x
   
mkDataSet :: (KnownNat i, KnownNat o) => (MonadFail m, MonadFileSystem m) => Parameters -> m (DataSet i o)
mkDataSet p = do
      trainSet <- mkPat (p ^. trainFile)
      testSet <- mkPat (p ^. testFile)

      return $ DataSet trainSet testSet
   where
      mkPat fl = readMatrixM fl >>= liftEither . toPat