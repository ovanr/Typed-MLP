{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# Language QuasiQuotes #-}

module MLP.App (
   runInIO,
   run,
   train,
) where

import MLP.Types
    ( curEpoch,
      curError,
      dataSet,
      epochs,
      hiddenLayers,
      learningRate,
      minErrorRate,
      network,
      numInputs,
      numOutputs,
      params,
      runApp,
      topology,
      testSet,
      trainSet,
      App,
      MonadFileSystem(printText),
      Parameters,
      Pattern(Pattern),
      State(State) )
import MLP.Config (readConf, mkDataSet)
import MLP.Network (AllCon, ArrList, Learn(..), Net, MLP(..), NetInstance, arrLast)
import Numeric.LinearAlgebra.Static (R)
import Control.Lens (view, use, (+=), (^.), (.=))
import Control.Monad (when, forM_, forM)
import qualified Data.Vector.Storable as V (snoc, sum)
import Control.Monad.Writer.Class (tell)
import PyF (fmt)
import qualified Data.Text.Lazy as TL
import Control.Monad.Random.Class ( MonadRandom )
import Data.Singletons ( withSomeSing )
import Data.Singletons.Prelude.Tuple ()
import Data.Singletons.Prelude.List ()
import Fcf ()
import Fcf.Data.List ()
import Data.Singletons.TypeLits ( KnownNat, Nat, SNat(..) )
import Data.Constraint ( Dict(..) )
import Numeric.Natural ()
import Data.Maybe ( fromJust )
import MLP.NatList ( List(List), SNatList(..) )

type NetConstraints (i :: Nat) (hs :: [Nat]) (o :: Nat) =
   (AllCon KnownNat (i ': o ': hs), AllCon KnownNat (Topo (Net i hs o)), Learn (Net i hs o))

netDict :: forall (i :: Nat) (o :: Nat) (hs :: [Nat]). (AllCon KnownNat (i ': o ': hs)) =>
           SNatList ('List hs) -> 
           Dict (NetConstraints i hs o)
netDict =
   \case
      SNatNil -> Dict
      SNatCons _ SNatNil -> Dict
      SNatCons _ (SNatCons _ SNatNil) -> Dict
      SNatCons _ (SNatCons _ (SNatCons _ SNatNil)) -> Dict


runInIO :: FilePath -> IO ()
runInIO fl = do
   conf <- readConf fl
   let i  = conf ^. topology.numInputs
       hs = conf ^. topology.hiddenLayers
       o  = conf ^. topology.numOutputs

   withSomeSing i $ \(SNat :: SNat i) ->
      withSomeSing o $ \(SNat :: SNat o) ->
         withSomeSing (List (0:hs)) $
            \case 
               (SNatCons _ snatlist) -> 
                  case netDict @i @o snatlist of
                     (Dict :: Dict (NetConstraints i hs o)) -> run @i @hs @o @IO conf

run :: forall i hs o m. (NetConstraints i hs o, MonadFileSystem m, MonadRandom m) => Parameters -> m ()
run conf = do
   dt <- mkDataSet @i @o conf
   net <- create @(Net i hs o)

   let st = State dt conf net 0 0
       ((a, st'),log) = runApp train st

   mapM_ printText log

trainOnSet :: forall i o hs. NetConstraints i hs o => App i hs o Double
trainOnSet = do
   trainset <- use (dataSet.trainSet)
   lr <- use (params.learningRate)

   errorRates <- forM trainset $ \(Pattern i o) -> do
      net <- use network

      let outs   = netOut i net
          deltas = mkDeltas outs o net
          actOut = fromJust $ arrLast @o outs
          err = netError @(Net i hs o) actOut o
      
      network .= newNet lr i deltas outs net

      -- tell [fmt| \n ==> Pattern Error {err:.4f} \n |]
      -- tell [fmt| \n ==> Deltas {deltas:s} \n |]
      -- tell [fmt| \n ==> Expected Output {o:s} \n |]
      -- tell [fmt| \n ==> Actual Output {actOut:s} \n |]
      -- tell [fmt| \n Network Dump \n {net:s} \n|]

      return err

   let totalErr = sum errorRates / fromIntegral (length trainset)

   tell [[fmt| \n ==> Epoch Train Error {totalErr:f} \n |]]

   return totalErr

evaluateOnSet :: forall i o hs. NetConstraints i hs o => App i hs o Double
evaluateOnSet = do
   testset <- use (dataSet.testSet)
   net <- use network

   errorRates <- forM testset $ \(Pattern i o) -> do

      let outs   = netOut i net
          deltas = mkDeltas outs o net
          actOut = fromJust $ arrLast @o outs
          err = netError @(Net i hs o) actOut o

      return err

   let totalErr = sum errorRates / fromIntegral (length testset)
   curError .= totalErr

   tell [[fmt| \n ==> Epoch Test Error {totalErr:f} \n |]]

   return totalErr

loop :: App i hs o () -> App i hs o ()
loop trainAction = do
   epochTotal <- use (params.epochs)
   epochNow <- use curEpoch

   when (epochNow < epochTotal) $ do
      trainAction

      curError <- use curError
      minError <- use (params.minErrorRate)

      when (curError > minError) $ do
         loop trainAction

train :: NetConstraints i hs o => App i hs o ()
train = do
   tell ["Starting Training process...\n"]

   loop $ do
      epoch <- use curEpoch

      tell [[fmt| \n ==> Epoch {epoch:d} \n |]]
      
      trainOnSet
      evaluateOnSet


      curEpoch += 1