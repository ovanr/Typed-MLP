{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

{-# Language QuasiQuotes #-}

module MLP.App (
   run,
   train,
) where

import MLP.Types
import MLP.Config (readConf, mkDataSet)
import MLP.Network (AllCon, ArrList, Learn(..), Net, MLP(..), NetInstance, arrLast)
import Numeric.LinearAlgebra.Static (R)
import Control.Lens (view, use, (+=), (^.), (.=))
import Control.Monad (when, forM_, forM)
import qualified Data.Vector.Storable as V (snoc, sum)
import Control.Monad.Writer.Class (tell)
import PyF (fmt)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TIO (putStrLn)
import Data.Singletons
import Data.Singletons.Prelude.Tuple
import Data.Singletons.Prelude.List
import Fcf
import Fcf.Data.List
import Data.Singletons.TypeLits
import Data.Constraint
import Numeric.Natural
import Data.Maybe
import MLP.NatList

netDict :: forall (i :: Nat) (hs :: [Nat]) (o :: Nat). (AllCon KnownNat (i ': o ': hs)) =>
           SNat i -> 
           SNatList ('NatList hs) -> 
           SNat o -> 
           Dict (SingI (Topo (Net i hs o)), AllCon KnownNat (Topo (Net i hs o)), Learn (Net i hs o))
netDict (SNat :: SNat i) l (SNat :: SNat o) =
   case l of
      SNatNil -> Dict
      SNatCons (_ :: SNat h1) SNatNil -> Dict
      SNatCons (_ :: SNat h2) (SNatCons (_ :: SNat h1) SNatNil) -> Dict
      SNatCons (_ :: SNat h3) (SNatCons (_ :: SNat h2) (SNatCons (_ :: SNat h1) SNatNil)) -> Dict


run :: FilePath -> IO ()
run fl = do
   conf <- readConf fl
   let i = conf ^. topology.numInputs
       hs = conf ^. topology.hiddenLayers
       o = conf ^. topology.numOutputs

   withSomeSing i $ \snatI@(SNat :: SNat i1) ->
      withSomeSing o $ \snatO@(SNat :: SNat o1) ->
         withSomeSing (MyList (0:hs)) $
            \case 
               (SNatCons _ snatlist) -> 
                  case netDict snatI snatlist snatO of
                     (Dict :: Dict (_1, _2, Learn (Net i hs o))) -> do
                        dt <- mkDataSet @i @o conf
                        net <- create @(Net i hs o)
                        let st = State dt conf net 0 0
                        let t = topo @(Net i hs o)
                        let ((a, n),log) = runApp train st
                        TIO.putStrLn log


trainOnSet :: forall i o hs. (AllCon KnownNat (i ': o ': hs), AllCon KnownNat (Topo (Net i hs o)), Learn (Net i hs o)) => App i hs o Double
trainOnSet = do
   trainset <- use (dataSet.trainSet)
   net <- use network
   lr <- use (params.learningRate)

   errorRates <- forM trainset $ \(Pattern i o) -> do
      let outs = netOut i net
      let deltas = mkDeltas outs o net
      let net2 = newNet lr i deltas outs net
      network .= net2
      let actOut = fromJust $ arrLast @o outs
      let err = netError @(Net i hs o) actOut o
      tell [fmt| Pattern Error {err:.4f} \n |]
      return err

   let totalErr = sum errorRates
   
   tell [fmt| Epoch Error {totalErr:f} \n |]

   curError .= totalErr
   return totalErr


evaluateOnSet :: App i hs o ()
evaluateOnSet = undefined

loop :: App i hs o () -> App i hs o ()
loop trainAction = do
   epochTotal <- use (params.epochs)
   epochNow <- use curEpoch

   when (epochNow < epochTotal) $ do
      tell "new epoch\n"
      trainAction

      curError <- use curError
      minError <- use (params.minErrorRate)

      when (4 > minError) $ do
         loop trainAction

train :: (AllCon KnownNat (i ': o ': hs), AllCon KnownNat (Topo (Net i hs o)), Learn (Net i hs o)) => App i hs o ()
train = do
   tell "Starting Training process...\n"
   inps <- use (dataSet.trainSet)
   tell (TL.pack . show $ inps)
   loop $ do
      epoch <- use curEpoch

      tell [fmt| Epoch {epoch:d} \n |]
      
      trainOnSet
      --evaluateOnSet

      curEpoch += 1