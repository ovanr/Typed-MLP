{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

{-# Language QuasiQuotes #-}

module MLP.App (
   run,
   train,
) where

import MLP.Types
import Numeric.LinearAlgebra (Vector, R, Matrix, (#>), asRow, flatten, subMatrix, cols)
import MLP.Config (readConf, mkDataSet, mkState)
import Control.Lens (view, use, (+=), (^.), (.=))
import Control.Monad (when, forM_, forM)
import qualified Data.Vector.Storable as V (snoc, sum)
-- import Control.Monad.State.Class
-- import Control.Monad.Reader.Class
import Control.Monad.Writer.Class (tell)
import PyF (fmt)
import qualified Data.Text.Lazy.IO as TIO (putStrLn)
import Data.Singletons
import Numeric.Natural

run :: FilePath -> IO ()
run fl = do
   conf <- readConf fl
   let i = conf ^. topology.numInputs
       hs = conf ^. topology.hiddenLayers
       o = conf ^. topology.numOutputs

   case toSing i of
      SomeSing (_ :: Sing i) -> 
         case toSing hs of
            SomeSing (_ :: Sing hs) ->
               case toSing o of
                  SomeSing (_ :: Sing o) -> do
                     let dt = mkDataSet @i @o conf
                     let st = mkState @i @hs @o
                     TIO.putStrLn ""


calculateError :: Vector R -> Vector R -> Double
calculateError expOut actOut = V.sum ((expOut - actOut) ** 2) / 2

trainOnSet :: App i hs o Double
trainOnSet = do
   trainset <- view (dataSet.trainSet)
   weights <- use weights

   errorRates <- forM trainset $ \pat -> do
      tell "forward start\n"
      let outputs = forwardPass (pat ^. input) weights
      tell "forward done\n"
      let expectedOut = pat ^. output
          deltas = backwardPass outputs expectedOut weights
      updateWeights deltas
      return $ calculateError (last outputs) expectedOut

   curError .= sum errorRates
   return $ sum errorRates


evaluateOnSet :: App i hs o ()
evaluateOnSet = do
   testset <- view (dataSet.testSet)
   weights <- use weights

   errorRates <- forM testset $ \pat -> do
      let actualOut = forwardPass (pat ^. input) weights
          expectedOut = pat ^. output
      return $ calculateError (last actualOut) expectedOut

   return ()

loop :: App i hs o () -> App i hs o ()
loop trainAction = do
   epochTotal <- view (params.epochs)
   epochNow <- use curEpoch

   when (epochNow < epochTotal) $ do
      trainAction

      curError <- use curError
      minError <- view (params.minErrorRate)

      when (curError > minError) $ do
         loop trainAction

train :: App i hs o ()
train = do
   tell "Starting Training process..."
   
   loop $ do
      epoch <- use curEpoch

      tell [fmt| Epoch {epoch:d} \n |]
      
      trainOnSet
      --evaluateOnSet

      curEpoch += 1