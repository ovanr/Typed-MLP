{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module MLP.MLP (
   run,
   train,
) where

import MLP.Types (MLP, params, epochs, minErrorRate, curEpoch, curError, runMLP, topology, dataSet, trainSet, testSet, input, output, weights)
import Numeric.LinearAlgebra (Vector, R, Matrix, (#>), asRow, flatten, subMatrix, cols)
import MLP.Config (readEnv, makeState)
import Control.Lens (view, use, (+=), (^.), (.=))
import Control.Monad (when, forM_, forM)
import qualified Data.Vector.Storable as V (snoc, sum)
-- import Control.Monad.State.Class
-- import Control.Monad.Reader.Class
import Control.Monad.Writer.Class (tell)
import PyF (fmt)
import qualified Data.Text.Lazy.IO as TIO (putStrLn)

run :: FilePath -> IO ()
run config = do
   env <- readEnv config
   let state = makeState (env ^. params.topology)
   let (_,resultState,log) = runMLP train env state
   TIO.putStrLn log

data ForwardPass (a :: [Nat]) where
   Nil :: ForwardPass '[]
   Outputs :: R o -> ForwardPass os -> ForwardPass (o ': os)

forwardPass :: R i -> MLPNetwork i o -> ForwardPass os
forwardPass = scanl calcOutput
   where
      calcOutput :: R i -> L o i -> R o
      calcOutput inVec weightLayer = weightLayer #> inVec

backwardPass :: [Vector R] -> Vector R -> [Matrix R] -> [Vector R]
backwardPass outputs expOut weightList = 
   reverse $ lastLayerDeltas : restDeltas 

   where
      lastLayerDeltas :: Vector R
      lastLayerDeltas = (last outputs - expOut) * calcDerivSigmoidNet (last outputs)
      restDeltas :: [Vector R]
      restDeltas = scanl calcDeltas lastLayerDeltas $ zip (reverse weightList) (reverse . init $ outputs)
      calcDerivSigmoidNet :: Vector R -> Vector R
      calcDerivSigmoidNet vec = vec*(1-vec)
      calcDeltas :: Vector R -> (Matrix R, Vector R) -> Vector R
      calcDeltas deltas (weight,output) = reduce (asRow deltas <> weight) * calcDerivSigmoidNet output
      reduce :: Matrix R -> Vector R
      reduce matrix = flatten $ subMatrix (0,0) (1,cols matrix - 1) matrix


updateWeights :: [Vector R] -> MLP ()
updateWeights x = return ()


calculateError :: Vector R -> Vector R -> Double
calculateError expOut actOut = V.sum ((expOut - actOut) ** 2) / 2

trainOnSet :: MLP Double
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


evaluateOnSet :: MLP ()
evaluateOnSet = do
   testset <- view (dataSet.testSet)
   weights <- use weights

   errorRates <- forM testset $ \pat -> do
      let actualOut = forwardPass (pat ^. input) weights
          expectedOut = pat ^. output
      return $ calculateError (last actualOut) expectedOut

   return ()

loop :: MLP () -> MLP ()
loop trainAction = do
   epochTotal <- view (params.epochs)
   epochNow <- use curEpoch

   when (epochNow < epochTotal) $ do
      trainAction

      curError <- use curError
      minError <- view (params.minErrorRate)

      when (curError > minError) $ do
         loop trainAction


train :: MLP ()
train = do
   tell "Starting Training process..."
   
   loop $ do
      epoch <- use curEpoch

      tell [fmt| Epoch {epoch:d} \n |]
      
      trainOnSet
      --evaluateOnSet

      curEpoch += 1