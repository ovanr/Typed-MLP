{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module MLP.Types (
   Pattern(..),
   input,
   output,
   DataSet(..),
   trainSet,
   testSet,
   Parameters(..),
   topology,
   learningRate,
   decayLearningRate,
   normalise,
   epochs,
   minErrorRate,
   trainFile,
   testFile,
   verbose,
   Topology(..),
   numInputs,
   hiddenLayers,
   numOutputs,
   topoFold,
   dataSet,
   params,
   State(..),
   network,
   curEpoch,
   curError,
   App(..),
   runApp,
   zipWithPat,
   MonadFileSystem(..)
)  where

import Control.Lens (makeLenses, (^..), (^.), ix, folding, Fold)
import Data.Aeson (FromJSON(..), withObject, withArray, (.:), (.:?), (.!=))
import Numeric.LinearAlgebra.Data (Vector, Matrix, loadMatrix)
import Numeric.LinearAlgebra.Static (R, L, zipWithVector)
import Data.Aeson.Lens (key, values, _Integral)
import Data.Aeson.TH (deriveToJSON, defaultOptions, Options(..))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Control.Monad.State.Lazy (StateT(..), runStateT)
import Control.Monad.Writer.Lazy (Writer(..), runWriter)
import qualified Data.Text.Lazy as T 
import Data.Functor.Identity (Identity)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as B
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (MonadTrans)
import GHC.Generics (Generic)
import GHC.TypeLits
import MLP.Network (Net(..), AllCon, Learn(..), MLP(..))
import Data.Singletons (SingI(..))
import Numeric.Natural

data Pattern (i :: Nat) (o :: Nat) = Pattern {
   _input :: R i,
   _output :: R o
}

instance (KnownNat i, KnownNat o) => Show (Pattern i o) where
   show (Pattern i o) = "Pattern " ++ show i ++ show o

-- rank 2 function
apply :: (forall n. R n -> R n -> R n) -> Pattern i o -> Pattern i o -> Pattern i o
apply f (Pattern in1 out1) (Pattern in2 out2) = Pattern (f in1 in2) (f out1 out2)

zipWithPat :: (KnownNat i, KnownNat o) => (Double -> Double -> Double) -> Pattern i o -> Pattern i o -> Pattern i o
zipWithPat f (Pattern i1 o1) (Pattern i2 o2) =
   Pattern (zipWithVector f i1 i2) (zipWithVector f o1 o2)

instance Num (Pattern i o) where
   (-) p1 p2 = apply (-) p1 p2
   (+) p1 p2 = apply (+) p1 p2
   (*) p1 p2 = apply (*) p1 p2
   abs (Pattern inp out) = Pattern (abs inp) (abs out)
   signum (Pattern inp out) = Pattern (signum inp) (signum out)
   fromInteger num = Pattern (fromInteger num) (fromInteger num)

instance Fractional (Pattern i o) where
   (/) p1 p2 = apply (/) p1 p2
   recip (Pattern inp out) = Pattern (recip inp) (recip out)
   fromRational rat = Pattern (fromRational rat) (fromRational rat)
   
data DataSet i o = DataSet {
   _trainSet :: [Pattern i o],
   _testSet :: [Pattern i o]
} deriving Show

data Topology = Topology {
   _numInputs :: Natural,
   _hiddenLayers :: [Natural],
   _numOutputs :: Natural
} deriving Show

data Parameters = Parameters {
   _topology :: Topology,
   _learningRate :: Double,
   _decayLearningRate :: Bool,
   _epochs :: Natural,
   _minErrorRate :: Double,
   _normalise :: Bool,
   _trainFile :: FilePath,
   _testFile :: FilePath,
   _verbose :: Bool
} deriving Show

data State i hs o = State {
   _dataSet :: DataSet i o,
   _params :: Parameters,
   _network :: Net i hs o,
   _curEpoch :: Natural,
   _curError :: Double
}

makeLenses ''Pattern
makeLenses ''DataSet
makeLenses ''Topology

topoFold :: Fold Topology Natural
topoFold = folding $ \s -> (s ^. numInputs) : (s ^. hiddenLayers) ++ (s ^.. numOutputs)

makeLenses ''Parameters
makeLenses ''State

newtype App i hs o a = App { unApp :: StateT (State i hs o) (Writer T.Text) a }
   deriving (Functor, 
             Applicative, 
             Monad,
             MonadWriter T.Text,
             MonadState (State i hs o))

runApp :: App i hs o a -> State i hs o -> ((a, State i hs o), T.Text)
runApp app = runWriter . runStateT (unApp app)

class (Monad m) => MonadFileSystem m where
   readFileM :: FilePath -> m B.ByteString
   readMatrixM :: FilePath -> m (Matrix Double)

instance MonadFileSystem IO where
   readFileM fl = B.readFile fl
   readMatrixM = loadMatrix

$(deriveToJSON defaultOptions{fieldLabelModifier=drop 1} ''Topology)
$(deriveToJSON defaultOptions{fieldLabelModifier=drop 1} ''Parameters)

instance FromJSON Topology where
   parseJSON = withObject "Topology" (\o -> do
      inputs <- o .: "numInputs"
      guard (inputs > 0)

      outputs <- o .: "numOutputs"
      guard (outputs > 0)

      let hidden =  o ^.. ix "hiddenLayers".values._Integral

      return $ Topology inputs hidden outputs)

instance FromJSON Parameters where
   parseJSON = withObject "Parameters" (\o -> do
      _topology <- o .: "topology" 
      _learningRate <- o .:? "learningRate" .!= 0.6
      _decayLearningRate <- o .:? "decayLearningRate" .!= False
      _normalise <- o .:? "normalise" .!= False
      _epochs <- o .:? "epochs" .!= 1000
      _minErrorRate <- o .: "minErrorRate"
      _trainFile <- o .: "trainFile"
      _testFile <- o .: "testFile"

      _verbose <- o .:? "verbose" .!= False

      return $ Parameters {..})