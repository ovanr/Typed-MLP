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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

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
import qualified Data.Text as T 
import Data.Functor.Identity (Identity)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as B
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.State.Class (MonadState(..))
import GHC.TypeLits ( KnownNat, Nat )
import MLP.Network (Net(..), AllCon, Learn(..), MLP(..))
import Data.Singletons (SingI(..))
import Numeric.Natural ( Natural )
import qualified Data.Text.IO as TIO (putStrLn)

data Pattern (i :: Nat) (o :: Nat) = Pattern {
   _input :: R i,
   _output :: R o
} deriving Show

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
   _network :: !(Net i hs o),
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

type Log = [T.Text]
newtype App (i :: Nat) (hs :: [Nat]) (o :: Nat) (a :: *) = App {
   runApp ::  State i hs o -> ((a, State i hs o), Log)
}

instance Functor (App i hs o) where
   fmap f (App app) = 
      App $ \s ->
         let ((!a,!s'),!l) = app s
            in ((f a,s'),l)

instance Applicative (App i hs o) where
   pure a = App $ \s -> ((a,s), mempty)

   (App fab) <*> (App fa) =
      App $ \s ->
         let ((!ab,!s'),!l) = fab s
             ((a, s''), l') = fa s'
            in ((ab a, s''), l <> l')

instance Monad (App i hs o) where
   return = pure 

   (App ma) >>= amb =
      App $ \s ->
         let ((!a, !s'), !l) = ma s
             ((b, s''), l') = runApp (amb a) s'
            in ((b, s''), l <> l')

instance MonadWriter Log (App i hs o) where
   writer (!a, !l) = App $ \s -> ((a,s),l)
   tell !l = App $ \s -> (((),s),l)
   listen (App app) =
      App $ \s ->
         let ((!a,!s''),!l) = app s 
            in (((a,l),s''),l)

   pass (App app) = 
      App $ \s ->
         let (((!a,!f),!s'),!l) = app s 
            in ((a,s'),l)

instance MonadState (State i hs o) (App i hs o) where
   get = App $ \s -> ((s,s),mempty)
   put s = App . const $ (((), s), mempty)
   state app = App ((,mempty).app)

class (Monad m, MonadFail m) => MonadFileSystem m where
   readFileM :: FilePath -> m B.ByteString
   readMatrixM :: FilePath -> m (Matrix Double)
   printText :: T.Text -> m ()

instance MonadFileSystem IO where
   readFileM fl = B.readFile fl
   readMatrixM = loadMatrix
   printText = TIO.putStrLn

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