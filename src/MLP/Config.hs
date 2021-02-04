{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module MLP.Config (
   readEnv,
   makeState
) where

import Data.Aeson (eitherDecode)
import Numeric.LinearAlgebra.Data (loadMatrix, (><), Matrix, R)
import Control.Lens ((^.), foldlOf, (%~), (&))
import MLP.DataSet (toPat, normPat)
import MLP.Types (trainFile, 
                  testFile, 
                  topology, 
                  normalise, 
                  topoFold,
                  DataSet(..), 
                  Parameters(..),
                  MLPEnv(..), 
                  MLPState(..),
                  Topology,
                  MonadFileSystem(..))
import Control.Monad ((=<<))
import System.FilePath (splitFileName, takeFileName, (</>))

liftEither :: (MonadFail m) => Either String b -> m b
liftEither (Left err) = fail err
liftEither (Right x) = return x

normPath :: FilePath -> FilePath -> FilePath
normPath fl1 fl2 = let (baseDir,_) = splitFileName fl1
                     in baseDir </> takeFileName fl2

readEnv :: (MonadFail m, MonadFileSystem m) => FilePath -> m MLPEnv
readEnv config = do
      params' <- liftEither.eitherDecode =<< readFileM config

      let params = params' & trainFile %~ normPath config
                           & testFile %~ normPath config

      rawSet <- readMatrixM (params ^. trainFile)
      let topo = params ^. topology
      trainSet <- liftEither (makeSet params rawSet)

      rawSet <- readMatrixM (params ^. testFile)
      testSet <- liftEither (makeSet params rawSet)

      return $ MLPEnv (DataSet trainSet testSet) params

   where
      makeSet params rawSet = do
         let norm = params ^. normalise
             topo = params ^. topology
         set <- parseDataSet topo rawSet
         if norm then
            Right (normDataSet set)
         else
            Right set

makeState :: Topology -> MLPState
makeState topo = let weights = foldlOf topoFold makeWeight [] topo
                  in MLPState weights 0 0
   where 
      makeWeight :: [Matrix R] -> (Int,Int) -> [Matrix R]
      makeWeight acc (x,y) = let matrix = (><) (x+1) y [0..]
                              in acc ++ [matrix]