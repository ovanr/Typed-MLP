{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables #-}

import qualified Numeric.LinearAlgebra.Static as S
import Numeric.LinearAlgebra.Data
import GHC.TypeLits
import Data.Proxy
import Data.Maybe (mapMaybe)
import Data.Monoid (getSum, Sum(..))
import Data.Coerce

data Pattern (i :: Nat) (o :: Nat) = Pattern {
   input :: [S.R i],
   output :: [S.R o]
}

handlePs :: (KnownNat i, KnownNat o) => Pattern i o -> Integer
handlePs (Pattern i o) = getSum $ foldMap (coerce . natVal) i
--readMat :: (KnownNat n, KnownNat n1) => IO (Pattern n n1)
readMat = do
    m <- loadMatrix "matrix.dat"
    inputs <- read <$> getLine
    let Just someNatI = someNatVal (fromIntegral inputs)
        Just someNatO = someNatVal (fromIntegral $ cols m - (fromIntegral inputs))

    let ins = toRows (takeColumns inputs m)
        outs = toRows (dropColumns inputs m)

    case someNatI of
       SomeNat (_ :: Proxy i) ->
         case someNatO of
            SomeNat (_ :: Proxy o) ->
               let insp = mapMaybe (\v -> S.create v :: Maybe (S.R i)) ins
                   outsp = mapMaybe (\v -> S.create v :: Maybe (S.R o)) outs
                in return $ handlePs $ Pattern insp outsp
