{-# LANGUAGE GADTs, DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications, TypeOperators, RankNTypes, FlexibleContexts #-}

import Numeric.LinearAlgebra.Static
import GHC.TypeLits
import Data.Proxy
import Data.Maybe (mapMaybe)
import Data.Monoid (getSum, Sum(..))
import Data.Coerce
import MLP.Types (Pattern(..))
import MLP.Network (Net(..), AllCon, ArrList(..), Weights(..), MLP(..), Learn(..), fromScalarR, netUpd)
import Data.Singletons

data EncPat where
    EncPat :: forall i o. (KnownNat i, KnownNat o) => Pattern i o -> EncPat

data EncNet where
    EncNet :: forall i hs o. (SingI (Topo (Net i hs o)), Learn (Net i hs o), AllCon KnownNat (i ': o ': hs), MLP (Net i hs o)) => Net i hs o -> EncNet

readMat :: IO (EncPat, EncNet)
readMat = do
    i <- read <$> (putStr "inputs: " >> getLine)
    o <- read <$> (putStr "outputs: " >> getLine)

    let Just someNatI = someNatVal (ceiling i)
        Just someNatO = someNatVal (ceiling o)

    case someNatI of
       SomeNat (_ :: Proxy i) ->
         case someNatO of
            SomeNat (_ :: Proxy o) ->
               let n = EncNet . SLayer $ Weights (matrix @o @i [1..(i * o)] ) (vector @o [1..o])
                   p = EncPat $ Pattern (vector @i [1..i]) (vector @o [1..o])
                 in pure (p,n)

io :: forall i o hs. (KnownNat i, KnownNat o) => Net i hs o -> [Integer]
io _ = map fromIntegral . fromSing $ sing @'[i, o]

main :: IO ()
main = do
    (EncPat (Pattern i o), EncNet n) <- readMat
    let arrl = mkDeltas i o n

    case netUpd 1.0 i arrl n of
        SLayer l -> print l
        _ -> print ""