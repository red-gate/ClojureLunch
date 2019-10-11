module MachineLearning where

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import GHC.Generics (Generic)
import GHC.TypeNats

import Data.Vector

type ℝ = Double 

newtype Dim (n :: Nat) t = Dim t
  deriving (Show, Generic)

newtype Matrix a = Vector (Vector a)

newtype R n = R (Dim n (Vector ℝ))
newtype L m n = L (Dim m (Dim n (Matrix ℝ)))


vector :: (KnownNat n) => [Double] -> Maybe (R n)
vector xs = case natVal (Proxy :: Proxy n) of 
    size ->  if (Prelude.length xs) == (fromIntegral size) then Just (R (Dim (fromList xs))) else Nothing