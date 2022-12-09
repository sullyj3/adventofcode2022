module V2Lite where
import           Utils (CardinalDir (..))

data V2 a = V2 a a
  deriving (Eq, Ord, Foldable, Functor, Show, Traversable)

instance Applicative V2 where
  pure x = V2 x x
  V2 f g <*> V2 x y = V2 (f x) (g y)

instance Num a ⇒ Num (V2 a) where
  (+) :: V2 a → V2 a → V2 a
  (+) = liftA2 (+)

  (*) :: V2 a → V2 a → V2 a
  (*) = liftA2 (*)

  abs :: V2 a → V2 a
  abs = fmap abs

  signum :: V2 a → V2 a
  signum = fmap signum

  fromInteger :: Integer → V2 a
  fromInteger = pure . fromInteger

  negate :: V2 a → V2 a
  negate = fmap negate

unvurry ∷ (a → a → b) → V2 a → b
unvurry f (V2 x y) = f x y

moveCardinal ∷ Int → CardinalDir → V2 Int → V2 Int
moveCardinal n dir x0 = case dir of
  U → x0 + V2 0    n
  D → x0 + V2 0    (-n)
  L → x0 + V2 (-n) 0
  R → x0 + V2 n    0

move1Cardinal ∷ CardinalDir → V2 Int → V2 Int
move1Cardinal = moveCardinal 1

chebyshevDist ∷ (Num a, Ord a) ⇒ V2 a → V2 a → a
chebyshevDist x y = unvurry max (abs (x - y))

