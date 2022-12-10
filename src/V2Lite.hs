module V2Lite where
import           Utils (CardinalDir (..))

data V2 a = V2 a a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

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

scale ∷ Num a ⇒ a → V2 a → V2 a
scale k = fmap (k *)

(*^) ∷ Num a ⇒ a → V2 a → V2 a
(*^) = scale

unitCardinal ∷ CardinalDir → V2 Int
unitCardinal = \case
  U → V2 0    1
  D → V2 0    (-1)
  L → V2 (-1) 0
  R → V2 1    0

moveCardinal ∷ Int → CardinalDir → V2 Int → V2 Int
moveCardinal n dir x0 = x0 + n *^ unitCardinal dir

move1Cardinal ∷ CardinalDir → V2 Int → V2 Int
move1Cardinal = moveCardinal 1

-- Metrics
euclideanDist ∷ Floating a ⇒ V2 a → V2 a → a
euclideanDist (V2 x1 y1) (V2 x2 y2) = sqrt $ (x2 - x1)**2 + (y2 - y1)**2

manhattanDist ∷ Num a ⇒ V2 a → V2 a → a
manhattanDist (V2 x1 y1) (V2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

chebyshevDist ∷ (Num a, Ord a) ⇒ V2 a → V2 a → a
chebyshevDist x y = unvurry max (abs (x - y))
