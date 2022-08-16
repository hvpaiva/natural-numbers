module Natural
  ( Nat (..),
    toString,
    toNatural,
  )
where

data Nat = Zero | Succ Nat deriving (Eq, Show)

toString :: Nat -> String
toString = show . fromEnum

toNatural :: Int -> Nat
toNatural = toEnum

instance Semigroup Nat where
  Zero <> n = n
  n <> Zero = n
  (Succ n) <> (Succ n') = Succ (n <> n')

instance Monoid Nat where
  mempty = Zero

instance Num Nat where
  Zero + n = n
  n + Zero = n
  (Succ n) + n' = Succ (n + n')

  Zero - _ = Zero
  n - Zero = n
  (Succ n) - (Succ n') = n - n'

  Zero * _ = Zero
  _ * Zero = Zero
  (Succ n) * n' = n * n' + n'

  abs = id

  signum Zero = Zero
  signum _ = Succ Zero

  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n - 1))

instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero (Succ _) = LT
  compare (Succ _) Zero = GT
  compare (Succ n) (Succ n') = compare n n'

instance Enum Nat where
  toEnum n
    | n <= 0 = Zero
    | otherwise = Succ (toEnum (n - 1))

  fromEnum Zero = 0
  fromEnum (Succ n) = 1 + fromEnum n

  enumFrom n = n : enumFrom (Succ n)

  enumFromThen n m = n : enumFromThen m (m + n)

  enumFromTo n m = takeWhile (<= m) (enumFrom n)

  enumFromThenTo n m n' = takeWhile (<= m) (enumFromThen n n')

instance Bounded Nat where
  minBound = Zero
  maxBound = Succ (maxBound :: Nat)
