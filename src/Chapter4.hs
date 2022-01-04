{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter4 where

import Control.Lens

-- Polymorphic Lenses

-- Lens (Vorpal x) (Vorpal y) x y

data Preferences a b = Preferences
  { _best :: a,
    _worst :: b
  }
  deriving stock (Show)

-- This changes only the first type variable, but not both of them at
-- the same time.
best :: Lens (Preferences a b) (Preferences a' b) a a'
best = lens getter setter
  where
    getter (Preferences a _) = a
    setter (Preferences a b) a' = Preferences a' b

-- Could use a tuple to change both at the same time though.
data Preferences' a = Preferences'
  { _best' :: a,
    _worst' :: a
  }
  deriving stock (Show)

bestAndWorst :: Lens (Preferences' a) (Preferences' b) (a, a) (b, b)
bestAndWorst = lens getter setter
  where
    getter (Preferences' b w) = (b, w)
    setter (Preferences' b w) (b', w') = Preferences' b' w'

-- Change type variable of this type via lens.
data Result e = Result
  { _lineNumber :: Int,
    _result :: Either e String
  }
  deriving stock (Show)

changeErr :: Lens (Result e) (Result e') (Either e String) (Either e' String)
changeErr = lens getter setter
  where
    getter (Result _ r) = r
    setter res x = res {_result = x}

-- Is it possible to change more than one type variable at a time?
-- Yes! Just have to focus on 'one thing' which is actually two
-- things.

-- This is basically id as a lens.
foo :: Lens s t s t
foo = lens getter setter
  where
    getter = id
    setter _ t = t

twoTypeVarsChanged = set foo (True, False) (3 :: Int, "abc")

-- Is it lawful? Yes!
-- 1) view foo (set foo x struct) == view foo x == x
-- 2) set foo (view foo struct) struct == view foo struct == struct
-- 3) set foo y (set foo x struct) == y == set foo y struct

-- Wanted: lens to change the type variable from a to b
data Predicate a = Predicate (a -> Bool)

-- Simply swapping out the whole field 'content' as before feels like
-- cheating...
predL :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predL = lens getter setter
  where
    getter (Predicate f) = f
    setter (Predicate f) g = Predicate g

-- Composing Lenses

data Player = Player
  deriving stock (Show)

data Wool = Wool
  deriving stock (Show)

data Sweater = Sweater
  deriving stock (Show)

data Item a = Item
  { _material :: a,
    _amount :: Int
  }
  deriving stock (Show)

makeLenses ''Item

weave Wool = Sweater

gameState :: (Player, Item Wool)
gameState = (Player, Item Wool 5)

-- Exercises

-- Blanks filled in:
-- view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))

-- fiveEightDomino :: Lens' Five Eight
-- mysteryDomino :: Lens' Eight Two -- filled in here
-- twoThreeDomino :: Lens' Two Three
-- dominoTrain :: Lens' Five Three
-- dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

-- Given signature:
-- Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
--   ==> Lens Platypus BabySloth Armadillo Hedgehog

-- The last one is a bit too tedious-looking
