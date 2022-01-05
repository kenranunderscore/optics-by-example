{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter6 where

import Control.Lens
import Data.Generics.Labels
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics (Generic)

data Role
  = Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving stock (Show, Eq, Ord)

data CrewMember = CrewMember
  { name :: String,
    role :: Role,
    talents :: [String]
  }
  deriving stock (Show, Eq, Ord, Generic)

type Crew = S.Set CrewMember

roster :: Crew
roster =
  S.fromList
    [ CrewMember "Grumpy Roger" Gunner ["Juggling", "Arbitrage"],
      CrewMember "Long-John Bronze" PowderMonkey ["Origami"],
      CrewMember "Salty Steve" PowderMonkey ["Charcuterie"],
      CrewMember "One-eyed Jack" Navigator []
    ]

-- folded :: Foldable f => Fold (f a) a

crewMembers :: Fold Crew CrewMember
crewMembers = folded

crewRoles :: Fold Crew Role
crewRoles = folded . #role

-- both :: Bitraversable r => Traversal (r a a) (r b b) a b

-- Exercises - Simple Folds

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- beastSizes ^.. folded == beastSizes
-- beastSizes ^.. folded . folded == ["Sirens", "Kraken", "Ogopogo"]  (yields snd for tuples)
-- beastSizes ^.. folded . folded . folded == "SirensKrakenOgopogo"
-- beastSizes ^.. folded . _2 == same as 2nd example
-- toListOf (folded . folded) [[1,2,3], [4,5,6]] == [1,2,3,4,5,6]
-- toListOf (folded . folded) (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
--   == "CaptainFirst Mate"  (first folded -> map values; 2nd concatenates)
-- ("Hello", "It's me") ^.. both . folded == "HelloIt's me"
-- ("Why", "so", "serious?") ^.. each == ["Why", "so", "serious?"]

quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("Why", "so", "serious?"), ("This", "is", "SPARTA")]

-- quotes ^.. each . each . each == "Whysoserious?ThisisSPARTA"

-- toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')] == [1,2,3]
--   => folded . _1 :: Fold [(Integer, Char)] Integer

-- toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"]) == ["one", "two", "three"]
--   => _2 . folded :: Fold (Bool, S.Set String) String

-- toListOf (folded . folded) (M.fromList [("Jack", "Captain"), ("Will", "First Mate")]) == "CaptainFirst Mate"
--   => folded . folded :: Fold (M.Map String String) Char

-- Filled-in blanks:
-- [1, 2, 3] ^.. each == [1, 2, 3]
-- ("Light", "Dark") ^.. folded . _1 == ["Light"]
-- [("Light", "Dark"), ("Happy", "Sad")] ^.. each . both == ["Light", "Dark", "Happy", "Sad"] (or each.each)
-- [("Light", "Dark"), ("Happy", "Sad")] ^.. each . _1 == ["Light", "Happy"]
-- [("Light", "Dark"), ("Happy", "Sad")] ^.. each . _2 . each == "DarkSad"
-- ("Bond", "James", "Bond") ^.. each == ["Bond", "James", "Bond"]
