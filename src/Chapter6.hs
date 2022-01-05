{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

newtype Name = Name {name :: String}
  deriving stock (Eq, Show, Generic)

data ShipCrew = ShipCrew
  { shipName :: Name,
    captain :: Name,
    firstMate :: Name,
    conscripts :: [Name]
  }
  deriving stock (Eq, Show, Generic)

-- Want: list the names of all crew members, including captain and
-- first mate ==> 'folding' is like 'lens', it can create custom Folds

-- folding :: Foldable f => (s -> f a) -> Fold s a

collectCrewMembers :: ShipCrew -> [Name]
collectCrewMembers ShipCrew {..} =
  [captain, firstMate] <> conscripts

crewMembers' :: Fold ShipCrew Name
crewMembers' = folding collectCrewMembers

myCrew :: ShipCrew
myCrew =
  ShipCrew
    { shipName = Name "Purple Pearl",
      captain = Name "Grumpy Roger",
      firstMate = Name "Long-John Bronze",
      conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
    }

-- Want: unpack and uppercase Names. Could map over the result above,
-- but it's possible with a combinator, too!

-- to :: (s -> a) -> Fold s a

-- myCrew ^.. crewMembers . #name . to (fmap toUpper)

crewNames :: Fold ShipCrew Name
crewNames =
  folding
    ( \s ->
        s ^.. #captain
          <> s ^.. #firstMate
          <> s ^.. #conscripts . folded
    )

-- Exercises -- Custom Folds

-- Filled-in blanks:
-- ["Yer", "a", "wizard", "Harry"] ^.. folded . folded == "YerawizardHarry"
-- [[1,2,3], [4,5,6]] ^.. folded . folding (take 2) == [1,2,4,5]
-- [[1,2,3], [4,5,6]] ^.. folded . to (take 2) == [[1,2], [4,5]]
-- ["bob", "otto", "hannah"] ^.. folded . to reverse == ["bob", "otto", "hannah"]
-- ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded == "cbafed"

-- [1..5] ^.. folded . to (100*) == [100, 200, 300, 400, 500]
-- (1, 2) ^.. both == [1,2]
-- [(1, "one"), (2, "two")] ^.. folded . folded == ["one", "two"]
-- (Just 1, Just 2, Just 3) ^.. each . folded == [1,2,3]
-- [Left 1, Right 2, Left 3] ^.. folded . folded == [2]
-- [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . both . folded == [1..8]
-- [1..4] ^.. folded . to (\n -> if even n then Right n else Left n) == [Left 1, Right 2, Left 3, Right 4]
-- [(1, (2, 3)), (4, (5, 6))] ^.. folded . to (\(a,(b,c)) -> [a,b,c]) . folded == [1..6]
-- [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\(a,b) -> a ^.. folded <> b ^.. folded) == [1,2]
-- [(1, "one"), (2, "two")] ^.. folded . folding (\(a,b) -> [Left a, Right b])
-- S.fromList ["apricots", "apples"] ^.. folded . to reverse . folded "selppaastocirpa"

-- [(12, 45, 66), (91, 123, 87)] ^.. folded . to (reverse . show . (\(_,b,_) -> b)) . folded == "54321"
-- [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding (\(a, b) -> if odd a then Nothing else Just b) = ["b", "d"]
