{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Section3 where

import Control.Lens

-- 3.3 Records

data Ship = Ship
  { _name :: String,
    _numCrew :: Int
  }
  deriving (Show)

name :: Lens' Ship String
name = lens getter setter
  where
    getter = _name
    setter ship newName = ship {_name = newName}

-- Laws

data Err
  = ReallyBadError {_msg :: String}
  | ExitCode {_code :: Int}

-- msg :: Lens' Err String
-- msg = lens getter setter
--   where
--     getter = \case
--       ReallyBadError message -> message
--       ExitCode _ -> ""
--     setter (ReallyBadError _) newMessage = ReallyBadError newMessage
--     setter exitCode _ = exitCode

-- msg :: Lens' Err String
-- msg = lens getter setter
--   where
--     getter = \case
--       ReallyBadError message -> message
--       exitCode -> show exitCode
--     setter (ReallyBadError _) newMessage = ReallyBadError newMessage
--     setter exitCode _ = exitCode

-- | A lens which violates all three laws.
lawless :: Lens' Int Int
lawless = lens id (\value _newValue -> value + 1)

-- | Viewing after setting returns the just-set value.
testSetGet :: Eq b => Lens' a b -> a -> b -> Bool
testSetGet l struct val = view l (set l val struct) == val

-- | Setting what's viewed doesn't change the structure.
testGetSet :: Eq a => Lens' a b -> a -> Bool
testGetSet l struct = set l (view l struct) struct == struct

-- | Setting twice is the same as setting only the second time.
testSetSet :: Eq a => Lens' a b -> a -> b -> b -> Bool
testSetSet l struct val1 val2 =
  set l val2 (set l val1 struct) == set l val2 struct

-- False
firstLawHoldsForLawless = testSetGet lawless 5 3

-- False
secondLawHoldsForLawless = testGetSet lawless 3

-- False
thirdLawHoldsForLawless = testSetSet lawless 0 3 5

-- Next: Find lawful lens of type Lens' Builder String
data Builder = Builder
  { _context :: [String],
    _build :: [String] -> String
  }

tricky :: Lens' Builder String
tricky = lens getter setter
  where
    getter (Builder context build) = build context
    setter builder@(Builder context build) s =
      let newBuild xs =
            if xs == context then s else build xs
       in builder {_build = newBuild}

-- Laws:
-- 1) view tricky (set tricky "foo" b)
--      == (_build $ set tricky "foo" b) (_context b)
--      == "foo"
-- 2) set tricky (view tricky b) b
--      == set tricky ((_build b) (_context b)) b
--      == TODO
