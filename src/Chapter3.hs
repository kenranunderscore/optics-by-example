{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter3 where

import Control.Lens

-- 3.3 Records

data Ship = Ship
  { _name :: String,
    _numCrew :: Int
  }
  deriving stock (Show)

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
    setter (Builder context build) s =
      let newBuild xs =
            if xs == context then s else build xs
       in Builder context newBuild

-- Laws:
-- 1) view tricky (set tricky s b)
--      == (_build $ set tricky s b) (_context b)
--      == s
-- 2) To show: set tricky (view tricky b) b == b
--    We have to show that the result's _build function has identical output to
--    b's _build for all inputs. Note that setting tricky doesn't change
--    the _context.
--
--    let c = _context b
--    (_build $ set tricky (view tricky b)) xs
--      == (_build $ set tricky (_build b c)) xs
--      == if xs == c then _build b c else _build b xs
--      == _build b xs
-- 3) The newBuild only ever 'changes' the value on the Builder's context, and
--    the latest 'change' wins.

-- Virtual Fields

data User = User
  { _firstName :: String,
    _lastName :: String,
    -- _username :: String,
    _email :: String
  }
  deriving stock (Show)

makeLenses ''User

-- Should return "fl@example.com" after removing the username field
u = User "first" "last" {- "fl" -} "fl@example.com" ^. username

username :: Lens' User String
username = lens getter setter
  where
    getter = _email
    setter user s = user {_email = s}

fullName :: Lens' User String
fullName = lens getter setter
  where
    getter User {..} = _firstName <> " " <> _lastName
    setter user name =
      let first = takeWhile (' ' /=) name
          last = tail $ dropWhile (' ' /=) name
       in user {_firstName = first, _lastName = last}

-- Self-Correcting Lenses

data ProducePrices = ProducePrices
  { _limePrice :: Float,
    _lemonPrice :: Float
  }
  deriving stock (Show)

setPrice price otherPrice =
  ( realPrice,
    if abs delta <= 0.5
      then otherPrice
      else (if delta < 0 then realPrice - 0.5 else realPrice + 0.5)
  )
  where
    realPrice = if price < 0 then 0 else price
    delta = otherPrice - realPrice

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
  where
    getter = _limePrice
    setter ProducePrices {..} price =
      uncurry ProducePrices $ setPrice price _lemonPrice

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
  where
    getter = _lemonPrice
    setter ProducePrices {..} price =
      uncurry ProducePrices $ setPrice price _limePrice
