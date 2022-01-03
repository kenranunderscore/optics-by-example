{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter2 where

import Control.Lens
import Data.Bits.Lens (bitAt)
import Data.Char (toUpper)
import Data.Data.Lens (biplate)
import Data.Function ((&))
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Lens (negated)

type Country = Text

type Street = Text

type Name = Text

data Address = Address
  { _country :: Country,
    _street :: Street
  }
  deriving stock (Show)

makeLenses ''Address

data Person = Person
  { _address :: Address,
    _name :: Name
  }
  deriving stock (Show)

makeLenses ''Person

x = view (address . country) $ Person (Address "DE" "Foo-Str.") "Peter"

y = set _3 False (1, 2, 3)

z =
  sumOf (folded . _2 . _Left) $
    [(True, Left 10), (False, Right "pepperoni"), (True, Left 20)]

s =
  let stories = ["This one time at band camp", "Nuff said.", "This is a short story"]
   in over
        (traversed . filtered ((> 10) . length))
        (\story -> take 10 story ++ "...")
        stories

-- beside
addRightsSubtractLefts = sumOf (folded . beside negated id) [Left 1, Right 10, Left 2, Right 20]

-- capitalize all the words in a string
capitalizedWords = "why is a raven like a writing desk" & worded . _head %~ toUpper

-- 'discover' all integers to multiply them by 100
allIntsTimes100 =
  let x :: (Maybe Integer, Either (String, [Integer]) ()) = (Just 3, Left ("hello", [13, 15, 17]))
   in x & biplate *~ (100 :: Integer)

-- reverse order of even numbers in a sequence
reversedEvens = [1, 2, 3, 4, 5, 6, 7, 8] & partsOf (traversed . filtered even) %~ reverse

-- flip 2nd bit of each number
flippedBits = [1 :: Int, 2, 3, 4] & traversed . bitAt 1 %~ not

-- show a prompt for each question, then fill with user input
-- getAnswers :: IO (String, String, String)
getAnswers =
  let prompts = ("Who are you?", "What's your quest?", "What's your favourite color?")
   in prompts & each %%~ (\p -> putStrLn p >> getLine)
