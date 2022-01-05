{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter5 where

import Control.Lens
import Data.Char (toUpper)
import Data.Generics.Labels
import GHC.Generics (Generic)

data Gate = Gate
  { open :: Bool,
    oilTemp :: Float
  }
  deriving stock (Eq, Show, Generic)

data Army = Army
  { archers :: Int,
    knights :: Int
  }
  deriving stock (Eq, Show, Generic)

data Kingdom = Kingdom
  { name :: String,
    army :: Army,
    gate :: Gate
  }
  deriving stock (Eq, Show, Generic)

duloc :: Kingdom
duloc = Kingdom "Duloc" (Army 22 14) (Gate True 10)

goalA =
  duloc
    & #name .~ "Duloc: a perfect place"
    & #army . #knights .~ 42
    & #gate . #open .~ False

goalAHard =
  duloc
    & #name <>~ ": a perfect place"
    & #army . #knights +~ 28
    & #gate . #open &&~ False

goalB =
  duloc
    & #name .~ "Dulocinstein"
    & #army . #archers .~ 17
    & #army . #knights .~ 26
    & #gate . #oilTemp .~ 100

goalBHard =
  duloc
    & #name <>~ "instein"
    & #army . #archers -~ 5
    & #army . #knights +~ 12
    & #gate . #oilTemp ^~ 2

goalC =
  duloc
    & #name <<>~ ": Home"
    & _2 . #name <>~ " of the talking Donkeys"
    & _2 . #gate . #oilTemp //~ 2

-- (False, "opossums") & _1 ||~ True

-- ((True, "Dudley"), 55.0)
--   & _1 . _2 <>~ " - the worst"
--   & _2 -~ 15
--   & _2 //~ 2
--   & _1 . _2 %~ map toUpper
--   & _1 . _1 &&~ False
