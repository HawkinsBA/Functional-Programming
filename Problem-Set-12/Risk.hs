{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.Bifunctor (first)
import Data.List
import Data.Typeable

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

-- Exercise 2

rolls :: Int -> Rand StdGen [DieValue]
rolls n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  atkRolls <- rolls (attackers bf)
  defRolls <- rolls (defenders bf)
  let adPairs  = zip (sort atkRolls) (sort defRolls)
      atkLoss  = length (filter (\x -> snd x > fst x) adPairs)
      defLoss  = length (filter (\x -> fst x > snd x) adPairs)
  return (Battlefield ((attackers bf) - atkLoss) ((defenders bf) - defLoss))
