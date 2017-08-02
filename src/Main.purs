module Main where

import Control.Monad.Eff.Console (log)
import Data.List.NonEmpty (length)
import Prelude (discard, show)
import UOM (lengthSum, massSum)

main = do 
  log (show massSum)
  log (show lengthSum)


