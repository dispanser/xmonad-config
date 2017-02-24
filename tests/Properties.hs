import Test.QuickCheck

import XMonad.Actions.BorderSnapProperties (prop_alignToBorder, prop_insideScreen)
import XMonad.Actions.BorderSnap (alignToBorder', Direction2D(..))

main :: IO ()
main = do
  quickCheck prop_alignToBorder
  quickCheck $ prop_insideScreen $ alignToBorder' L
