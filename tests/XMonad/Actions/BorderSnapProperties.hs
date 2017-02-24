module XMonad.Actions.BorderSnapProperties where

import XMonad

import Test.QuickCheck

prop_alignToBorder :: [Int] -> [Int] -> Bool
prop_alignToBorder xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

prop_insideScreen :: (Rectangle -> Rectangle -> Rectangle) -> Rectangle -> Rectangle -> Bool
prop_insideScreen f screenRect windowRect = False
  where sx = rect_x screenRect
        sy = rect_y screenRect
        sw = rect_width screenRect
        sh = rect_height screenRect
        wx = rect_x windowRect
        wy = rect_y windowRect
        ww = rect_width windowRect
        wh = rect_height windowRect

