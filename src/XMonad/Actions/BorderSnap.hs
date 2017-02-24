module XMonad.Actions.BorderSnap
  ( Direction2D(..)
  , alignToBorder
  , alignToBorder'
  ) where

import XMonad
import XMonad.Util.Types (Direction2D(..))

alignToBorder :: Direction2D -> Window -> X ()
alignToBorder = undefined

-- given a direction and a screen rectangle and a window rectangle,
-- compute a new window rectangle such that it is aligned with the
-- border of the screen rectangle
alignToBorder' :: Direction2D -> Rectangle -> Rectangle -> Rectangle
alignToBorder' L Rectangle { rect_x = srx } wr@Rectangle { rect_x = wx  } = wr { rect_x = srx }
alignToBorder' R Rectangle { rect_x = srx, rect_width = srw } wr@Rectangle { rect_x = wx, rect_width = ww } =
  wr { rect_x = srx + ( fromIntegral (srw - ww) ) }
alignToBorder' U Rectangle { rect_y = sry } wr@Rectangle { rect_y = wy  } = wr { rect_y = sry }
alignToBorder' D Rectangle { rect_y = sry, rect_height = srh } wr@Rectangle { rect_y = wy, rect_height = wh } =
  wr { rect_y = sry + ( fromIntegral (srh - wh) ) }

align :: ( Rectangle -> Rectangle -> Rectangle ) -> Window -> X ()
align = undefined

-- getSnap horiz collidedist d w = do
--     wa <- io $ getWindowAttributes d w
--     screen <- W.current <$> gets windowset
--     let sr = screenRect $ W.screenDetail screen
--         wl = W.integrate' . W.stack $ W.workspace screen
