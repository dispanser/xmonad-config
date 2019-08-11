module XMonad.Actions.Resubmap (
                             -- * Usage
                             -- $usage
                             resubmap,
                             resubmapDefault,
                             resubmapDefaultWithKey
                            ) where
import Data.Bits
import Data.Maybe (maybe, fromMaybe)
import XMonad hiding (keys)
import qualified Data.Map as M
import Control.Monad.Fix (fix)

-- | Given a 'Data.Map.Map' from key bindings to X () actions, return
--   an action which waits for a user keypress and executes the
--   corresponding action, or does nothing if the key is not found in
--   the map.
resubmap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
resubmap = resubmapDefault (return ())

-- | Like 'submap', but executes a default action if the key did not match.
resubmapDefault :: X () -> M.Map (KeyMask, KeySym) (X ()) -> X ()
resubmapDefault = resubmapDefaultWithKey . const

-- | Like 'submapDefault', but sends the unmatched key to the default
-- action as argument.
resubmapDefaultWithKey :: ((KeyMask, KeySym) -> X ())
                     -> M.Map (KeyMask, KeySym) (X ())
                     -> X ()
resubmapDefaultWithKey defAction keys = do
    XConf { theRoot = root, display = d } <- ask

    _ <- io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
    _ <- io $ grabPointer d root False buttonPressMask grabModeAsync grabModeAsync
                     none none currentTime

    (m, s) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent d (keyPressMask .|. buttonPressMask) p
        ev <- getEvent p
        case ev of
          KeyEvent { ev_keycode = code, ev_state = m } -> do
            keysym <- keycodeToKeysym d code 0
            if isModifierKey keysym
                then nextkey
                else return (m, keysym)
          _ -> return (0, 0)
    -- Remove num lock mask and Xkb group state bits
    m' <- cleanMask $ m .&. ((1 `shiftL` 12) - 1)

    io $ ungrabPointer d currentTime
    io $ ungrabKeyboard d currentTime

    -- everything above is just grabbing keyboard etc, now we're actually
    -- checking our map for the shortcut, or fall back to the other one
    fromMaybe ( defAction (m', s) ) $ M.lookup (m', s) keys
    -- maybe ( defAction (m', s) ) (\a -> a >> resubmapDefaultWithKey defAction keys ) ( M.lookup (m', s) keys )
    -- case maybeAction of
    --   -- Just action -> xmessage "oops" >> action >> resubmapDefaultWithKey defAction keys
    --   Just action -> action
    --   Nothing     -> defAction (m', s)

xmessage :: String -> X ()
xmessage msg = spawn $ "xmessage " ++ msg
