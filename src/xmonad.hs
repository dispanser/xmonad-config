import qualified Data.Map as M

import XMonad
import XMonad.ManageHook
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Actions.TagWindows

import XMonad.Hooks.ManageHelpers

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing

import XMonad.Util.NamedScratchpad

type TagKey = (Char, KeySym)

type Tag = Char

myTerminal, myBrowser :: String
myTerminal = "urxvt"
myBrowser  = "vimb"

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "vimb" --> doRectFloat rightBarRect
  , title =? "xmessage" --> doRectFloat centeredRect
  ] <+>
    namedScratchpadManageHook scratchpads

scratchpads :: [NamedScratchpad]
scratchpads =
    [ shellScratchpad "htop"   ( customFloating $ centeredRect )
    , tmuxScratchpad "_mail"   ( customFloating $ centeredRect )
    , tmuxScratchpad "hud"     ( customFloating $ upperBarRect )
    , NS "chromium" "chromium" (className =? "Chromium") ( customFloating $ rightBarRect )
    ]

tmuxScratchpad :: String -> ManageHook -> NamedScratchpad
tmuxScratchpad session = NS session command (resource =? session)
  where command = "urxvt -name " ++ session ++ " -e zsh -i -c \"tas " ++ session ++ "\""

shellScratchpad :: String -> ManageHook -> NamedScratchpad
shellScratchpad session = NS session command (resource =? session)
  where command = "urxvt -name " ++ session ++ " -e " ++ session

centeredRect = W.RationalRect 0.2 0.2 0.6 0.6
upperBarRect = W.RationalRect 0.0 0.0 1.0 0.3
rightBarRect = W.RationalRect 0.4 0.6 1.0 1.0

-- explicit list of tags
tags :: [Tag]
tags = [ 'e' -- editor
       , 't' -- test runner (or anything else used for validation / checks
       , 'b' -- build
       , 'p' -- projects main terminal
       , 'd' -- project-related documentation
       , 'o' -- org mode: project-related org or similar
       ]

-- simple thing that checks all potential sources for keybindings for our main mask:
-- tags, copyBindings, globalTags, named scratchpads, mainBindings
-- reports any key that is used multiple times
validateBindings :: Bool
validateBindings = True

keyToCode :: M.Map Char KeySym
keyToCode = M.fromList $ zip (['a' .. 'z'] ++ ['0' .. '9']) ([xK_a .. xK_z] ++ [xK_0 .. xK_9])

resizeStepSize :: Dimension
resizeStepSize = 120

myModMask, tagSelectMask, tagToggleMask, workspaceMask :: ButtonMask
myModMask           = mod5Mask
myShiftMask         = myModMask .|. shiftMask
myControlMask       = myModMask .|. controlMask
myAltMask           = myModMask .|. mod1Mask
tagSelectMask       = myModMask
tagToggleMask       = tagSelectMask .|. mod4Mask
workspaceMask       = myModMask

main :: IO ()
main = xmonad $ defaultConfig
  { borderWidth        = 2
  , modMask            = myModMask
  , terminal           = myTerminal
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook
  , keys               = myKeys
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00" }

myLayoutHook = noBorders
        . smartSpacing 1
        . mkToggle (NOBORDERS ?? NBFULL ?? EOT)
        $ myLayout

myLayout = simplestFloat ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 3/6
     delta   = 5/100

-- submap to trigger / start applications
appSubmap :: M.Map ( ButtonMask, KeySym ) ( X () )
appSubmap = M.fromList
  [ ( (0, xK_x), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  , ( (0, xK_t), spawn myTerminal)
  , ( (0, xK_b), spawn myBrowser)
  , ( (0, xK_e), spawn "emacs")
  , ( (0, xK_v), spawn "gvim")
  , ( (0, xK_p), spawn "pidgin")
  ]

-- submaps for various prompt-based actions
promptSubmap :: M.Map ( ButtonMask, KeySym ) ( X () )
promptSubmap = M.fromList
  [ ( (0, xK_b), spawn "/home/pi/bin/browser")
  , ( (0, xK_p), spawn "passmenu")
  , ( (0, xK_d), spawn "dmenu_run")
  , ( (0, xK_g), goToSelected defaultGSConfig) -- %! Push window back into tiling
  ]

-- submaps for less common window operations
windowSubmap :: M.Map ( ButtonMask, KeySym ) ( X () )
windowSubmap = M.fromList
  [ ( (0, xK_s), withFocused $ windows . W.sink)
  , ( (0, xK_f), withFocused float)
  , ( (0, xK_l), sendMessage NextLayout)
  , ( (0, xK_k), kill)
  , ( (0, xK_h), withFocused hide)
  ]

myMainKeys :: [(( ButtonMask, KeySym ), X () )]
myMainKeys =
  [ ( (myModMask, xK_a), submap appSubmap)
  , ( (myModMask, xK_z), submap promptSubmap)
  , ( (myModMask, xK_w), submap windowSubmap)
  , ( (myModMask, xK_r), toggleWS' ["NSP"])
  , ( (myModMask, xK_s), nextScreen)
  , ( (myModMask, xK_h), namedScratchpadAction scratchpads "htop")
  , ( (myModMask, xK_m), namedScratchpadAction scratchpads "_mail")
  , ( (myModMask, xK_c), namedScratchpadAction scratchpads "chromium")
  , ( (myModMask, xK_q), namedScratchpadAction scratchpads "hud")
  , ( (myModMask, xK_f), sendMessage $ Toggle NBFULL)
  ]

myBaseKeys :: XConfig Layout -> [(( ButtonMask, KeySym ), X () )]
myBaseKeys conf = myMainKeys ++
  [ ( (mod1Mask, xK_Tab), windows W.focusUp >> windows W.shiftMaster)
  , ( (mod1Mask .|. shiftMask, xK_Tab), windows W.focusDown)
  , ( (myModMask, xK_Return), windows W.focusMaster  )
  -- , ( (myModMask, xK_Return), windows W.focusMaster  )
  -- , ( (myModMask, xK_n     ), windows W.focusUp >> windows W.shiftMaster)
  -- , ( (myModMask, xK_p     ), windows W.focusDown )

  -- dealing with workspaces and screens
  , ( (myShiftMask, xK_s), shiftNextScreen)

  -- Modifying the window order
  , ( (myShiftMask, xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
  , ( (myShiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
  , ( (myShiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

  -- resizing the master/slave ratio
  -- , ( (myModMask,     xK_h), sendMessage Shrink) -- %! Shrink the master area
  -- , ( (myModMask,     xK_l), sendMessage Expand) -- %! Expand the master area

  -- move floating windows: snap to next barrier. Last param is a Maybe Int threshold in pixels
  , ( (myShiftMask,   xK_h),  withFocused $ snapMove L Nothing)
  , ( (myShiftMask,   xK_l),  withFocused $ snapMove R Nothing)
  , ( (myShiftMask,   xK_k),  withFocused $ snapMove U Nothing)
  , ( (myShiftMask,   xK_j),  withFocused $ snapMove D Nothing)

  -- resize floating windows, snapping
  , ( (myControlMask, xK_h),  withFocused $ snapShrink R Nothing)
  , ( (myControlMask, xK_l),  withFocused $ snapGrow   R Nothing)
  , ( (myControlMask, xK_j),  withFocused $ snapGrow   D Nothing)
  , ( (myControlMask, xK_k),  withFocused $ snapShrink D Nothing)

  -- resize floating windows, fixed steps
  , ( (myAltMask,     xK_z), withFocused $ keysResizeWindow (-resizeStepSize, 0) (0, 0) )
  , ( (myAltMask,     xK_l), withFocused $ keysResizeWindow (resizeStepSize,  0) (0, 0) )
  , ( (myAltMask,     xK_j), withFocused $ keysResizeWindow (0, resizeStepSize)  (0, 0) )
  , ( (myAltMask,     xK_k), withFocused $ keysResizeWindow (0, -resizeStepSize) (0, 0) )
  ]

myKeys :: XConfig Layout -> M.Map ( ButtonMask, KeySym ) ( X () )
myKeys conf = M.fromList $
  myBaseKeys conf ++
  [((m .|. workspaceMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++
  buildTagKeys tags focusUpTagged

buildTagKeys :: [Tag] -> ( String -> X () ) -> [(( ButtonMask, KeySym ), X () )]
buildTagKeys tagKeys action =
  [((tagSelectMask, keyToCode M.! key), action [key] >> windows W.shiftMaster) | key <- tagKeys ] ++
  [((tagToggleMask, keyToCode M.! key), (withFocused . toggleTag) [key])       | key <- tagKeys ]

toggleTag :: String -> Window -> X ()
toggleTag tag window = do
  tagActive <- hasTag tag window
  if tagActive
  then delTag tag window
  else addTag tag window
