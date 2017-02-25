import Debug.Trace

import Control.Monad (replicateM_)
import qualified Data.Map as M

import XMonad
import XMonad.ManageHook
import XMonad.Prompt (amberXPConfig)
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects (dynamicProjects, switchProjectPrompt, changeProjectDirPrompt)
import XMonad.Actions.FloatKeys (keysResizeWindow)
import XMonad.Actions.FloatSnap (Direction2D ( .. ), snapShrink, snapGrow, snapMove)
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)
import XMonad.Actions.Navigation2D (windowGo)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.TagWindows (addTag, delTag, focusDownTagged, focusUpTagged, hasTag)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowGo (raiseMaybe)

import XMonad.Hooks.ManageHelpers

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.Spacing (smartSpacing)

import XMonad.Util.NamedScratchpad

import MyWorkspaces

type Tag = Char

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int]

myTerminal, myBrowser, myEditor :: String
myBrowser  = "vimb"
myEditor   = "emacsclient -c"
myTerminal = "urxvt"


myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook scratchpads
  <+> composeAll
  [ className =? "Vimb"  --> doRectFloat rightBarRect <+> addTagHook "d"
  , title =? "xmessage"  --> doRectFloat centeredRect
  , className =? "Emacs" --> doRectFloat leftBarRect <+> addTagHook "e"
  , pure True            --> doFloat
  ]

addTagHook :: String -> ManageHook
addTagHook tag = do
  w <- ask
  liftX $ addTag tag w
  idHook

scratchpads :: [NamedScratchpad]
scratchpads =
    [ shellScratchpad "htop"   ( customFloating $ centeredRect )
    , tmuxScratchpad "_mail"   ( customFloating $ centeredRect )
    , tmuxScratchpad "hud"     ( customFloating $ upperBarRect )
    , NS "chromium" "chromium" (className =? "Chromium") ( customFloating $ rightBarRect )
    , NS "pidgin_contacts" "pidgin" isPidginContactList   (customFloating $ contactBarRect )
    , NS "pidgin_messages" "pidgin" isPidginMessageWindow (customFloating $ centeredRect )
    ]

isPidginContactList, isPidginMessageWindow, isPidginClass, isBuddy :: Query Bool
isPidginContactList   = isPidginClass <&&> isBuddy
isPidginMessageWindow = isPidginClass <&&> notQ ( isBuddy )
isPidginClass = className =? "Pidgin"
isBuddy = title =? "Buddy List"

notQ :: Query Bool -> Query Bool
notQ query = do
  b <- query
  pure $ not b

localScratchpad :: (String -> ManageHook -> NamedScratchpad) -> String -> ManageHook -> WorkspaceId -> NamedScratchpad
localScratchpad nsF session hook ws = nsF session' hook
  where session' = session ++ '_' : ws

tmuxScratchpad :: String -> ManageHook -> NamedScratchpad
tmuxScratchpad session = NS session command (resource =? session)
  where command = tmux session

shellScratchpad :: String -> ManageHook -> NamedScratchpad
shellScratchpad session = NS session command (resource =? session)
  where command = "urxvt -name " ++ session ++ " -e " ++ session

tmux :: String -> String
tmux session = myTerminal ++ " -name "  ++ session ++ " -e zsh -i -c \"tas " ++ session ++ "\""

centeredRect   = W.RationalRect 0.2 0.2 0.6 0.6
upperBarRect   = W.RationalRect 0.0 0.0 1.0 0.4
rightBarRect   = W.RationalRect 0.5 0.0 0.5 1.0
leftBarRect    = W.RationalRect 0.0 0.0 0.5 0.9
contactBarRect = W.RationalRect 0.9 0.0 0.1 1.0
upperRightRect = W.RationalRect 0.5 0.0 0.5 0.5
lowerRightRect = W.RationalRect 0.5 0.5 0.5 0.5
upperLeftRect  = W.RationalRect 0.0 0.0 0.5 0.5

-- explicit list of tags
tags :: [Tag]
tags = [ 'e' -- editor
       , 'd' -- project-related documentation
       , 'o' -- org mode: project-related org or similar
       , 'x' -- assign freely, 'extended'
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

myModMask, myShiftMask, myControlMask, myAltMask, tagToggleMask, workspaceMask :: ButtonMask
myModMask           = mod5Mask
myShiftMask         = myModMask .|. shiftMask
myControlMask       = myModMask .|. controlMask
myAltMask           = myModMask .|. mod1Mask
tagToggleMask       = myModMask .|. mod4Mask
workspaceMask       = myModMask

main :: IO ()
main = xmonad $ dynamicProjects projects defaultConfig
  { borderWidth        = 2
  , modMask            = myModMask
  , terminal           = myTerminal
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook
  , logHook            = updatePointer (0.5, 0.5) (0, 0)
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
  , ( (0, xK_e), spawn myEditor)
  , ( (0, xK_v), spawn "gvim")
  , ( (0, xK_p), spawn "pidgin")
  , ( (0, xK_h), namedScratchpadAction scratchpads "htop")
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

mySubmap :: M.Map ( KeyMask, KeySym) ( X () ) -> X ()
-- mySubmap = resubmapDefaultWithKey $ (\k -> xmessage ("huhu, unexpected key: " ++ (show $ snd k) ) )
mySubmap = submap

myMainKeys :: [(( ButtonMask, KeySym ), X () )]
myMainKeys =
  [ ( (myModMask, xK_a), mySubmap appSubmap)
  , ( (myModMask, xK_z), mySubmap promptSubmap)
  , ( (myModMask, xK_w), mySubmap windowSubmap)
  , ( (myModMask, xK_r), toggleWS' ["NSP"])
  , ( (myModMask, xK_period), nextWS)
  , ( (myModMask, xK_comma),  prevWS)
  , ( (myShiftMask, xK_period), shiftToNext)
  , ( (myShiftMask, xK_comma),  shiftToPrev)
  , ( (myModMask, xK_s), nextScreen)
  , ( (myModMask, xK_m), namedScratchpadAction scratchpads "_mail")
  , ( (myModMask, xK_c), namedScratchpadAction scratchpads "chromium")
  , ( (myModMask, xK_q), namedScratchpadAction scratchpads "hud")
  , ( (myModMask, xK_f), sendMessage $ Toggle NBFULL)
  , ( (myModMask, xK_0), windows $ W.greedyView "NSP")
  , ( (myModMask, xK_b), runOrRaiseLocal "build")
  , ( (myModMask, xK_t), runOrRaiseLocal "test")
  , ( (myModMask, xK_l), windowGo R False)
  , ( (myModMask, xK_h), windowGo L False)
  , ( (myModMask, xK_k), windowGo U False)
  , ( (myModMask, xK_j), windowGo D False)
  , ( (myModMask, xK_i), replicateM_ 3 $ withFocused $ sendKeyEvent 0 xK_z)
  ]

-- | Send a key to the window
sendKeyEvent :: ButtonMask -> KeySym -> Window -> X ()
sendKeyEvent mask sym w = do
  dpy <- asks display
  io $ allocaXEvent $ \e -> do
    rw      <- rootWindow dpy $ defaultScreen dpy
    keyCode <- keysymToKeycode dpy sym
    setEventType e keyPress
    setKeyEvent e w rw 0 mask keyCode True
    sendEvent dpy w False structureNotifyMask e
  pure ()

myBaseKeys :: XConfig Layout -> [(( ButtonMask, KeySym ), X () )]
myBaseKeys conf = myMainKeys ++
  [ ( (myModMask,   xK_Return), promote)
  , ( (myShiftMask, xK_Return), windows W.focusMaster)

  -- basic window switch via mod-{n,p}. Mix in shift to not bring front
  , ( (myShiftMask, xK_Return), promote)
  , ( (myModMask,   xK_n), windows W.focusUp >> promote)
  , ( (myModMask,   xK_p), windows W.focusDown >> promote)
  , ( (myShiftMask, xK_n), windows W.focusUp)
  , ( (myShiftMask, xK_p), windows W.focusDown)

  , ( (myModMask,   xK_y), namedScratchpadAction scratchpads "pidgin_messages")
  , ( (myShiftMask, xK_y), namedScratchpadAction scratchpads "pidgin_contacts")

  , ( (myShiftMask, xK_s), shiftNextScreen)
  , ( (myModMask,   xK_space), switchProjectPrompt    amberXPConfig)
  , ( (myShiftMask, xK_space), changeProjectDirPrompt amberXPConfig)

  -- move floating windows: snap to next barrier. Last param is a Maybe Int
  -- threshold in pixels but I couldn't find any impact;
  -- TODO: check snapMove sources to understand param
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
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++ buildTagKeys tags focusUpTagged

tagControl :: [( ButtonMask, String -> X () )]
tagControl = [ ( myModMask,     \k -> focusUpTagged   k >> promote)
             , ( myShiftMask,   \k -> focusDownTagged k >> promote)
             , ( tagToggleMask, withFocused . toggleTag )
             ]

buildTagKeys :: [Tag] -> ( String -> X () ) -> [(( ButtonMask, KeySym ), X () )]
buildTagKeys tagKeys action =
  [ ( ( modMask, keyToCode M.! key ), action [key] ) | (modMask, action ) <- tagControl, key <- tagKeys ]

toggleTag :: String -> Window -> X ()
toggleTag tag window = do
  tagActive <- hasTag tag window
  if tagActive
  then delTag tag window
  else addTag tag window

runOrRaiseLocal :: String -> X ()
runOrRaiseLocal name = do
  workspace <- gets (W.currentTag . windowset)
  let localName = workspace ++ "_" ++ name
  raiseMaybe (spawn $ tmux localName) (resource =? localName)
  promote

xmessage :: String -> X ()
xmessage msg = spawn $ "xmessage '" ++ msg ++ "' -default okay"
