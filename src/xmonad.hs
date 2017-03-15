import Debug.Trace

-- current workings
-- - Sublayouts w/ ResizableTall as outer layout
-- - inner layout: Simplest (need to get tabs working, but subTabs doesn't cut it)
-- - mnemonics: the key bindings for floating have been remapped to wasd
-- - windownavigation via mod-[hjkl]
-- - window merging via shiftMod-[hjkl] (pull into group)
-- - shrink / expand via altMod-[hl] (master) [jk] (slave)
-- - navigate inside tabs via mod-[np]
-- - swap Up / down via shiftMod-[np] (inconsistent with tab-navigation)
-- - spacing between windows (5px, also on edges)


import Control.Monad (replicateM_)
import qualified Data.Map as M
import Data.Monoid (Endo(..))

import XMonad
import XMonad.ManageHook
import XMonad.Prompt (defaultXPConfig)
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects (dynamicProjects, switchProjectPrompt, changeProjectDirPrompt)
import XMonad.Actions.FloatKeys (keysResizeWindow)
import XMonad.Actions.FloatSnap (Direction2D ( .. ), snapShrink, snapGrow, snapMove)
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig, gridselect, gridselectWorkspace)
-- import XMonad.Actions.Navigation2D (windowGo)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.TagWindows (addTag, delTag, focusDownTagged, focusUpTagged, hasTag)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowGo (raiseMaybe)

import XMonad.Hooks.ManageHelpers

import XMonad.Layout.BoringWindows
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation

import XMonad.Util.NamedScratchpad

import MyWorkspaces

type Tag = Char

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 3 :: Int]

myTerminal, myBrowser, myEditor :: String
myBrowser  = "vimb"
myEditor   = "emacsclient -c"
myTerminal = "urxvt"

connectionNames = [ "Saaleblick", "lambda", "peta", "bahn", "tomtom-internal" ]
myConnections = zip connectionNames connectionNames

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook scratchpads
  <+> composeAll
  [ title =? "xmessage"  --> doRectFloat centeredRect
  , className =? "Vimb"  --> addTagHook "b"
  , className =? "Emacs" --> addTagHook "e"
  -- , pure True            --> doFloat -- catch-all to floating: disabled!
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
    , NS "chromium" "chromium" (className =? "Chromium") ( customFloating $ leftBarRect )
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
leftBarRect    = W.RationalRect 0.0 0.0 0.5 1.0
contactBarRect = W.RationalRect 0.9 0.0 0.1 1.0
upperRightRect = W.RationalRect 0.5 0.0 0.5 0.5
lowerRightRect = W.RationalRect 0.5 0.5 0.5 0.5
upperLeftRect  = W.RationalRect 0.0 0.0 0.5 0.5

-- explicit list of tags
tags :: [Tag]
tags = [ 'e' -- editor
       , 'b' -- project-related documentation
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
  { borderWidth        = 1
  , modMask            = myModMask
  , terminal           = myTerminal
  , workspaces         = myWorkspaces
  , layoutHook         = myLayoutHook
  , manageHook         = myManageHook
  , logHook            = updatePointer (0.5, 0.5) (0, 0)
  , keys               = myKeys
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00" }

myLayoutHook = noBorders
        . smartSpacingWithEdge 5
        . mkToggle (NOBORDERS ?? NBFULL ?? EOT)
        $ myLayout

-- myLayout = simplestFloat ||| tiled
-- myLayout = windowNavigation $ subTabbed $ boringWindows tiled
myLayout = windowNavigation $ subLayout [] Simplest $ boringWindows outerLayout
  where
     outerLayout   = ResizableTall nmaster resizeDelta masterRatio slaveRatios
     nmaster       = 1
     resizeDelta   = 5/100
     masterRatio   = 3/6
     slaveRatios   = [2] -- make the first slave window twice the height of the rest

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
  , ( (0, xK_s), spawn "passmenu")
  , ( (0, xK_d), spawn "dmenu_run")
  , ( (0, xK_g), goToSelected defaultGSConfig) -- %! Push window back into tiling
  , ( (0, xK_w), connectToNetwork) -- %! Push window back into tiling
  , ( (0, xK_p), gridselectWorkspace defaultGSConfig W.greedyView) -- %! Push window back into tiling
  ]

-- submaps for less common window operations
windowSubmap :: M.Map ( ButtonMask, KeySym ) ( X () )
windowSubmap = M.fromList
  [ ( (0, xK_s),         withFocused $ windows . W.sink)
  , ( (shiftMask, xK_s), sinkAll)
  , ( (0, xK_f),         withFocused float)
  , ( (0, xK_l),         sendMessage NextLayout)
  , ( (0, xK_k),         kill)
  , ( (0, xK_h),         withFocused hide)
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
  -- , ( (myModMask, xK_l), windowGo R False)
  -- , ( (myModMask, xK_h), windowGo L False)
  -- , ( (myModMask, xK_k), windowGo U False)
  -- , ( (myModMask, xK_j), windowGo D False)

  -- SubLayout: merge windows, explode
  , ( (myControlMask, xK_h), sendMessage $ pullGroup L)
  , ( (myControlMask, xK_l), sendMessage $ pullGroup R)
  , ( (myControlMask, xK_k), sendMessage $ pullGroup U)
  , ( (myControlMask, xK_j), sendMessage $ pullGroup D)
  , ( (myControlMask, xK_x), withFocused $ sendMessage . UnMerge)
  , ( (myControlMask .|. shiftMask, xK_x), withFocused $ sendMessage . UnMergeAll)

  -- SubLayout: iterate inside a single window
  , ( (myModMask,   xK_n), onGroup W.focusDown')
  , ( (myShiftMask, xK_p), onGroup W.focusUp')
  , ( (myModMask, xK_l), sendMessage $ Go R)
  , ( (myModMask, xK_h), sendMessage $ Go L)
  , ( (myModMask, xK_k), sendMessage $ Go U)
  , ( (myModMask, xK_j), sendMessage $ Go D)
  , ( (myShiftMask, xK_l), sendMessage $ Swap R)
  , ( (myShiftMask, xK_h), sendMessage $ Swap L)
  , ( (myShiftMask, xK_k), sendMessage $ Swap U)
  , ( (myShiftMask, xK_j), sendMessage $ Swap D)
  , ( (myAltMask,   xK_BackSpace), sendMessage Shrink)
  , ( (myAltMask,   xK_l), sendMessage Expand)
  , ( (myAltMask,   xK_j), sendMessage MirrorShrink)
  , ( (myAltMask,   xK_k), sendMessage MirrorExpand)

  -- , ( (myModMask, xK_i), replicateM_ 3 $ withFocused $ sendKeyEvent 0 xK_z)
  -- , ( (myShiftMask, xK_j), sendMessage $ IncMasterN (-1))
  -- , ( (myShiftMask, xK_k), sendMessage $ IncMasterN 1)
  ]

myBaseKeys :: XConfig Layout -> [(( ButtonMask, KeySym ), X () )]
myBaseKeys conf = myMainKeys ++
  [ ( (myModMask,   xK_Return), promote)
  , ( (myShiftMask, xK_Return), windows W.focusMaster)

  -- basic window switch via mod-{n,p}. Mix in shift to not bring front
  , ( (myShiftMask, xK_Return), promote)
  -- , ( (myModMask,   xK_n), windows W.focusUp >> promote)
  -- , ( (myModMask,   xK_p), windows W.focusDown >> promote)
  -- , ( (myShiftMask, xK_n), windows W.focusUp)
  -- , ( (myShiftMask, xK_p), windows W.focusDown)
  -- , ( (myModMask,   xK_n), focusDown)
  -- , ( (myModMask,   xK_p), focusUp)
  , ( (myShiftMask, xK_n), windows W.swapDown)
  , ( (myShiftMask, xK_p), windows W.swapUp)

  , ( (myModMask,   xK_y), namedScratchpadAction scratchpads "pidgin_messages")
  , ( (myShiftMask, xK_y), namedScratchpadAction scratchpads "pidgin_contacts")

  , ( (myShiftMask, xK_s), shiftNextScreen)
  , ( (myModMask,   xK_space), switchProjectPrompt    defaultXPConfig)
  , ( (myShiftMask, xK_space), changeProjectDirPrompt defaultXPConfig)

  -- , ( (myModMask,   xK_f), withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f)

  -- move floating windows: snap to next barrier. Last param is a Maybe Int
  -- threshold in pixels but I couldn't find any impact;
  -- TODO: check snapMove sources to understand param
  , ( (myShiftMask,   xK_a),  withFocused $ snapMove L Nothing)
  , ( (myShiftMask,   xK_d),  withFocused $ snapMove R Nothing)
  , ( (myShiftMask,   xK_w),  withFocused $ snapMove U Nothing)
  , ( (myShiftMask,   xK_s),  withFocused $ snapMove D Nothing)

  -- resize floating windows, snapping
  , ( (myControlMask, xK_a),  withFocused $ snapShrink R Nothing)
  , ( (myControlMask, xK_d),  withFocused $ snapGrow   R Nothing)
  , ( (myControlMask, xK_w),  withFocused $ snapShrink D Nothing)
  , ( (myControlMask, xK_s),  withFocused $ snapGrow   D Nothing)

  -- resize floating windows, fixed steps
  , ( (myAltMask,     xK_a), withFocused $ keysResizeWindow (-resizeStepSize, 0) (0, 0) )
  , ( (myAltMask,     xK_d), withFocused $ keysResizeWindow (resizeStepSize,  0) (0, 0) )
  , ( (myAltMask,     xK_w), withFocused $ keysResizeWindow (0, -resizeStepSize) (0, 0) )
  , ( (myAltMask,     xK_s), withFocused $ keysResizeWindow (0, resizeStepSize)  (0, 0) )
  ]

myKeys :: XConfig Layout -> M.Map ( ButtonMask, KeySym ) ( X () )
myKeys conf = M.fromList $
  myBaseKeys conf ++
  [((m .|. workspaceMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++ buildTagKeys tags focusUpTagged

tagControl :: [( ButtonMask, String -> X () )]
tagControl = [ ( myModMask,     \k -> focusUpTagged   k)
             , ( myShiftMask,   \k -> focusDownTagged k)
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

connectToNetwork :: X ()
connectToNetwork = do
  maybeCon <- gridselect defaultGSConfig myConnections
  case maybeCon of
    Just con -> spawn $ "sudo netctl switch-to " ++ con
    Nothing  -> pure ()

xmessage :: String -> X ()
xmessage msg = spawn $ "xmessage '" ++ msg ++ "' -default okay"

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
