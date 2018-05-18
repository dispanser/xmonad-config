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

-- TODO: short term
-- - change tag behavior: we don't want to pull the window into master
-- - add tag behavior: use shift to pull the window into master
-- - workspace-aware chromium sessions: per-session tabs remembered

-- - workspace-ware emacs sessions
-- TODO: long term
-- - automatically group some windows, e.g. all vimbs into a group


-- keybinding overview:
-- modMask: alt-gr (mod5Mask)
import Control.Monad (replicateM_)
import qualified Data.Map as M
import Data.Monoid (Endo(..))
import Data.List (isPrefixOf, isSuffixOf)

import XMonad
import XMonad.ManageHook
import XMonad.Prompt (defaultXPConfig)
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, toggleWS')
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Actions.DynamicProjects (dynamicProjects, shiftToProjectPrompt, switchProjectPrompt, changeProjectDirPrompt)
import XMonad.Actions.FloatKeys (keysResizeWindow)
import XMonad.Actions.FloatSnap (Direction2D ( .. ), snapShrink, snapGrow, snapMove)
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig, gridselect, gridselectWorkspace)
-- import XMonad.Actions.Navigation2D (windowGo)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.TagWindows (addTag, delTag, focusUpTagged, focusDownTagged, hasTag, withTagged)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowGo (ifWindow, raiseMaybe, raiseHook)

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Accordion
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing (spacing, spacingWithEdge, smartSpacing, smartSpacingWithEdge)
import XMonad.Layout.SubLayouts
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

import XMonad.Util.NamedScratchpad

import Debug.TrackFloating (useTransientFor, trackFloating)
import MyWorkspaces

type Tag = Char

myWorkspaces :: [WorkspaceId]
myWorkspaces = []

myTerminal, myBrowser, myEditor :: String
myBrowser  = "vimb"
myQute     = "qutebrowser --backend webengine"
myEditor   = "emacsclient -c"
myTerminal = "urxvt"

connectionNames = [ "Saaleblick", "lambda", "sigma", "bahn", "tomtom-internal" ]
myConnections = zip connectionNames connectionNames

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook scratchpads
  <+> composeAll
  [ title =? "xmessage"              --> doRectFloat centeredRect
  , appName `endsWith` "_overlay"    --> doRectFloat rightBarRect
  , className =? "Pinentry"          --> doRectFloat smallCentered
  , className =? "Vimb"              --> addTagHook "b"
  , className =? "Firefox"           --> addTagHook "b"
  , className =? "qutebrowser"       --> addTagHook "b"
  , className =? "Emacs"             --> addTagHook "e"
  , className =? "Gvim"              --> addTagHook "v"
  , className =? "Apvlv"             --> addTagHook "d"
  , className =? "jetbrains-idea-ce" --> addTagHook "i"
  , className =? "URxvt"             --> addTagHook "u"
  , role      =? "browser-edit"      --> doRectFloat lowerRightRect
  , appName   =? "browser-edit"      --> doRectFloat lowerRightRect
  -- , pure True            --> doFloat -- catch-all to floating: disabled!
  ]
    where role = stringProperty "WM_WINDOW_ROLE"

-- add a tag to a window via ManageHook
addTagHook :: String -> ManageHook
addTagHook tag = do
  w <- ask
  liftX $ addTag tag w
  idHook

scratchpads :: [NamedScratchpad]
scratchpads =
    [ shellScratchpad "htop"                              ( customFloating centeredRect )
    , shellScratchpad "journalctl -xf"                    ( customFloating centeredRect )
    , tmuxScratchpad "_mail"                              ( customFloating centeredRect )
    , tmuxScratchpad "hud"                                ( customFloating upperBarRect )
    , emacsScratchpad "gtd" "/home/pi/gtd/master.org"     ( customFloating centeredRect )
    , emacsScratchpad "scratch" "/tmp/scratch"            ( customFloating centeredRect )
    , NS "chromium" "chromium" (className `startsWith` "Chromium")  ( customFloating leftBarRect )
    , NS "firefox" "firefox" (className =? "Firefox")     ( customFloating leftBarRect )
    , NS "qutebrowser" myQute (className =? "qutebrowser") ( customFloating leftBarRect )
    , NS "pidgin_contacts" "pidgin" isPidginContactList   ( customFloating contactBarRect )
    , NS "pidgin_messages" "pidgin" isPidginMessageWindow ( customFloating lowerRightRect )
    ]

emacsScratchpad :: String -> String -> ManageHook -> NamedScratchpad
emacsScratchpad name file = NS name command query
  where
    command = "emacs -T " ++ name ++ " " ++ file
    query   = title =? name

isPidginContactList, isPidginMessageWindow, isPidginClass, isBuddy :: Query Bool
isPidginContactList   = isPidginClass <&&> isBuddy
isPidginMessageWindow = isPidginClass <&&> notQ ( isBuddy )
isPidginClass = className =? "Pidgin"
isBuddy = title =? "Buddy List"

notQ :: Query Bool -> Query Bool
notQ query = do
  b <- query
  pure $ not b

-- query that checks if the provided query ends with the given sequence.
endsWith :: Eq a => Query [a] -> [a] -> Query Bool
endsWith q x = isSuffixOf x <$> q

-- query that checks if the provided query starts with the given sequence.
startsWith :: Eq a => Query [a] -> [a] -> Query Bool
startsWith q x = isPrefixOf x <$> q

localThing :: String -> Window -> X ()
localThing name w = do
  an <- runQuery appName w
  ws <- gets (W.currentTag . windowset)
  let localName = ws ++ "_" ++ name
  if an == localName
  then windows $ W.shift "NSP"
  else ifWindow (appName =? localName) (doShiftAndFocus ws) (spawn $ tmux localName)

-- | Move the window to a given workspace
-- doShift' :: WorkspaceId -> ManageHook
-- doShift' i = doF . W.shiftWin i =<< ask
doShiftAndFocus :: WorkspaceId -> ManageHook
doShiftAndFocus i = do
  w <- ask
  (doF . W.shiftWin i) w

localScratchpad :: (String -> ManageHook -> NamedScratchpad) -> String -> ManageHook -> WorkspaceId -> NamedScratchpad
localScratchpad nsF session hook ws = nsF session' hook
  where session' = session ++ '_' : ws

tmuxScratchpad :: String -> ManageHook -> NamedScratchpad
tmuxScratchpad session = NS session command (appName =? session)
  where command = tmux session

shellScratchpad :: String -> ManageHook -> NamedScratchpad
shellScratchpad session = NS session command (appName =? session)
  where command = "urxvt -name " ++ session ++ " -e " ++ session

tmux :: String -> String
tmux session = myTerminal ++ " -name "  ++ session ++ " -e zsh -i -c \"tas " ++ session ++ "\""

x          = 1920 :: Rational
y          = 1080 :: Rational

gapSize    = 5 :: Rational
fullWidth  = ( x - 2*gapSize ) / x
fullHeight = ( y - 2*gapSize ) / y
left = gapSize / x
up   = gapSize / y

centeredRect   = W.RationalRect 0.2 0.2 0.6 0.6
smallCentered  = W.RationalRect 0.35 0.4 0.3 0.2
upperBarRect   = W.RationalRect left up fullWidth (1 / 3)
rightBarRect   = W.RationalRect (1/2) up (fullWidth / 2) fullHeight
leftBarRect    = W.RationalRect left up (fullWidth / 2) fullHeight
contactBarRect = W.RationalRect 0.9 up 0.1 fullHeight
lowerRightRect = W.RationalRect (1/2) (1/2) (fullWidth / 2) (fullHeight / 2)

-- explicit list of tags
tags :: [Tag]
tags = [ 'b' -- project-related documentation (auto-assigned to vimb)
       , 'e' -- editor / emacs (auto-assigned to emacs instances)
         -- , 'o' -- org mode: project-related org or similar (not auto-assigned)
       , 'd' -- documentation of any kind
       , 'v' -- vim instance
       , 'x' -- assign freely, 'extended'
       , 'i' -- idea
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
  , startupHook        = setWMName "LG3D"
  , manageHook         = myManageHook
  , logHook            = updatePointer (0.5, 0.5) (0, 0)
  , keys               = myKeys
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00" }

myLayoutHook = noBorders
               . windowNavigation
               . trackFloating
               . useTransientFor
               . addTopBar
               . addTabs shrinkText myTabTheme
               -- . spacingWithEdge 5
               -- . spacing 5
               . smartSpacing 5
               . mkToggle (FULL ?? MIRROR ?? EOT)
               $ tabs ||| subs
  where
    subs          = subLayout [] innerLayout $ boringWindows outerLayout
    tabs          = Simplest
    addTopBar     = noFrillsDeco shrinkText topBarTheme
    tallLayout    = ResizableTall nmaster resizeDelta masterRatio slaveRatios
    outerLayout   = tallLayout ||| Accordion
    innerLayout   = Simplest ||| Accordion
    nmaster       = 1
    resizeDelta   = 5/100
    masterRatio   = 3/6
    -- the ratios seem to contain the master window in the computation. if first and
    -- second entry are identical, a third window will have size 0.
    -- the current setting makes the second window twice the size of the third (if there
    -- are only three)
    slaveRatios   = [1.6, 1.3]

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
  -- TODO: logging scratchpad doesn't work very well, probably because
  -- the command takes parameters, so the name of the window doeesn't
  -- properly match the command line supplied arg
  , ( (0, xK_l), namedScratchpadAction scratchpads "journalctl -xf")
  ]

-- submaps for various prompt-based actions
promptSubmap :: M.Map ( ButtonMask, KeySym ) ( X () )
promptSubmap = M.fromList
  [ ( (0, xK_b), spawn $ "/home/pi/bin/browser-dmenu " ++ myBrowser)
  , ( (0, xK_c), spawn "/home/pi/bin/browser-dmenu chromium")
  , ( (0, xK_f), spawn "/home/pi/bin/browser-dmenu firefox")
  , ( (0, xK_q), spawn "/home/pi/bin/browser-dmenu qutebrowser")
  , ( (0, xK_s), spawn "passmenu")
  , ( (0, xK_d), spawn "dmenu_run")
  , ( (0, xK_g), goToSelected defaultGSConfig)
  , ( (0, xK_w), connectToNetwork)
  , ( (0, xK_p), gridselectWorkspace defaultGSConfig W.greedyView)
  ]

-- submaps for less common window operations
windowSubmap :: M.Map ( ButtonMask, KeySym ) ( X () )
windowSubmap = M.fromList
  [ ( (0, xK_s),         withFocused $ windows . W.sink)
  , ( (shiftMask, xK_s), sinkAll)
  , ( (0, xK_f),         withFocused float)
  , ( (0, xK_l),         sendMessage NextLayout)
  , ( (0, xK_i),         toSubl NextLayout)
  , ( (0, xK_k),         kill)
  ]

mySubmap :: M.Map ( KeyMask, KeySym) ( X () ) -> X ()
-- mySubmap = resubmapDefaultWithKey $ (\k -> xmessage ("huhu, unexpected key: " ++ (show $ snd k) ) )
mySubmap = submap

myMainKeys :: [(( ButtonMask, KeySym ), X () )]
myMainKeys =
  [ ( (myModMask, xK_a), mySubmap appSubmap)
  , ( (myModMask, xK_z), mySubmap promptSubmap)
  , ( (myModMask, xK_w), mySubmap windowSubmap)
  , ( (myModMask, xK_r), toggleWS' ["NSP", "eclipse"])
  , ( (myModMask, xK_s), nextScreen)
  , ( (myModMask, xK_m), namedScratchpadAction scratchpads "_mail")
  , ( (myModMask, xK_c), namedScratchpadAction scratchpads "qutebrowser")
  , ( (myAltMask, xK_c), namedScratchpadAction scratchpads "firefox")
  , ( (myShiftMask, xK_c), namedScratchpadAction scratchpads "chromium")
  , ( (myModMask, xK_q), namedScratchpadAction scratchpads "hud")
  , ( (myModMask, xK_g), namedScratchpadAction scratchpads "gtd")
  , ( (myModMask, xK_backslash), namedScratchpadAction scratchpads "scratch")
  , ( (myModMask, xK_f), sendMessage $ Toggle FULL)
  , ( (myModMask, xK_slash), sendMessage $ Toggle MIRROR)
  , ( (myModMask, xK_b), runOrRaiseLocal "build")
  , ( (myModMask, xK_t), runOrRaiseLocal "term")

  -- SubLayout: iterate inside a single group
  , ( (myModMask, xK_period), onGroup W.focusDown')
  , ( (myModMask, xK_comma), onGroup W.focusUp')

  -- SubLayout: go / swap in the four directions
  , ( (myModMask, xK_l),   sendMessage $ Go R)
  , ( (myModMask, xK_h),   sendMessage $ Go L)
  , ( (myModMask, xK_k),   sendMessage $ Go U)
  , ( (myModMask, xK_j),   sendMessage $ Go D)
  , ( (myShiftMask, xK_l), sendMessage $ Swap R)
  , ( (myShiftMask, xK_h), sendMessage $ Swap L)
  , ( (myShiftMask, xK_k), sendMessage $ Swap U)
  , ( (myShiftMask, xK_j), sendMessage $ Swap D)
  , ( (myControlMask, xK_h), sendMessage Shrink)
  , ( (myControlMask, xK_l), sendMessage Expand)
  , ( (myControlMask, xK_j), sendMessage MirrorShrink)
  , ( (myControlMask, xK_k), sendMessage MirrorExpand)

  -- SubLayout: merge windows, explode
  , ( (myAltMask, xK_BackSpace), sendMessage $ pullGroup L)
  , ( (myAltMask, xK_l), sendMessage $ pullGroup R)
  , ( (myAltMask, xK_k), sendMessage $ pullGroup U)
  , ( (myAltMask, xK_j), sendMessage $ pullGroup D)
  , ( (myAltMask, xK_x), withFocused $ sendMessage . UnMerge)
  , ( (myAltMask .|. shiftMask, xK_x), withFocused $ sendMessage . UnMergeAll)

  -- overlay terminal: one per workspace. Very similar to named scratchpads,
    -- but doesn't have to be registered at startup.
  , ( (myModMask, xK_o),   withFocused $ localThing "overlay")
  ]

myBaseKeys :: XConfig Layout -> [(( ButtonMask, KeySym ), X () )]
myBaseKeys conf = myMainKeys ++
  [ ( (myModMask,   xK_Return), promote)
  , ( (myShiftMask, xK_Return), windows W.focusMaster)

  -- basic window switch via mod-{n,p}. Mix in shift to not bring front
  , ( (myShiftMask, xK_Return), promote)
  , ( (myShiftMask, xK_n),   windows W.swapDown)
  , ( (myShiftMask, xK_p),   windows W.swapUp)
  , ( (myModMask,   xK_n),   windows W.focusDown)
  , ( (myModMask,   xK_p),   windows W.focusUp)

  , ( (myModMask,   xK_y), namedScratchpadAction scratchpads "pidgin_messages")
  , ( (myShiftMask, xK_y), namedScratchpadAction scratchpads "pidgin_contacts")

  , ( (myShiftMask, xK_s), shiftNextScreen)
  , ( (myModMask,   xK_space), switchProjectPrompt    defaultXPConfig)
  , ( (myShiftMask, xK_space), shiftToProjectPrompt   defaultXPConfig)
  , ( (myControlMask,xK_space), changeProjectDirPrompt defaultXPConfig)
  , ( (myModMask,   xK_i), windows $ W.greedyView "eclipse")

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
  myBaseKeys conf
  -- [ ( myModMask, xK_0 ), withNthWorkspace . W.greedyView]
  -- TODO: withNthWorkspace
  -- [((m .|. workspaceMask, k), windows $ f i)
  --   | (i, k) <- zip [0 ..] [xK_1 .. xK_9]
  --   , (f, m) <- [(withNthWorkspace W.greedyView, 0),
  --                (withNthWorkspace W.shift, shiftMask)]]
  ++ buildTagKeys tags

tagControl :: [( ButtonMask, String -> X () )]
tagControl = [ ( myModMask,     \t -> focusUpTagged   t)
             -- , ( myShiftMask,   \t -> focusDownTagged t)
             , ( tagToggleMask, withFocused . toggleTag )
             ]

buildTagKeys :: [Tag] -> [(( ButtonMask, KeySym ), X () )]
buildTagKeys tagKeys =
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
  raiseMaybe (spawn $ tmux localName) (appName =? localName)

connectToNetwork :: X ()
connectToNetwork = do
  maybeCon <- gridselect defaultGSConfig myConnections
  case maybeCon of
    Just con -> spawn $ "sudo netctl switch-to " ++ con
    Nothing  -> pure ()

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

-- curtesy of ethanschoonover config somewhere over at github
myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    , decoHeight            = tabHeight
    }

topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

topbar      = 5
tabHeight   = 14

myFont      = "-*-terminus-medium-*-*-*-*-180-*-*-*-*-*-*"
-- myFont      = "xft:Iosevka:pixelsize=12"

active      = blue

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
