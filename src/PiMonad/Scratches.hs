{-# LANGUAGE RecordWildCards #-}

module PiMonad.Scratches ( projectBrowser
                         , toScratch
                         , fromScratchOrFocus
                         , localScratch
                         , triggerScratch
                         , globalKitty
                         , globalTmux
                         , globalScratch
                         , ScratchApp (..)
                         )
where

import           PiMonad.Workspaces             (getMainWorkspace)
import           System.FilePath.Posix          ((</>))
import           XMonad
import           XMonad.Actions.DynamicProjects (Project (..), currentProject)
import           XMonad.Actions.WindowGo        (ifWindow)
import           XMonad.Hooks.ManageHelpers     (doRectFloat)
import qualified XMonad.StackSet                as W
import qualified Debug.Trace                    as DT

data ScratchApp  = ScratchApp
        { commandF :: Project -> String
        , queryF   :: Project -> Query Bool
        , hook     :: ManageHook
        -- , baseKey  :: KeySym
        }
globalScratch :: String -> Query Bool -> W.RationalRect -> ScratchApp
globalScratch command query rect = ScratchApp {
    commandF = const command,
    queryF   = const query,
    hook     = query --> doRectFloat rect
}

globalKitty :: String -> W.RationalRect -> ScratchApp
globalKitty shellCommand = globalScratch command query
  where
    name'   = filter (/= ' ') shellCommand
    command = "kitty --name " ++ name' ++ " -e " ++ shellCommand
    query   = appName =? name'

globalTmux :: String -> W.RationalRect -> ScratchApp
globalTmux shellCommand = globalScratch command query
  where
    name'   = filter (/= ' ') shellCommand
    command = "kitty --name " ++ name' ++ " -e zsh -i -c \"tas " ++ shellCommand ++ "\""
    query   = appName =? name'

-- | create a scratchpad given a function that expects the tag suffix as a @String@
--   and produces the command to be executed, and the local window type that is
--   a part of the globally valid local tag name
localScratch :: (String -> String) -> String -> X ()
localScratch cmdF suffix = withWindowSet $ \ws -> do
  let tag         = W.currentTag ws
  let focused     = W.peek ws
  let localName   = DT.traceShowId $ getMainWorkspace tag ++ "_" ++ suffix
  let command     = cmdF localName
  let windowQuery = fromScratchOrFocus (appName =? localName) tag command
  case focused of
    Just w -> do
      an <- runQuery appName w
      if an == localName
        then toScratch
        else windowQuery
    Nothing -> windowQuery

triggerScratch :: ScratchApp -> X ()
triggerScratch ScratchApp { .. } = withWindowSet $ \ws -> do
  pr <- currentProject
  let command = commandF pr
  let query   = queryF   pr
  let focused = W.peek ws
  let windowQuery = fromScratchOrFocus query (W.currentTag ws) command
  case focused of
    Just w -> do
      matches <- runQuery query w
      if matches
        then toScratch
        else windowQuery
    Nothing -> windowQuery

-- this looks suspicibly similar to the localScratch above
-- TODO: investigate
-- - projectScratch expects a function that, given a project,
--   creates a command (string) and a query; the other one
--   expects a function that produces a command from a string
-- -

projectScratch :: ( Project -> (String, Query Bool) ) -> X ()
projectScratch cmdF = withWindowSet $ \ws -> do
  pr <- currentProject
  let (command, query) = cmdF pr
  let focused = W.peek ws
  let windowQuery = fromScratchOrFocus query (W.currentTag ws) command
  case focused of
    Just w -> do
      matches <- runQuery query w
      if matches
        then toScratch
        else windowQuery
    Nothing -> windowQuery

-- if the windows' workspace is visible, go there and focus
-- otherwise, bring to current workspace and focus
showOrBring :: WorkspaceId -> ManageHook
showOrBring i = do
  w <- ask
  -- let (Just curentWS) = W.findTag w
  doF $ showOrBringWindow i w

-- showOrBringWindow :: WorkspaceId -> Window -> W.StackSet -> ManageHook
showOrBringWindow i w ss =
    let (Just currentWS) = W.findTag w ss
        visibleWS = W.tag . W.workspace <$> W.visible ss
    in if currentWS `elem` visibleWS -- the window is visible elsewhere
          then W.focusWindow w $ W.view i ss
          else W.focusWindow w $ W.shiftWin i w ss

toScratch :: X ()
toScratch = windows $ W.shift "NSP"

-- find a window using the given query, focus it on the current workspace or
-- on another, visible workspace. If it doesn't exist, run the command instead.
fromScratchOrFocus :: Query Bool -> WorkspaceId -> String -> X ()
fromScratchOrFocus q i c = ifWindow q (showOrBring i) (spawn c)

projectBrowser :: X ()
projectBrowser = projectScratch cmdF
  where
    cmdF Project {..} = ( "qutebrowser --qt-arg name " ++ localName ++
                             " --target window --basedir " ++ projectDir
                           , appName =? localName )
      where localName  = getMainWorkspace projectName ++ "_qute"
            projectDir = projectDirectory </> ".qute"

-- | create a scratchpad given a function that takes the project and produces
-- the command to be executed, and another function that
--   a part of the globally valid local tag name
