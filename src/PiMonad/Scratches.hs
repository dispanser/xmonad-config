{-# LANGUAGE RecordWildCards #-}

module PiMonad.Scratches ( showOrBring
                         , projectBrowser
                         )
where

import           PiMonad.Workspaces             (getMainWorkspace)
import           System.FilePath.Posix          ((</>))
import           XMonad
import           XMonad.Actions.DynamicProjects (Project (..), currentProject)
import           XMonad.Actions.WindowGo        (ifWindow)
import qualified XMonad.StackSet                as W

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

projectScratch :: ( Project -> (String, Query Bool) ) -> X ()
projectScratch cmdF = withWindowSet $ \ws -> do
  pr <- currentProject
  let (command, query) = cmdF pr
  let focused = W.peek ws
  let windowQuery = ifWindow query (showOrBring $ W.currentTag ws) (spawn command)
  case focused of
    Just w -> do
      matches <- runQuery  query w
      if matches
        then windows $ W.shift "NSP"
        else windowQuery
    Nothing -> windowQuery
