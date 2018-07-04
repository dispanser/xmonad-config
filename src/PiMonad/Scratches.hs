{-# LANGUAGE RecordWildCards #-}

module PiMonad.Scratches ( doShiftAndFocus
                         , projectBrowser
                         )
where

import           PiMonad.Workspaces             (getMainWorkspace)
import           System.FilePath.Posix          ((</>))
import           XMonad
import           XMonad.Actions.DynamicProjects (Project (..), currentProject)
import           XMonad.Actions.WindowGo        (ifWindow)
import qualified XMonad.StackSet                as W

doShiftAndFocus :: WorkspaceId -> ManageHook
doShiftAndFocus i = do
  w <- ask
  doF $ W.focusWindow w . W.shiftWin i w

projectBrowser :: X ()
projectBrowser = projectScratch cmdF
  where
    cmdF (Project {..} ) = ( "qutebrowser --qt-arg name " ++ localName ++
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
  liftIO $ putStrLn command
  let focused = W.peek ws
  let windowQuery = ifWindow query (doShiftAndFocus $ W.currentTag ws) (spawn command)
  case focused of
    Just w -> do
      matches <- runQuery  query w
      if matches
        then windows $ W.shift "NSP"
        else windowQuery
    Nothing -> windowQuery
