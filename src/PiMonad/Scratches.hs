{-# LANGUAGE RecordWildCards #-}

module PiMonad.Scratches ( fromScratchOrFocus
                         , triggerScratch
                         , globalKitty
                         , globalTmux
                         , globalScratch
                         , localTmux
                         , endsWith
                         , projectBrowser'
                         , ScratchApp (..)
                         )
where

import           Data.List                      (isSuffixOf)
import           PiMonad.Workspaces             (getMainWorkspace)
import           System.FilePath.Posix          ((</>))
import           XMonad
import           XMonad.Actions.DynamicProjects (Project (..), currentProject)
import           XMonad.Actions.WindowGo        (ifWindow)
import           XMonad.Hooks.ManageHelpers     (doRectFloat)
import qualified XMonad.StackSet                as W

data ScratchApp  = ScratchApp
        { commandF :: Project -> String
        , queryF   :: Project -> Query Bool
        , hook     :: Maybe ManageHook
        }

term :: String -> String -> String
term shellCommand cn  =
  "kitty --name " ++ cn ++ " -e " ++ shellCommand

globalScratch :: String -> Query Bool -> W.RationalRect -> ScratchApp
globalScratch command query rect = ScratchApp {
    commandF = const command,
    queryF   = const query,
    hook     = Just (query --> doRectFloat rect)
}

globalKitty :: String -> W.RationalRect -> ScratchApp
globalKitty shellCommand = globalScratch command query
  where
    name'   = filter (/= ' ') shellCommand
    command = term shellCommand name'
    query   = appName =? name'

globalTmux :: String -> W.RationalRect -> ScratchApp
globalTmux shellCommand = globalScratch command query
  where
    name'   = filter (/= ' ') shellCommand
    command = term ("zsh -i -c \"tas " ++ shellCommand ++ "\"") name'
    query   = appName =? name'

localTmux :: String -> W.RationalRect -> ScratchApp
localTmux suffix rect =
  let session pr =  getMainWorkspace (projectName pr) ++ "_" ++ suffix
  in ScratchApp {
      commandF = \pr -> term ("zsh -i -c \"tas " ++ session pr ++ "\"") (session pr),
      queryF   = \pr -> appName =? session pr,
      hook     = Nothing
     }

-- query that checks if the provided query ends with the given sequence.
endsWith :: Eq a => Query [a] -> [a] -> Query Bool
endsWith q suffix = isSuffixOf suffix <$> q

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

projectBrowser' :: ScratchApp
projectBrowser' =
   let localName pr = getMainWorkspace (projectName pr)
       command pr = "qutebrowser --qt-arg name " ++ localName pr ++ " --target window --basedir " ++ (projectDirectory pr </> ".qute")
   in ScratchApp {
     commandF = command,
     queryF   = \pr -> appName =? localName pr,
     hook     = Nothing
   }

-- | create a scratchpad given a function that takes the project and produces
-- the command to be executed, and another function that
--   a part of the globally valid local tag name
