module MyWorkspaces ( projects
                    , projectFile
                    , toggleSideWorkspace
                    , getMainWorkspace )
where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.DynamicProjects ( Project (..), projectDirectory
                                      , currentProject)
import XMonad.Actions.DynamicWorkspaces (addWorkspace)

import Control.Monad (filterM)
import Data.List (isSuffixOf)
import Data.Semigroup ((<>))
import System.Directory (listDirectory, getHomeDirectory, doesDirectoryExist)
import System.FilePath.Posix (makeRelative, (</>))

projectRoot :: IO FilePath
projectRoot = (</> "wip") <$> getHomeDirectory

projects :: IO [Project]
projects = do
  pr      <- projectRoot
  entries <- filter (/= ".done") <$> listDirectory pr
  dirs    <- filterM (doesDirectoryExist . (pr </>)) entries
  return $ concatMap (projectsForDir pr) dirs

-- | resolve a file relative to the current project directory
projectFile :: FilePath -> X FilePath
projectFile f = ( </> f) . projectDirectory <$> currentProject

projectsForDir :: FilePath -> WorkspaceId -> [Project]
projectsForDir pr d = [ main, side ]
  where main = Project { projectName = d
    , projectDirectory = pr </> d
    , projectStartHook = Nothing }
        side = main { projectName = d ++ "_side" }

getSideWorkspace :: WorkspaceId -> WorkspaceId
getSideWorkspace ws
  | isSuffixOf "_side" ws = ws
  | otherwise             = ws ++ "_side"

getMainWorkspace :: WorkspaceId -> WorkspaceId
getMainWorkspace ws
  | isSuffixOf "_side" ws = take (length ws - 5) ws
  | otherwise             = ws

getOtherWorkspace :: WorkspaceId -> WorkspaceId
getOtherWorkspace ws
  | isSuffixOf "_side" ws = take (length ws - 5) ws
  | otherwise             = ws ++ "_side"

toggleSideWorkspace :: X ()
toggleSideWorkspace = do
  workspace <- gets (W.currentTag . windowset)
  addWorkspace $ getOtherWorkspace workspace
