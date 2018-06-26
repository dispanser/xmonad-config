module MyWorkspaces (projects, projectFile) where

import XMonad
import XMonad.Actions.DynamicProjects (Project (..), projectDirectory, currentProject)

import Control.Monad (filterM)
import Data.Semigroup ((<>))
import System.Directory (listDirectory, getHomeDirectory, doesDirectoryExist)
import System.FilePath.Posix (makeRelative, (</>))

projectRoot :: IO FilePath
projectRoot = (</> "wip") <$> getHomeDirectory

wip :: IO [Project]
wip = do
  pr          <- projectRoot
  entries     <- filter (/= "done") <$> listDirectory pr
  dirs        <- filterM (doesDirectoryExist . (pr </>)) entries
  return $ map (projectForDir pr) dirs

-- | resolve a file relative to the current project directory
projectFile :: FilePath -> X FilePath
projectFile f = ( </> f) . projectDirectory <$> currentProject

projectForDir :: FilePath -> FilePath -> Project
projectForDir pr d = Project
  { projectName      = d
  , projectDirectory = pr </> d
  , projectStartHook = Nothing }

