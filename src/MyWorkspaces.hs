module MyWorkspaces (projects) where

import XMonad
import XMonad.Actions.DynamicProjects (Project (..))

projects :: [Project]

projects =
  [ Project { projectName      = "system"
            , projectDirectory = "~/configs"
            , projectStartHook = Nothing
            }

  , Project { projectName      = "xmonad-config"
            , projectDirectory = "~/src/configs/xmonad"
            , projectStartHook = Just $ do spawn "emacs xmonad-config/src/xmonad.hs"
                                           spawn "emacs master.org"
            }
  ]


