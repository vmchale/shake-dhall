module Development.Shake.Dhall ( needDhall
                               ) where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Containers.ListUtils (nubOrd)
import           Development.Shake         (Action, need)
import           Dhall.Dep

-- | 'need' some @.dhall@ files and imported dependencies
needDhall :: [FilePath] -> Action ()
needDhall fps =
    need =<< liftIO (nubOrd . concat . (fps:) <$> traverse getAllFileDeps fps)
