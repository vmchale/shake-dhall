module Development.Shake.Dhall ( needDhall
                               ) where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Containers.ListUtils (nubOrd)
import           Development.Shake         (Action, need)
import           Dhall.Dep

-- | Need some @.dhall@ files and imported dependencies
needDhall :: [FilePath] -> Action ()
needDhall fps = do
    transDeps <- liftIO (concat <$> traverse getAllFileDeps fps)
    need (fps ++ nubOrd transDeps)
