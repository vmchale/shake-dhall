module Development.Shake.Dhall ( needDhall
                               ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              (group, sort)
import           Development.Shake      (Action, need)
import           Dhall.Dep

needDhall :: [FilePath] -> Action ()
needDhall fps = do
    transDeps <- liftIO (concat <$> traverse getAllFileDeps fps)
    let rmdups = fmap head . group . sort
    need (fps ++ rmdups transDeps)

