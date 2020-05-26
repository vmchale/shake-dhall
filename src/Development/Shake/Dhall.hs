module Development.Shake.Dhall ( needDhall
                               , yamlRules
                               ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.List                  (group, sort)
import           Development.Shake          (Action, Rules, command, need, (%>))
import           Development.Shake.FilePath ((-<.>))
import           Dhall.Dep

-- | Need some @.dhall@ files and imported dependencies
needDhall :: [FilePath] -> Action ()
needDhall fps = do
    transDeps <- liftIO (concat <$> traverse getAllFileDeps fps)
    let rmdups = fmap head . group . sort
    need (fps ++ rmdups transDeps)

-- | Rules for generating YAML (from Dhall source)
yamlRules :: Rules ()
yamlRules =
    "//*.yml" %> \out -> do
        let srcFile = out -<.> "dhall"
        needDhall [srcFile]
        command [] "dhall-to-yaml-ng" ["--file", srcFile, "--output", out]
