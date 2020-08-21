module Development.Shake.Dhall ( needDhall
                               , needDhallCli
                               , dhallDeps
                               ) where

import           Control.Monad             (filterM, (<=<))
import           Control.Monad.IO.Class    (liftIO)
import           Data.Containers.ListUtils (nubOrd)
import           Development.Shake         (Action, Stdout (Stdout), command,
                                            doesFileExist, need)
import           Dhall.Dep

-- | 'need' some @.dhall@ files and imported dependencies
needDhall :: [FilePath] -> Action ()
needDhall fps =
    need =<< liftIO (nubOrd . concat . (fps:) <$> traverse getAllFileDeps fps)

-- | Same as 'needDhallCli' but shells out to the command-line executable
--
-- @since 0.1.1.0
needDhallCli :: [FilePath] -> Action ()
needDhallCli =
    need . concat <=< traverse dhallDeps

-- | Uses @dhall resolve --transitive-dependencies@ to work; command-line tool
-- must be installed.
--
-- @since 0.1.1.0
dhallDeps :: FilePath -> Action [FilePath]
dhallDeps inp = do
    (Stdout out) <- command [] "dhall" ["resolve", "--transitive-dependencies", "--file", inp]
    (inp:) <$> filterM doesFileExist (lines out)
