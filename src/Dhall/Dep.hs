module Dhall.Dep ( getFileDeps
                 , getAllFileDeps
                 ) where

import           Control.Exception (throw)
import           Data.Foldable     (toList)
import           Data.List         (group, sort)
import           Data.Maybe        (catMaybes)
import qualified Data.Text.IO      as T
import           Dhall.Core        (Import, ImportType (..), importHashed,
                                    importType)
import           Dhall.Import      (localToPath)
import           Dhall.Parser      (exprFromText)
import           System.FilePath   (takeDirectory, (</>))

-- | Given a path, the file paths it depends on
getFileDeps :: FilePath -> IO [FilePath]
getFileDeps fp = do
    contents <- T.readFile fp
    let fileDir = takeDirectory fp
        fileMod = (fileDir </>)
        tree = either throw id (exprFromText fp contents)
        imports = toList tree
    catMaybes <$> traverse (fmap (fileMod <$>) . fromImport) imports

-- | Get all transitive dependencies
getAllFileDeps :: FilePath -> IO [FilePath]
getAllFileDeps fp = do
    deps <- getFileDeps fp
    level <- traverse getFileDeps deps
    let rmdups = fmap head . group . sort
        next = rmdups (mconcat (deps : level))
    pure $ if null level then deps else next

-- TODO: custom?
fromImport :: Import -> IO (Maybe FilePath)
fromImport = fromImportType . importType . importHashed

fromImportType :: ImportType -> IO (Maybe FilePath)
fromImportType (Local pre fp) = Just <$> localToPath pre fp
fromImportType _              = pure Nothing