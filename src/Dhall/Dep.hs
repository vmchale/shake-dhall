module Dhall.Dep ( getFileDeps
                 , getAllFileDeps
                 ) where

import           Control.Exception         (throw)
import           Control.Monad             ((<=<))
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable             (toList)
import           Data.Maybe                (catMaybes)
import qualified Data.Text.IO              as T
import           Dhall.Core                (Import, ImportType (..),
                                            importHashed, importType)
import           Dhall.Import              (localToPath)
import           Dhall.Parser              (exprFromText)
import           System.Directory          (canonicalizePath,
                                            makeRelativeToCurrentDirectory)
import           System.FilePath           (isAbsolute, takeDirectory, (</>))

-- | Given a path, the file paths it depends on
getFileDeps :: FilePath -> IO [FilePath]
getFileDeps fp = do
    contents <- T.readFile fp
    let fileDir = takeDirectory fp
        fileMod fp' = if isAbsolute fp' then fp' else fileDir </> fp'
        tree = either throw id (exprFromText fp contents)
        imports = toList tree
    traverse canonicalizeRelative =<<
        catMaybes <$> traverse (fmap (fileMod <$>) . fromImport) imports

canonicalizeRelative :: FilePath -> IO FilePath
canonicalizeRelative = makeRelativeToCurrentDirectory <=< canonicalizePath

-- | Get all transitive dependencies
getAllFileDeps :: FilePath -> IO [FilePath]
getAllFileDeps fp = do
    deps <- getFileDeps fp
    level <- traverse getAllFileDeps deps
    pure $ if null level
        then deps
        else nubOrd (concat (deps : level))

fromImport :: Import -> IO (Maybe FilePath)
fromImport = fromImportType . importType . importHashed

fromImportType :: ImportType -> IO (Maybe FilePath)
fromImportType (Local pre fp) = Just <$> localToPath pre fp
fromImportType _              = pure Nothing
