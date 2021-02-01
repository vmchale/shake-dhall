module Dhall.Dep ( getFileDeps
                 , getAllFileDeps
                 ) where

import           Control.Exception         (throw)
import           Control.Monad             ((<=<))
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable             (toList)
import qualified Data.Text.IO              as T
import           Dhall.Core                (Import (Import), ImportMode (Code),
                                            ImportType (Local), importHashed,
                                            importType)
import           Dhall.Import              (localToPath)
import           Dhall.Parser              (exprFromText)
import           System.Directory          (canonicalizePath,
                                            makeRelativeToCurrentDirectory)
import           System.FilePath           (isAbsolute, takeDirectory, (</>))

-- | Given a path, the file paths it depends on
getFileDeps :: FilePath -> IO [FilePath]
getFileDeps fp =
    traverse canonicalizeRelative . catFilePaths =<< getCodeDeps fp

getCodeDeps :: FilePath -> IO [DhallImport]
getCodeDeps fp = do
    contents <- T.readFile fp
    let fileDir = takeDirectory fp
        fileMod fp' = if isAbsolute fp' then fp' else fileDir </> fp'
        importMod = mapFp fileMod
        tree = either throw id (exprFromText fp contents)
        imports = toList tree
    filter isRelevant <$>
        traverse ((importMod <$>) . fromImport) imports

canonicalizeRelative :: FilePath -> IO FilePath
canonicalizeRelative = makeRelativeToCurrentDirectory <=< canonicalizePath

getAllCodeDeps :: FilePath -> IO [DhallImport]
getAllCodeDeps fp = do
    deps <- traverse (traverseFp canonicalizeRelative) =<< getCodeDeps fp
    let nextSrc = codeImports deps
    level <- traverse getAllCodeDeps nextSrc
    pure $ if null level
        then deps
        else nubOrd (concat (deps : level))

-- | Get all transitive dependencies
getAllFileDeps :: FilePath -> IO [FilePath]
getAllFileDeps =
    fmap catFilePaths . getAllCodeDeps

data DhallImport = DhallCode FilePath
                 | OtherImport FilePath
                 | Irrelevant -- for URLs and such
                 deriving (Ord, Eq)

codeImports :: [DhallImport] -> [FilePath]
codeImports []                 = []
codeImports (Irrelevant:is)    = codeImports is
codeImports (DhallCode fp:is)  = fp : codeImports is
codeImports (OtherImport{}:is) = codeImports is

mapFp :: (FilePath -> FilePath) -> DhallImport -> DhallImport
mapFp f (DhallCode fp)   = DhallCode $ f fp
mapFp f (OtherImport fp) = OtherImport $ f fp
mapFp _ Irrelevant       = Irrelevant

traverseFp :: Applicative f => (FilePath -> f FilePath) -> DhallImport -> f DhallImport
traverseFp f (DhallCode fp)   = DhallCode <$> f fp
traverseFp f (OtherImport fp) = OtherImport <$> f fp
traverseFp _ Irrelevant       = pure Irrelevant

catFilePaths :: [DhallImport] -> [FilePath]
catFilePaths []                  = []
catFilePaths (Irrelevant:is)     = catFilePaths is
catFilePaths (DhallCode fp:is)   = fp : catFilePaths is
catFilePaths (OtherImport fp:is) = fp : catFilePaths is

isRelevant :: DhallImport -> Bool
isRelevant Irrelevant = False
isRelevant _          = True

fromImport :: Import -> IO DhallImport
fromImport i@(Import _ Code) = asDhall <$> mImport i
fromImport i                 = asOther <$> mImport i

mImport :: Import -> IO (Maybe FilePath)
mImport = fromImportType . importType . importHashed

asDhall :: Maybe FilePath -> DhallImport
asDhall (Just fp) = DhallCode fp
asDhall Nothing   = Irrelevant

asOther :: Maybe FilePath -> DhallImport
asOther (Just fp) = OtherImport fp
asOther Nothing   = Irrelevant

fromImportType :: ImportType -> IO (Maybe FilePath)
fromImportType (Local pre fp) = Just <$> localToPath pre fp
fromImportType _              = pure Nothing
