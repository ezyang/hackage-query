{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (readFile, catch)

import Control.Exception
import Control.Applicative
import Control.Monad.Writer

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe
import Data.Foldable (foldMap)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Key as Key

import System.Console.CmdArgs
import System.FilePath
import System.Directory
import System.IO.Strict
import System.IO hiding (readFile, hGetContents)

import Language.Haskell.Exts.Syntax hiding (ModuleName)
import qualified Language.Haskell.Exts.Syntax as Syntax
import Language.Haskell.Exts (parseFileContents)
import qualified Language.Haskell.Exts.Parser as Haskell

import Distribution.PackageDescription
import qualified Distribution.ModuleName as DistModule
import qualified Distribution.PackageDescription.Parse as DistParse
import Distribution.Verbosity
import Distribution.Simple.Utils

--------------
-- Driver code

data HackageCollision = HackageCollision
    { argDir :: FilePath
    } deriving (Data, Typeable, Show, Eq)

hackageCollision :: Mode HackageCollision
hackageCollision = mode HackageCollision
    { argDir = def &= args & typ "DIR" & argPos 0
    }

modes :: [Mode HackageCollision]
modes = [hackageCollision]

main :: IO ()
main = do
    directory <- argDir <$> cmdArgs "HackageCollision v0.1, (C) Edward Z. Yang 2010" modes
    names <- getVisibleDirectoryContents directory
    bag <- mconcat <$> mapM (findIdentifiers directory) names
    render . Key.sort (negate . Set.size . snd) . Map.toList . unSetMap $ bag

render :: [(Name, Set Syntax.ModuleName)] -> IO ()
render results = do
    mapM_ renderResult results
    mapM_ renderModules $ take 50 results
        where renderResult  (v, ms) = putStr . unwords $ [show (Set.size ms), show v]
              renderModules (v, ms) = putStr . unwords $ [show v, show ms]

--------------------
-- Data declarations

newtype SetMap k v = SetMap { unSetMap :: Map k (Set v) }

instance (Ord k, Ord v) => Monoid (SetMap k v) where
    mempty = SetMap Map.empty
    mappend (SetMap a) (SetMap b) = SetMap $ Map.unionWith Set.union a b
    mconcat = SetMap . Map.unionsWith Set.union . map unSetMap

setMapSingleton :: (Ord k, Ord v) => k -> v -> SetMap k v
setMapSingleton k v = SetMap $ Map.singleton k (Set.singleton v)

---------------------
-- Utility functions

-- | Strictly parses a Haskell file with extensions.
parseFile :: FilePath -> IO (Haskell.ParseResult Module)
parseFile filename = tryParse utf8 `catch` ioConst (tryParse latin1)
    where
        ioConst :: a -> IOException -> a
        ioConst a _ = a
        tryParse enc =
            withFile filename ReadMode $ \h -> do
                -- this does the wrong thing for latin1; better idea is to
                -- parametrize over encodings and try both
                hSetEncoding h enc
                contents <- hGetContents h
                -- haskell-src-exts emits a user exception we need to catch
                (return $! parseFileContents contents)
                    `catch` errorHandler
        errorHandler :: ErrorCall -> IO (Haskell.ParseResult Module)
        errorHandler e = return (Haskell.ParseFailed (SrcLoc filename 0 0) (show e))

parseResultToMaybe :: Haskell.ParseResult a -> Maybe a
parseResultToMaybe (Haskell.ParseOk a) = Just a
parseResultToMaybe _ = Nothing

getVisibleDirectoryContents :: FilePath -> IO [String]
getVisibleDirectoryContents d = filter (`notElem` [".",".."]) <$> getDirectoryContents d

---------------------------
-- Actual manipulation code

-- | Grabs the public identifiers given a Hackage directory and a module name.
findIdentifiers :: FilePath -> String -> IO (SetMap Name Syntax.ModuleName)
findIdentifiers hackageDir packageName = do
    packageDir <- findLatestPackageDir hackageDir packageName
    pkgd <- readPackageDir packageDir packageName
    let moduleNames = packageModuleNames pkgd
        sourceDirs  = packageSourceDirs  packageDir pkgd
    files <- liftM (map snd) $ findModuleFiles (packageDir:sourceDirs) [".hs"] moduleNames
    modules <- readModuleFiles files
    return $ foldMap modulePublicIdentifiers modules

-- | Parses a list of module files, dropping module files that fail to
-- parse
readModuleFiles :: [FilePath] -> IO [Module]
readModuleFiles paths = do
    parses <- mapM ((`catch` errHandler) . fmap Just . parseFile) paths
    return . mapMaybe parseResultToMaybe . catMaybes $ parses
    where errHandler :: IOException -> IO (Maybe a)
          errHandler e = do
                hPutStrLn stderr (show e) -- (directory ++ ":" ++ name ++ ". " ++ show e)
                return Nothing

-- | Extracts public identifiers from a module's abstract syntax tree.
modulePublicIdentifiers :: Module -> SetMap Name Syntax.ModuleName
modulePublicIdentifiers (Module _ m _ _ (Just exports) _ _) = mconcat . map handleExport $ exports
    where handleExport x = case x of
            EVar (UnQual n) -> add n
            EAbs (UnQual n) -> add n
            EThingAll (UnQual n) -> add n -- XXX also lookup the rest
            -- XXX: Can a qualified export have unqualified insides? I
            -- don't think so...
            EThingWith (UnQual n) cs -> add n `mappend` (mconcat . map handleCName) cs
            _ -> mempty
          handleCName x = case x of
            VarName n -> add n
            ConName n -> add n
          add n = setMapSingleton n m
-- should also case for an empty exports list, in which case all
-- identifiers are exported
modulePublicIdentifiers _ = mempty

-- | Determines the directory corresponding to the package directory of
-- the latest version of 'packageName' in an extracted Hackage archive
-- located in 'hackageDir'.
findLatestPackageDir :: FilePath -> String -> IO FilePath
findLatestPackageDir hackageDir packageName = do
    let versionsDir = hackageDir </> packageName
    version <- head <$> getVisibleDirectoryContents versionsDir
    return $ versionsDir </> version </> packageName ++ "-" ++ version

-- | Reads the Cabal package file out of a package directory.
readPackageDir :: FilePath -> String -> IO GenericPackageDescription
readPackageDir packageDir packageName = do
    let cabalFile = packageDir </> packageName ++ ".cabal"
    DistParse.readPackageDescription silent cabalFile

-- | Retrieves all of the source directories (where module hirearchies
-- live) in a package.  Paths are prefixed by 'packageDir'.
packageSourceDirs :: FilePath -> GenericPackageDescription -> [FilePath]
packageSourceDirs packageDir = map (packageDir </>)
                             . concatMap hsSourceDirs
                             . allBuildInfo
                             . packageDescription

-- | Retrieves all of the exposed libraries from a package.
packageLibraries :: GenericPackageDescription -> [Library]
packageLibraries pkgd = condLibraries ++ uncondLibraries
    where condLibraries = maybe [] (execWriter . handleCondNode) (condLibrary pkgd)
            where handleCondNode (CondNode a _ ps) = tell [a] >> mapM_ handleTuple ps
                  handleTuple (_, n, Just m) = handleCondNode n >> handleCondNode m
                  handleTuple (_, n, Nothing) = handleCondNode n
          uncondLibraries = maybe [] return . library $ packageDescription pkgd

-- | Retrieves all of the exposed module names from a package.
packageModuleNames :: GenericPackageDescription -> [DistModule.ModuleName]
packageModuleNames = concatMap exposedModules . packageLibraries
