{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (readFile, catch)

import Control.Exception
import Control.Applicative
import Control.Monad.Writer

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Traversable (for)

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

import Distribution.Package
import Distribution.PackageDescription
import qualified Distribution.ModuleName as DistModule
import qualified Distribution.PackageDescription.Parse as DistParse
import Distribution.Verbosity
import Distribution.Simple.Utils

--------------
-- Driver code

data HackageQuery = Collisions { argDir :: FilePath }
                  | Tools { argDir :: FilePath }
    deriving (Data, Typeable, Show, Eq)

dirFlags = args & typ "HACKAGE-DIR" & argPos 0

modes :: [Mode HackageQuery]
modes = [collisions, tools]

main :: IO ()
main = do
    args <- cmdArgs "HackageCollision v0.1, (C) Edward Z. Yang 2010" modes
    case args of
        Collisions {} -> runCollisions args
        Tools {} -> runTools args

--------------------
-- Data declarations

newtype SetMap k v = SetMap { unSetMap :: Map k (Set v) }
    deriving (Show)

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

-- | Returns a list of visible file names in a directory.
getVisibleDirectoryContents :: FilePath -> IO [String]
getVisibleDirectoryContents d = filter (`notElem` [".",".."]) <$> getDirectoryContents d

-------------------------------------
-- Drilling into the Hackage database

withPackages :: FilePath
             -> (String -> FilePath -> GenericPackageDescription -> IO a)
             -> IO [a]
withPackages hackageDir f = do
    packageNames <- getVisibleDirectoryContents hackageDir
    mapM (\name -> withPackage hackageDir name (f name)) packageNames

-- Runs an action with a description of the package
withPackage :: FilePath -> String
            -> (FilePath -> GenericPackageDescription -> IO a)
            -> IO a
withPackage hackageDir packageName f = do
    packageDir <- findLatestPackageDir hackageDir packageName
    pkgd <- readPackageDir packageDir packageName
    f packageDir pkgd

-- Runs an action for each visible module in a package.
forModules :: FilePath -> GenericPackageDescription
           -> (Module -> IO a)
           -> IO [a]
forModules packageDir pkgd f = do
    let moduleNames = packageModuleNames pkgd
        sourceDirs  = packageSourceDirs  packageDir pkgd
    filePairs <- findModuleFiles (packageDir:sourceDirs) [".hs"] moduleNames
    let files = map (uncurry (</>)) filePairs
    modules <- parseModuleFiles files
    forM modules f

-- | Parses a list of module files, dropping module files that fail to
-- parse
parseModuleFiles :: [FilePath] -> IO [Module]
parseModuleFiles paths = do
    parses <- mapM ((`catch` errHandler) . fmap Just . parseFile) paths
    return . mapMaybe parseResultToMaybe . catMaybes $ parses
    where errHandler :: IOException -> IO (Maybe a)
          errHandler e = do
                hPutStrLn stderr (show e) -- (directory ++ ":" ++ name ++ ". " ++ show e)
                return Nothing

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

-------------
-- Collisions

collisions = mode Collisions { argDir = def &= dirFlags }
          &= text "Search Hackage for identifier collisions"
runCollisions args = do
    let hackageDir = argDir args
    packageNames <- getVisibleDirectoryContents hackageDir
    bag <- mconcat . concat <$> mapM (findIdentifiers hackageDir) packageNames
    render . Key.sort (negate . Set.size . snd) . Map.toList . unSetMap $ bag
    where render :: [(Name, Set Syntax.ModuleName)] -> IO ()
          render results = do
                mapM_ renderResult results
                mapM_ renderModules $ take 50 results
                    where renderResult  (v, ms) = putStr . unwords $ [show (Set.size ms), show v]
                          renderModules (v, ms) = putStr . unwords $ [show v, show ms]

-- | Grabs the public identifiers given a Hackage directory and a module name.

findIdentifiers :: FilePath -> String -> IO [SetMap Name Syntax.ModuleName]
findIdentifiers hackageDir packageName =
    withPackage hackageDir packageName $ \packageDir pkgd ->
        forModules packageDir pkgd $ \m ->
            return (modulePublicIdentifiers m)

-- | Extracts public identifiers from a module's abstract syntax tree.
modulePublicIdentifiers :: Module -> SetMap Name Syntax.ModuleName
modulePublicIdentifiers (Module _ m _ _ (Just exports) _ _) = foldMap handleExport $ exports
    where handleExport x = case x of
            EVar (UnQual n) -> add n
            EAbs (UnQual n) -> add n
            EThingAll (UnQual n) -> add n -- XXX also lookup the rest
            -- XXX: Can a qualified export have unqualified insides? I
            -- don't think so...
            EThingWith (UnQual n) cs -> add n `mappend` (foldMap handleCName) cs
            _ -> mempty
          handleCName x = case x of
            VarName n -> add n
            ConName n -> add n
          add n = setMapSingleton n m
-- should also case for an empty exports list, in which case all
-- identifiers are exported
modulePublicIdentifiers _ = mempty

--------
-- Tools

tools = mode Tools { argDir = def &= dirFlags }
          &= text "Search Hackage for build tool usage"
runTools args = do
    let hackageDir = argDir args
    results <- withPackages hackageDir $ \packageName pkgDir pkgd -> do
        let tools = packageBuildToolNames pkgd
        return $ foldMap (\(PackageName tool) -> setMapSingleton tool packageName) tools
    let bag = mconcat results
        sortedResults = Key.sort (negate . Set.size . snd) . Map.toList . unSetMap $ bag
    forM_ sortedResults $ \(tool, packages) -> do
        putStrLn $ tool ++ " (" ++ show (Set.size packages) ++ ")"
        forM_ (Set.toList packages) $ \package ->
            putStrLn $ "    " ++ package
        putStrLn ""

packageBuildToolNames :: GenericPackageDescription -> [PackageName]
packageBuildToolNames pkgd = uncondTools ++ condLibraryTools
    where uncondTools = flattenTools
                      . allBuildInfo
                      . packageDescription
                      $ pkgd
          condLibraryTools = flattenTools
                           . maybe [] (execWriter . handleCondNode)
                           $ (condLibrary pkgd)
          handleCondNode (CondNode a _ ps) = tell [libBuildInfo a] >> mapM_ handleCondTuple ps
          handleCondTuple (_, n, Just m) = handleCondNode n >> handleCondNode m
          handleCondTuple (_, n, Nothing) = handleCondNode n
          flattenTools = map (\(Dependency name _) -> name) . concatMap buildTools
