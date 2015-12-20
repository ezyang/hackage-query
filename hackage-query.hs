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
import Data.List
import Data.Version

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
import Distribution.Version

--------------
-- Driver code

data HackageQuery = Collisions { argDir :: FilePath }
                  | Tools { argDir :: FilePath }
                  | Dependencies { argDir :: FilePath }
    deriving (Data, Typeable, Show, Eq)

dirFlags = args &= typ "HACKAGE-DIR" &= argPos 0

mymodes :: HackageQuery
mymodes = modes [collisions, tools, dependencies]

main :: IO ()
main = do
    args <- cmdArgs mymodes
    case args of
        Collisions {} -> runCollisions args >> return ()
        Tools {} -> runTools args >> return ()
        Dependencies {} -> runDependencies args >> return ()

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

withPackages' :: (FilePath -> String -> (FilePath -> GenericPackageDescription -> IO a) -> IO r)
              -> FilePath
              -> (FilePath -> GenericPackageDescription -> IO a)
              -> IO [r]
withPackages' h hackageDir f = do
    packageNames <- getVisibleDirectoryContents hackageDir
    mapM (\name -> h hackageDir name f) packageNames

withPackages1 = withPackages' withPackage1
withPackages = withPackages' withPackage

-- Runs an action with a description of the package
withPackage1 :: FilePath -> String
             -> (FilePath -> GenericPackageDescription -> IO a)
             -> IO a
withPackage1 hackageDir packageName f = do
    packageDir <- findLatestPackageDir hackageDir packageName
    pkgd <- readPackageDir packageDir packageName
    f packageDir pkgd

-- Handles the case where there are multiple versions
withPackage :: FilePath -> String
            -> (FilePath -> GenericPackageDescription -> IO a)
            -> IO (String, [a])
withPackage hackageDir packageName f = do
    packageDirs <- findAllPackageDirs hackageDir packageName
    r <- mapM (\d -> readPackageDir d packageName >>= f d) packageDirs
    return (packageName, r)

-- Runs an action for each visible module in a package.
forModules :: FilePath -> GenericPackageDescription
           -> (Module -> IO a)
           -> IO [a]
forModules packageDir pkgd f = do
    let moduleNames = packageModuleNames pkgd
        sourceDirs  = packageSourceDirs  packageDir pkgd
        errHandler :: SomeException -> IO [a]
        errHandler _ = return []
    filePairs <- findModuleFiles (packageDir:sourceDirs) [".hs"] moduleNames `catch` errHandler
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
-- XXX method is kind of skeevy
findLatestPackageDir :: FilePath -> String -> IO FilePath
findLatestPackageDir hackageDir packageName = (head . reverse . sort) `fmap` findAllPackageDirs hackageDir packageName

-- | Determines the directory corresponding to the package directory of
-- the latest version of 'packageName' in an extracted Hackage archive
-- located in 'hackageDir'.
findAllPackageDirs :: FilePath -> String -> IO [FilePath]
findAllPackageDirs hackageDir packageName = do
    let versionsDir = hackageDir </> packageName
    versions <- getVisibleDirectoryContents versionsDir
    -- XXX assumes that no new directory is created
    return $ map (\v -> versionsDir </> v) versions

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

collisions = Collisions { argDir = def &= dirFlags }
          &= help "Search Hackage for identifier collisions"
runCollisions args = do
    let hackageDir = argDir args
    packageNames <- getVisibleDirectoryContents hackageDir
    bag <- mconcat . concat <$> mapM (findIdentifiers hackageDir) packageNames
    render . Key.sort (negate . Set.size . snd) . Map.toList . unSetMap $ bag
    where render :: [(Name, Set Syntax.ModuleName)] -> IO ()
          render results = do
                let xs = filter isSymbol results
                mapM_ renderResult xs
                mapM_ renderModules xs
                    where renderResult  (v, ms) = putStrLn (show (Set.size ms) ++ " " ++ unwrapName v)
                          renderModules (v, ms) = putStrLn (show v ++ " " ++ show ms)
          unwrapName (Syntax.Ident n) = n
          unwrapName (Syntax.Symbol n) = n
          isSymbol (Syntax.Symbol _, _) = True
          isSymbol _ = False

-- | Grabs the public identifiers given a Hackage directory and a module name.

findIdentifiers :: FilePath -> String -> IO [SetMap Name Syntax.ModuleName]
findIdentifiers hackageDir packageName =
    withPackage1 hackageDir packageName $ \packageDir pkgd ->
        forModules packageDir pkgd $ \m ->
            evaluate (modulePublicIdentifiers m)

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

unPackageName (PackageName s) = s
getPackageName pkgd = unPackageName (pkgName (package (packageDescription pkgd)))

--------
-- Tools

tools = Tools { argDir = def &= dirFlags }
          &= help "Search Hackage for build tool usage"
runTools args = do
    let hackageDir = argDir args
    results <- withPackages1 hackageDir $ \pkgDir pkgd -> do
        let packageName = getPackageName pkgd
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

----------------
-- Compatibility

-- XXX watch out: you want easy to handle data first
-- so that rapid iteration on visualization is possible


-- package name, package dep, date, *versions (blob)*

dependencies = Dependencies { argDir = def &= dirFlags }
            &= help "Analyze per-version dependencies"
runDependencies args = do
    let hackageDir = argDir args
    withPackages hackageDir $ \pkgDir pkgd -> do
        let packageName = getPackageName pkgd
        let packageVer = packageVersion pkgd
        let deps = buildDepends (packageDescription pkgd)
        -- XXX We're going to falsely assume that there is only one version
        -- interval
        forM_ deps $ \(Dependency dep ver) ->
            case versionIntervals (toVersionIntervals ver) of
              ((LowerBound lb _,rub):xs) -> do
                when (not (null xs)) $ hPutStrLn stderr ("oops, too much data " ++ packageName ++ "-" ++ showVersion packageVer)
                let ub = case rub of NoUpperBound -> Nothing
                                     UpperBound x _ -> Just x
                putStrLn $ packageName ++ "," ++ showVersion packageVer ++ "," ++ unPackageName dep ++ "," ++ showVersion lb ++ "," ++ maybe "" showVersion ub
              [] -> hPutStrLn stderr ("oops, no data " ++ packageName ++ "-" ++ showVersion packageVer)
