{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (readFile, catch)

import Control.Exception
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Key as Key

import System.Console.CmdArgs
import System.FilePath
import System.Directory
import System.IO.Strict
import System.IO hiding (readFile, hGetContents)

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts (parseFileContents)
import qualified Language.Haskell.Exts.Parser as Haskell

import Distribution.PackageDescription
import qualified Distribution.PackageDescription.Parse as DistParse
import Distribution.Verbosity
import Distribution.Simple.Utils

data HackageCollision = HackageCollision
    { argDir :: FilePath
    } deriving (Data, Typeable, Show, Eq)

type SetMap a b = Map a (Set b)

hackageCollision :: Mode HackageCollision
hackageCollision = mode HackageCollision
    { argDir = def &= args & typ "DIR" & argPos 0
    }

modes :: [Mode HackageCollision]
modes = [hackageCollision]

-- | Strictly parses an hs file.
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

main :: IO ()
main = do
    directory <- argDir <$> cmdArgs "HackageCollision v0.1, (C) Edward Z. Yang 2010" modes
    names <- getVisibleDirectoryContents directory
    bag <- Map.unionsWith Set.union <$> mapM (getPublicNamesFromPackage directory) names
    render . Key.sort (negate . Set.size . snd) $ Map.toList bag

render :: [(Name, Set ModuleName)] -> IO ()
render results = do
    mapM_ renderResult results
    mapM_ renderModules $ take 50 results
        where renderResult  (v, ms) = putStr . unwords $ [show (Set.size ms), show v]
              renderModules (v, ms) = putStr . unwords $ [show v, show ms]

getPublicNamesFromPackage :: FilePath -> String -> IO (SetMap Name ModuleName)
getPublicNamesFromPackage directory name = do
    let
        errHandler :: IOException -> IO (Maybe a)
        errHandler e = do
            hPutStrLn stderr (show e) -- (directory ++ ":" ++ name ++ ". " ++ show e)
            return Nothing
    paths <- getPublicModulePaths directory name
    parses <- mapM ((`catch` errHandler) . fmap Just . parseFile) paths
    return $ (`execState` Map.empty) . mapM_ getPublicNames
                                     . mapMaybe parseResultToMaybe
                                     . catMaybes
                                     $ parses

getPublicNames :: Module -> State (Map Name (Set ModuleName)) ()
getPublicNames (Module _ m _ _ (Just exports) _ _) = mapM_ handleExport exports
    where handleExport x = case x of
            EVar (UnQual n) -> add n
            EAbs (UnQual n) -> add n
            EThingAll (UnQual n) -> add n -- XXX also lookup the rest
            -- XXX: Can a qualified export have unqualified insides? I
            -- don't think so...
            EThingWith (UnQual n) cs -> add n >> mapM_ handleCName cs
            _ -> return ()
          handleCName x = case x of
            VarName n -> add n
            ConName n -> add n
          add n = modify (Map.insertWith Set.union n (Set.singleton m))
getPublicNames _ = return ()

-- ddarius : No.  I mean stick the local functions into a where clause,
-- parameterize as necessary, and then rewrite the
-- body in the four or five lines it will then be.

getPublicModulePaths :: FilePath -> String -> IO [FilePath]
getPublicModulePaths d name = do
    let subdir = d </> name
    version <- head <$> getVisibleDirectoryContents subdir
    let realdir = subdir </> version </> name ++ "-" ++ version
        pkg = realdir </> name ++ ".cabal"
    pkgd <- DistParse.readPackageDescription silent pkg
    let buildPrefix = map (realdir </>)
                    . concatMap hsSourceDirs
                    . allBuildInfo
                    . packageDescription
                    $ pkgd
        moduleNames = concatMap exposedModules . packageLibraries $ pkgd
        findModuleName m = do
            file <- findModuleFile (realdir : buildPrefix) [".hs"] m
            return . Just $ realdir </> snd file
        errHandler :: IOException -> IO (Maybe a)
        errHandler _ = {-hPutStrLn stderr (show e) >> -}return Nothing
    maybeModulePaths <- mapM (\m -> findModuleName m `catch` errHandler) moduleNames
    return . catMaybes $ maybeModulePaths

packageLibraries :: GenericPackageDescription -> [Library]
packageLibraries pkgd = condLibraries ++ uncondLibraries
    where condLibraries = maybe [] (execWriter . handleCondNode) (condLibrary pkgd)
            where handleCondNode (CondNode a _ ps) = tell [a] >> mapM_ handleTuple ps
                  handleTuple (_, n, Just m) = handleCondNode n >> handleCondNode m
                  handleTuple (_, n, Nothing) = handleCondNode n
          uncondLibraries = maybe [] return . library $ packageDescription pkgd
