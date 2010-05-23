{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (readFile)

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe
import Data.List
import Data.Char

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Key as Key

import System.Console.CmdArgs
import System.FilePath
import System.Directory
import System.IO.Strict

import Language.Haskell.Syntax
import qualified Language.Haskell.Parser as Haskell

import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription
import qualified Distribution.PackageDescription.Parse as DistParse
import Distribution.Verbosity

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

parseFile :: FilePath -> IO (Haskell.ParseResult HsModule)
parseFile filename = Haskell.parseModule <$> readFile filename

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

render :: [(HsName, Set Module)] -> IO ()
render results = do
    mapM_ renderResult results
    mapM_ renderModules $ take 50 results
        where renderResult  (v, ms) = putStr (show (Set.size ms)) >> putStr " " >> print v
              renderModules (v, ms) = putStr (show v) >> putStr " " >> print ms

getPublicNamesFromPackage :: FilePath -> String -> IO (SetMap HsName Module)
getPublicNamesFromPackage directory name = do
    paths <- getPublicModulePaths directory name
    parses <- mapM ((`catch` const (return Nothing)) . fmap Just . parseFile) paths
    return $ (`execState` Map.empty) . mapM_ getPublicNames
                                     . mapMaybe parseResultToMaybe
                                     . catMaybes
                                     $ parses

getPublicNames :: HsModule -> State (Map HsName (Set Module)) ()
getPublicNames (HsModule _ m (Just exports) _ _) = mapM_ handleExport exports
    where handleExport (HsEVar (UnQual n)) = add n
          handleExport (HsEAbs (UnQual n)) = add n
          handleExport (HsEThingAll (UnQual n)) = add n -- XXX also lookup the rest
          -- XXX: Can a qualified export have unqualified insides? I
          -- don't think so...
          handleExport (HsEThingWith (UnQual n) cs) = add n >> mapM_ handleCName cs
          handleExport _ = return ()
          handleCName (HsVarName n) = add n
          handleCName (HsConName n) = add n
          add n = modify (Map.insertWith Set.union n (Set.singleton m))
getPublicNames _ = return ()

getPublicModulePaths :: FilePath -> String -> IO [FilePath]
getPublicModulePaths d name = do
    let subdir = d </> name
    version <- head <$> getVisibleDirectoryContents subdir
    let realdir = subdir </> version </> name ++ "-" ++ version
        pkg = realdir </> name ++ ".cabal"
    gd <- DistParse.readPackageDescription silent pkg
    return . map ((realdir </>) . (++ ".hs") . toFilePath)
           . concatMap exposedModules
           $ packageLibraries gd

packageLibraries :: GenericPackageDescription -> [Library]
packageLibraries gd = condLibraries gd ++ uncondLibraries gd
    where condLibraries gd = maybe [] (execWriter . handleCondNode) (condLibrary gd)
            where handleCondNode (CondNode a _ ps) = tell [a] >> mapM_ handleTuple ps
                  handleTuple (_, n, Just m) = handleCondNode n >> handleCondNode m
                  handleTuple (_, n, Nothing) = handleCondNode n
          uncondLibraries gd = maybe [] return . library $ packageDescription gd
