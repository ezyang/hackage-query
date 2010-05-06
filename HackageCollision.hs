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
    { dir :: FilePath
    } deriving (Data, Typeable, Show, Eq)

hackageCollision :: Mode HackageCollision
hackageCollision = mode HackageCollision
    { dir = def &= args & typ "DIR" & argPos 0
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
    directory <- dir <$> cmdArgs "HackageCollision v0.1, (C) Edward Z. Yang 2010" modes
    names <- getVisibleDirectoryContents directory
    bags <- mapM (getPublicNamesFromPackage directory) names
    let bag = Map.unionsWith Set.union bags
        results = Key.sort (negate . Set.size . snd) $ Map.toList bag
    mapM_ renderResult results
    mapM_ renderModules $ take 50 results
        where renderResult (v, ms) = putStr (show (Set.size ms)) >> putStr " " >> print v
              renderModules (v, ms) = putStr (show v) >> putStr " " >> print ms

getPublicNamesFromPackage :: FilePath -> String -> IO (Map HsName (Set Module))
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
           . libraryToModules
           $ condLibraries gd ++ uncondLibraries gd

libraryToModules :: [Library] -> [ModuleName]
libraryToModules = concatMap exposedModules

condLibraries :: GenericPackageDescription -> [Library]
condLibraries gd = maybe [] (execWriter . handler) (condLibrary gd)
    where handler (CondNode a _ ps) = tell [a] >> mapM_ handler' ps
          handler' (_, n, Just m) = handler n >> handler m
          handler' (_, n, Nothing) = handler n

uncondLibraries :: GenericPackageDescription -> [Library]
uncondLibraries gd = maybe [] return . library $ packageDescription gd
