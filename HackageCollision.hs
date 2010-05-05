{-# LANGUAGE DeriveDataTypeable #-}

import Foreign

import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import qualified Data.List.Key as Key
import Data.Char

import System
import System.Console.CmdArgs
import System.FilePath
import System.Directory
import System.IO
import System.IO.Unsafe

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse hiding (ParseOk)
import Distribution.Verbosity

data HackageCollision = HackageCollision
    { dir :: FilePath
    } deriving (Data, Typeable, Show, Eq)

hackageCollision = mode HackageCollision
    { dir = def &= args & typ "DIR" & argPos 0
    }

modes = [hackageCollision]

readFile' f = do
  h <- openFile f ReadMode
  s <- hFileSize h
  fp <- mallocForeignPtrBytes (fromIntegral s)
  len <- withForeignPtr fp $ \buf -> hGetBuf h buf (fromIntegral s)
  lazySlurp fp 0 len

lazySlurp :: ForeignPtr Word8 -> Int -> Int -> IO String
lazySlurp fp ix len
  | ix == len = return []
  | otherwise = do
     c <- withForeignPtr fp $ \p -> peekElemOff p ix
     cs <- unsafeInterleaveIO (lazySlurp fp (ix+1) len)
     return (chr (fromIntegral c) : cs)

parseFile filename = readFile' filename >>= return . parseModule

parseResultToMaybe (ParseOk a) = Just a
parseResultToMaybe _ = Nothing

getVisibleDirectoryContents d = filter (`notElem` [".",".."]) <$> getDirectoryContents d

main = do
    args <- cmdArgs "HackageCollision v0.1, (C) Edward Z. Yang 2010" modes
    let directory = dir args
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
                                     . catMaybes
                                     . map parseResultToMaybe
                                     . catMaybes
                                     $ parses

getPublicNames :: HsModule -> State (Map HsName (Set Module)) ()
getPublicNames (HsModule _ m (Just exports) _ decls) = mapM_ handleExport exports
    where handleExport (HsEVar (UnQual n)) = add n
          handleExport (HsEAbs (UnQual n)) = add n
          handleExport (HsEThingAll (UnQual n)) = add n -- XXX also lookup the rest
          -- XXX: Can a qualified export have unqualified insides? I
          -- don't think so...
          handleExport (HsEThingWith (UnQual n) cs) = add n >> mapM_ handleCName cs
          handleExport _ = return ()
          handleCName (HsVarName n) = add n
          handleCName (HsConName n) = add n
          add n = modify (\s -> Map.insertWith Set.union n (Set.singleton m) s)
getPublicNames _ = return ()

getPublicModulePaths :: FilePath -> String -> IO [FilePath]
getPublicModulePaths dir name = do
    let subdir = dir </> name
    version <- head <$> getVisibleDirectoryContents subdir
    let realdir = subdir </> version </> name ++ "-" ++ version
        package = realdir </> name ++ ".cabal"
    gd <- readPackageDescription silent package
    return . map ((realdir </>) . (++ ".hs") . toFilePath)
           . libraryToModules
           $ condLibraries gd ++ uncondLibraries gd

libraryToModules :: [Library] -> [ModuleName]
libraryToModules = concatMap exposedModules

condLibraries gd = maybe [] (execWriter . handler) (condLibrary gd)
    where handler (CondNode a _ ps) = tell [a] >> mapM_ handler' ps
          handler' (_, n, Just m) = handler n >> handler m
          handler' (_, n, Nothing) = handler n

uncondLibraries gd = maybe [] return . library $ packageDescription gd
