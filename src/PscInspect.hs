module PscInspect (
    getImportableModules,
    hasPursExtension
) where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

bowerComponentDir :: FilePath
bowerComponentDir = "bower_components"
bowerSourceDir :: FilePath
bowerSourceDir = "src"

type ModuleName = String

hasPursExtension :: FilePath -> Bool
hasPursExtension fp = FilePath.takeExtension fp == ".purs"

-- TODO Check if the dir contains any .purs files
isPurescriptModuleDir :: FilePath -> IO Bool
isPurescriptModuleDir = Directory.doesDirectoryExist

toPureScriptModule :: FilePath -> IO (Maybe ModuleName)
toPureScriptModule fp =
    if hasPursExtension fp
        then return $ Just $ FilePath.takeBaseName fp
        else do
            dirExists <- isPurescriptModuleDir fp
            if dirExists
                then return $ Just $ FilePath.takeBaseName fp
                else return Nothing

getBowerComponentModules :: FilePath -> IO [FilePath]
getBowerComponentModules bowerComponentPath = do
    let srcDir = bowerComponentPath </> bowerSourceDir
    srcDirExists <- Directory.doesDirectoryExist srcDir
    if srcDirExists
        then do
            srcContents <- getEntries srcDir
            pureScriptModules <- mapM toPureScriptModule srcContents
            return $ catMaybes pureScriptModules
        else return []

removeDotDirs :: [FilePath] -> [FilePath]
removeDotDirs = filter (\d -> d /= "." && d /= "..")

getEntries :: FilePath -> IO [FilePath]
getEntries fp = do
    dirs <- Directory.getDirectoryContents fp
    return $ (fp </>) <$> removeDotDirs dirs

uniqueSort :: Ord a => [a] -> [a]
uniqueSort = sort >>> group >>> fmap head

getImportableModules :: IO [String]
getImportableModules = do
    cwd <- Directory.getCurrentDirectory
    let bowerDir = cwd </> bowerComponentDir
    bowerComponentDirs <- getEntries bowerDir
    importableModules <- mapM getBowerComponentModules bowerComponentDirs
    return $ uniqueSort $ concat importableModules
