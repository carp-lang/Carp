module Path where

import qualified System.Directory as D
import qualified System.FilePath.Posix as FP
import qualified System.FilePath.Windows as FPW
import qualified System.IO as IO
import Util

(</>) :: FilePath -> FilePath -> FilePath
(</>) = case platform of
  Windows -> (FPW.</>)
  _ -> (FP.</>)

cachePath :: FilePath -> IO FilePath
cachePath = xdgPath D.XdgCache

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = fmap toStandard . D.canonicalizePath . toNative

configPath :: FilePath -> IO FilePath
configPath = xdgPath D.XdgConfig

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing createParents = D.createDirectoryIfMissing createParents . toNative

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = D.removeDirectoryRecursive . toNative

doesPathExist :: FilePath -> IO Bool
doesPathExist = D.doesPathExist . toNative

doesFileExist :: FilePath -> IO Bool
doesFileExist = D.doesFileExist . toNative

getCurrentDirectory :: IO FilePath
getCurrentDirectory = toStandard <$> D.getCurrentDirectory

slurp :: FilePath -> IO String
slurp = IO.readFile . toNative

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = D.setCurrentDirectory . toNative

takeDirectory :: FilePath -> FilePath
takeDirectory = FP.takeDirectory

takeFileName :: FilePath -> FilePath
takeFileName = FP.takeFileName

toNative :: FilePath -> FilePath
toNative = if platform == Windows then map (\x -> if x == '/' then '\\' else x) else id

toStandard :: FilePath -> FilePath
toStandard = if platform == Windows then map (\x -> if x == '\\' then '/' else x) else id

xdgPath :: D.XdgDirectory -> FilePath -> IO FilePath
xdgPath t = fmap toStandard . D.getXdgDirectory t . (</>) "carp" . toNative
