{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
-- | Cache information about previous builds
module Stack.Build.Cache
    ( tryGetBuildCache
    , tryGetConfigCache
    , tryGetCabalMod
    , getInstalledExes
    , buildCacheTimes
    , tryGetFlagCache
    , deleteCaches
    , markExeInstalled
    , markExeNotInstalled
    , writeFlagCache
    , writeBuildCache
    , writeConfigCache
    , writeCabalMod
    , setTestSuccess
    , unsetTestSuccess
    , checkTestSuccess
    , setTestBuilt
    , unsetTestBuilt
    , checkTestBuilt
    , setBenchBuilt
    , unsetBenchBuilt
    , checkBenchBuilt
    ) where

import           Control.Exception.Enclosed (catchIO, handleIO, tryIO)
import           Control.Monad.Catch        (MonadThrow, catch, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Reader
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, mapMaybe)
import           GHC.Generics (Generic)
import           Path
import           Path.IO
import           Stack.Build.Types
import           Stack.Constants
import           Stack.Types
import           System.Directory           (createDirectoryIfMissing,
                                             getDirectoryContents,
                                             removeFile)
import           System.IO.Error (isDoesNotExistError)

-- | Directory containing files to mark an executable as installed
exeInstalledDir :: (MonadReader env m, HasEnvConfig env, MonadThrow m)
                => InstallLocation -> m (Path Abs Dir)
exeInstalledDir Snap = (</> $(mkRelDir "installed-packages")) `liftM` installationRootDeps
exeInstalledDir Local = (</> $(mkRelDir "installed-packages")) `liftM` installationRootLocal

-- | Get all of the installed executables
getInstalledExes :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadThrow m)
                 => InstallLocation -> m [PackageIdentifier]
getInstalledExes loc = do
    dir <- exeInstalledDir loc
    files <- liftIO $ handleIO (const $ return []) $ getDirectoryContents $ toFilePath dir
    return $ mapMaybe parsePackageIdentifierFromString files

-- | Mark the given executable as installed
markExeInstalled :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadThrow m)
                 => InstallLocation -> PackageIdentifier -> m ()
markExeInstalled loc ident = do
    dir <- exeInstalledDir loc
    liftIO $ createDirectoryIfMissing True $ toFilePath dir
    ident' <- parseRelFile $ packageIdentifierString ident
    let fp = toFilePath $ dir </> ident'
    -- TODO consideration for the future: list all of the executables
    -- installed, and invalidate this file in getInstalledExes if they no
    -- longer exist
    liftIO $ writeFile fp "Installed"

-- | Mark the given executable as not installed
markExeNotInstalled :: (MonadReader env m, HasEnvConfig env, MonadIO m, MonadThrow m)
                    => InstallLocation -> PackageIdentifier -> m ()
markExeNotInstalled loc ident = do
    dir <- exeInstalledDir loc
    ident' <- parseRelFile $ packageIdentifierString ident
    let fp = toFilePath $ dir </> ident'
    liftIO $ catchIO (removeFile fp) (\_ -> return ())

-- | Stored on disk to know whether the flags have changed or any
-- files have changed.
data BuildCache = BuildCache
    { buildCacheTimes :: !(Map FilePath FileCacheInfo)
      -- ^ Modification times of files.
    }
    deriving (Generic)
instance Binary BuildCache

-- | Try to read the dirtiness cache for the given package directory.
tryGetBuildCache :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m, HasEnvConfig env)
                 => Path Abs Dir -> m (Maybe (Map FilePath FileCacheInfo))
tryGetBuildCache = liftM (fmap buildCacheTimes) . tryGetCache buildCacheFile

-- | Try to read the dirtiness cache for the given package directory.
tryGetConfigCache :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m, HasEnvConfig env)
                  => Path Abs Dir -> m (Maybe ConfigCache)
tryGetConfigCache = tryGetCache configCacheFile

-- | Try to read the mod time of the cabal file from the last build
tryGetCabalMod :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m, HasEnvConfig env)
               => Path Abs Dir -> m (Maybe ModTime)
tryGetCabalMod = tryGetCache configCabalMod

-- | Try to load a cache.
tryGetCache :: (MonadIO m, Binary a, MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m, HasEnvConfig env)
            => (Path Abs Dir -> m (Path Abs File))
            -> Path Abs Dir
            -> m (Maybe a)
tryGetCache get' dir = do
    fp <- get' dir
    liftIO
        (catch
             (fmap (decodeMaybe . L.fromStrict) (S.readFile (toFilePath fp)))
             (\e -> if isDoesNotExistError e
                       then return Nothing
                       else throwM e))
  where decodeMaybe =
            either (const Nothing) (Just . thd) . Binary.decodeOrFail
          where thd (_,_,x) = x

-- | Write the dirtiness cache for this package's files.
writeBuildCache :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m, HasEnvConfig env)
                => Path Abs Dir -> Map FilePath FileCacheInfo -> m ()
writeBuildCache dir times =
    writeCache
        dir
        buildCacheFile
        (BuildCache
         { buildCacheTimes = times
         })

-- | Write the dirtiness cache for this package's configuration.
writeConfigCache :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m, HasEnvConfig env)
                => Path Abs Dir
                -> ConfigCache
                -> m ()
writeConfigCache dir = writeCache dir configCacheFile

-- | See 'tryGetCabalMod'
writeCabalMod :: (MonadIO m, MonadReader env m, HasConfig env, MonadThrow m, MonadLogger m, HasEnvConfig env)
              => Path Abs Dir
              -> ModTime
              -> m ()
writeCabalMod dir = writeCache dir configCabalMod

-- | Delete the caches for the project.
deleteCaches :: (MonadIO m, MonadReader env m, HasConfig env, MonadLogger m, MonadThrow m, HasEnvConfig env)
             => Path Abs Dir -> m ()
deleteCaches dir = do
    {- FIXME confirm that this is acceptable to remove
    bfp <- buildCacheFile dir
    removeFileIfExists bfp
    -}
    cfp <- configCacheFile dir
    removeFileIfExists cfp

-- | Write to a cache.
writeCache :: (Binary a, MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
           => Path Abs Dir
           -> (Path Abs Dir -> m (Path Abs File))
           -> a
           -> m ()
writeCache dir get' content = do
    fp <- get' dir
    liftIO
        (L.writeFile
             (toFilePath fp)
             (Binary.encode content))

flagCacheFile :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env)
              => Installed
              -> m (Path Abs File)
flagCacheFile installed = do
    rel <- parseRelFile $
        case installed of
            Library gid -> ghcPkgIdString gid
            Executable ident -> packageIdentifierString ident
    dir <- flagCacheLocal
    return $ dir </> rel

-- | Loads the flag cache for the given installed extra-deps
tryGetFlagCache :: (MonadIO m, MonadThrow m, MonadReader env m, HasEnvConfig env)
                => Installed
                -> m (Maybe ConfigCache)
tryGetFlagCache gid = do
    file <- flagCacheFile gid
    eres <- liftIO $ tryIO $ Binary.decodeFileOrFail $ toFilePath file
    case eres of
        Right (Right x) -> return $ Just x
        _ -> return Nothing

writeFlagCache :: (MonadIO m, MonadReader env m, HasEnvConfig env, MonadThrow m)
               => Installed
               -> ConfigCache
               -> m ()
writeFlagCache gid cache = do
    file <- flagCacheFile gid
    liftIO $ do
        createDirectoryIfMissing True $ toFilePath $ parent file

        Binary.encodeFile (toFilePath file) cache

-- | Mark a test suite as having succeeded
setTestSuccess :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
               => Path Abs Dir
               -> m ()
setTestSuccess dir =
    writeCache
        dir
        testSuccessFile
        True

-- | Mark a test suite as not having succeeded
unsetTestSuccess :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
                 => Path Abs Dir
                 -> m ()
unsetTestSuccess dir =
    writeCache
        dir
        testSuccessFile
        False

-- | Check if the test suite already passed
checkTestSuccess :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
                 => Path Abs Dir
                 -> m Bool
checkTestSuccess dir =
    liftM
        (fromMaybe False)
        (tryGetCache testSuccessFile dir)

-- | Mark a test suite as having built
setTestBuilt :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
               => Path Abs Dir
               -> m ()
setTestBuilt dir =
    writeCache
        dir
        testBuiltFile
        True

-- | Mark a test suite as not having built
unsetTestBuilt :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
                 => Path Abs Dir
                 -> m ()
unsetTestBuilt dir =
    writeCache
        dir
        testBuiltFile
        False

-- | Check if the test suite already built
checkTestBuilt :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
                 => Path Abs Dir
                 -> m Bool
checkTestBuilt dir =
    liftM
        (fromMaybe False)
        (tryGetCache testBuiltFile dir)

-- | Mark a bench suite as having built
setBenchBuilt :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
               => Path Abs Dir
               -> m ()
setBenchBuilt dir =
    writeCache
        dir
        benchBuiltFile
        True

-- | Mark a bench suite as not having built
unsetBenchBuilt :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
                 => Path Abs Dir
                 -> m ()
unsetBenchBuilt dir =
    writeCache
        dir
        benchBuiltFile
        False

-- | Check if the bench suite already built
checkBenchBuilt :: (MonadIO m, MonadLogger m, MonadThrow m, MonadReader env m, HasConfig env, HasEnvConfig env)
                 => Path Abs Dir
                 -> m Bool
checkBenchBuilt dir =
    liftM
        (fromMaybe False)
        (tryGetCache benchBuiltFile dir)
