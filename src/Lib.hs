{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map
import RIO.Process

import Control.Monad (when)
import Data.Aeson (FromJSON, Value(..), (.:), parseJSON)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TEL
import Data.Time
       (defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone,
        utcToLocalTime)
import Data.Version (showVersion)
import qualified Data.Yaml as Y
import Options.Applicative
import Paths_borg_runner (version)
import System.Directory (doesFileExist)
import System.Environment (setEnv)
import System.Exit (die)
import System.Posix.User (getRealUserID)

data App = App
  { appConfig :: !BorgConfig
  , appLogger :: !LogFunc
  , appProcessContext :: !ProcessContext
  }

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

class HasConfig env where
  configL :: Lens' env BorgConfig

instance HasConfig App where
  configL = lens appConfig (\x y -> x {appConfig = y})

instance HasLogFunc App where
  logFuncL = lens appLogger (\x y -> x {appLogger = y})

data BorgRunnerOpts = BorgRunnerOpts
  { configFilePath :: FilePath
  , debug :: Bool
  }

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption
    (showVersion version)
    (short 'v' <> long "version" <> help "Show version.")

borgRunnerOpts :: Parser BorgRunnerOpts
borgRunnerOpts =
  BorgRunnerOpts <$>
  strOption
    (long "config" <> short 'c' <> metavar "PATH" <>
     help "Absolute path to the config file.") <*>
  switch (long "debug" <> help "Debug mode. Verbose output.")

data BorgConfig = BorgConfig
  { excludes :: [T.Text]
  , includes :: [T.Text]
  , mountCommand :: Command
  , mountPath :: FilePath
  , repoName :: T.Text
  , networkName :: T.Text
  , networkDevice :: T.Text
  , password :: T.Text
  } deriving (Eq, Show)

instance FromJSON BorgConfig where
  parseJSON (Object o) = do
    excludes <- o .: "excludes"
    includes <- o .: "includes"
    mountCommand <- o .: "mount_command"
    mountPath <- o .: "mount_path"
    repoName <- o .: "repo_name"
    networkName <- o .: "network_name"
    networkDevice <- o .: "network_device"
    password <- o .: "password"
    return $ BorgConfig {..}
  parseJSON _ = fail "Expected Object for Config value"

decodeConfig :: FilePath -> IO (Either Y.ParseException BorgConfig)
decodeConfig p = Y.decodeFileEither p

readConfig :: FilePath -> IO (BorgConfig)
readConfig p = do
  exists <- doesFileExist p
  when (not exists) (die "Config file does not exist.")
  c <- decodeConfig p
  case c of
    Left err -> die (show err)
    Right c' -> return c'

entrypoint :: BorgRunnerOpts -> IO ()
entrypoint (BorgRunnerOpts configFilePath debug) = do
  requireRoot
  c <- readConfig configFilePath
  lo <- logOptionsHandle stderr debug
  let env = Map.fromList [("BORG_PASSPHRASE", (password c)), ("DISPLAY", ":0")]
  defPc <- mkDefaultProcessContext
  pc <- modifyEnvVars defPc (\xs -> Map.union env xs)
  withLogFunc lo $ \l -> do
    let app = App {appConfig = c, appLogger = l, appProcessContext = pc}
    runRIO app $ runBackup

type Command = T.Text

requireRoot :: IO ()
requireRoot = do
  uid <- getRealUserID
  when (not $ uid == 0) (die "Please run as root")

setBorgEnv :: T.Text -> IO ()
setBorgEnv p = setEnv "BORG_PASSPHRASE" (show p)

mountBackups ::
     (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
mountBackups = do
  logInfo "Mounting backup volume"
  env <- ask
  let path = mountPath $ env ^. configL
  let cmd = mountCommand $ env ^. configL
  (out, err) <- proc "mount" [] readProcess_
  logDebug (displayShow out)
  logDebug (displayShow err)
  if (T.isInfixOf (T.pack path) (textify out))
    then logInfo "Backup volume is already mounted"
    else proc (T.unpack cmd) [] runProcess_

umountBackups ::
     (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
umountBackups = do
  logInfo "Unmounting backup volume"
  env <- ask
  let path = mountPath $ env ^. configL
  proc "umount" [path] runProcess_

notify :: (HasLogFunc env, HasProcessContext env) => T.Text -> RIO env ()
notify m = do
  logDebug $ displayShow $ "Notifying user: " <> m
  proc "/usr/bin/notify-send" ["-u", "normal", T.unpack m] runProcess_

runBackup ::
     (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
runBackup = do
  env <- ask
  let config = env ^. configL
  let net = networkName config
  logDebug "Starting Borg Backup"
  nets <- getNetworks
  today <- liftIO getToday
  when (T.isInfixOf ("yes:" <> net) nets) $ do
    logInfo "Connected to home wifi"
    logInfo "Starting backup process"
    mountBackups
    backups <- listBackups
    when
      (T.isInfixOf today backups)
      (liftIO $ die "Today's backup is already there. Doing nothing.")
    logInfo "Backing up stuffz"
    logInfo "Notifying user"
    notify "Borg backup started"
    runBackup'
    pruneBackup
    notify "Borg backup finished"
  umountBackups

textify :: BL.ByteString -> T.Text
textify = TL.toStrict . TEL.decodeUtf8

getToday :: IO (Text)
getToday = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  return $
    T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" $ utcToLocalTime tz now

listBackups ::
     (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env (T.Text)
listBackups = do
  env <- ask
  let config = env ^. configL
  -- The list command actually writes its output to stdout as would be expected
  (out, err) <-
    proc "borg" (map T.unpack $ ["list", repository config]) readProcess_
  logDebug (displayShow out)
  logDebug (displayShow err)
  return $ textify out

getNetworks :: (HasLogFunc env, HasProcessContext env) => RIO env (T.Text)
getNetworks = do
  (out, err) <-
    proc "nmcli" ["-t", "-f", "active,ssid", "dev", "wifi"] readProcess_
  logDebug (displayShow out)
  logDebug (displayShow err)
  return $ textify out

repository :: BorgConfig -> T.Text
repository config = (T.pack $ mountPath config) <> "/" <> repoName config

runBackup' ::
     (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
runBackup' = do
  env <- ask
  let config = env ^. configL
  let name = "'{hostname}-{now}'"
  (_, err) <-
    proc
      "borg"
      (map T.unpack $
       [ "create"
       , "--compression"
       , "zlib,9"
       , "-v"
       , "--stats"
       , (repository config) <> "::" <> name
       ] ++
       (includes config) ++ ["--exclude-caches"] ++ (exs $ excludes config))
      readProcess_
    -- We want to always log stderr here since borg writes all of it's logging to stderr by default (see: man borg).
    -- This is because borg want to be as silent as possible and the default log lever is WARNING.
    -- We on the other hand want to log a summary at the end of the process so that user can see what was going on
    -- hence the use of the "--stats" flag.
  logInfo (displayShow err)
  where
    exs xs = map (\x -> "--exclude=" <> x) xs

pruneBackup ::
     (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
pruneBackup = do
  env <- ask
  let config = env ^. configL
  (out, err) <-
    proc
      "borg"
      (map T.unpack $
       [ "prune"
       , "-v"
       , "--list"
       , repository config
       , "--keep-daily=7"
       , "--keep-weekly=4"
       , "--keep-monthly=6"
       , "--prefix=" <> "'{hostname}-'"
       ])
      readProcess_
  logInfo (displayShow out)
  logDebug (displayShow err)
