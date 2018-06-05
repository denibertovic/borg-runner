{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import           Prelude                 (print)
import           RIO
import qualified RIO.Map                 as Map
import           RIO.Process

import           Control.Monad           (when)
import           Data.Aeson              (FromJSON, ToJSON, Value (..),
                                          parseJSON, toJSON, (.:), (.=))
import           Data.Semigroup          ((<>))
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TEL
import           Data.Time               (defaultTimeLocale, formatTime,
                                          getCurrentTime, getCurrentTimeZone,
                                          utcToLocalTime)
import           Data.Version            (showVersion)
import           Data.Yaml               (decodeFileEither)
import qualified Data.Yaml               as Y
import           Options.Applicative
import           Paths_borg_runner       (version)
import           System.Directory        (doesFileExist, makeAbsolute)
import           System.Environment      (lookupEnv, setEnv)
import           System.Exit             (ExitCode (..), die, exitWith)
import           System.Posix.User       (getEffectiveUserID, getRealUserID)


data App = App { appConfig         :: !BorgConfig
               , appLogger         :: !LogFunc
               , appProcessContext :: !ProcessContext
               }

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

class HasConfig env where
  configL :: Lens' env BorgConfig

instance HasConfig App where
  configL = lens appConfig (\x y -> x { appConfig = y })

instance HasLogFunc App where
  logFuncL = lens appLogger (\x y -> x { appLogger = y })

data BorgRunnerOpts = BorgRunnerOpts
            { configFilePath :: FilePath
            , debug          :: Bool
            }

versionOpt = infoOption (showVersion version) (
               short 'v'
               <> long "version"
               <> help "Show version.")

borgRunnerOpts :: Parser BorgRunnerOpts
borgRunnerOpts = BorgRunnerOpts
    <$> strOption
        ( long "config"
        <> short 'c'
        <> metavar "PATH"
        <> help "Absolute path to the config file." )
    <*> switch
        ( long "debug"
        <> help "Debug mode. Verbose output." )


data BorgConfig = BorgConfig { excludes :: [T.Text]
                     , includes         :: [T.Text]
                     , mountCommand     :: Command
                     , mountPath        :: FilePath
                     , repoName         :: T.Text
                     , networkName      :: T.Text
                     , networkDevice    :: T.Text
                     , password         :: T.Text
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
    Right c  -> return c

entrypoint :: BorgRunnerOpts -> IO ()
entrypoint (BorgRunnerOpts configFilePath debug) = do
  requireRoot
  c <- readConfig configFilePath
  lo <- logOptionsHandle stderr debug
  let env = Map.fromList [ ("BORG_PASSPHRASE", (password c))
                         , ("DISPLAY", ":0")
                         ]
  defPc <- mkDefaultProcessContext
  pc <- modifyEnvVars defPc (\xs -> Map.union env xs)
  withLogFunc lo $ \l -> do
    let app = App { appConfig=c, appLogger=l, appProcessContext=pc }
    runRIO app $ runBackup

type Command = T.Text

requireRoot :: IO ()
requireRoot = do
  uid <- getRealUserID
  when (not $ uid == 0) (die "Please run as root")

setBorgEnv :: T.Text -> IO ()
setBorgEnv p = setEnv "BORG_PASSPHRASE" (show p)

mountBackups :: (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
mountBackups = do
  logInfo "Mounting backup volume"
  env <- ask
  let path = mountPath $ env ^. configL
  let cmd = mountCommand $ env ^. configL
  (out,  _) <-  proc "mount" [] readProcess_
  if (T.isInfixOf (T.pack path) (TL.toStrict $ TEL.decodeUtf8 out)) then
      logInfo "Backup volume is already mounted"
  else
      proc (T.unpack cmd) [] runProcess_

umountBackups :: (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
umountBackups = do
  logInfo "Unmounting backup volume"
  env <- ask
  let path = mountPath $ env ^. configL
  proc "umount" [path] runProcess_

notify :: (HasConfig env, HasLogFunc env, HasProcessContext env) => T.Text -> RIO env ()
notify m = proc "/usr/bin/notify-send" ["-u", "normal", T.unpack m] runProcess_

runBackup :: (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
runBackup = do
  env <- ask
  let config = env ^. configL
  let net = networkName config
  logDebug "Starting Borg Backup"
  (out, _) <- proc "nmcli" ["-t", "-f", "active,ssid", "dev", "wifi"] readProcess_
  now <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  let today = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" $ utcToLocalTime tz now
  when (T.isInfixOf ("yes:" <> net) (TL.toStrict $ TEL.decodeUtf8 out)) $ do
    logInfo "Connected to home wifi"
    logInfo "Starting backup process"
    mountBackups
    (out, _) <- proc "borg" (map T.unpack $ ["list", (T.pack $ mountPath config) <> "/" <> (repoName config)]) readProcess_
    when (T.isInfixOf today (TL.toStrict $ TEL.decodeUtf8 out)) (liftIO $ die "Today's backup is already there. Doing nothing.")
    logInfo "Backing up stuffz"
    logInfo "Notifying user"
    notify "Borg backup started"
    runBackup'
    pruneBackup
    notify "Borg backup finished"
    umountBackups

runBackup' :: (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
runBackup' = do
    env <- ask
    let config = env ^. configL
    let name = "'{hostname}-{now}'"
    (out, _) <- proc "borg" ( map T.unpack $ [ "create"
                            , "--compression"
                            , "zlib,9"
                            , "-v"
                            , "--stats"
                            , (T.pack $ mountPath config) <> "/" <> (repoName config) <> "::" <> name
                            ] ++ (includes config) ++ ["--exclude-caches"] ++ (exs $ excludes config))
                            readProcess_
    logDebug (displayShow out)
  where exs xs = map (\x -> "--exclude=" <> x) xs


pruneBackup :: (HasConfig env, HasLogFunc env, HasProcessContext env) => RIO env ()
pruneBackup = do
    (out, _) <- proc "borg" (map T.unpack $ [ "prune"
                            , "-v"
                            , "--list"
                            , "--keep-daily=7"
                            , "--keep-weekly=4"
                            , "--keep-monthly=6"
                            , "--prefix=" <> "'{hostname}-'"
                            ])
                            readProcess_
    logDebug (displayShow out)
