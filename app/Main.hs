module Main where

import           RIO

import           Data.Yaml           (decodeFile)
import qualified Data.Yaml           as Y
import           Lib

import qualified Data.Text           as T
import           Options.Applicative


main :: IO ()
main = execParser opts >>= entrypoint
  where
    opts = info (helper <*> versionOpt <*> borgRunnerOpts)
      ( fullDesc
     <> progDesc "A helper tool for running borg backups."
     <> header "borg-runner - A helper tool for running borg backups." )

