module Main where

import RIO

import Options.Applicative

import Lib

main :: IO ()
main = execParser opts >>= entrypoint
  where
    opts =
      info
        (helper <*> versionOpt <*> borgRunnerOpts)
        (fullDesc <> progDesc "A helper tool for running borg backups." <>
         header "borg-runner - A helper tool for running borg backups.")
