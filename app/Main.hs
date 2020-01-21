{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Import
import Options.Applicative.Simple
import qualified Paths_cast_finder
import RIO.Process
import Run

main :: IO ()
main = do
    (options, ()) <-
        simpleOptions
            $(simpleVersion Paths_cast_finder.version)
            "Header for command line arguments"
            "Program description, also for command line arguments"
            (Options <$>
             switch (long "verbose" <> short 'v' <> help "Verbose output?") <*>
             switch (long "debug" <> help "Debug output") <*>
             strOption
                 (long "language" <> short 'l' <>
                  help
                      "Which language parser to use and which extesion to filter for") <*>
             many (strArgument (metavar "PATH")))
            empty
    lo <- logOptionsHandle stderr (optionsVerbose options)
    pc <- mkDefaultProcessContext
    withLogFunc lo $ \lf ->
        let app =
                App
                    { appLogFunc = lf
                    , appProcessContext = pc
                    , appOptions = options
                    , appOutputHandle = stdout
                    }
         in runRIO app run
