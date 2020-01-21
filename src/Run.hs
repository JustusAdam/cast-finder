{-# LANGUAGE OverloadedStrings #-}

module Run
    ( run
    ) where

import Import
import RIO.Directory
import RIO.FilePath
import Text.Printf

import Language.C.FindCast
import qualified Language.Java.FindCast as Java

discover :: MonadIO m => FilePath -> m [FilePath]
discover start =
    liftIO $ do
        isFile <- doesFileExist start
        if isFile
            then pure [start]
            else concat <$>
                 (listDirectory start >>= mapM (discover . (start </>)))

langs :: String -> (String, FilePath -> RIO App [(Int, String)])
langs "java" = (".java", Java.analyze)
langs "c" = (".c", analyze)
langs l = error $ "Unsupported language: " ++ l

run :: RIO App ()
run = do
    logInfo "Scanning files"
    lang <- asks (optionsLanguage . appOptions)
    let (ext, analyze) = langs lang
    h <- asks appOutputHandle
    files <-
        filter (isExtensionOf ext) . concat <$>
        (asks (optionsTargets . appOptions) >>= traverse discover)
    results <-
        for files $ \file -> do
            locs <- analyze file
            unless (null locs) $
                liftIO $ do
                    hPrintf h "\n### %v ###\n" file
                    mapM_ (\(loc, cast) -> hPrintf h "%i: %v" loc cast) locs
            pure $ length locs
    liftIO $
        hPrintf
            h
            "\n\nAnalyzed %i files, found %i casts\n"
            (length results)
            (sum results)
