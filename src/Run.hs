{-# LANGUAGE OverloadedStrings #-}

module Run
    ( run
    ) where

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq
import Control.Exception hiding (try)
import Import
import RIO.Directory
import RIO.FilePath
import System.IO (hPutStrLn, print)
import Text.Printf

import qualified Language.C.FindCast as C
import qualified Language.Java.FindCast as Java

discover :: MonadIO m => (FilePath -> Bool) -> Chan FilePath -> FilePath -> m ()
discover p queue = liftIO . go
  where
    go start = do
        isFile <- doesFileExist start
        if isFile
            then when (p start) $ writeChan queue start
            else listDirectory start >>= mapM_ (go . (start </>))

langs :: String -> (String, FilePath -> RIO App (Either String [(Int, String)]))
langs "java" = (".java", Java.analyze)
langs "c" = (".c", C.analyze)
langs l = error $ "Unsupported language: " ++ l

reportMVarWait :: MonadUnliftIO m => String -> m a -> m a
reportMVarWait msg =
    try >=> \case
        Left BlockedIndefinitelyOnMVar ->
            error $ "Caught MVar exception on " ++ msg
        Right res -> pure res

run :: RIO App ()
run = do
    logInfo "Scanning files"
    lang <- asks (optionsLanguage . appOptions)
    let (ext, analyze) = langs lang
    h <- asks appOutputHandle
    void $
        newChan >>= \fileQueue -> do
            resultQueue <- newChan
            ttravH <-
                async $
                asks (optionsTargets . appOptions) >>=
                traverse (discover (isExtensionOf ext) fileQueue)
            void $ wait ttravH
            logDebug "File tree traversal ended"
            cores <- liftIO getNumCapabilities
            replicateM_ (cores * 2) $
                async $
                forever $ do
                    file <- readChan fileQueue
                    res <- analyze file
                    logDebug $ "Analyzed: " <> fromString file
                    res `deepseq` writeChan resultQueue (file, res)
            logDebug "Workers spawned"
            let go acc =
                    try (readChan resultQueue) >>= \case
                        Right (file, res) -> do
                            x <-
                                case res of
                                    Left err -> do
                                        logError $
                                            "Parsing for " <> fromString file <>
                                            " failed with:"
                                        logError $ fromString err
                                        pure Nothing
                                    Right [] -> pure Nothing
                                    Right locs -> do
                                        liftIO $ do
                                            hPrintf h "\n### %v ###\n" file
                                            mapM_
                                                (uncurry (hPrintf h "%i: %v\n"))
                                                locs
                                        let l = length locs
                                        l `seq` pure (Just l)
                            go (x : acc)
                        Left BlockedIndefinitelyOnMVar ->
                            liftIO $
                            hPrintf
                                h
                                "\n\nAnalyzed %i files, found %i casts (%i errors)\n"
                                (length acc)
                                (sum $ catMaybes acc)
                                (length $ filter isNothing acc)
            go []
            logDebug "Printer terminated"
