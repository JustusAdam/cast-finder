{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Run (run) where

import qualified Prelude as P
import Import
import RIO.FilePath
import Text.Printf
import System.IO (hPutStrLn)
import RIO.Directory
import Text.PrettyPrint (renderStyle, style)
import qualified Text.PrettyPrint as PP
import qualified RIO.Text as T
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (partition)
import Data.Generics.Product
import Data.Generics.Sum
import Lens.Micro

import qualified Language.C as C
import qualified Language.C.System.Preprocess as C
import qualified Language.C.System.GCC as C

getCasts :: C.CTranslUnit -> [Cast]
getCasts = toListOf l
  where l = types @(C.CExpression C.NodeInfo) . _Ctor @"CCast"

discover :: MonadIO m => FilePath -> m [FilePath]
discover start = liftIO $ do
    isFile <- doesFileExist start
    if isFile
       then pure [start]
       else
           concat <$> ( listDirectory start >>= mapM (discover . (start </>)) )

data StripPP = StripPP

stripPP :: C.CppArgs -> IO ExitCode
stripPP args = do
    ppRes <- T.unlines . fix select False False . T.lines <$> readFileUtf8 file
    writeFileUtf8 "pp-res.c.i" ppRes
    writeFileUtf8 ofile ppRes
    pure ExitSuccess
  where
    file = C.inputFile args
    ofile = fromMaybe (file <.> "i") $ C.outputFile args
    select rec_ wasCont inComment = \case
        [] -> []
        (x:xs)
            | inComment -> case T.breakOn "*/" x of
                              (_, rest) | T.null rest -> contInComment xs
                                        | otherwise -> cont (T.drop 2 rest:xs)
            | inPP -> (if isCont then contInPP else cont) xs
            | otherwise ->
              let (begin, rest) = T.breakOn "/*" x
              in if T.null rest
                    then fst (T.breakOn "//" x):cont xs
                    else begin:contInComment (T.drop 2 rest:xs)
          where
            inPP = wasCont || "#" `T.isPrefixOf` T.stripStart x
            isCont = "\\" `T.isSuffixOf` T.stripEnd x
            contInComment = rec_ False True
            contInPP = rec_ True False
            cont = rec_ False False

instance C.Preprocessor StripPP where
    parseCPPArgs _ args = Right (C.rawCppArgs rest f, [])
      where
        (rest,[f]) = partition (\case '-':_ -> True; _ -> False) args
    runCPP _ = stripPP

type Cast = (C.CDecl, C.CExpr, C.NodeInfo)

analyze :: (MonadIO m) => FilePath -> m [Cast]
analyze file =
    liftIO $ either (error . show) getCasts <$> C.parseCFile pp Nothing opts file
  where
    pp = C.newGCC "gcc"
    opts = ["-I/usr/local/opt/openssl@1.1/include", "-DNO_GETTEXT=1"]

render :: Cast -> String
render (a,b,c) = renderStyle style $
    PP.int (C.posRow $ c ^. typed @C.Position) PP.<> ":" PP.<+>
    C.pretty (C.CCast a b c)

run :: RIO App ()
run = do
  logInfo "Scanning files"
  files <- filter (isExtensionOf ".c") . concat <$> ( asks (targets . appOptions) >>= traverse discover )

  for_ files $ \file -> do
      locs <- analyze file
      h <- asks appOutputHandle
      liftIO $ do
          hPrintf h "\n### %v ###\n" file
          mapM_ (hPutStrLn h . render) locs
