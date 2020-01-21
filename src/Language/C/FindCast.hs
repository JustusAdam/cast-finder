{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Language.C.FindCast where

import Import

import qualified Language.C as C
import qualified Language.C.System.GCC as C
import qualified Language.C.System.Preprocess as C

import Data.Generics.Product
import Data.Generics.Sum
import Data.List (partition)
import qualified Data.Text as T
import RIO.FilePath
import qualified RIO.Text as T
import Text.PrettyPrint (renderStyle, style)

getCasts :: C.CTranslUnit -> [Cast]
getCasts = toListOf l
  where
    l = types @(C.CExpression C.NodeInfo) . _Ctor @"CCast"

data StripPP =
    StripPP

stripPP :: C.CppArgs -> IO ExitCode
stripPP args = do
    ppRes <- T.unlines . fix select False False . T.lines <$> readFileUtf8 file
    --writeFileUtf8 "pp-res.c.i" ppRes
    writeFileUtf8 ofile ppRes
    pure ExitSuccess
  where
    file = C.inputFile args
    ofile = fromMaybe (file <.> "i") $ C.outputFile args
    select rec_ wasCont inComment =
        \case
            [] -> []
            (x:xs)
                | inComment ->
                    case T.breakOn "*/" x of
                        (_, rest)
                            | T.null rest -> contInComment xs
                            | otherwise -> cont (T.drop 2 rest : xs)
                | inPP ->
                    (if isCont
                         then contInPP
                         else cont)
                        xs
                | otherwise ->
                    let (begin, rest) = T.breakOn "/*" x
                     in if T.null rest
                            then fst (T.breakOn "//" x) : cont xs
                            else begin : contInComment (T.drop 2 rest : xs)
                where inPP = wasCont || "#" `T.isPrefixOf` T.stripStart x
                      isCont = "\\" `T.isSuffixOf` T.stripEnd x
                      contInComment = rec_ False True
                      contInPP = rec_ True False
                      cont = rec_ False False

instance C.Preprocessor StripPP where
    parseCPPArgs _ args = Right (C.rawCppArgs rest f, [])
      where
        (rest, [f]) =
            partition
                (\case
                     '-':_ -> True
                     _ -> False)
                args
    runCPP _ = stripPP

type Cast = (C.CDecl, C.CExpr, C.NodeInfo)

analyze :: (MonadIO m) => FilePath -> m [(Int, String)]
analyze file =
    liftIO $
    map render . either (error . show) getCasts <$>
    C.parseCFile pp Nothing opts file
  where
    pp = C.newGCC "gcc"
    opts = ["-I/usr/local/opt/openssl@1.1/include", "-DNO_GETTEXT=1"]

render :: Cast -> (Int, String)
render (a, b, c) =
    ( C.posRow $ c ^. typed @C.Position
    , renderStyle style $ C.pretty (C.CCast a b c))
