{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Language.Java.FindCast where

import Control.Category ((>>>))
import Data.Function
import Data.Generics.Product
import Data.Generics.Sum
import Import
import qualified System.IO as SIO

import qualified Language.Java.Parser as Parse
import qualified Language.Java.Pretty as PP
import Language.Java.Syntax

analyze :: MonadIO m => FilePath -> m [(Int, String)]
analyze file = do
    f <- liftIO $ SIO.readFile file
    pure $
        f & Parse.parser Parse.compilationUnit & either (error . show) id &
        toListOf (casts . _Ctor @"Cast") &
        map (uncurry Cast >>> PP.prettyPrint >>> (0, ))
  where
    casts :: Traversal' CompilationUnit Exp
    casts = types
