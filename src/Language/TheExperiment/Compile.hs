module Language.TheExperiment.Compile where

import Text.PrettyPrint

import Language.C.Pretty

import Language.TheExperiment.Inferrer.Inferrer
import Language.TheExperiment.Parser
import Language.TheExperiment.CodeGen.Gen
import Control.Monad.ErrorM

compile :: String -> IO ()
compile filename = do
  result <- parseFile filename
  case result of
    Left err -> putStrLn $ show err
    Right m -> case runErrorM $ infer m of
      Left errs -> putStrLn $ showErrors errs
      Right (warns, genM) -> do
        putStrLn $ showWarnings warns
        let cCode = render $ pretty $ genModule genM
        writeFile "output.c" cCode

--infer :: Module a -> ErrorM.ErrorM (Module GenType)
--parseFile :: String -> IO (Either ParseError (Module ()))
--genModule :: Module GenType -> CTranslUnit

