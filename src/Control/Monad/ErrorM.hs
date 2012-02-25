{-#Language GeneralizedNewtypeDeriving #-}
module Control.Monad.ErrorM ( runErrorM
                            , ErrorM (..)
                            , Error (..)
                            , Warning (..)
                            , Errors
                            , addError
                            , throwFatalError
                            , addWarning
                            , showWarnings
                            , showErrors
                            ) where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Either

import Text.PrettyPrint

data Warning = Warning Doc

warningDoc :: Warning -> Doc
warningDoc (Warning d) = d

data Error = Error Doc
           | FatalError Doc

errorDoc :: Error -> Doc
errorDoc (Error d) = d
errorDoc (FatalError d) = d

type Errors = [Either Error Warning]

newtype ErrorM a = ErrorM (StateT Errors (Either Errors) a)
    deriving (Monad, Applicative, Functor, MonadFix)


runErrorM :: ErrorM a -> Either Errors ([Warning], a)
runErrorM (ErrorM m) = do
    (a, errorsAndWarnings) <- runStateT m []
    let (errors, warnings) = partitionEithers errorsAndWarnings
    case errors of
         [] -> Right (warnings, a)
         _  -> Left errorsAndWarnings


addError :: Doc -> ErrorM ()
addError d = ErrorM $ modify $ ((Left $ Error d) :)

throwFatalError ::Doc -> ErrorM a
throwFatalError d = ErrorM $ do
    modify $ ((Left $ FatalError d) :)
    errorsAndWarnings <- get -- extract errors from the State monad
    _ <- lift $ Left errorsAndWarnings -- and lift them into the Either monad (this causes an immediate error, skipping further evaluation)
    return undefined

addWarning :: Doc -> ErrorM ()
addWarning d = ErrorM $ modify $ ((Right $ Warning d) :)

showWarnings :: [Warning] -> String
showWarnings warns = render $ vcat (fmap warningDoc warns)

showErrors :: Errors -> String
showErrors a = render $ vcat (fmap errorDoc errs) $+$ vcat (fmap warningDoc warns)
  where
    (errs, warns) = partitionEithers a

-- #TODO maybe add a particular type of error for compiler errors (to make it easier to quickcheck that they are never generated)