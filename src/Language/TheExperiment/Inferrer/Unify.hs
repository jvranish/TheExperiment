
module Language.TheExperiment.Inferrer.Unify where

import Control.Monad

import Text.Parsec.Pos
import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.Inferrer.Type
import Language.TheExperiment.Inferrer.InferrerM
import Language.TheExperiment.Inferrer.Pretty
import Language.TheExperiment.Misc


data UnifyErrorLoc = UnifyErrorLoc SourcePos Doc


unify :: UnifyErrorLoc -> TypeRef -> TypeRef -> Inferrer TypeRef
unify ~errLoc@(UnifyErrorLoc pos nodeDesc) expected inferred = do
        sameRef <- refEq expected inferred
        case sameRef of
          True -> success
          False -> do 
            expected' <- readType expected
            inferred' <- readType inferred
            unifiedType <- unify' expected' inferred'
            subsType expected unifiedType
            subsType inferred unifiedType
            return unifiedType
    where
        unify' (TypeName a)  (TypeName b) | a == b = success
        unify' (Std a)       (Std b)      | a == b = success
        unify' (NType a)     (NType b)    | a == b = success
        unify' (Pointer a)   (Pointer b) = do
            aUb <- unify errLoc a b
            newType $ Pointer aUb
        unify' (Array a1 a2) (Array b1 b2) = do
            a1Ub1 <- unify errLoc a1 b1
            a2Ub2 <- unify errLoc a2 b2
            newType $ Array a1Ub1 a2Ub2
        unify' (Func aParams aRet) (Func bParams bRet) = do
            when (length aParams /= length bParams) $ do
              addUnifyError "Mismatch in number of parameters."
            params <- unifyList aParams bParams
            ret <- unify errLoc aRet bRet
            newType $ Func params ret
        unify' (Var aName aConstraints) (Var bName bConstraints) =
          resolveConstraints [aName, bName] aConstraints bConstraints

        unify' (Var aName aConstraints) _                        =
          resolveConstraints [aName]        aConstraints [inferred]

        unify' _                        (Var bName bConstraints) =
          resolveConstraints [bName]        bConstraints [expected]

        unify' _                        _                        = failure

        resolveConstraints names [] [] = resolveVar names []
        resolveConstraints names [] xs = resolveVar names xs
        resolveConstraints names xs [] = resolveVar names xs
        resolveConstraints names xs ys = do
          c <- intersectByM typeEq xs ys
          case c of
              [] -> do
                  -- #TODO display the two constraint sets
                  addUnifyError "Incompatible Constraints."
                  resolveVar names []
              _ -> resolveVar names c

        resolveVar _     [x] = return x
        resolveVar names xs  = newType $ Var (msum names) xs


        unifyList (x:xs) (y:ys) = do
            u <- unify errLoc x y
            us <- unifyList xs ys
            return (u:us)
        unifyList _ _ = return []

        success = return expected
        failure = do
          addUnifyError "Could not unify types."
          return expected

        addUnifyError msg = do
          pExpected <- prettyTypeRef expected
          pInferred <- prettyTypeRef inferred
          let longmsg = 
                text "Expected type" <> colon <+> pExpected $+$
                text "Inferred type" <> colon <+> pInferred $+$
                nodeDesc
          addError $ prettyError pos msg longmsg
