{-#Language StandaloneDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.Inferrer.Pretty where

import Control.Monad hiding (mapM)
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Char
import Data.Foldable
import Data.Traversable
import Data.FixedList
import qualified Data.List as List

import qualified Data.Map as Map

import qualified Control.Monad.GraphT as GraphT

import Text.Parsec.Pos
import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.Inferrer.InferrerM
import Language.TheExperiment.Inferrer.Type
import Language.TheExperiment.Pretty.Type
import Language.TheExperiment.Misc


import qualified Language.TheExperiment.Parser.AST.Type as ParserAST

import Prelude hiding (mapM)

-- #TODO
-- I'll probably want to make the typeref an explicit element of 
-- expressions, hmmmm maybe....  perhaps if I wrapped it in a Maybe? nope
--  I need to update it on the fly
--  perhaps use a nodeData ref instead of tagging with the environment
--  hmmm, then I can easily update everything




data EitherF a = EitherF (Either String (Type a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

prettyEitherF a = prettyType $ convertToParsedType a

deriving instance Foldable (Either e) -- neatest feature ever.
deriving instance Traversable (Either e)


prettyError :: SourcePos -> String -> Doc -> Doc
prettyError pos msg longmsg = text "Error in" <+> text (show pos) <> colon <+> text msg $+$ nest 2 longmsg


prettyTypeRef :: TypeRef -> Inferrer Doc
prettyTypeRef t = liftM text $ showType t

-- #TODO this needs major cleanup
convertToParsedType (Y (EitherF (Left name))) = ParserAST.TypeVariable undefined name
convertToParsedType (Y (EitherF (Right (TypeName name)))) =  ParserAST.TypeName undefined name
convertToParsedType (Y (EitherF (Right (Var (Just name) _)))) =  ParserAST.TypeVariable undefined name
convertToParsedType (Y (EitherF (Right (Std stdType)))) =  ParserAST.TypeName undefined (show stdType)
convertToParsedType (Y (EitherF (Right (Func params ret)))) =  ParserAST.FunctionType undefined (fmap convertToParsedType params) (convertToParsedType ret)


showType :: TypeRef -> Inferrer String
showType (TypeRef ref)  = Inferrer $ lift $ showType' ref

showType' :: (Monad m) => GraphT.GraphRef Type -> GraphT.GraphT Type m String
showType' ref = do
  (a, t) <- flattenType ref
  return $ (render $ prettyEitherF a)  ++ " : " ++ (List.intercalate ", " $ fmap showVarDef $ Map.assocs t)

showVarDef :: (String, Y EitherF) -> String
showVarDef (s, a) = s ++ " = " ++ (render $ prettyEitherF a)

data Y f = Y { unY :: (f (Y f)) }
{-}
instance (Pretty (f (Y f))) => Pretty (Y f) where
  pPrintPrec l p (Y a) = pPrintPrec l p a
-}
--flattenType :: (Monad m, Traversable f) => ContextRef t -> ContextT s (f (ContextRef s)) m (Y (EitherF f), Map.Map String (Y (EitherF f)))
flattenType :: (Monad m) => GraphT.GraphRef Type -> GraphT.GraphT Type m (Y EitherF, Map.Map String (Y EitherF))
flattenType ref = GraphT.forkMappedContext (ref :. Nil) (EitherF . Right) $ \(ref' :. Nil) ->
    evalStateT (runStateT (flatten ref') Map.empty) varNames


flatten :: (Monad m) => GraphT.GraphRef EitherF -> StateT (Map.Map [Char] (Y EitherF)) (StateT [String] (GraphT.GraphT EitherF m)) (Y EitherF)
flatten ref = do
    hasCycle <- lift $ lift $ GraphT.reachableFrom ref ref
    a <- lift $ lift $ GraphT.readRef ref
    case a of
      EitherF (Right (Var _ _)) -> liftM Y $ writeName ref =<< lift genSym
      _ -> case hasCycle of
          True -> do {- replace node with new symbol, add symbol to map, fill out that row in the map -}
            newSym <- liftM (fmap toUpper) $ lift genSym
            x <- writeName ref newSym
            flatA <- mapM flatten a
            modifyT $ Map.insert newSym $ Y flatA
            return $ Y x
          False -> liftM Y $ mapM flatten a
  where
    writeName ref' varName = do
      let x = EitherF $ Left varName
      lift $ lift $ GraphT.writeRef ref' x
      return x

