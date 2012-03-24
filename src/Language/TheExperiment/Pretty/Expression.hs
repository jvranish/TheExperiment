module Language.TheExperiment.Pretty.Expression
  ( prettyExpression
  ) where

import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser.Expression


--data ParenContext = NoneC | FunctionTypeC | TypeCallC
--  deriving (Show, Ord, Eq)


prettyExpression :: ParsedExpr -> Doc
prettyExpression = prettyExpression' 0

prettyExpression' context expr = case expr of
  Identifier _ _ name NotOperator -> text name
  Call _ _ f params -> (prettyExpression' 0 f) <> parens (hsep (punctuate comma (fmap (prettyExpression' 0) params)))
  Literal _ _ lit -> text $ show lit

{-
prettyType' :: ParenContext -> ParsedType -> Doc
prettyType' context t = case t of
  TypeName     _ name -> text name
  TypeVariable _ name -> text name
  TypeCall   _ f args -> cParens TypeCallC $ 
      prettyType' TypeCallC f <+> hsep (fmap (prettyType' TypeCallC) args)
  FunctionType _ params ret -> cParens FunctionTypeC $ 
      hsep (punctuate comma (fmap (prettyType' FunctionTypeC) params)) <+>
      text "->" <+> prettyType' FunctionTypeC ret
  where
    cParens c doc | context >= c = parens doc
    cParens _ doc = doc

    -}