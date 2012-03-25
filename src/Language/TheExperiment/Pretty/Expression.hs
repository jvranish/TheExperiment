module Language.TheExperiment.Pretty.Expression
  ( prettyExpression
  ) where

import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser.Expression
import Language.TheExperiment.Pretty.Literal


--data ParenContext = NoneC | FunctionTypeC | TypeCallC
--  deriving (Show, Ord, Eq)


prettyExpression :: ParsedExpr -> Doc
prettyExpression = prettyExpression' 0

prettyExpression' context expr = case expr of
    Identifier _ _ name _ -> text name
    Call _ _ (Identifier _ _ _ (In p name)) [a, b] -> cParens p $ prettyExpression' p a <+> text name <+> prettyExpression' p b
    Call _ _ (Identifier _ _ _ (InR p name)) [a, b] -> cParens p $ prettyExpression' p a <+> text name <+> prettyExpression' p b
    Call _ _ (Identifier _ _ _ (InL p name)) [a, b] -> cParens p $ prettyExpression' p a <+> text name <+> prettyExpression' p b
    Call _ _ (Identifier _ _ _ (Pre p name)) [a] -> cParens p $ text name <> prettyExpression' p a
    Call _ _ (Identifier _ _ _ (Post p name)) [a] -> cParens p $ prettyExpression' p a <> text name
    Call _ _ f params -> (prettyExpression' 0 f) <> parens (hsep (punctuate comma (fmap (prettyExpression' 0) params)))
    Literal _ _ lit -> prettyLiteral lit
  where
    cParens c doc | context >= c = parens doc
    cParens _ doc = doc
{-

a * (b * c)

a + b + c
(a +(b + c))
((a + b) + c)
    -}