module Language.TheExperiment.Pretty.Type
  ( prettyTypeSignature
  , prettyType
  ) where

import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.AST.Type

type ParenContext = Integer

none, functionType, typeCall :: ParenContext
none = 0
functionType = 1
typeCall = 2


prettyTypeSignature :: TypeSignature -> Doc
prettyTypeSignature (TypeSignature [] t) = prettyType t
prettyTypeSignature (TypeSignature cs t) = 
  hsep (fmap prettyTypeConstraint cs) <+> text "=>" <+> prettyType t

prettyTypeConstraint :: TypeConstraint -> Doc
prettyTypeConstraint (TypeConstraint name cs) = 
  text name <+> colon <+> hsep (punctuate (text "|") (fmap prettyType cs))

prettyType :: ParsedType -> Doc
prettyType = prettyType' 0

prettyType' :: ParenContext -> ParsedType -> Doc
prettyType' context (ParsedType _ t) = case t of
  TypeName name              -> text name
  TypeVariable (Just name) _ -> text name
  TypeVariable Nothing     _ -> text "*error*"
  TypeCall f args            -> cParens typeCall $ 
      prettyType' typeCall f <+> hsep (fmap (prettyType' typeCall) args)
  FunctionType params ret    -> cParens typeCall $ 
      hsep (punctuate comma (fmap (prettyType' functionType) params)) <+>
      text "->" <+> prettyType' functionType ret
  where
    cParens c doc | context >= c = parens doc
    cParens _ doc = doc