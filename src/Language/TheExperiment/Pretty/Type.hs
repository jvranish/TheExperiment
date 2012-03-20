module Language.TheExperiment.Pretty.Type
  ( prettyTypeSignature
  , prettyType
  ) where

import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.AST.Type

data ParenContext = NoneC | FunctionTypeC | TypeCallC
  deriving (Show, Ord, Eq)


prettyTypeSignature :: TypeSignature -> Doc
prettyTypeSignature (TypeSignature [] t) = prettyType t
prettyTypeSignature (TypeSignature cs t) = 
  hsep (punctuate comma (fmap prettyTypeConstraint cs)) <+> text "=>" <+> prettyType t

prettyTypeConstraint :: TypeConstraint -> Doc
prettyTypeConstraint (TypeConstraint name cs) = 
  text name <+> colon <+> hsep (punctuate (text " |") (fmap prettyType cs))

prettyType :: ParsedType -> Doc
prettyType = prettyType' NoneC

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