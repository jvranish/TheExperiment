module Language.TheExperiment.PrettyPrint where

import Language.TheExperiment.AST
import Text.PrettyPrint.HughesPJ

data ParenContext = None
                  | FunctionArg
                  | Always
  deriving (Show)

ppParsedType :: ParsedType -> Doc
ppParsedType = flip _ppParsedType None

_ppParsedType :: ParsedType -> ParenContext -> Doc
_ppParsedType TypeName     { typeName = name }    _ = text name
_ppParsedType TypeVariable { typeVariable = var } _ = text var
_ppParsedType TypeCall     { typeFunction = fun,
                             typeParams = params} nesting = case nesting of
                                                              None        -> doc
                                                              FunctionArg -> doc
                                                              Always      -> parens doc
  where
    doc = fun_doc <+> (nest 2 params_doc)
    fun_doc = _ppParsedType fun Always
    params_doc = vcat (map (flip _ppParsedType Always) params)
_ppParsedType FunctionType { argTypes = args,
                             returnType = ret } nesting = case nesting of
                                                              None        -> doc
                                                              FunctionArg -> parens doc
                                                              Always      -> parens doc
  where
    doc = args_doc <+> text "->" <+> (nest 2 ret_doc)
    args_doc = vcat $ punctuate comma (map (flip _ppParsedType FunctionArg) args)
    ret_doc = _ppParsedType ret FunctionArg
    
