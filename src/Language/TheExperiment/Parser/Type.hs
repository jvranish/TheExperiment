module Language.TheExperiment.Parser.Type
  ( aTypeSignature
  , aType
  ) where

import Control.Applicative hiding ( (<|>), many )

import Control.Monad

import Text.Parsec

import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.AST.Type
 

aTypeSignature :: EParser TypeSignature
aTypeSignature = liftM2 TypeSignature (sepBy1 aTypeConstraint comma) (reservedOp "=>" >> aType)
             <|> liftM (TypeSignature []) aType
             <?> "type signature"

-- The 'try' is specially positioned to not consume any input if until we 
--   match the ':' then we're committed
aTypeConstraint :: EParser TypeConstraint
aTypeConstraint = liftM2 TypeConstraint (try $ lowerIdent <* reservedOp ":") (sepBy1 aTypeName $ reservedOp "|")

aTypeTerm :: EParser ParsedType
aTypeTerm = aTypeName <|> aTypeVariable <|> parens aType <?> "Type Term"

aType :: EParser ParsedType
aType = do
  pos        <- getPosition
  paramTypes <- sepBy aTypeCall comma
  funcReturn <- optionMaybe $ do
      _ <- symbol "->"
      aTypeTerm
  case (paramTypes, funcReturn) of
    (ts , Just ret) -> return $ ParsedType pos $ FunctionType ts ret
    ([t], Nothing)  -> return $ t
    (_  , Nothing)  -> parserZero <?> "->"


aTypeCall :: EParser ParsedType
aTypeCall = do
    pos         <- getPosition
    typeFunc    <- aTypeTerm
    typeParams' <- many aTypeTerm
    return $ case typeParams' of
               [] -> typeFunc
               _  -> ParsedType pos $ TypeCall typeFunc typeParams'

aTypeName :: EParser ParsedType
aTypeName = liftMtp TypeName upperIdent

aTypeVariable :: EParser ParsedType
aTypeVariable = liftMtp (flip TypeVariable [] . Just) lowerIdent


liftMtp :: (a -> Type ParsedType) -> EParser a -> EParser ParsedType
liftMtp f a = liftM2 ParsedType getPosition (liftM f a)
