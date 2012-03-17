{-#Language GeneralizedNewtypeDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.AST where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Text.Parsec.Pos
import Text.PrettyPrint

import Data.Foldable
import Data.Traversable

import qualified Language.TheExperiment.Type as T
-- import Language.TheExperiment.CodeGenType -- Only used for type constraint not
                                          --  actually needed

data Literal = StringLiteral String
             | CharLiteral Char
             | IntegerLiteral Integer
             | BinLiteral Integer
             | HexLiteral Integer
             | OctalLiteral Integer
             | FloatLiteral String Double -- String is parsed representation.
    deriving (Show, Eq, Ord)
-- #TODO replace this with something else in Type.hs?

-- format indicator for pretty printing (keeps track of precedence, etc...)
data OpFormat = NotOperator
              | ExplicitOperator
              | In Rational
              | InR Rational
              | InL Rational
              | Pre Rational
              | Post Rational
  deriving (Show, Ord, Eq)

data TypeSig = TypeSig [TypeConstraint] ParsedType
  deriving (Show, Ord, Eq)

data TypeConstraint = TypeConstraint String [ParsedType]
  deriving (Show, Ord, Eq)

-- #TODO change this name to just 'Type' 
data ParsedType = TypeName   { -- Int, Var, Foo, Void
                  typePos      :: SourcePos,
                  typeName     :: String
                } |
                TypeVariable { -- a, b, c, d, bees
                  typePos      :: SourcePos,
                  typeVariable :: String
                } |
                TypeCall     { -- Foo a Int, Foo a b, Foo (Foo Var)
                  typePos      :: SourcePos,
                  typeFunction :: ParsedType,
                  typeParams   :: [ParsedType]
                } |
                FunctionType { -- (a, b) -> c
                  typePos      :: SourcePos,
                  argTypes     :: [ParsedType],
                  returnType   :: ParsedType
                }
  deriving (Show, Eq, Ord)

data Expr a
        = Call       { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , callFunc     :: Expr a
                     , callParams   :: [Expr a]
                     }
        | Identifier { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , idName       :: String
                     , opFormat     :: OpFormat
                     }
        | Literal    { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , literal      :: Literal
                     }
        {-
        | Member     { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , memberExpr   :: Expr a
                     , memberName   :: String
                     }
        -}
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

pPrintExpr :: Expr a -> Doc
pPrintExpr (Call _ _ f args) = pPrintExpr f <> parens (hsep $ punctuate comma $ fmap pPrintExpr args)
pPrintExpr (Identifier _ _ name _) = text name
pPrintExpr (Literal _ _ l) = text $ show l

data Statement a
        = Assign   { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , assignName   :: String
                   , assignExpr   :: Expr a
                   }
        | If       { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , ifCond       :: Expr a
                   , ifThen       :: Statement a
                   , ifElse       :: Maybe (Statement a)
                   }
        | While    { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , whileCond    :: Expr a
                   , whileBody    :: Statement a
                   }
        | ExprStmt { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , stmtExpr     :: Expr a
                   }
        | Return   { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , returnExpr   :: Expr a
                   }
        | Block    { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , blockBody    :: ([Definition a], [Statement a])
                   }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

pPrintStmt (Assign _ _ name expr) = text name <+> equals <+> pPrintExpr expr
pPrintStmt (If _ _ cond t Nothing) = text "if" <+> pPrintExpr cond $+$ pPrintStmt t 
pPrintStmt (If _ _ cond t (Just e)) = text "if" <+> pPrintExpr cond $+$ pPrintStmt t $+$ pPrintStmt e
pPrintStmt (While _ _ cond b) = text "while" <+> pPrintExpr cond $+$ pPrintStmt b
pPrintStmt (ExprStmt _ _ e) = pPrintExpr e
pPrintStmt (Return _ _ e) = text "return" <+> pPrintExpr e
pPrintStmt (Block _ _ (d, b)) = text "begin" $+$ nest 2 (pPrintBlock d b) $+$ text "end"

pPrintBlock defs body = vcat (fmap pPrintDef defs) $+$ vcat (fmap pPrintStmt body)



data VarDef a 
        = VarDef { varDefPos      :: SourcePos -- this pos
                  -- is the position of the var name
                 , varDefNodeData :: a
                 , varName        :: String
                 }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

pPrintVarDef (VarDef _ _ name) = text name

returnId :: String
returnId = "#return"

-- #TODO this should be renamed to: Definition
data Definition a
        = TopVarDef { topStmtPos      :: SourcePos
                    , topStmtNodeData :: a
                    , varDef          :: VarDef a
                    -- , varInitExpr     :: Maybe (Expr a)
                    , sig             :: Maybe TypeSig
                    }
        | FuncDef   { topStmtPos      :: SourcePos
                    , topStmtNodeData :: a
                    , funcName        :: String
                    , funcParams      :: [VarDef a]
                    , funcRet         :: VarDef a
                    , funcStmt        :: Statement a
                    , sig             :: Maybe TypeSig
                    }
        | Foreign   { topStmtPos      :: SourcePos
                    , topStmtNodeData :: a
                    , foreignName     :: String
                    -- This has the same type to match, but it will
                    --  _always_ have a signature (enforces by parser)
                    , sig             :: Maybe TypeSig } 
        | TypeDef   { topStmtPos      :: SourcePos
                    , topStmtNodeData :: a
                    , typeDefName     :: String
                    , typeDefType     :: T.TypeDef ParsedType
                    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

pPrintDef (TopVarDef _ _ vdef _) = text "var" <+> pPrintVarDef vdef
pPrintDef (FuncDef _ _ name params _ stmt _) = text "def" <+> text name <> parens (hsep $ punctuate comma $ fmap pPrintVarDef params) $+$ pPrintStmt stmt
pPrintDef (Foreign _ _ name _) = text "foreign" <+> text name

data Module a = Module SourcePos [Definition a]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

pPrintModule (Module _ defs) = vcat $ fmap pPrintDef defs

pPrintPos :: SourcePos -> Doc 
pPrintPos pos = text (sourceName pos) <> colon <> int (sourceLine pos) <> colon <> int (sourceColumn pos)

class NodeType a where
-- nothing, just a second step to make sure you
-- are really intending to allow the type to be
-- used as a NodeType.
-- instance NodeType GenType where



instance Arbitrary ParsedType where
  arbitrary = frequency [(10, arbTypeName), (10, arbTypeVariable), (1, arbTypeCall), (1, arbFunctionType)]
    where
      pos = initialPos "arbitrary source"
      uppers = elements ['A'..'Z']
      lowers = elements ['a'..'z']
      chars = elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
      arbTypeName = do
        first <- uppers
        rest <- listOf chars
        return $ TypeName { typePos = pos, typeName = (first : rest) }
      arbTypeVariable = do
        first <- lowers
        rest <- listOf chars
        return $ TypeVariable { typePos = pos, typeVariable = (first : rest) }
      arbTypeCall = do
        fun <- arbitrary
        params <- arbitrary
        return $ TypeCall { typePos = pos, typeFunction = fun, typeParams = params }
      arbFunctionType = do
        args <- arbitrary
        ret <- arbitrary
        return $ FunctionType { typePos = pos, argTypes = args, returnType = ret }
  shrink (TypeName { typePos = pos', typeName = name})
    = [TypeName { typePos = pos', typeName = n' } | n' <- shrink name]
  shrink (TypeVariable { typePos = pos', typeVariable = var})
    = [TypeVariable { typePos = pos', typeVariable = v' } | v' <- shrink var]
  shrink (TypeCall { typePos = pos', typeFunction = fun, typeParams = params})
    = [TypeCall { typePos = pos', typeFunction = f', typeParams = p' } |
           f' <- shrink fun,
           p' <- shrink params]
  shrink (FunctionType { typePos = pos', argTypes = args, returnType = ret})
    = [FunctionType { typePos = pos', argTypes = a', returnType = r' } |
           a' <- shrink args,
           r' <- shrink ret]
    

