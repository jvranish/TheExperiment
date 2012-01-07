module Language.TheExperiment.Inferer where


inferExpr :: Expr -> Inferer TypeRef

unify :: Expr TypeRef -> TypeRef -> TypeRef -> Inferer TypeRef


data Type a = TypeName String
            | Std StdType
            | Pointer a
            | Array a a -- first parameter should only be a type variable or an 
                        -- NType (type level number), eventually this would be
                        -- moved to the std libs, and have aconstraint on that
                        -- parameter
            | NType Integer

 (TypeName a) (TypeName b) | a == b -> success
 (Std a)      (Std b)      | a == b -> success
 (NType a)    (NType b)    | a == b -> success
 (Pointer a) (Pointer b)            -> do
    aUb <- unify a b
    newType $ Pointer aUb
 (Array a1 a2) (Array b1 b2) -> do
    a1Ub1 <- unify a1 b1
    a2Ub2 <- unify a2 b2
    newType $ Array a1Ub1 a2Ub2

infer
  env
  typeenv
  