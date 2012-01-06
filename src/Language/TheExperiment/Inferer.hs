module Language.TheExperiment.Inferer where



what to do with inferer


inferExpr :: Expr -> Inferer TypeRef

unify :: Expr TypeRef -> TypeRef -> TypeRef -> Inferer TypeRef

unifyType 

case of

  MonoType a -> case a of
    
  Var a  


in the event of a unification failure we shouldn't need the actual types that failed..
hmmm  yeah we do

    our failure needs the current expression
      and the two types that failed
start with exported functions
if expression is already being infered, unify monomorphically

unroll blocks

if variable is assigned a block, replace all references to that variable with the block, repeat
if block is passed to function, 

if a

    conditional

if needs to be a built in primative (at least until we have pattern matchin)