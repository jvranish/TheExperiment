module Language.TheExperiment.Inferer where



what to do with inferer


inferExpr :: Expr -> Inferer TypeRef

start with exported functions
if expression is already being infered, unify monomorphically

unroll blocks

if variable is assigned a block, replace all references to that variable with the block, repeat
if block is passed to function, 

if a

    conditional

if needs to be a built in primative (at least until we have pattern matchin)