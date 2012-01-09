module Language.TheExperiment.Builtin where


import Language.C.Syntax.Ops

{-
Eventually, type information should exist in some of these tables
-}

cBinOpTable :: [(String, CBinaryOp)]
cBinOpTable = [ ("Builtin.add",  CAddOp)
              , ("Builtin.sub",  CSubOp)
              , ("Builtin.mul",  CMulOp)
              , ("Builtin.div",  CDivOp)
              , ("Builtin.rmd",  CRmdOp)
              , ("Builtin.shl",  CShlOp)
              , ("Builtin.shr",  CShrOp)
              , ("Builtin.le",   CLeOp)
              , ("Builtin.gr",   CGrOp)
              , ("Builtin.leq",  CLeqOp)
              , ("Builtin.geq",  CGeqOp)
              , ("Builtin.eq",   CEqOp)
              , ("Builtin.neg",  CNeqOp)
              , ("Builtin.and",  CAndOp)
              , ("Builtin.xor",  CXorOp)
              , ("Builtin.or",   COrOp)
              , ("Builtin.lAnd", CLndOp)
              , ("Builtin.lOr",  CLorOp)
              ]

cUnaryOpTable :: [(String, CUnaryOp)]
cUnaryOpTable = [ ("Builtin.addr",     CAdrOp)
                , ("Builtin.indr",     CIndOp)
                , ("Builtin.neg",      CMinOp)
                , ("Builtin.logNeg",   CNegOp)
                , ("Builtin.onesComp", CCompOp)
                ]
{-
casts

("Builtin.index", CIndex)

CExpr -> CExpr -> NodeInfo -> CExpr

-}
