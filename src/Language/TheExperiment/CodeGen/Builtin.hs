module Language.TheExperiment.CodeGen.Builtin where


import Language.C.Syntax.Ops

{-
Eventually, type information should exist in some of these tables
-}

cBinOpTable :: [(String, CBinaryOp)]
cBinOpTable = [ ("add",  CAddOp)
              , ("sub",  CSubOp)
              , ("mul",  CMulOp)
              , ("div",  CDivOp)
              , ("rmd",  CRmdOp)
              , ("shl",  CShlOp)
              , ("shr",  CShrOp)
              , ("le",   CLeOp)
              , ("gr",   CGrOp)
              , ("leq",  CLeqOp)
              , ("geq",  CGeqOp)
              , ("eq",   CEqOp)
              , ("neg",  CNeqOp)
              , ("and",  CAndOp)
              , ("xor",  CXorOp)
              , ("or",   COrOp)
              , ("lAnd", CLndOp)
              , ("lOr",  CLorOp)
              ]

cUnaryOpTable :: [(String, CUnaryOp)]
cUnaryOpTable = [ ("addr",     CAdrOp)
                , ("indr",     CIndOp)
                , ("neg",      CMinOp)
                , ("logNeg",   CNegOp)
                , ("onesComp", CCompOp)
                ]
