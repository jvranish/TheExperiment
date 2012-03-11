module Main where

import Language.TheExperiment.PrettyPrint
import Text.Parsec.Pos
import Language.TheExperiment.AST
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass

main :: IO ()
main = do
  putStrLn $ render $ pPrint t1
  putStrLn $ render $ pPrint t2
  putStrLn $ render $ pPrint t3
  putStrLn $ render $ pPrint t4
  putStrLn $ render $ pPrint t5

{- TopVarDefs -}

{- Types -}
pos = initialPos "pretty source"

tc1 = (TypeCall     { typeFunction = (TypeName { typeName = "TypeCall1", typePos = pos }),
                      typeParams = [ tc2, (TypeVariable { typeVariable = "a", typePos = pos }) ],
                      typePos = pos})
tc2 = (TypeCall     { typeFunction = (TypeName { typeName = "TypeCall2", typePos = pos }),
                      typeParams = [ (TypeVariable { typeVariable = "b", typePos = pos }) ],
                      typePos = pos})

t1 = (TypeName     { typeName     = "Foo", typePos = pos } )
t2 = (TypeVariable { typeVariable = "a",   typePos = pos } )
t3 = (TypeCall     { typeFunction = (TypeName { typeName = "Bar", typePos = pos }),
                     typeParams = [ tc1
                                  , (TypeVariable { typeVariable = "a", typePos = pos })
                                  ],
                     typePos = pos})

t4 = (FunctionType pos [TypeVariable { typeVariable = "a", typePos = pos},
                        TypeVariable { typeVariable = "b", typePos = pos}]
                       (TypeVariable { typeVariable = "c", typePos = pos}))
t5 =
      (FunctionType {
        typePos = pos,
        argTypes = [
          TypeCall {
              typePos = pos,
              typeFunction = TypeName {
                typePos = pos,
                typeName = "Foo"
              },
              typeParams = [ TypeVariable {typePos = pos, typeVariable = "a"} ]
            },
          FunctionType {
            typePos = pos,
            argTypes = [
              TypeVariable {typePos = pos, typeVariable = "a"}],
              returnType = TypeVariable {typePos = pos, typeVariable = "b"}
          }
        ],
        returnType = t4
      })


