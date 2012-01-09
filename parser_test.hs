import Language.TheExperiment.Parser

inputs :: [String]
inputs = [
    "\"Hello World\"",
    "\'C\'",
    "0b110011", -- 51
    "0x12345678", -- 305419896
    "0o10",  -- 8
    "1234567890"
  ]

main :: IO ()
main = do
  putStrLn $ unlines $ map parseExpr inputs
