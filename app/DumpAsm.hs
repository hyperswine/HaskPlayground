module DumpAsm where
import FPL1 (runCompiler)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  src <- TIO.readFile "examples/fact.fpl"
  case runCompiler src of
    Left e -> print e
    Right (asm, _ws) -> TIO.putStr asm
