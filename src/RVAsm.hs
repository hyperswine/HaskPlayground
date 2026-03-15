{-# LANGUAGE OverloadedStrings #-}

module RVAsm where

import Data.Char (isDigit)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die)

registers = Set.fromList $ ["zero", "ra", "sp", "gp", "tp"] ++ ["t" <> T.pack (show i) | i <- [0 .. 6]] ++ ["s" <> T.pack (show i) | i <- [0 .. 11]] ++ ["a" <> T.pack (show i) | i <- [0 .. 7]] ++ ["x" <> T.pack (show i) | i <- [0 .. 31]]

isRegister = (`Set.member` registers)

isImmediate t = case T.unpack t of
  [] -> False
  ('0' : 'x' : rest) -> all (\c -> isDigit c || c `elem` ['a' .. 'f'] || c `elem` ['A' .. 'F']) rest
  ('-' : rest) -> all isDigit rest && not (null rest)
  s -> all isDigit s && not (null s)

-- base[offset]
parseMemoryOperand operand = case T.breakOn "[" operand of
  (base, rest) | not (T.null rest) -> let offset = T.takeWhile (/= ']') (T.drop 1 rest) in (T.strip base, T.strip offset)
  _ -> (T.strip operand, "0")

splitComment line = case T.breakOn "#" line of
  (code, comment) | not $ T.null comment -> (T.strip code, "  # " <> T.strip (T.drop 1 comment))
  _ -> (T.strip line, "")

translateLine rawLine =
  if T.null line || T.isPrefixOf "." line || T.isSuffixOf ":" line
    then rawLine
    else case T.breakOn "=" line of
      -- MOST NON ARITH OR LOGICAL INSTRUCTIONS ARE THESE. Comments are only allowed in trailing contexts or leading if no instructions
      (_, rest) | T.null rest -> translateStandalone line comment
      -- MOST LOGICAL INSTRUCTIONS
      (lhs, rhs) -> translateAssignment (T.strip lhs) (T.strip $ T.drop 1 rhs) comment
  where
    (line, comment) = splitComment rawLine

translateStandalone line comment = case parts of
  [] -> line <> comment
  -- System instructions
  ["environment-call"] -> "    ecall" <> comment
  ["environment-break"] -> "    ebreak" <> comment
  ["fence-memory"] -> "    fence" <> comment
  ["fence-instruction"] -> "    fence.i" <> comment
  ["return"] -> "    ret" <> comment
  -- Unconditional jumps
  ["jump", target] -> "    j " <> target <> comment
  ["jump-link", target] -> "    jal ra, " <> target <> comment
  ["jump-link", dest, target] -> "    jal " <> dest <> ", " <> target <> comment
  ["jump-register", reg] -> "    jalr zero, 0(" <> reg <> ")" <> comment
  ["jump-link-register", reg] -> "    jalr ra, 0(" <> reg <> ")" <> comment
  ["jump-link-register", dest, reg] -> "    jalr " <> dest <> ", 0(" <> reg <> ")" <> comment
  -- Branch instructions
  ("branch" : cond : reg1 : reg2 : target : _) ->
    let branchOp = case cond of
          "equal" -> Just "beq"
          "not-equal" -> Just "bne"
          "less-than" -> Just "blt"
          "greater-equal" -> Just "bge"
          "less-than-unsigned" -> Just "bltu"
          "greater-equal-unsigned" -> Just "bgeu"
          _ -> Nothing
     in case branchOp of
          Just op -> "    " <> op <> " " <> reg1 <> ", " <> reg2 <> ", " <> target <> comment
          Nothing -> "    " <> line <> comment
  -- Store without assignment
  ("store" : width : src : dest : _) ->
    let (base, offset) = parseMemoryOperand dest
        storeOp = case width of
          "byte" -> Just "sb"
          "half" -> Just "sh"
          "word" -> Just "sw"
          "double" -> Just "sd"
          _ -> Nothing
     in case storeOp of
          Just op -> "    " <> op <> " " <> src <> ", " <> offset <> "(" <> base <> ")" <> comment
          Nothing -> "    " <> line <> comment
  _ -> "    " <> line <> comment
  where
    parts = T.words line

translateAssignment dest rhs comment = case parts of
  [] -> dest <> " = " <> rhs <> comment
  -- Simple assignment
  [src] ->
    if isImmediate src
      then "    li " <> dest <> ", " <> src <> comment
      else if isRegister src then "    mv " <> dest <> ", " <> src <> comment else "    la " <> dest <> ", " <> src <> comment
  -- Load operations
  ("load" : rest) -> translateLoad dest rest comment
  -- Store to memory address
  ("store" : width : src : _)
    | T.elem '[' dest ->
        let (base, offset) = parseMemoryOperand dest
            storeOp = case width of
              "byte" -> Just "sb"
              "half" -> Just "sh"
              "word" -> Just "sw"
              "double" -> Just "sd"
              _ -> Nothing
         in case storeOp of
              Just op -> "    " <> op <> " " <> src <> ", " <> offset <> "(" <> base <> ")" <> comment
              Nothing -> "    " <> dest <> " = " <> rhs <> comment
  -- Operations with arguments
  (op : args) -> translateOperation dest op args comment
  where
    parts = T.words rhs

translateLoad dest parts comment = case parts of
  ("unsigned" : width : memOp) ->
    let src = T.unwords memOp
        (base, offset) = parseMemoryOperand src
        loadOp = case width of
          "byte" -> Just "lbu"
          "half" -> Just "lhu"
          "word" -> Just "lwu"
          _ -> Nothing
     in case loadOp of
          Just op -> "    " <> op <> " " <> dest <> ", " <> offset <> "(" <> base <> ")" <> comment
          Nothing -> "    " <> dest <> " = load unsigned " <> T.unwords parts <> comment
  (width : memOp) ->
    let src = T.unwords memOp
        (base, offset) = parseMemoryOperand src
        loadOp = case width of
          "byte" -> Just "lb"
          "half" -> Just "lh"
          "word" -> Just "lw"
          "double" -> Just "ld"
          _ -> Nothing
     in case loadOp of
          Just op -> "    " <> op <> " " <> dest <> ", " <> offset <> "(" <> base <> ")" <> comment
          Nothing -> "    " <> dest <> " = load " <> T.unwords parts <> comment
  _ -> "    " <> dest <> " = load " <> T.unwords parts <> comment

translateOperation dest op args comment = case (op, args) of
  -- Unary operations
  ("not", [src]) -> "    xori " <> dest <> ", " <> src <> ", -1" <> comment
  ("negate", [src]) -> "    sub " <> dest <> ", zero, " <> src <> comment
  ("set-equal-zero", [src]) -> "    sltiu " <> dest <> ", " <> src <> ", 1" <> comment
  ("set-not-equal-zero", [src]) -> "    sltu " <> dest <> ", zero, " <> src <> comment
  ("move", [src]) -> "    mv " <> dest <> ", " <> src <> comment
  ("negate-word", [src]) -> "    subw " <> dest <> ", zero, " <> src <> comment
  ("sign-extend-word", [src]) -> "    sext.w " <> dest <> ", " <> src <> comment
  -- Binary operations
  (_, [src1, src2]) ->
    if isImmediate src2
      then
        translateImmediateOp dest op src1 src2 comment
      else
        translateRegisterOp dest op src1 src2 comment
  _ -> "    " <> dest <> " = " <> op <> " " <> T.unwords args <> comment

translateImmediateOp dest op src1 imm comment =
  let asmOp = case op of
        "add" -> Just "addi"
        "add-word" -> Just "addiw"
        "set-less-than" -> Just "slti"
        "set-less-than-unsigned" -> Just "sltiu"
        "xor" -> Just "xori"
        "or" -> Just "ori"
        "and" -> Just "andi"
        "shift-left" -> Just "slli"
        "shift-right" -> Just "srli"
        "shift-right-arithmetic" -> Just "srai"
        "shift-left-word" -> Just "slliw"
        "shift-right-word" -> Just "srliw"
        "shift-right-arithmetic-word" -> Just "sraiw"
        _ -> Nothing
   in case asmOp of
        Just instr -> "    " <> instr <> " " <> dest <> ", " <> src1 <> ", " <> imm <> comment
        Nothing -> "    " <> dest <> " = " <> op <> " " <> src1 <> " " <> imm <> comment

translateRegisterOp dest op src1 src2 comment =
  let asmOp = case op of
        -- RV64I Base arithmetic/logical
        "add" -> Just "add"
        "subtract" -> Just "sub"
        "shift-left" -> Just "sll"
        "set-less-than" -> Just "slt"
        "set-less-than-unsigned" -> Just "sltu"
        "xor" -> Just "xor"
        "shift-right" -> Just "srl"
        "shift-right-arithmetic" -> Just "sra"
        "or" -> Just "or"
        "and" -> Just "and"
        -- RV64I 32-bit operations
        "add-word" -> Just "addw"
        "subtract-word" -> Just "subw"
        "shift-left-word" -> Just "sllw"
        "shift-right-word" -> Just "srlw"
        "shift-right-arithmetic-word" -> Just "sraw"
        -- M extension
        "multiply" -> Just "mul"
        "multiply-high" -> Just "mulh"
        "multiply-high-signed-unsigned" -> Just "mulhsu"
        "multiply-high-unsigned" -> Just "mulhu"
        "divide" -> Just "div"
        "divide-unsigned" -> Just "divu"
        "remainder" -> Just "rem"
        "remainder-unsigned" -> Just "remu"
        "multiply-word" -> Just "mulw"
        "divide-word" -> Just "divw"
        "divide-unsigned-word" -> Just "divuw"
        "remainder-word" -> Just "remw"
        "remainder-unsigned-word" -> Just "remuw"
        _ -> Nothing
   in case asmOp of
        Just instr -> "    " <> instr <> " " <> dest <> ", " <> src1 <> ", " <> src2 <> comment
        Nothing -> "    " <> dest <> " = " <> op <> " " <> src1 <> " " <> src2 <> comment

translate code = T.unlines $ map translateLine (T.lines code)

main = do
  args <- getArgs
  case args of
    -- Read from stdin, write to stdout
    [] -> do
      input <- TIO.getContents
      TIO.putStr (translate input)
    -- Read from file, write to stdout
    [inFile] -> do
      input <- TIO.readFile inFile
      TIO.putStr (translate input)
    -- Read from file, write to file
    [inFile, outFile] -> do
      input <- TIO.readFile inFile
      TIO.writeFile outFile (translate input)
    _ ->
      die
        "Usage: riscv-pseudo [input-file] [output-file]\n\
        \  No args: read stdin, write stdout\n\
        \  One arg: read file, write stdout\n\
        \  Two args: read input-file, write output-file"
