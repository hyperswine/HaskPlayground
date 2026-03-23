{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import RVAsm
import System.Exit (exitFailure)

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

knownRegisters :: [Text]
knownRegisters = ["zero", "ra", "sp", "gp", "tp"] ++ ["t" <> T.pack (show i) | i <- [0 .. 6 :: Int]] ++ ["s" <> T.pack (show i) | i <- [0 .. 11 :: Int]] ++ ["a" <> T.pack (show i) | i <- [0 .. 7 :: Int]] ++ ["x" <> T.pack (show i) | i <- [0 .. 31 :: Int]]

genRegister :: Gen Text
genRegister = Gen.element knownRegisters

genDecimalImmediate :: Gen Text
genDecimalImmediate = do
  n <- Gen.int (Range.linear 0 65535)
  pure (T.pack (show n))

genNegativeImmediate :: Gen Text
genNegativeImmediate = do
  n <- Gen.int (Range.linear 1 65535)
  pure ("-" <> T.pack (show n))

genHexImmediate :: Gen Text
genHexImmediate = do
  digits <- Gen.list (Range.linear 1 8) (Gen.element (['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']))
  pure ("0x" <> T.pack digits)

genImmediate :: Gen Text
genImmediate = Gen.choice [genDecimalImmediate, genNegativeImmediate, genHexImmediate]

genMemoryOperand :: Gen Text
genMemoryOperand = do
  base <- genRegister
  offset <- Gen.int (Range.linear (-2048) 2047)
  pure (base <> "[" <> T.pack (show offset) <> "]")

-- ---------------------------------------------------------------------------
-- isRegister properties
-- ---------------------------------------------------------------------------

-- Every register in the canonical set is recognised
prop_isRegister_knownRegisters :: Property
prop_isRegister_knownRegisters = property $ do
  reg <- forAll genRegister
  assert (isRegister reg)

-- Common non-register identifiers are rejected
prop_isRegister_rejectsNonRegisters :: Property
prop_isRegister_rejectsNonRegisters = property $ do
  nonReg <- forAll $ Gen.element ["r0", "rA", "foo", "bar", "register", "a8", "s12", "t7", "xa"]
  assert (not (isRegister nonReg))

-- ---------------------------------------------------------------------------
-- isImmediate properties
-- ---------------------------------------------------------------------------

prop_isImmediate_decimal :: Property
prop_isImmediate_decimal = property $ do
  imm <- forAll genDecimalImmediate
  assert (isImmediate imm)

prop_isImmediate_negative :: Property
prop_isImmediate_negative = property $ do
  imm <- forAll genNegativeImmediate
  assert (isImmediate imm)

prop_isImmediate_hex :: Property
prop_isImmediate_hex = property $ do
  imm <- forAll genHexImmediate
  assert (isImmediate imm)

-- Empty string is never an immediate
prop_isImmediate_rejectsEmpty :: Property
prop_isImmediate_rejectsEmpty = property $ assert (not (isImmediate ""))

-- Registers are not immediates
prop_isImmediate_rejectsRegisters :: Property
prop_isImmediate_rejectsRegisters = property $ do
  reg <- forAll genRegister
  assert (not (isImmediate reg))

-- ---------------------------------------------------------------------------
-- splitComment properties
-- ---------------------------------------------------------------------------

-- Code portion never contains a '#' character
prop_splitComment_codeNoHash :: Property
prop_splitComment_codeNoHash = property $ do
  code <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
  commentText <- forAll $ Gen.text (Range.linear 0 30) Gen.alphaNum
  let (codeResult, _) = splitComment (code <> " # " <> commentText)
  assert (not (T.isInfixOf "#" codeResult))

-- Lines with no '#' return an empty comment field
prop_splitComment_noCommentWhenNoHash :: Property
prop_splitComment_noCommentWhenNoHash = property $ do
  code <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
  let (_, commentResult) = splitComment code
  assert (T.null commentResult)

-- splitComment is a total function on arbitrary text (no '\n')
-- USEFUL WAY TO DO THINGS
prop_splitComment_nocrash :: Property
prop_splitComment_nocrash = property $ do
  line <- forAll $ Gen.text (Range.linear 0 100) (Gen.filter (/= '\n') Gen.unicode)
  _ <- pure (splitComment line)
  success

-- ---------------------------------------------------------------------------
-- parseMemoryOperand properties
-- ---------------------------------------------------------------------------

-- base[offset] is decomposed into (base, offset)
prop_parseMemoryOperand_brackets :: Property
prop_parseMemoryOperand_brackets = property $ do
  base <- forAll genRegister
  offset <- forAll $ Gen.int (Range.linear (-2048) 2047)
  let operand = base <> "[" <> T.pack (show offset) <> "]"
  let (parsedBase, parsedOffset) = parseMemoryOperand operand
  parsedBase === base
  parsedOffset === T.pack (show offset)

-- Operands without brackets default the offset to "0"
prop_parseMemoryOperand_noBrackets :: Property
prop_parseMemoryOperand_noBrackets = property $ do
  base <- forAll genRegister
  let (parsedBase, parsedOffset) = parseMemoryOperand base
  parsedBase === base
  parsedOffset === "0"

-- parseMemoryOperand never crashes
prop_parseMemoryOperand_nocrash :: Property
prop_parseMemoryOperand_nocrash = property $ do
  operand <- forAll $ Gen.text (Range.linear 0 60) (Gen.filter (/= '\n') Gen.unicode)
  _ <- pure (parseMemoryOperand operand)
  success

-- ---------------------------------------------------------------------------
-- translateLine properties
-- ---------------------------------------------------------------------------

-- translateLine is total — never crashes on arbitrary single-line input
prop_translateLine_nocrash :: Property
prop_translateLine_nocrash = property $ do
  line <- forAll $ Gen.text (Range.linear 0 100) (Gen.filter (/= '\n') Gen.unicode)
  _ <- pure (translateLine line)
  success

-- Empty lines are passed through unchanged
prop_translateLine_emptyPassthrough :: Property
prop_translateLine_emptyPassthrough = property $ translateLine "" === ""

-- Labels (lines ending with ':') pass through unchanged
prop_translateLine_labelPassthrough :: Property
prop_translateLine_labelPassthrough = property $ do
  name <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
  let lbl = name <> ":"
  translateLine lbl === lbl

-- Directives (lines starting with '.') pass through unchanged
prop_translateLine_directivePassthrough :: Property
prop_translateLine_directivePassthrough = property $ do
  rest <- forAll $ Gen.text (Range.linear 0 20) Gen.alphaNum
  let directive = "." <> rest
  translateLine directive === directive

-- Non-empty translated instructions are indented with 4 spaces
prop_translateLine_instructionsIndented :: Property
prop_translateLine_instructionsIndented = property $ do
  dest <- forAll genRegister
  src <- forAll genRegister
  op <- forAll $ Gen.element ["add", "subtract", "xor", "or", "and", "shift-left", "shift-right", "shift-right-arithmetic", "multiply", "divide", "remainder"]
  let line = dest <> " = " <> op <> " " <> src <> " " <> src
  assert (T.isPrefixOf "    " (translateLine line))

-- Load instructions are indented
prop_translateLine_load_indented :: Property
prop_translateLine_load_indented = property $ do
  dest <- forAll genRegister
  base <- forAll genRegister
  offset <- forAll $ Gen.int (Range.linear 0 2047)
  width <- forAll $ Gen.element ["byte", "half", "word", "double"]
  let line = dest <> " = load " <> width <> " " <> base <> "[" <> T.pack (show offset) <> "]"
  assert (T.isPrefixOf "    " (translateLine line))

-- Branch instructions are indented regardless of condition keyword
prop_translateLine_branch_indented :: Property
prop_translateLine_branch_indented = property $ do
  cond <- forAll $ Gen.element ["equal", "not-equal", "less-than", "greater-equal", "less-than-unsigned", "greater-equal-unsigned"]
  r1 <- forAll genRegister
  r2 <- forAll genRegister
  target <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
  let line = "branch " <> cond <> " " <> r1 <> " " <> r2 <> " " <> target
  assert (T.isPrefixOf "    " (translateLine line))

-- Comments attached to translated lines are preserved in the output
prop_translateLine_commentPreserved :: Property
prop_translateLine_commentPreserved = property $ do
  dest <- forAll genRegister
  imm <- forAll genDecimalImmediate
  commentText <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
  let line = dest <> " = " <> imm <> " # " <> commentText
  let result = translateLine line
  assert (T.isInfixOf "#" result)

-- ---------------------------------------------------------------------------
-- Specific golden-style instruction translations
-- ---------------------------------------------------------------------------

prop_translate_return :: Property
prop_translate_return = property $ translateLine "return" === "    ret"

prop_translate_ecall :: Property
prop_translate_ecall = property $ translateLine "environment-call" === "    ecall"

prop_translate_ebreak :: Property
prop_translate_ebreak = property $ translateLine "environment-break" === "    ebreak"

prop_translate_fence :: Property
prop_translate_fence = property $ translateLine "fence-memory" === "    fence"

-- Immediate assignment: dest = <imm> → li dest, imm
prop_translateAssignment_immediate :: Property
prop_translateAssignment_immediate = property $ do
  dest <- forAll genRegister
  imm <- forAll genDecimalImmediate
  let line = dest <> " = " <> imm
  translateLine line === "    li " <> dest <> ", " <> imm

-- Register-to-register move: dest = src → mv dest, src
prop_translateAssignment_register :: Property
prop_translateAssignment_register = property $ do
  dest <- forAll genRegister
  src <- forAll genRegister
  let line = dest <> " = " <> src
  translateLine line === "    mv " <> dest <> ", " <> src

-- Binary reg+imm operations produce the correct immediate-form mnemonic
prop_translateImmOp_add :: Property
prop_translateImmOp_add = property $ do
  dest <- forAll genRegister
  src <- forAll genRegister
  imm <- forAll genDecimalImmediate
  let line = dest <> " = add " <> src <> " " <> imm
  translateLine line === "    addi " <> dest <> ", " <> src <> ", " <> imm

-- Binary reg+reg operations produce the correct register-form mnemonic
prop_translateRegOp_add :: Property
prop_translateRegOp_add = property $ do
  dest <- forAll genRegister
  src1 <- forAll genRegister
  src2 <- forAll genRegister
  let line = dest <> " = add " <> src1 <> " " <> src2
  translateLine line === "    add " <> dest <> ", " <> src1 <> ", " <> src2

prop_translateRegOp_xor :: Property
prop_translateRegOp_xor = property $ do
  dest <- forAll genRegister
  src1 <- forAll genRegister
  src2 <- forAll genRegister
  let line = dest <> " = xor " <> src1 <> " " <> src2
  translateLine line === "    xor " <> dest <> ", " <> src1 <> ", " <> src2

-- Store instruction (standalone form) is indented
prop_translateStandalone_store :: Property
prop_translateStandalone_store = property $ do
  width <- forAll $ Gen.element ["byte", "half", "word", "double"]
  src <- forAll genRegister
  base <- forAll genRegister
  offset <- forAll $ Gen.int (Range.linear 0 2047)
  let line = "store " <> width <> " " <> src <> " " <> base <> "[" <> T.pack (show offset) <> "]"
  assert (T.isPrefixOf "    " (translateLine line))

-- ---------------------------------------------------------------------------
-- translate (multi-line) properties
-- ---------------------------------------------------------------------------

-- translate preserves the number of lines
prop_translate_linecount :: Property
prop_translate_linecount = property $ do
  numLines <- forAll $ Gen.int (Range.linear 1 20)
  line <- forAll $ Gen.text (Range.linear 0 80) Gen.alphaNum
  let input = T.unlines (replicate numLines line)
  let output = translate input
  length (T.lines output) === length (T.lines input)

-- translate is total on programs with arbitrary single-line content
prop_translate_nocrash :: Property
prop_translate_nocrash = property $ do
  numLines <- forAll $ Gen.int (Range.linear 0 10)
  ls <- forAll $ Gen.list (Range.singleton numLines) (Gen.text (Range.linear 0 80) (Gen.filter (/= '\n') Gen.unicode))
  _ <- pure (translate (T.unlines ls))
  success

-- Directives in a program are unchanged after translate
prop_translate_directivesUnchanged :: Property
prop_translate_directivesUnchanged = property $ do
  directive <- forAll $ Gen.element [".text", ".data", ".bss", ".section .text", ".global _start"]
  let result = translate directive
  T.strip result === directive

-- Labels in a program are unchanged after translate
prop_translate_labelsUnchanged :: Property
prop_translate_labelsUnchanged = property $ do
  name <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
  let lbl = name <> ":"
  T.strip (translate lbl) === lbl

-- An empty program translates to an empty output
prop_translate_emptyProgram :: Property
prop_translate_emptyProgram = property $ T.strip (translate "") === ""

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  ok <-
    checkParallel $
      Group
        "RVAsm"
        [ ("isRegister: known registers accepted", prop_isRegister_knownRegisters),
          ("isRegister: non-registers rejected", prop_isRegister_rejectsNonRegisters),
          ("isImmediate: decimal", prop_isImmediate_decimal),
          ("isImmediate: negative", prop_isImmediate_negative),
          ("isImmediate: hex", prop_isImmediate_hex),
          ("isImmediate: empty rejected", prop_isImmediate_rejectsEmpty),
          ("isImmediate: registers rejected", prop_isImmediate_rejectsRegisters),
          ("splitComment: code has no hash", prop_splitComment_codeNoHash),
          ("splitComment: no hash => empty comment", prop_splitComment_noCommentWhenNoHash),
          ("splitComment: no crash", prop_splitComment_nocrash),
          ("parseMemoryOperand: bracket form", prop_parseMemoryOperand_brackets),
          ("parseMemoryOperand: no brackets", prop_parseMemoryOperand_noBrackets),
          ("parseMemoryOperand: no crash", prop_parseMemoryOperand_nocrash),
          ("translateLine: no crash", prop_translateLine_nocrash),
          ("translateLine: empty passthrough", prop_translateLine_emptyPassthrough),
          ("translateLine: label passthrough", prop_translateLine_labelPassthrough),
          ("translateLine: directive passthrough", prop_translateLine_directivePassthrough),
          ("translateLine: ops are indented", prop_translateLine_instructionsIndented),
          ("translateLine: load indented", prop_translateLine_load_indented),
          ("translateLine: branch indented", prop_translateLine_branch_indented),
          ("translateLine: comment preserved", prop_translateLine_commentPreserved),
          ("translate: return => ret", prop_translate_return),
          ("translate: environment-call => ecall", prop_translate_ecall),
          ("translate: environment-break => ebreak", prop_translate_ebreak),
          ("translate: fence-memory => fence", prop_translate_fence),
          ("translate: imm assignment => li", prop_translateAssignment_immediate),
          ("translate: reg assignment => mv", prop_translateAssignment_register),
          ("translate: add reg+imm => addi", prop_translateImmOp_add),
          ("translate: add reg+reg => add", prop_translateRegOp_add),
          ("translate: xor reg+reg => xor", prop_translateRegOp_xor),
          ("translate: store standalone indented", prop_translateStandalone_store),
          ("translate: line count preserved", prop_translate_linecount),
          ("translate: no crash on programs", prop_translate_nocrash),
          ("translate: directives unchanged", prop_translate_directivesUnchanged),
          ("translate: labels unchanged", prop_translate_labelsUnchanged),
          ("translate: empty program", prop_translate_emptyProgram)
        ]
  unless ok exitFailure
