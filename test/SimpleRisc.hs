{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use catMaybes" #-}

module SimpleRisc where

import Clash.Prelude hiding (And, Xor)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import "haskplayground" SimpleRisc
import qualified Prelude as P
import qualified "haskplayground" SdramSimpleRisc as S
import qualified Rv32Model as Model
import Control.Monad (when)

-- | The 64 KiB RAM as a sparse map: copying a 16384-entry vector on every
-- write would make the simulations far too slow.  Unwritten words read as
-- 'ramDefault'.  Indices are RAM word indices (address - 0x8000_0000) / 4.
data Ram = Ram {ramDefault :: Word32, ramWords :: IntMap.IntMap Word32}

blankRam :: Word32 -> Ram
blankRam fill = Ram fill IntMap.empty

ramRead :: Ram -> MemAddr -> Word32
ramRead (Ram fill contents) address = IntMap.findWithDefault fill (fromIntegral address) contents

ramWrite :: MemAddr -> Word32 -> Ram -> Ram
ramWrite address value (Ram fill contents) = Ram fill (IntMap.insert (fromIntegral address) value contents)

-- | Physical memory as the circuit sees it: bit 14 of the index selects the
-- boot ROM, which the simulation runs from the same embedded image.
memoryRead :: Ram -> MemAddr -> Word32
memoryRead ram address
  | testBit address 14 = bootRomContents !! (address .&. 0x1ff)
  | otherwise = ramRead ram address

-- Mirror the command registers followed by synchronous block RAM.
data Sim = Sim
  { simMachine :: Machine,
    simRam :: Ram,
    simRamOutput :: Word32,
    simMemoryWord :: Word32,
    simReadCommand :: MemAddr,
    simWriteCommand :: Maybe (MemAddr, Word32),
    simTransmitted :: [Byte]
  }

stepSim :: Sim -> Maybe Byte -> Sim
stepSim = stepSimReady True

stepSimReady :: Bool -> Sim -> Maybe Byte -> Sim
stepSimReady txReady Sim {..} received =
  let (machine', (readAddress, writeCommand, txByte)) =
        machineStep simMachine (simMemoryWord, received, txReady)
      nextRamOutput = memoryRead simRam simReadCommand
      ram' = case simWriteCommand of
        Nothing -> simRam
        Just (writeAddress, value) -> ramWrite writeAddress value simRam
      transmitted' = case txByte of
        Nothing -> simTransmitted
        Just byte -> simTransmitted P.++ [byte]
   in Sim machine' ram' nextRamOutput simRamOutput readAddress writeCommand transmitted'

runInputs :: Sim -> [Maybe Byte] -> Sim
runInputs = P.foldl stepSim

startSim :: Machine -> Ram -> Sim
startSim machine ram = Sim machine ram 0 0 0 Nothing []

ramFromList :: [Word32] -> Ram
ramFromList program = Ram 0 (IntMap.fromList (P.zip [0 ..] program))

-- | Step a program in RAM with no UART input until control returns to the
-- boot ROM (a trap with mtvec 0, or a finisher reset), returning the cycles.
runUntilHalt :: Int -> Sim -> (Int, Sim)
runUntilHalt fuel = go 0
  where
    go cycles sim
      | romAddress (cpuPc (simMachine sim)) || cycles >= fuel = (cycles, sim)
      | otherwise = go (cycles + 1) (stepSim sim Nothing)

-- | Keep running (the boot ROM, after a program ends) until "DONE" has been
-- sent.
runUntilDone :: Int -> Sim -> Sim
runUntilDone fuel sim
  | doneBytes `List.isSuffixOf` simTransmitted sim || fuel <= 0 = sim
  | otherwise = runUntilDone (fuel - 1) (stepSim sim Nothing)

-- | A program at the start of RAM, entered the way the boot ROM's 'R' enters
-- it, with the ROM having already consumed the power-on BOOTCAUSE.
runningMachine :: [(Index 32, Word32)] -> Machine
runningMachine registers =
  initialMachine
    { cpuRegs = P.foldr (P.uncurry replace) (repeat 0) registers,
      cpuPhase = Fetch,
      cpuPc = ramBase,
      cpuInRom = False,
      bootCause = causeNone
    }

-- | Host bytes the way a UART delivers them: one at a time with gaps, so the
-- boot ROM's polling loop has time to take each from the one-byte receiver.
hostBytes :: Int -> [Byte] -> [Maybe Byte]
hostBytes gap = P.concatMap (\byte -> Just byte : P.replicate gap Nothing)

doneBytes :: [Byte]
doneBytes = [0x44, 0x4f, 0x4e, 0x45]

wordBytes :: Word32 -> [Byte]
wordBytes word =
  [ resize word,
    resize (word `shiftR` 8),
    resize (word `shiftR` 16),
    resize (word `shiftR` 24)
  ]

-- Drive a real registered bus transaction to its CPU acknowledgement. Stop
-- before Fetch so unit assertions can inspect the writeback/exception pulse.
settleBus :: Machine -> (Machine, StepOutput)
settleBus = go (32 :: Int)
  where
    go fuel machine
      | cpuPhase machine /= BusWait || fuel == 0 = (machine, (0, Nothing, Nothing))
      | otherwise = let (next, output) = machineStep machine (0, Nothing, True)
                    in if cpuPhase next == BusWait then go (fuel - 1) next else (next, output)

commitSettled :: Machine -> Bool -> (Machine, RegisterWrite, StepOutput)
commitSettled machine ready =
  let (queued, write, output) = commitInstruction machine ready
   in if cpuPhase queued == BusWait
        then let (completed, response) = settleBus queued
              in (completed, cpuPendingWrite completed, response)
        else (queued, write, output)

-- Instruction encoders -------------------------------------------------------

addi :: Word32 -> Word32 -> Word32 -> Word32
addi rd rs1 imm = ((imm .&. 0xfff) `shiftL` 20) .|. (rs1 `shiftL` 15) .|. (rd `shiftL` 7) .|. 0x13

lui :: Word32 -> Word32 -> Word32
lui rd imm20 = (imm20 `shiftL` 12) .|. (rd `shiftL` 7) .|. 0x37

-- | @sw rs2, imm(rs1)@
sw :: Word32 -> Word32 -> Word32 -> Word32
sw rs2 rs1 imm =
  (((imm `shiftR` 5) .&. 0x7f) `shiftL` 25)
    .|. (rs2 `shiftL` 20)
    .|. (rs1 `shiftL` 15)
    .|. (0b010 `shiftL` 12)
    .|. ((imm .&. 0x1f) `shiftL` 7)
    .|. 0x23

bne :: Word32 -> Word32 -> Word32 -> Word32
bne rs1 rs2 imm =
  (((imm `shiftR` 12) .&. 1) `shiftL` 31)
    .|. (((imm `shiftR` 5) .&. 0x3f) `shiftL` 25)
    .|. (rs2 `shiftL` 20)
    .|. (rs1 `shiftL` 15)
    .|. (0b001 `shiftL` 12)
    .|. (((imm `shiftR` 1) .&. 0xf) `shiftL` 8)
    .|. (((imm `shiftR` 11) .&. 1) `shiftL` 7)
    .|. 0x63

-- RV32I execution -------------------------------------------------------------

data Arithmetic
  = Add
  | Sub
  | ShiftLeft
  | SetLessThan
  | SetLessThanUnsigned
  | Xor
  | ShiftRight
  | ShiftRightArithmetic
  | Or
  | And
  deriving (Bounded, Enum, Eq, Show)

arithmeticEncoding :: Arithmetic -> (Word32, Word32)
arithmeticEncoding operation = case operation of
  Add -> (0x00, 0b000)
  Sub -> (0x20, 0b000)
  ShiftLeft -> (0x00, 0b001)
  SetLessThan -> (0x00, 0b010)
  SetLessThanUnsigned -> (0x00, 0b011)
  Xor -> (0x00, 0b100)
  ShiftRight -> (0x00, 0b101)
  ShiftRightArithmetic -> (0x20, 0b101)
  Or -> (0x00, 0b110)
  And -> (0x00, 0b111)

encodeArithmetic :: Arithmetic -> Word32
encodeArithmetic operation =
  let (funct7, funct3) = arithmeticEncoding operation
   in (funct7 `shiftL` 25)
        .|. (2 `shiftL` 20) -- rs2 = x2
        .|. (1 `shiftL` 15) -- rs1 = x1
        .|. (funct3 `shiftL` 12)
        .|. (3 `shiftL` 7) -- rd = x3
        .|. 0x33

arithmeticResult :: Arithmetic -> Word32 -> Word32 -> Word32
arithmeticResult operation a b = case operation of
  Add -> a + b
  Sub -> a - b
  ShiftLeft -> a `shiftL` shiftAmount
  SetLessThan -> boolWordTest (asSigned a < asSigned b)
  SetLessThanUnsigned -> boolWordTest (a < b)
  Xor -> a `xor` b
  ShiftRight -> a `shiftR` shiftAmount
  ShiftRightArithmetic -> fromSigned (asSigned a `shiftR` shiftAmount)
  Or -> a .|. b
  And -> a .&. b
  where
    shiftAmount = fromIntegral (b .&. 0x1f)

asSigned :: Word32 -> Signed 32
asSigned = bitCoerce

fromSigned :: Signed 32 -> Word32
fromSigned = bitCoerce

boolWordTest :: Bool -> Word32
boolWordTest False = 0
boolWordTest True = 1

prop_arithmetic_instruction_stores_expected_result :: Property
prop_arithmetic_instruction_stores_expected_result = property $ do
  operation <- forAll Gen.enumBounded
  a <- forAll (Gen.integral Range.constantBounded)
  b <- forAll (Gen.integral Range.constantBounded)

  let machine = runningMachine [(1, a), (2, b), (31, ramBase)]
      -- op x3,x1,x2; sw x3,256(x31) with x31 = RAM base; then off the end
      ram = ramFromList [encodeArithmetic operation, sw 3 31 256]
      (_, halted) = runUntilHalt 200 (startSim machine ram)
      finished = runUntilDone 3000 halted
      expected = arithmeticResult operation a b

  -- Registers are checked on return to the ROM, which then reuses them.
  cpuRegs (simMachine halted) !! (3 :: Index 32) === expected
  ramRead (simRam finished) 64 === expected
  assert (romAddress (cpuPc (simMachine halted)))
  simTransmitted finished === doneBytes

prop_boot_rom_loads_programs :: Property
prop_boot_rom_loads_programs = withTests 20 . property $ do
  wordsToProgram <- forAll (Gen.list (Range.linear 1 20) (Gen.integral Range.constantBounded))
  let count = P.length wordsToProgram
      countLo = fromIntegral count :: Byte
      countHi = fromIntegral (count `shiftR` 8) :: Byte
      frame = 0x50 : countLo : countHi : P.concatMap wordBytes wordsToProgram
      -- Power-on: the ROM starts at 0 and stays silent.
      programmed = runInputs (startSim initialMachine (blankRam 0xdead_beef))
        (P.replicate 500 Nothing P.++ hostBytes 600 frame P.++ P.replicate 2000 Nothing)
      actual = P.map (ramRead (simRam programmed) . fromIntegral) [0 .. count - 1]

  actual === wordsToProgram
  -- A zero word after the image makes running off its end trap.
  ramRead (simRam programmed) (fromIntegral count) === 0
  ramRead (simRam programmed) (fromIntegral count + 1) === 0xdead_beef
  simTransmitted programmed === []
  assert (romAddress (cpuPc (simMachine programmed)))

-- Control flow and fetch timing -------------------------------------------------

prop_encoders_match_known_instructions :: Property
prop_encoders_match_known_instructions = withTests 1 . property $ do
  sw 3 0 256 === 0x1030_2023
  bne 1 0 (0 - 4) === 0xfe00_9ee3
  addi 1 1 (0 - 1) === 0xfff0_8093

prop_backward_branch_in_final_word_loops :: Property
prop_backward_branch_in_final_word_loops = property $ do
  n <- forAll (Gen.integral (Range.linear 1 20))
  -- addi x1,x0,n; loop: addi x1,x1,-1; bne x1,x0,loop
  let ram = ramFromList [addi 1 0 n, addi 1 1 (0 - 1), bne 1 0 (0 - 4)]
      (_, halted) = runUntilHalt 1000 (startSim (runningMachine []) ram)
      finished = runUntilDone 3000 halted
      regs = cpuTrapRegisters (simMachine halted)

  cpuRegs (simMachine halted) !! (1 :: Index 32) === 0
  -- The loop exits into the word after the program, which is zero.
  trapEpc regs === ramBase + 12
  trapCause regs === 2
  simTransmitted finished === doneBytes

-- Exercise every bank boundary and x0 through real decode/writeback stages.
prop_register_banks_and_writeback :: Property
prop_register_banks_and_writeback = property $ do
  salt <- forAll (Gen.integral Range.constantBounded)
  -- x31 is the RAM base for x0..x30's stores; x31 itself goes last, through
  -- a freshly loaded x30.
  let value i = salt + fromIntegral i * 0x1020304
      -- Data at RAM word 256, clear of the 65-word program.
      base = ramBase + 0x400
      registers = (31, base) : [(fromIntegral i, value i) | i <- [0..30 :: Int]]
      program = P.concat [[addi r r 1, sw r 31 (4*r)] | r <- [0..30]]
        P.++ [addi 31 31 1, lui 30 0x80000, sw 31 30 (0x400 + 4*31)]
      (_, halted) = runUntilHalt 3000 (startSim (runningMachine registers) (ramFromList program))
  assert (romAddress (cpuPc (simMachine halted)))
  P.mapM_ (\i -> ramRead (simRam halted) (fromIntegral (256+i)) === if i == 0 then 0 else value i + 1) [0..30 :: Int]
  ramRead (simRam halted) (256 + 31) === base + 1

-- Every barrel-shifter distance through register and immediate instructions.
-- Include negative inputs to prove sign extension across the new boundary.
prop_shift_stages_all_distances :: Property
prop_shift_stages_all_distances = withTests 1 . property $ do
  P.mapM_ checkShift
    [(op, a, n, immediate) | op <- [ShiftLeft, ShiftRight, ShiftRightArithmetic],
      a <- [0, 1, maxBound, 0x8000_0000, 0x7fff_ffff, 0x5555_5555],
      n <- [0..31], immediate <- [False, True]]
  where
    checkShift (op, a, n, immediate) = do
      let registerInstruction = encodeArithmetic op
          instruction = if immediate
            then (registerInstruction .&. complement (31 `shiftL` 20) .&. complement 0x7f)
              .|. (n `shiftL` 20) .|. 0x13
            else registerInstruction
          machine = runningMachine [(1, a), (2, n)]
          (cycles, halted) = runUntilHalt 50 (startSim machine (ramFromList [instruction]))
      cpuRegs (simMachine halted) !! (3 :: Index 32) === arithmeticResult op a n
      assert (romAddress (cpuPc (simMachine halted)))
      -- Ten clocks for the shift, then ten to trap on the zero word after it.
      cycles === 10 + 10

prop_split_comparisons_match_reference :: Property
prop_split_comparisons_match_reference = property $ do
  a <- forAll (Gen.integral Range.constantBounded)
  b <- forAll (Gen.integral Range.constantBounded)
  let edges = [0, 1, 0xffff, 0x10000, 0x7fffffff, 0x80000000, 0xffff0000, maxBound]
  P.mapM_ (\(left, right) -> do
    let parts = compareOperands left right
    signedComparison parts === (signed32 left < signed32 right)
    unsignedComparison parts === (left < right)
    P.mapM_ (\kind -> branchTakenPrepared kind parts === branchTaken kind left right) [0..7]
    ) ((a,b) : [(left,right) | left <- edges, right <- edges])

prop_registered_execution_matches_compute :: Property
prop_registered_execution_matches_compute = property $ do
  bits <- forAll (Gen.integral Range.constantBounded)
  opcode <- forAll (Gen.element [0x37, 0x17, 0x13, 0x33, 0x03, 0x23, 0x63, 0x67, 0x6f, 0x73])
  a <- forAll (Gen.integral Range.constantBounded)
  b <- forAll (Gen.integral Range.constantBounded)
  pc <- forAll (Gen.integral Range.constantBounded)
  let raw = (bits .&. complement 0x7f) .|. opcode
      -- M operations have their own iterative unit, covered separately.
      instruction = if opcode == 0x33 && raw `shiftR` 25 == 1 then clearBit raw 25 else raw
      immediate = decodeImmediate instruction
      operandA = if instructionRs1 instruction == 0 then 0 else a
      operandB = if instructionRs2 instruction == 0 then 0 else b
      op2 = if opcode == 0x33 then operandB else immediate
      before = initialMachine {cpuPhase = OperandSelect,
        cpuPc = pc, cpuInstruction = instruction, cpuABanks = repeat a, cpuBBanks = repeat b,
        cpuImm = immediate, cpuIsRegOp = opcode == 0x33}
      step m = let (m', _, _) = cpuStep m 0 Nothing True in m'
      prepared = step before
      executed = step prepared
      completed = step executed
      expected = compute pc instruction operandA operandB immediate op2
  cpuPhase prepared === Execute
  cpuOp2 prepared === op2
  cpuPhase executed === ExecuteFinish
  cAlu (cpuComputed completed) === cAlu expected
  cLink (cpuComputed completed) === cLink expected
  cTarget (cpuComputed completed) === cTarget expected
  cTaken (cpuComputed completed) === cTaken expected
  cAddress (cpuComputed completed) === cAddress expected
  cpuPc completed === pc
  cpuCountRetirement completed === False
  cpuPendingWrite completed === Nothing

prop_simple_instructions_take_nine_cycles_each :: Property
prop_simple_instructions_take_nine_cycles_each = property $ do
  n <- forAll (Gen.int (Range.linear 1 50))
  let ram = ramFromList (P.replicate n (addi 1 1 1))
      (cycles, halted) = runUntilHalt 1000 (startSim (runningMachine []) ram)

  -- Nine clocks per instruction, then ten to fetch the zero word after the
  -- program, decode it as illegal and trap to the boot ROM.
  cycles === 9 * n + 10
  cpuRegs (simMachine halted) !! (1 :: Index 32) === fromIntegral n

prop_store_into_next_instruction_is_fetched_fresh :: Property
prop_store_into_next_instruction_is_fetched_fresh = withTests 1 . property $ do
  -- addi x1,x0,1; sw x5,8(x31) where x5 holds "addi x6,x0,42" and x31 the
  -- RAM base; <word 2 overwritten>
  let ram = ramFromList [addi 1 0 1, sw 5 31 8, 0]
      (_, halted) = runUntilHalt 1000 (startSim (runningMachine [(5, addi 6 0 42), (31, ramBase)]) ram)

  cpuRegs (simMachine halted) !! (6 :: Index 32) === 42
  trapEpc (cpuTrapRegisters (simMachine halted)) === ramBase + 12

prop_uart_tx_store_waits_for_ready :: Property
prop_uart_tx_store_waits_for_ready = property $ do
  bytes <- forAll (Gen.list (Range.singleton 3) (Gen.integral Range.constantBounded))
  readiness <- forAll (Gen.list (Range.linear 0 60) Gen.bool)
  let registers = (2, uartTxData) : P.zip [1, 3, 4] (P.map resize bytes)
      -- sw x1,0(x2); sw x3,0(x2); sw x4,0(x2)
      ram = ramFromList [sw 1 2 0, sw 3 2 0, sw 4 2 0]
      waiting = P.foldl (\sim ready -> stepSimReady ready sim Nothing) (startSim (runningMachine registers) ram) readiness
      finished = runUntilDone 5000 waiting

  simTransmitted finished === bytes P.++ doneBytes

-- Whole circuit ----------------------------------------------------------------

uartFrame :: Byte -> [Bit]
uartFrame byte =
  P.concatMap
    (P.replicate clocksPerBit)
    (low : P.map (boolToBit . testBit byte) [0 .. 7] P.++ [high])

prop_circuit_programs_and_runs_over_uart :: Property
prop_circuit_programs_and_runs_over_uart = withTests 1 . property $ do
  let program =
        [ lui 2 0x10000, -- x2 = UART TXDATA
          addi 1 0 0x48, -- 'H'
          sw 1 2 0,
          addi 1 0 0x69, -- 'i' (stalls while 'H' is still being sent)
          sw 1 2 0
        ]
      frame = [0x50, fromIntegral (P.length program), 0] P.++ P.concatMap wordBytes program P.++ [0x52]
      idle k = P.replicate k high
      waveform =
        idle 20
          P.++ P.concatMap (\byte -> uartFrame byte P.++ idle 20) frame
          P.++ idle (8 * 10 * clocksPerBit)
      output = simulateN @System (P.length waveform) simpleRisc waveform
      received = [value | Just value <- rxTrace output]

  received === [0x48, 0x69] P.++ doneBytes

prop_boot_rom_clears_memory :: Property
prop_boot_rom_clears_memory = withTests 1 . property $ do
  let commanded = runInputs (startSim initialMachine (blankRam 0xdead_beef))
        (P.replicate 500 Nothing P.++ [Just 0x4d])
      -- The ROM clears words in order; the last one marks completion.
      clearing fuel sim
        | ramRead (simRam sim) 16383 == 0 || fuel <= (0 :: Int) = sim
        | otherwise = clearing (fuel - 1) (stepSim sim Nothing)
      cleared = clearing 2_000_000 commanded

  ramRead (simRam commanded) 0 === 0xdead_beef
  assert (P.all (\index -> ramRead (simRam cleared) index == 0) [0 .. 16383])
  simTransmitted cleared === []

-- RV32M ----------------------------------------------------------------------

data MulDivOp = Mul | Mulh | Mulhsu | Mulhu | Div | Divu | Rem | Remu
  deriving (Bounded, Enum, Eq, Show)

-- | @op x3, x1, x2@: funct7 = 1, funct3 = the operation's position.
encodeMulDiv :: MulDivOp -> Word32
encodeMulDiv operation =
  (1 `shiftL` 25)
    .|. (2 `shiftL` 20)
    .|. (1 `shiftL` 15)
    .|. (fromIntegral (fromEnum operation) `shiftL` 12)
    .|. (3 `shiftL` 7)
    .|. 0x33

-- | The RISC-V M semantics, computed on unbounded integers.
mulDivResult :: MulDivOp -> Word32 -> Word32 -> Word32
mulDivResult operation a b = case operation of
  Mul -> fromInteger (ua * ub)
  Mulh -> fromInteger ((sa * sb) `shiftR` 32)
  Mulhsu -> fromInteger ((sa * ub) `shiftR` 32)
  Mulhu -> fromInteger ((ua * ub) `shiftR` 32)
  Div
    | b == 0 -> maxBound
    | overflow -> a
    | otherwise -> fromInteger (sa `quot` sb)
  Divu
    | b == 0 -> maxBound
    | otherwise -> fromInteger (ua `quot` ub)
  Rem
    | b == 0 -> a
    | overflow -> 0
    | otherwise -> fromInteger (sa `rem` sb)
  Remu
    | b == 0 -> a
    | otherwise -> fromInteger (ua `rem` ub)
  where
    ua = toInteger a
    ub = toInteger b
    sa = toInteger (asSigned a)
    sb = toInteger (asSigned b)
    overflow = a == 0x8000_0000 && b == maxBound

-- | Random words, often the ones division and sign handling get wrong.
operandGen :: Gen Word32
operandGen =
  Gen.frequency
    [ (3, Gen.integral Range.constantBounded),
      (1, Gen.integral (Range.linear 0 20)),
      (2, Gen.element [0, 1, 2, maxBound, maxBound - 1, 0x8000_0000, 0x7fff_ffff])
    ]

prop_muldiv_matches_reference :: Property
prop_muldiv_matches_reference = withTests 400 . property $ do
  operation <- forAll Gen.enumBounded
  a <- forAll operandGen
  b <- forAll operandGen
  -- op x3,x1,x2; sw x3,256(x31) with x31 = RAM base
  let ram = ramFromList [encodeMulDiv operation, sw 3 31 256]
      (_, halted) = runUntilHalt 200 (startSim (runningMachine [(1, a), (2, b), (31, ramBase)]) ram)
      expected = mulDivResult operation a b

  assert (romAddress (cpuPc (simMachine halted)))
  cpuRegs (simMachine halted) !! (3 :: Index 32) === expected
  ramRead (simRam halted) 64 === expected

-- UART timing ----------------------------------------------------------------

clocksPerBit :: Int
clocksPerBit = fromIntegral uartClocksPerBit

txTrace :: Byte -> Byte -> [(Bit, Bool)]
txTrace byte ignoredByte =
  let requests = Just byte : P.replicate (10 * clocksPerBit) (Just ignoredByte) P.++ [Nothing]
      (_, outputs) = List.mapAccumL step TxIdle requests
   in outputs
  where
    step state request =
      let (state', output) = txStep state request
       in (state', output)

prop_uart_tx_is_cycle_exact_and_latches_byte :: Property
prop_uart_tx_is_cycle_exact_and_latches_byte = property $ do
  byte <- forAll (Gen.integral Range.constantBounded)
  ignoredByte <- forAll (Gen.integral Range.constantBounded)
  let trace = txTrace byte ignoredByte
      expected =
        P.replicate clocksPerBit low
          P.++ P.concatMap (P.replicate clocksPerBit . boolToBit . testBit byte) [0 .. 7]
          P.++ P.replicate clocksPerBit high
  case List.uncons trace of
    Nothing -> failure
    Just (accepted, afterAccepted) -> do
      let (frame, afterFrame) = P.splitAt (10 * clocksPerBit) afterAccepted
      accepted === (high, True)
      P.map P.fst frame === expected
      assert (P.all (not . P.snd) frame)
      afterFrame === [(high, True)]

rxTrace :: [Bit] -> [Maybe Byte]
rxTrace inputBits = P.snd (List.mapAccumL step RxIdle inputBits)
  where
    step state serialBit = rxStep state serialBit

prop_uart_rx_samples_115200_8n1 :: Property
prop_uart_rx_samples_115200_8n1 = property $ do
  byte <- forAll (Gen.integral Range.constantBounded)
  let waveform =
        P.replicate 12 high
          P.++ P.replicate clocksPerBit low
          P.++ P.concatMap (P.replicate clocksPerBit . boolToBit . testBit byte) [0 .. 7]
          P.++ P.replicate clocksPerBit high
          P.++ P.replicate 12 high
      received = [value | Just value <- rxTrace waveform]

  received === [byte]

prop_uart_rx_holding_register_is_one_byte_deep :: Property
prop_uart_rx_holding_register_is_one_byte_deep = property $ do
  first <- forAll (Gen.filter (/= 0x03) (Gen.integral Range.constantBounded))
  second <- forAll (Gen.filter (/= 0x03) (Gen.integral Range.constantBounded))
  let running = initialMachine {cpuPhase = Fetch}
      (withFirst, _) = machineStep running (0, Just first, True)
      -- Keep this latch-focused property from advancing into an instruction
      -- decode between the two simulated receive cycles.
      (afterSecond, _) = machineStep withFirst {cpuPhase = Fetch} (0, Just second, True)
      readThroughBus address m =
        let (completed, write, _) = commitSettled m
              {cpuPhase = Commit, cpuInstruction = 0x00002083,
               cpuComputed = Computed Nothing 0 0 Nothing address} True
         in (maybe 0 P.snd write, completed)
      (status, afterStatus) = readThroughBus uartStatus afterSecond
      (value, consumed) = readThroughBus uartRxData afterStatus
      (emptyStatus, _) = readThroughBus uartStatus consumed

  rxHolding afterSecond === Just first
  status === 0b11
  value === resize first
  rxHolding consumed === Nothing
  emptyStatus === 0b01

-- First machine-mode slice: exception entry must be precise and side-effect free.
prop_precise_exception_entry :: Property
prop_precise_exception_entry = property $ do
  pc <- forAll (Gen.integral (Range.linear 0 0x3fff))
  enabled <- forAll Gen.bool
  let faultPc = pc * 4
      base = initialMachine
        { cpuPhase = Commit, cpuPc = faultPc,
          cpuRegs = replace (1 :: Index 32) 0x12345678 (repeat 0),
          rxHolding = Just 0x5a,
          cpuTrapRegisters = initialTrapRegisters {trapVector = 0x100, trapMie = enabled}
        }
      -- instruction, target/address, taken branch, cause, mtval
      cases = [ (0x00000000, 0, Nothing, 2, 0),
                (0xffffffff, 0, Nothing, 2, 0xffffffff),
                (0x00000073, 0, Nothing, 11, 0),
                (0x00100073, 0, Nothing, 3, faultPc),
                (0x00200073, 0, Nothing, 2, 0x00200073),
                (0x000000ef, 6, Nothing, 0, 6), -- JAL x1
                (0x000080e7, 6, Nothing, 0, 6), -- JALR x1
                (0x00000063, 6, Just True, 0, 6),
                (0x00002083, 1, Nothing, 4, 1), -- LW
                (0x00002083, 2, Nothing, 4, 2),
                (0x00002083, 3, Nothing, 4, 3),
                (0x00001083, 1, Nothing, 4, 1), -- LH
                (0x00005083, 3, Nothing, 4, 3), -- LHU
                (0x00002023, 1, Nothing, 6, 1), -- SW
                (0x00001023, 3, Nothing, 6, 3), -- SH
                (0x00002083, 0x10000, Nothing, 5, 0x10000),
                (0x00002023, 0x10000, Nothing, 7, 0x10000),
                (0x00002083, 0x10001, Nothing, 4, 0x10001), -- alignment priority
                (0x00003083, uartRxData, Nothing, 2, 0x00003083), -- invalid UART load width
                (0x00003023, uartTxData, Nothing, 2, 0x00003023),
                (0x00002023, uartStatus, Nothing, 7, uartStatus)
              ]
  P.mapM_ (\(instruction, address, taken, cause, value) -> do
    let m = base {cpuInstruction = instruction,
                  cpuComputed = Computed Nothing (faultPc + 4) address taken address}
        (queued, write, output) = commitSettled m True
        (entered, trapWrite, trapOutput) = cpuStep queued 0 Nothing True
        regs = cpuTrapRegisters entered
    cpuPhase queued === Trap
    cpuPc queued === faultPc
    cpuTrapRegisters queued === cpuTrapRegisters base
    write === Nothing
    output === (0, Nothing, Nothing)
    cpuPhase entered === Fetch
    cpuPc entered === 0x100
    trapEpc regs === faultPc
    trapCause regs === cause
    trapValue regs === value
    trapMie regs === False
    trapMpie regs === enabled
    cpuRegs entered === cpuRegs base
    rxHolding entered === Just 0x5a
    trapWrite === Nothing
    trapOutput === (0, Nothing, Nothing)
    ) cases

-- With mtvec 0 a trap lands in the boot ROM, which reports the end of the
-- program; Ctrl-C resets into the ROM silently.
prop_unhandled_trap_returns_to_boot_rom :: Property
prop_unhandled_trap_returns_to_boot_rom = withTests 1 $ property $ do
  let m = (runningMachine []) {cpuInstruction = 0x00000073, cpuPhase = Commit}
      (queued, _, _) = commitInstruction m True
      (stopped, _, _) = cpuStep queued 0 Nothing True
      resumed = runUntilDone 3000 (startSim stopped (blankRam 0))
      reset = resetCpu stopped
      broken = runInputs (startSim reset (blankRam 0)) (P.replicate 3000 Nothing)
  cpuPc stopped === 0
  trapCause (cpuTrapRegisters stopped) === 11
  simTransmitted resumed === doneBytes
  cpuTrapRegisters reset === initialTrapRegisters
  cpuPc reset === 0
  bootCause reset === causeBreak
  simTransmitted broken === []
  -- In the boot ROM, 0x03 is an ordinary received byte, not a reset.
  let inRom = initialMachine {cpuPc = 0x40, bootCause = causeNone}
      (received, _) = machineStep inRom (0, Just 3, True)
  rxHolding received === Just 3
  bootCause received === causeNone
  cpuPc received === 0x40
  let (notTaken, write, _) = commitInstruction
        m {cpuInstruction = 0x00000063, cpuComputed = Computed Nothing 4 6 (Just False) 0} True
  cpuPhase notTaken === Fetch
  cpuPc notTaken === 4
  write === Nothing

prop_faulting_store_never_writes_ram :: Property
prop_faulting_store_never_writes_ram = withTests 1 $ property $ do
  let sim = startSim (runningMachine [(1, ramBase + 0x100), (2, 0xdeadbeef)])
            (ramFromList [sw 2 1 1, 0x00000073])
      (_, stopped) = runUntilHalt 100 sim
      drained = runUntilDone 3000 stopped
      regs = cpuTrapRegisters (simMachine stopped)
  trapCause regs === 6
  trapValue regs === ramBase + 0x101
  trapEpc regs === ramBase
  ramRead (simRam drained) 64 === 0
  simTransmitted drained === doneBytes

csrInstruction :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
csrInstruction address kind rd source =
  (address `shiftL` 20) .|. (source `shiftL` 15) .|. (kind `shiftL` 12) .|. (rd `shiftL` 7) .|. 0x73

prop_registered_csr_selection_matches_reference :: Property
prop_registered_csr_selection_matches_reference = property $ do
  value <- forAll (Gen.integral Range.constantBounded)
  address <- forAll (Gen.integral (Range.linear 0 0xfff))
  let base = initialMachine {
        cpuScratch = value, cpuInterruptEnable = value `xor` 0x888,
        cpuTrapRegisters = initialTrapRegisters {trapVector = value `xor` 0x305,
          trapEpc = value `xor` 0x341, trapCause = value `xor` 0x342,
          trapValue = value `xor` 0x343, trapMie = testBit value 3, trapMpie = testBit value 7},
        cpuCycle = Counter64 value (value `xor` 0xb80) False,
        cpuInstret = Counter64 (value `xor` 0xb02) (value `xor` 0xb82) False}
  P.mapM_ (\csr -> do
    let m = base {cpuInstruction = csrInstruction csr 2 1 0}
        (queued, _, _) = commitInstruction m True
        (readState, _, _) = cpuStep queued 0 Nothing True
    case readCsr (fromIntegral csr) base of
      Nothing -> cpuPhase readState === Trap
      Just (expected, _) -> do
        cpuPhase readState === CsrWrite
        cpuCsrValue readState === expected
    ) (address : [0x300, 0x301, 0x304, 0x305, 0x310, 0x340, 0x341, 0x342,
                  0x343, 0x344, 0xb00, 0xb80, 0xb02, 0xb82, 0xc00, 0xc80,
                  0xc02, 0xc82, 0xf11, 0xf12, 0xf13, 0xf14])

prop_csr_read_modify_write :: Property
prop_csr_read_modify_write = property $ do
  old <- forAll (Gen.integral Range.constantBounded)
  operand <- forAll (Gen.integral Range.constantBounded)
  P.mapM_ (\kind -> P.mapM_ (\source -> P.mapM_ (\rd -> do
    let instruction = csrInstruction 0x340 kind rd source
        immediate = kind >= 5
        value = if immediate then source else if source == 0 then 0 else operand
        write = kind == 1 || kind == 5 || source /= 0
        result = case kind .&. 3 of
          1 -> value
          2 -> old .|. value
          _ -> old .&. complement value
        m = initialMachine {cpuPhase = Commit, cpuPc = 16,
              cpuInstruction = instruction, cpuA = if source == 0 then 0 else operand,
              cpuScratch = old, cpuComputed = Computed Nothing 20 0 Nothing 0}
        (reading, _, _) = commitInstruction m True
        (writing, _, _) = cpuStep reading 0 Nothing True
        (finished, registerWrite, _) = cpuStep writing 0 Nothing True
    cpuPhase reading === CsrRead
    cpuPhase writing === CsrWrite
    cpuCsrValue writing === old
    cpuScratch writing === old
    cpuScratch finished === if write then result else old
    cpuPc finished === 20
    registerWrite === Just (regIndex rd, old)
    ) [0, 3]) [0, 1, 31]) [1, 2, 3, 5, 6, 7]

prop_csr_access_rules_and_masks :: Property
prop_csr_access_rules_and_masks = property $ do
  value <- forAll (Gen.integral Range.constantBounded)
  let base = initialMachine {cpuPhase = CsrRead}
      checkAccess address kind source faults = do
        let instruction = csrInstruction address kind 0 source
            (m, write, output) = cpuStep base {cpuInstruction = instruction,
              cpuCsrSelection = decodeCsrSelection (fromIntegral address),
              cpuCsrReadOnly = address == 0x301 || address .&. 0xc00 == 0xc00} 0 Nothing True
        cpuPhase m === if faults then Trap else CsrWrite
        write === Nothing
        output === (0, Nothing, Nothing)
        if faults then do
          cpuExceptionCause m === 2
          cpuExceptionValue m === instruction
        else success
  P.mapM_ (\address -> do
    checkAccess address 2 0 False
    checkAccess address 3 0 False
    checkAccess address 6 0 False
    checkAccess address 7 0 False
    checkAccess address 1 0 True
    checkAccess address 5 0 True
    checkAccess address 2 1 True -- rs1 has a zero value but is not x0
    checkAccess address 3 1 True
    ) [0x301, 0xf11, 0xf12, 0xf13, 0xf14]
  checkAccess 0x999 2 0 True
  checkAccess 0x999 1 0 True
  readCsr 0x301 base === Just (0x40001100, True)
  let written address = writeCsr address value base
      regs = cpuTrapRegisters (written 0x300)
  readCsr 0x300 (written 0x300) === Just (0x1800 .|. (value .&. 0x88), False)
  trapMie regs === testBit value 3
  trapMpie regs === testBit value 7
  trapVector (cpuTrapRegisters (written 0x305)) === value .&. complement 3
  trapEpc (cpuTrapRegisters (written 0x341)) === value .&. complement 3
  cpuInterruptEnable (written 0x304) === value .&. 0x888
  readCsr 0x310 (written 0x310) === Just (0, False)
  readCsr 0x344 (written 0x344) === Just (0, False)

prop_guest_handler_resumes_with_mret :: Property
prop_guest_handler_resumes_with_mret = withTests 1 $ property $ do
  -- mtvec = the handler at RAM offset 36; ECALL; resume; restore mtvec 0 and
  -- ECALL again, which falls back into the boot ROM.
  let program = [lui 1 0x80000, addi 1 1 36, csrInstruction 0x305 1 0 1,
                 0x00000073, addi 6 0 42,
                 csrInstruction 0x305 1 0 0, 0x00000073,
                 0, 0,
                 csrInstruction 0x341 2 2 0, addi 2 2 4,
                 csrInstruction 0x341 1 0 2, 0x30200073]
      (_, stopped) = runUntilHalt 1000 (startSim (runningMachine []) (ramFromList program))
      machine = simMachine stopped
  assert (romAddress (cpuPc machine))
  cpuRegs machine !! (6 :: Index 32) === 42
  trapEpc (cpuTrapRegisters machine) === ramBase + 24
  trapCause (cpuTrapRegisters machine) === 11

prop_mret_restores_status :: Property
prop_mret_restores_status = property $ do
  previous <- forAll Gen.bool
  let m = initialMachine {cpuPhase = TrapReturn,
            cpuTrapRegisters = initialTrapRegisters {trapEpc = 0x100, trapMpie = previous}}
      (returned, write, _) = cpuStep m 0 Nothing True
      regs = cpuTrapRegisters returned
  cpuPc returned === 0x100
  cpuPhase returned === Fetch
  trapMie regs === previous
  trapMpie regs === True
  write === Nothing

-- Interrupts ------------------------------------------------------------------

-- | @lw rd, imm(rs1)@
lw :: Word32 -> Word32 -> Word32 -> Word32
lw rd rs1 imm = ((imm .&. 0xfff) `shiftL` 20) .|. (rs1 `shiftL` 15) .|. (0b010 `shiftL` 12) .|. (rd `shiftL` 7) .|. 0x03

wfi :: Word32
wfi = 0x1050_0073

-- | mtip follows mtime >= mtimecmp two clocks later, across the 32-bit halves.
prop_mtip_compares_64_bits :: Property
prop_mtip_compares_64_bits = property $ do
  let near = Gen.choice [Gen.element [0, 1, maxBound - 1, maxBound], Gen.enumBounded]
  timeLo <- forAll near
  timeHi <- forAll (Gen.integral (Range.linear 0 3))
  compareLo <- forAll near
  compareHi <- forAll (Gen.integral (Range.linear 0 3))
  let devices = initialDevices {devMtime = Counter64 timeLo timeHi False,
        devMtimecmpLo = compareLo, devMtimecmpHi = compareHi}
      settled = stepDevices False False NoEffect (stepDevices False False NoEffect devices)
      time = (resize timeHi `shiftL` 32) .|. resize timeLo :: Unsigned 64
      deadline = (resize compareHi `shiftL` 32) .|. resize compareLo
  devMtip settled === (time >= deadline)
  -- Writes to read-only pending are ignored; msip follows bit 0.
  devPlicPending (stepDevices False False (deviceWrite PlicPendingTarget 0x400) initialDevices) === False
  devMsip (stepDevices False False (deviceWrite ClintMsipTarget 3) initialDevices) === True

-- | A timer interrupt wakes WFI and enters the handler with mepc after WFI.
prop_timer_interrupt_wakes_wfi :: Property
prop_timer_interrupt_wakes_wfi = property $ do
  deadline <- forAll (Gen.integral (Range.linear 200 600))
  let program = [csrInstruction 0x305 1 0 1, sw 0 30 4, sw 7 30 0,
                 csrInstruction 0x304 1 0 5, csrInstruction 0x300 6 0 8, wfi, 0, 0,
                 csrInstruction 0x342 2 6 0, csrInstruction 0x341 2 8 0,
                 csrInstruction 0x305 1 0 0, 0x00000073]
      registers = [(1, ramBase + 32), (30, clintCompareLo), (5, 0x80), (7, deadline)]
      (cycles, stopped) = runUntilHalt 2000 (startSim (runningMachine registers) (ramFromList program))
      machine = simMachine stopped
  assert (romAddress (cpuPc machine))
  cpuRegs machine !! (6 :: Index 32) === 0x8000_0007
  cpuRegs machine !! (8 :: Index 32) === ramBase + 24
  -- The handler entered no earlier than the deadline and soon after it.
  assert (fromIntegral cycles > deadline && fromIntegral cycles < deadline + 80)
  assert (devMtip (cpuDevices machine))
  -- The handler cleared MIE; mip shows the timer.
  readCsr 0x344 machine === Just (0x80, False)

-- | A received byte raises the PLIC's external interrupt; claim returns
-- IRQ 10, and once the byte is read and the claim completed nothing remains.
prop_uart_interrupt_claims_through_plic :: Property
prop_uart_interrupt_claims_through_plic = property $ do
  -- 0x03 is Ctrl-C, which resets a running program instead.
  byte <- forAll (Gen.filter (/= 3) (Gen.integral Range.linearBounded))
  delay <- forAll (Gen.integral (Range.linear 200 400))
  let program = [csrInstruction 0x305 1 0 1, sw 5 30 0x28, sw 9 28 0, sw 5 26 0xc,
                 csrInstruction 0x304 1 0 10, csrInstruction 0x300 6 0 8, wfi, 0,
                 csrInstruction 0x342 2 6 0, lw 11 27 4, lw 12 26 8, sw 11 27 4,
                 lw 13 27 4, csrInstruction 0x305 1 0 0, 0x00000073]
      registers = [(1, ramBase + 32), (5, 1), (9, 0x400), (10, 0x800),
                   (26, uartTxData), (27, plicThreshold), (28, plicEnable), (30, 0x0c00_0000)]
      start = startSim (runningMachine registers) (ramFromList program)
      waiting = runInputs start (P.replicate delay Nothing)
      (_, stopped) = runUntilHalt 2000 (stepSim waiting (Just byte))
      machine = simMachine stopped
  assert (not (romAddress (cpuPc (simMachine waiting))))
  cpuPhase (simMachine waiting) === Commit
  assert (romAddress (cpuPc machine))
  cpuRegs machine !! (6 :: Index 32) === 0x8000_000b
  cpuRegs machine !! (11 :: Index 32) === uartIrq
  cpuRegs machine !! (12 :: Index 32) === resize byte
  cpuRegs machine !! (13 :: Index 32) === 0
  let devices = cpuDevices machine
  devPlicPending devices === False
  devPlicClaimed devices === False
  devMeip devices === False

-- | Interrupts stay masked while MIE is clear, and a reset restores quiet
-- devices while keeping time.
prop_reset_quietens_devices :: Property
prop_reset_quietens_devices = withTests 1 . property $ do
  let loud = initialDevices {devMsip = True, devMtimecmpLo = 0, devMtimecmpHi = 0, devMtip = True,
        devMtime = Counter64 77 1 False, devPlicEnable = True, devPlicPriority = 7, devUartRxInterrupt = True}
      machine = (runningMachine []) {cpuDevices = loud, cpuInterruptEnable = 0x888}
  interruptCause machine === Nothing
  interruptCause machine {cpuTrapRegisters = initialTrapRegisters {trapMie = True}} === Just 0x8000_0003
  let reset = cpuDevices (resetCpu machine)
  reset === initialDevices {devMtime = Counter64 77 1 False}

-- | A pending interrupt is taken at the first boundary after MRET sets MIE,
-- and not at all while mie masks it.
prop_mret_takes_pending_interrupt_at_once :: Property
prop_mret_takes_pending_interrupt_at_once = property $ do
  enabled <- forAll Gen.bool
  let pending = initialDevices {devMtip = True, devHiEqual = True, devLoAtLeast = True,
        devMtimecmpLo = 0, devMtimecmpHi = 0}
      m = (runningMachine []) {cpuPhase = TrapReturn, cpuDevices = pending,
            cpuInterruptEnable = if enabled then 0x80 else 0,
            cpuTrapRegisters = initialTrapRegisters {trapEpc = ramBase + 0x40, trapMpie = True, trapVector = ramBase + 0x100}}
      step machine = P.fst (machineStep machine (0, Nothing, True))
      fetching = P.head (P.dropWhile ((/= Fetch) . cpuPhase) (P.iterate step m))
      next = step fetching
  cpuPc fetching === ramBase + 0x40
  if enabled
    then do
      cpuPhase next === Trap
      let entered = P.head (P.dropWhile ((== Trap) . cpuPhase) (P.iterate step next))
      cpuPc entered === ramBase + 0x100
      trapCause (cpuTrapRegisters entered) === 0x8000_0007
      trapEpc (cpuTrapRegisters entered) === ramBase + 0x40
      trapMie (cpuTrapRegisters entered) === False
      trapMpie (cpuTrapRegisters entered) === True
    else cpuPhase next === FetchDelay

-- | A program that traps back into the ROM leaves its interrupt sources
-- armed (that is not a reset); 'R' quietens them before the next program.
prop_boot_rom_quietens_devices_before_run :: Property
prop_boot_rom_quietens_devices_before_run = withTests 1 . property $ do
  let (_, halted) = runUntilHalt 100 (startSim (runningMachine []) (ramFromList [0]))
      loud = (cpuDevices (simMachine halted)) {devMsip = True, devMtimecmpLo = 0, devMtimecmpHi = 0,
        devPlicPriority = 5, devPlicEnable = True, devPlicThreshold = 2, devPlicPending = True,
        devPlicClaimed = True, devUartTxInterrupt = True}
      armed = halted {simMachine = (simMachine halted) {cpuDevices = loud}}
      done = runUntilDone 20000 armed
      started = go (20000 :: Int) (runInputs done (hostBytes 400 [0x52]))
      go fuel sim
        | not (romAddress (cpuPc (simMachine sim))) || fuel == 0 = sim
        | otherwise = go (fuel - 1) (stepSim sim Nothing)
      devices = cpuDevices (simMachine started)
  simTransmitted done === doneBytes
  cpuPc (simMachine started) === ramBase
  devices === initialDevices {devMtime = devMtime devices}

-- | The SPI master exchanges one byte in mode 0: MOSI is stable at every
-- rising edge (MSB first), MISO is sampled there, SCK idles low and MOSI
-- high, and the bus reads DATA/STATUS/CTRL back.
prop_spi_exchanges_bytes_in_mode_0 :: Property
prop_spi_exchanges_bytes_in_mode_0 = property $ do
  divider <- forAll (Gen.integral (Range.linear 0 6))
  out <- forAll (Gen.integral Range.linearBounded)
  incoming <- forAll (Gen.integral Range.linearBounded)
  let configured = stepSpi Nothing (Just (1 .|. (divider `shiftL` 8))) initialSpi
      started = stepSpi (Just out) Nothing configured
      run spi rises sent fuel
        | not (spiBusy spi) || fuel == (0 :: Int) = (spi, P.reverse sent, rises)
        | otherwise =
            let fed = spi {spiMiso = if testBit incoming (7 - rises) then high else low}
                next = stepSpi Nothing Nothing fed
                rose = not (spiSck spi) && spiSck next
                (sck, mosi, _) = spiPins spi
             in if rose
                  then run next (rises + 1) ((mosi, sck) : sent) (fuel - 1)
                  else run next rises sent (fuel - 1)
      (done, sent, rises) = run started 0 [] 10000
  spiSelect configured === True
  spiDivider configured === fromIntegral divider
  rises === 8
  P.map P.fst sent === [if testBit out i then high else low | i <- [7, 6 .. 0]]
  P.map P.snd sent === P.replicate 8 low
  spiIn done === incoming
  spiPins done === (low, high, low)
  decodeMemoryTarget (ioBase + ioSpiData) === IoTarget
  decodeMemoryTarget (ioBase + 0xffc) === IoTarget

-- | The register FIFO behaves as a bounded queue: pushes to a full FIFO are
-- dropped, pops of an empty one do nothing, flush empties it.
prop_fifo_is_a_bounded_queue :: Property
prop_fifo_is_a_bounded_queue = property $ do
  ops <- forAll (Gen.list (Range.linear 0 200)
    ((,,) <$> Gen.frequency [(1, P.pure True), (30, P.pure False)]
          <*> Gen.maybe (Gen.integral Range.linearBounded :: Gen Byte) <*> Gen.bool))
  let model queue (flush, push, pop)
        | flush = []
        | otherwise =
            let popped = if pop then P.drop 1 queue else queue
                room = P.length queue < 4 || (pop && not (P.null queue))
             in case push of
                  Just b | room -> popped P.++ [b]
                  _ -> popped
      go :: Fifo 4 -> [Byte] -> [(Bool, Maybe Byte, Bool)] -> PropertyT IO ()
      go fifo queue [] = do
        P.fst (P.snd (fifoStep fifo (False, Nothing, False))) === listToMaybe queue
      go fifo queue (op : rest) = do
        let (fifo', (headByte, count)) = fifoStep fifo op
        headByte === listToMaybe queue
        fromIntegral count === P.length queue
        go fifo' (model queue op) rest
      listToMaybe xs = case xs of [] -> Nothing; x : _ -> Just x
  go (initialFifo :: Fifo 4) [] ops

-- | Drive the I/O complex alone: CPU-style word requests through its
-- handshake, an SPI slave that sends @slaveBytes@ (MSB first, changing on
-- falling edges, 0xff when exhausted), and a RAM on the second port with one
-- cycle of read latency. Returns the complex, the RAM, the bytes the master
-- sent (sampled at rising edges) and the answers to the requests.
data IoRig = IoRig {rigIo :: Io, rigRam :: IntMap.IntMap Word32, rigRamOut :: Word32,
  rigSlave :: [Byte], rigBit :: Int, rigSent :: [Bit], rigAnswers :: [Word32]}

rigStep :: Maybe BusRequest -> IoRig -> IoRig
rigStep request rig =
  let io = rigIo rig
      misoBit = case rigSlave rig of
        b : _ -> if testBit b (7 - rigBit rig) then high else low
        [] -> high
      (io', (answer, (sck, mosi, _), ramCommand)) = ioStep io (request, False, misoBit, rigRamOut rig)
      sckBefore = spiSck (ioSpi io)
      sckAfter = spiSck (ioSpi io')
      rising = not sckBefore && sckAfter
      falling = sckBefore && not sckAfter
      (slave', bit') = if falling then (if rigBit rig == 7 then (P.drop 1 (rigSlave rig), 0) else (rigSlave rig, rigBit rig + 1))
        else (rigSlave rig, rigBit rig)
      (ram', ramOut') = case ramCommand of
        Just (index, Just value) -> (IntMap.insert (fromIntegral index) value (rigRam rig), rigRamOut rig)
        Just (index, Nothing) -> (rigRam rig, IntMap.findWithDefault 0 (fromIntegral index) (rigRam rig))
        Nothing -> (rigRam rig, rigRamOut rig)
      _ = sck
   in rig {rigIo = io', rigRam = ram', rigRamOut = ramOut', rigSlave = slave', rigBit = bit',
        rigSent = if rising then mosi : rigSent rig else rigSent rig,
        rigAnswers = maybe (rigAnswers rig) (: rigAnswers rig) answer}

-- | One word access: hold the request until the registered answer.
rigAccess :: Word32 -> Bool -> Word32 -> IoRig -> IoRig
rigAccess offset write value rig0 = go (rigStep (Just q) rig0) (40 :: Int)
  where
    q = BusRequest (ioBase + offset) 2 write value
    n = P.length (rigAnswers rig0)
    go rig fuel
      | P.length (rigAnswers rig) > n || fuel == 0 = rigStep Nothing rig
      | otherwise = go (rigStep (Just q) rig) (fuel - 1)

rigUntilIdle :: IoRig -> IoRig
rigUntilIdle rig0 = go rig0 (200000 :: Int)
  where
    go rig fuel
      | not (dmaBusy (ioDma (rigIo rig))) || fuel == 0 = rig
      | otherwise = go (rigStep Nothing rig) (fuel - 1)

bitsToBytes :: [Bit] -> [Byte]
bitsToBytes bits = case P.splitAt 8 bits of
  (byte, rest) | P.length byte == 8 -> P.foldl (\acc b -> acc * 2 + (if b == high then 1 else 0)) 0 byte : bitsToBytes rest
  _ -> []

-- | The SPI DMA engine moves whole SD-style data blocks between the SPI
-- master and RAM's second port: receive after a start token (and refuse a
-- wrong one), and send RAM words byte by byte, little-endian.
prop_spi_dma_moves_blocks :: Property
prop_spi_dma_moves_blocks = property $ do
  words' <- forAll (Gen.list (Range.linear 1 16) (Gen.integral Range.linearBounded :: Gen Word32))
  waits <- forAll (Gen.integral (Range.linear 0 20))
  divider <- forAll (Gen.integral (Range.linear 0 3) :: Gen Word32)
  let bytesOf w = [resize (w `shiftR` (8 * i)) | i <- [0 .. 3]] :: [Byte]
      payload = P.concatMap bytesOf words'
      count = fromIntegral (4 * P.length words')
      base = 0x40 :: Word32
      fresh slave = IoRig initialIo IntMap.empty 0 slave 0 [] []
      setup slave = rigAccess ioSpiControl True (1 .|. (divider `shiftL` 8)) . rigAccess ioDmaCount True count
        . rigAccess ioDmaAddress True (ramBase + 4 * base) $ fresh slave
      -- receive: waits x 0xff, the token, the data
      received = rigUntilIdle (rigAccess ioDmaControl True 2 (setup (P.replicate waits 0xff P.++ [0xfe] P.++ payload)))
      status = P.head (rigAnswers (rigAccess ioDmaStatus False 0 received))
  [IntMap.findWithDefault 0 (fromIntegral base + i) (rigRam received) | i <- [0 .. P.length words' - 1]] === words'
  status === 0xfe00
  dmaCount (ioDma (rigIo received)) === 0
  -- send: RAM words out on MOSI, lowest address byte first
  let preload = (setup []) {rigRam = IntMap.fromList (P.zip [fromIntegral base ..] words')}
      sent = rigUntilIdle (rigAccess ioDmaControl True 1 preload)
  bitsToBytes (P.reverse (rigSent sent)) === payload
  -- a wrong token is an error, and nothing is written
  let refused = rigUntilIdle (rigAccess ioDmaControl True 2 (setup [0xff, 0x0b]))
      refusal = P.head (rigAnswers (rigAccess ioDmaStatus False 0 refused))
  refusal === (0x0b00 .|. 2)
  IntMap.null (rigRam refused) === True

-- | A program reaches the SPI master through the I/O complex: word stores
-- and loads in 0x1000_1xxx wait in BusIo for the complex's registered
-- answer. With MISO high the byte received is 0xff.
prop_program_reaches_io_complex :: Property
prop_program_reaches_io_complex = withTests 1 . property $ do
  -- x1 = I/O base; CTRL = select, divider 3; DATA = 0xa5; poll STATUS;
  -- read DATA and CTRL; ECALL (mtvec 0) returns to the ROM
  let program = [sw 2 1 8, sw 3 1 0, lw 4 1 4, bne 4 0 (0 - 4), lw 5 1 0, lw 6 1 8, 0x00000073]
      start = startSim (runningMachine [(1, ioBase), (2, 0x301), (3, 0xa5)]) (ramFromList program)
      go :: Int -> Sim -> Io -> (Sim, Io)
      go fuel sim io
        | romAddress (cpuPc (simMachine sim)) || fuel == 0 = (sim, io)
        | otherwise =
            let m = simMachine sim
                (io', (answer, _, _)) = ioStep io (ioRequest m, clearRegisters m, high, 0)
                injected = sim {simMachine = m {cpuDevices = (cpuDevices m) {devIo = answer}}}
             in go (fuel - 1) (stepSim injected Nothing) io'
      (stopped, io) = go 5000 start initialIo
      regs = cpuRegs (simMachine stopped)
  assert (romAddress (cpuPc (simMachine stopped)))
  regs !! (5 :: Index 32) === 0xff
  regs !! (6 :: Index 32) === 0x301
  regs !! (4 :: Index 32) === 0
  spiSelect (ioSpi io) === True

-- Differential testing against the reference model --------------------------

rType, iType, sType, bType :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
rType f7 rs2 rs1 f3 rd = (f7 `shiftL` 25) .|. (rs2 `shiftL` 20) .|. (rs1 `shiftL` 15) .|. (f3 `shiftL` 12) .|. (rd `shiftL` 7) .|. 0x33
iType imm rs1 f3 rd op = ((imm .&. 0xfff) `shiftL` 20) .|. (rs1 `shiftL` 15) .|. (f3 `shiftL` 12) .|. (rd `shiftL` 7) .|. op
sType imm rs2 rs1 f3 op = (((imm `shiftR` 5) .&. 0x7f) `shiftL` 25) .|. (rs2 `shiftL` 20) .|. (rs1 `shiftL` 15)
  .|. (f3 `shiftL` 12) .|. ((imm .&. 0x1f) `shiftL` 7) .|. op
bType imm rs2 rs1 f3 _ = (((imm `shiftR` 12) .&. 1) `shiftL` 31) .|. (((imm `shiftR` 5) .&. 0x3f) `shiftL` 25)
  .|. (rs2 `shiftL` 20) .|. (rs1 `shiftL` 15) .|. (f3 `shiftL` 12) .|. (((imm `shiftR` 1) .&. 0xf) `shiftL` 8)
  .|. (((imm `shiftR` 11) .&. 1) `shiftL` 7) .|. 0x63

jType :: Word32 -> Word32 -> Word32
jType imm rd = (((imm `shiftR` 20) .&. 1) `shiftL` 31) .|. (((imm `shiftR` 1) .&. 0x3ff) `shiftL` 21)
  .|. (((imm `shiftR` 11) .&. 1) `shiftL` 20) .|. (((imm `shiftR` 12) .&. 0xff) `shiftL` 12) .|. (rd `shiftL` 7) .|. 0x6f

-- | The data pointer: x31 points into the middle of a 256-word data region.
dataBase :: Word32
dataBase = ramBase + 0x2000

dataPointer :: Word32
dataPointer = dataBase + 0x200

-- | A few registers, so instructions depend on each other back to back.
genReg :: Gen Word32
genReg = Gen.frequency [(1, P.pure 0), (12, Gen.integral (Range.linear 1 8))]

genValue :: Gen Word32
genValue = Gen.frequency
  [ (3, Gen.element [0, 1, 2, 0xffff_ffff, 0x8000_0000, 0x7fff_ffff, 0x8000_0001]),
    (4, Gen.integral (Range.linear 0 64)),
    (4, Gen.integral Range.linearBounded) ]

-- | One instruction (or an auipc/jalr pair) at index i of a program whose
-- ECALL is at index n; every control transfer goes forward.
genInstruction :: Int -> Int -> Gen [Word32]
genInstruction i n = Gen.frequency
  [ (20, (\(f7, f3) rs2 rs1 rd -> [rType f7 rs2 rs1 f3 rd]) <$> Gen.element rOps <*> genReg <*> genReg <*> genReg),
    (14, (\f3 imm rs1 rd -> [iType imm rs1 f3 rd 0x13]) <$> Gen.element [0, 2, 3, 4, 6, 7]
          <*> Gen.integral (Range.linearFrom 0 (-2048) 2047) <*> genReg <*> genReg),
    (5, (\(f7, f3) shamt rs1 rd -> [iType ((f7 `shiftL` 5) .|. shamt) rs1 f3 rd 0x13])
          <$> Gen.element [(0, 1), (0, 5), (0x20, 5)] <*> Gen.integral (Range.linear 0 31) <*> genReg <*> genReg),
    (4, (\op imm rd -> [(imm `shiftL` 12) .|. (rd `shiftL` 7) .|. op]) <$> Gen.element [0x37, 0x17]
          <*> Gen.integral (Range.linear 0 0xfffff) <*> genReg),
    (12, (\f3 off rd -> [iType off 31 f3 rd 0x03]) <$> Gen.element [0, 1, 2, 4, 5] <*> genOffset <*> genReg),
    (12, (\f3 off rs2 -> [sType off rs2 31 f3 0x23]) <$> Gen.element [0, 1, 2] <*> genOffset <*> genReg),
    (1, (\f3 rs1 rd -> [iType 0 rs1 f3 rd 0x03]) <$> Gen.element [0, 2] <*> genReg <*> genReg),
    (8, (\f3 k odd rs1 rs2 -> [bType (4 * k + (if odd then 2 else 0)) rs2 rs1 f3 0])
          <$> Gen.element [0, 1, 4, 5, 6, 7] <*> forward <*> Gen.frequency [(30, P.pure False), (1, P.pure True)]
          <*> genReg <*> genReg),
    (3, (\k rd -> [jType (4 * k) rd]) <$> forward <*> genReg),
    (3, (\k odd rd -> [(9 `shiftL` 7) .|. 0x17, iType (4 * (k + 1) + (if odd then 2 else 0)) 9 0 rd 0x67])
          <$> forward <*> Gen.frequency [(20, P.pure False), (1, P.pure True)] <*> genReg),
    (6, (\csr f3 src rd -> [csrInstruction csr f3 rd src]) <$> Gen.element [0x340, 0x341, 0x342, 0x343, 0x304, 0x300]
          <*> Gen.element [1, 2, 3, 5, 6, 7] <*> genReg <*> genReg),
    (2, (\csr f3 rd -> [csrInstruction csr f3 rd 0]) <$> Gen.element [0x301, 0xf11, 0xf14, 0x310, 0x344]
          <*> Gen.element [2, 3, 6, 7] <*> genReg),
    (1, P.pure [0x0ff0_000f]),
    -- rarer: each of these ends the program
    (1, Gen.frequency
      [ (1, (\csr rd -> [csrInstruction csr 1 rd 1]) <$> Gen.element [0x301, 0xf12, 0x7c0] <*> genReg),
        (1, Gen.element [[0x0000_0073], [0x0010_0073]]),
        (1, (: []) <$> Gen.integral Range.linearBounded) ]) ]
  where
    rOps = [(0, f) | f <- [0 .. 7]] P.++ [(0x20, 0), (0x20, 5)] P.++ [(1, f) | f <- [0 .. 7]]
    remaining = fromIntegral (P.max 1 (n - i)) :: Word32
    forward = Gen.integral (Range.linear 1 remaining)
    genOffset = Gen.frequency
      [ (30, (* 4) <$> Gen.integral (Range.linearFrom 0 (-128) 127)),
        (1, Gen.integral (Range.linearFrom 0 (-512) 511)) ]

-- | Body items: plain instructions, software interrupts and their enables,
-- and bounded loops (x17 counts down; a backward BNE closes the loop).
genItem :: Int -> Int -> Gen [Word32]
genItem i n = Gen.frequency
  [ (40, genInstruction i n),
    (3, P.pure [sw 12 11 0]), -- raise msip
    (2, P.pure [csrInstruction 0x300 6 0 8]), -- csrrsi mstatus, MIE
    (2, P.pure [csrInstruction 0x304 6 0 8]), -- csrrsi mie, MSIE
    (3, do
        trips <- Gen.integral (Range.linear 1 4)
        m <- Gen.integral (Range.linear 1 6)
        inner <- P.concat <$> P.mapM (\k -> genInstruction (i + 1 + k) n) [0 .. m - 1]
        let loopBody = P.take m inner
            back = 0 - 4 * fromIntegral (P.length loopBody + 1)
        P.pure ([addi 17 0 trips] P.++ loopBody P.++ [addi 17 17 (0 - 1), bne 17 0 back])) ]

-- | Prologue, body, ECALL, and the trap handler:
--   x11 = CLINT msip, x12 = 1, mtvec = handler
--   handler: an interrupt clears msip, counts in x10 and returns (MRET);
--   an exception saves mepc/mcause/mtval in x14-x16, clears mtvec and ends
--   with ECALL into the ROM.
genProgram :: Gen [Word32]
genProgram = do
  n <- Gen.integral (Range.linear 1 64)
  let go i acc
        | i >= n = P.pure (P.reverse acc)
        | otherwise = do
            ws <- genItem i n
            go (i + P.length ws) (P.reverse ws P.++ acc)
  body <- P.take n <$> go 0 []
  enabled <- Gen.bool
  let enables = if enabled then [csrInstruction 0x300 6 0 8, csrInstruction 0x304 6 0 8] else []
      prologue = 5 + P.length enables
      handlerAt = prologue + P.length body + 1
      setup = [lui 11 0x02000, addi 12 0 1, (13 `shiftL` 7) .|. 0x17,
               addi 13 13 (fromIntegral (4 * (handlerAt - 2))), csrInstruction 0x305 1 0 13] P.++ enables
      handler = [csrInstruction 0x342 2 13 0, bType 16 0 13 5 0, sw 0 11 0, addi 10 10 1, 0x3020_0073,
                 csrInstruction 0x341 2 14 0, csrInstruction 0x342 2 15 0, csrInstruction 0x343 2 16 0,
                 csrInstruction 0x305 1 0 0, 0x0000_0073]
  P.pure (setup P.++ body P.++ [0x0000_0073] P.++ handler)

-- | Random straight-line and forward-branching RV32IM+Zicsr programs run on
-- the core and on the reference model reach the same state: registers,
-- trap CSRs, scratch, mie, every RAM word either touched, and the
-- instructions retired.
prop_core_matches_reference_model :: Property
prop_core_matches_reference_model = withTests 300 . property $ do
  program <- forAll genProgram
  values <- forAll (Gen.list (Range.singleton 8) genValue)
  dataWords <- forAll (Gen.list (Range.singleton 256) (Gen.integral Range.linearBounded))
  let regs = P.zip [1 .. 8] values P.++ [(31, dataPointer)]
      dataStart = fromIntegral ((dataBase - ramBase) `shiftR` 2) :: Int
      memory = IntMap.fromList (P.zip [0 ..] program P.++ P.zip [dataStart ..] dataWords)
      model = Model.runModel 10000 (Model.initialModel [(fromIntegral r, fromIntegral v) | (r, v) <- regs] (IntMap.map fromIntegral memory) (P.map fromIntegral (toList bootRomContents)))
      modelMemory = IntMap.map fromIntegral (Model.mMem model) :: IntMap.IntMap Word32
      modelReg r = fromIntegral (IntMap.findWithDefault 0 r (Model.mRegs model)) :: Word32
      w :: (Integral a) => a -> Word32
      w = fromIntegral
      (_, halted) = runUntilHalt 200000 (startSim (runningMachine [(fromIntegral r, v) | (r, v) <- regs]) (Ram 0 memory))
      -- the last register write and retirement land a cycle or two after
      -- the pc enters the ROM; five cycles cannot commit a ROM instruction
      drained = runInputs halted (P.replicate 5 Nothing)
      core = simMachine drained
      trapRegs = cpuTrapRegisters core
      coreMemory = ramWords (simRam halted)
      touched = IntMap.keys (IntMap.union coreMemory modelMemory)
  annotateShow (Model.mPc model, Model.mCause model, Model.mRetired model)
  -- the handler saves the first exception's mcause in x15
  let firstCause = IntMap.findWithDefault 0 15 (Model.mRegs model)
  classify "ends in ECALL" (firstCause == 11)
  classify "ends in an illegal instruction" (firstCause == 2)
  classify "ends in a misaligned access" (firstCause `P.elem` [4, 6])
  classify "ends in an access fault" (firstCause `P.elem` [5, 7])
  classify "ends in a misaligned jump" (firstCause == 0 && Model.mPc model == 0)
  classify "jumps into the ROM" (Model.mPc model /= 0)
  classify "retires 30 or more" (Model.mRetired model >= 30)
  classify "takes a software interrupt" (IntMap.findWithDefault 0 10 (Model.mRegs model) > 0)
  classify "runs a loop" (P.any (\word -> word .&. 0x7f == 0x63 && testBit word 31) program)
  -- a branch landing on a jalr can skip its auipc and loop: not a test
  when (Model.mPc model >= Model.ramBase) discard
  assert (romAddress (cpuPc core))
  [cpuRegs core !! (r :: Index 32) | r <- [1 .. 31]] === [modelReg r | r <- [1 .. 31 :: Int]]
  (trapEpc trapRegs, trapCause trapRegs, trapValue trapRegs) === (w (Model.mEpc model), w (Model.mCause model), w (Model.mTval model))
  (trapMie trapRegs, trapMpie trapRegs) === (Model.mMie model, Model.mMpie model)
  (cpuScratch core, cpuInterruptEnable core) === (w (Model.mScratch model), w (Model.mIe model))
  devMsip (cpuDevices core) === Model.mMsip model
  [IntMap.findWithDefault 0 k coreMemory | k <- touched] === [IntMap.findWithDefault 0 k modelMemory | k <- touched]
  counterValue (cpuInstret (simMachine drained)) === fromIntegral (Model.mRetired model)

-- Interpret the pipelined carry in the independent test reference only.
counterValue :: Counter64 -> Unsigned 64
counterValue Counter64 {..} =
  (resize (counterHi + if counterCarry then 1 else 0) `shiftL` 32) .|. resize counterLo

prop_split_counter_matches_64_bit_reference :: Property
prop_split_counter_matches_64_bit_reference = property $ do
  highWord <- forAll (Gen.integral Range.constantBounded)
  distance <- forAll (Gen.integral (Range.linear 0 5))
  operations <- forAll (Gen.list (Range.linear 20 50) $ do
    inc <- Gen.bool
    write <- Gen.choice [P.pure Nothing, Just <$> ((,) <$> Gen.bool <*> Gen.integral Range.constantBounded)]
    P.pure (inc, write))
  let initial = Counter64 (maxBound - distance) highWord False
      reference value (inc, write) = case write of
        Just (False, new) -> (value .&. 0xffffffff00000000) .|. resize new
        Just (True, new) -> (resize new `shiftL` 32) .|. (value .&. 0xffffffff)
        Nothing -> if inc then value + 1 else value
      checkSteps _ _ [] = success
      checkSteps counter value (op@(inc, write):rest) = do
        let counter' = tickCounter inc write counter
            value' = reference value op
        counterValue counter' === value'
        checkSteps counter' value' rest
  checkSteps initial (counterValue initial) operations
  counterValue (tickCounter False Nothing (tickCounter True Nothing (Counter64 maxBound maxBound False))) === 0

prop_counter_csr_reads_wait_for_carry :: Property
prop_counter_csr_reads_wait_for_carry = property $ do
  highWord <- forAll (Gen.integral Range.constantBounded)
  P.mapM_ (\address -> do
    let isCycle = address == 0xb80 || address == 0xc80
        m = initialMachine {cpuPhase = CsrRead,
              cpuInstruction = csrInstruction address 2 1 0,
              cpuCsrSelection = decodeCsrSelection (fromIntegral address),
              cpuCycle = if isCycle then Counter64 0 highWord True else initialCounter,
              cpuInstret = if isCycle then initialCounter else Counter64 0 highWord True}
        (waited, _) = machineStep m (0, Nothing, True)
        (readState, _) = machineStep waited (0, Nothing, True)
    cpuPhase waited === CsrRead
    cpuPhase readState === CsrWrite
    cpuCsrValue readState === highWord + 1
    ) [0xb80, 0xc80, 0xb82, 0xc82]

prop_counters_retire_precisely :: Property
prop_counters_retire_precisely = withTests 1 $ property $ do
  let program = [addi 10 0 7, sw 2 1 0, 0x0000a183, 0x02210233,
                 csrInstruction 0x340 2 5 0, addi 0 0 0, 0x00000073]
      (cycles, stopped) = runUntilHalt 1000
        (startSim (runningMachine [(1, ramBase + 0x100), (2, 5)]) (ramFromList program))
      drained = runInputs stopped (P.replicate 5 Nothing)
  counterValue (cpuInstret (simMachine drained)) === 6
  counterValue (cpuCycle (simMachine drained)) === fromIntegral (cycles + 5)
  let waiting = initialMachine {cpuPhase = BusWait, cpuInstruction = 0x0020a023,
          systemBus = initialBus {busPhase = BusTx, busRequest = BusRequest uartTxData 0 True 0x21}}
      stalled = P.iterate (\m -> P.fst (machineStep m (0, Nothing, False))) waiting P.!! 20
      (offered, (_, _, byte)) = machineStep stalled (0, Nothing, True)
      completed = P.fst (machineStep offered (0, Nothing, False))
      counted = P.fst (machineStep completed (0, Nothing, False))
  counterValue (cpuInstret stalled) === 0
  byte === Just 0x21
  counterValue (cpuInstret counted) === 1

prop_counter_write_overrides_retirement :: Property
prop_counter_write_overrides_retirement = withTests 1 $ property $ do
  let program = [addi 1 0 7, csrInstruction 0xb02 1 0 1,
                 csrInstruction 0xc02 2 3 0, csrInstruction 0xc02 2 4 0, 0x00000073]
      (_, stopped) = runUntilHalt 300 (startSim (runningMachine []) (ramFromList program))
      machine = simMachine stopped
  cpuRegs machine !! (3 :: Index 32) === 7
  cpuRegs machine !! (4 :: Index 32) === 8
  counterValue (cpuInstret machine) === 9
  -- Explicit writes override a simultaneous tick; a low write preserves a
  -- preceding overflow while a high write replaces the carried high word.
  tickCounter True (Just (False, 17)) (Counter64 0 9 True) === Counter64 17 10 False
  tickCounter True (Just (True, 17)) (Counter64 0 9 True) === Counter64 0 17 False
  P.mapM_ (\address -> do
    let m = initialMachine {cpuPhase = CsrRead,
              cpuInstruction = csrInstruction address 1 0 0,
              cpuCsrSelection = decodeCsrSelection (fromIntegral address), cpuCsrReadOnly = True}
        (fault, _, _) = cpuStep m 0 Nothing True
    cpuPhase fault === Trap
    cpuExceptionCause fault === 2
    ) [0xc00, 0xc80, 0xc02, 0xc82]

prop_registered_memory_decode :: Property
prop_registered_memory_decode = property $ do
  offset <- forAll (Gen.integral (Range.linear 0 65535))
  romOffset <- forAll (Gen.integral (Range.linear 0 2047))
  P.mapM_ (\(address, target) -> do
    let q = BusRequest address 2 False 0
        (accepted, earlyResponse, earlyOutput, earlyEffect) = busStep initialBus (Just q) 0 True Nothing 0 initialDevices
        (decoded, response, output, effect) = busStep accepted Nothing 0 True Nothing 0 initialDevices
    busPhase accepted === BusDecode
    busRequest accepted === q
    earlyResponse === Nothing
    earlyOutput === (0, Nothing, Nothing)
    earlyEffect === NoEffect
    busPhase decoded === BusDispatch
    busTarget decoded === target
    response === Nothing
    output === (0, Nothing, Nothing)
    effect === NoEffect
    ) [(ramBase + offset, RamTarget), (romOffset, RomTarget),
       (uartTxData, UartTxTarget), (uartStatus, UartStatusTarget), (uartRxData, UartRxTarget),
       (exitDevice, ExitTarget), (bootCauseDevice, BootCauseTarget),
       (exitDevice + 8, UnmappedTarget), (0x800, UnmappedTarget), (0x10000, UnmappedTarget),
       (0x7fff_fffc, UnmappedTarget), (ramBase + 0x10000, UnmappedTarget)]
  -- Physical indices: bit 14 is the ROM.
  memoryIndex ramBase === 0
  memoryIndex (ramBase + 0xfffc) === 0x3fff
  memoryIndex 0 === 0x4000
  memoryIndex 0x7fc === 0x41ff

prop_finisher_commands :: Property
prop_finisher_commands = property $ do
  code <- forAll (Gen.integral Range.constantBounded :: Gen (Unsigned 16))
  P.mapM_ (\(width, value, stops) -> do
    let base = initialMachine {cpuPhase = Commit, bootCause = causeNone,
          cpuPc = 4, cpuInstruction = if width == 2 then 0x0020a023 else 0x00209023,
          cpuB = value, cpuComputed = Computed Nothing 8 0 Nothing exitDevice,
          cpuTrapRegisters = initialTrapRegisters {trapVector = 0x100, trapValue = 42}}
        (queued, _, command) = commitInstruction base True
        (completed, response) = settleBus queued
        (counted, _) = machineStep completed (0, Nothing, True)
    cpuPhase queued === BusWait
    cpuBusRequest queued === Just (BusRequest exitDevice (if width == 2 then 2 else 1) True value)
    command === (0, Nothing, Nothing)
    response === (0, Nothing, Nothing)
    -- An exit resets into the boot ROM with cause EXIT; anything else is an
    -- ordinary store that retires.
    cpuPc completed === if stops then 0 else 8
    bootCause completed === if stops then causeExit else causeNone
    cpuTrapRegisters completed === if stops then initialTrapRegisters else cpuTrapRegisters base
    counterValue (cpuInstret counted) === 1
    ) [(2 :: Int, (resize code `shiftL` 16) .|. 0x3333, True),
       (2, 0x5555, True), (1, 0x5555, True), (1, 0x3333, True),
       (2, 0x7777, False), (2, 0, False)]
  -- Supported reads return zero; byte and neighboring-address accesses fault.
  P.mapM_ (\(instruction, address, expectedCause) -> do
    let m = initialMachine {cpuPhase = Commit, cpuInstruction = instruction,
          cpuComputed = Computed Nothing 4 0 Nothing address}
        (result, write, output) = commitSettled m True
    output === (0, Nothing, Nothing)
    case expectedCause of
      Nothing -> do
        write === Just (1, 0)
        cpuPhase result === Fetch
      Just cause -> do
        write === Nothing
        cpuPhase result === Trap
        cpuExceptionCause result === cause
        cpuExceptionValue result === address
    ) [(0x00002083, exitDevice, Nothing), (0x00001083, exitDevice, Nothing),
       (0x00005083, exitDevice, Nothing), (0x00000083, exitDevice, Just 5),
       (0x00004083, exitDevice, Just 5), (0x00200023, exitDevice, Just 7),
       (0x00202023, exitDevice + 2, Just 6),
       (0x00202023, exitDevice + 4, Just 7)]
  -- BOOTCAUSE reads the cause and clears it; it is read-only.
  P.mapM_ (\cause -> do
    let m = initialMachine {cpuPhase = Commit, bootCause = cause, cpuInstruction = 0x00002083,
          cpuComputed = Computed Nothing 4 0 Nothing bootCauseDevice}
        (result, write, _) = commitSettled m True
    write === Just (1, resize cause)
    bootCause result === causeNone
    ) [causeNone, causePowerOn, causeBreak, causeExit]

prop_bus_completion_and_rx_race :: Property
prop_bus_completion_and_rx_race = property $ do
  byte <- forAll (Gen.integral (Range.linear 128 255))
  address <- (\index -> ramBase + index * 4) <$> forAll (Gen.integral (Range.linear 0 16383))
  value <- forAll (Gen.integral Range.constantBounded)
  P.mapM_ (\(width, expected) -> do
    let before = initialBus {busPhase = BusDispatch, busTarget = UartRxTarget,
          busRequest = BusRequest uartRxData width False 0}
        (after, response, output, consume) = busStep before Nothing 0 True (Just byte) 0 initialDevices
        (_, repeated, repeatedOutput, repeatedConsume) = busStep after Nothing 0 True (Just byte) 0 initialDevices
    response === Just (BusResponse expected False False)
    output === (0, Nothing, Nothing)
    consume === ConsumeRx
    repeated === Nothing
    repeatedOutput === (0, Nothing, Nothing)
    repeatedConsume === NoEffect
    ) [(0, resize byte .|. 0xffffff00), (4, resize byte), (1, resize byte), (2, resize byte)]
  let writing = initialBus {busPhase = BusRamWrite,
        busRequest = BusRequest address 2 True value, busValue = value}
      (idle, ack, write, consume) = busStep writing Nothing 0 True Nothing 0 initialDevices
      (_, repeatedAck, repeatedWrite, _) = busStep idle Nothing 0 True Nothing 0 initialDevices
  ack === Just (BusResponse 0 False False)
  write === (0, Just (memoryIndex address, value), Nothing)
  consume === NoEffect
  repeatedAck === Nothing
  repeatedWrite === (0, Nothing, Nothing)
  -- A byte arriving while an empty RX read is completed belongs to the next
  -- read; it must not be cleared by an unconditional consume pulse.
  let reading = initialMachine {cpuPhase = BusWait,
        cpuInstruction = 0x0000a083,
        systemBus = initialBus {busPhase = BusDispatch, busTarget = UartRxTarget,
          busRequest = BusRequest uartRxData 2 False 0}}
      (arrived, _) = machineStep reading (0, Just byte, True)
  cpuBusResponse arrived === Just (BusResponse 0 False False)
  rxHolding arrived === Just byte

prop_bus_wait_faults_and_cancel :: Property
prop_bus_wait_faults_and_cancel = property $ do
  value <- forAll (Gen.integral Range.constantBounded)
  P.mapM_ (\write -> do
    let before = initialMachine {cpuPhase = BusWait,
          cpuPc = 4, cpuInstruction = if write then 0x0020a023 else 0x0000a083,
          cpuComputed = Computed Nothing 8 0 Nothing 0x20000000}
        (waiting, pendingWrite, output) = cpuStep before 0 Nothing True
        (fault, faultWrite, faultOutput) = cpuStep
          before {cpuBusResponse = Just (BusResponse value True False)} 0 Nothing True
        (completed, registerWrite, _) = cpuStep
          before {cpuBusResponse = Just (BusResponse value False False)} 0 Nothing True
    cpuPhase waiting === BusWait
    cpuPc waiting === 4
    cpuCountRetirement waiting === False
    pendingWrite === Nothing
    output === (0, Nothing, Nothing)
    cpuPhase fault === Trap
    cpuPc fault === 4
    cpuExceptionCause fault === if write then 7 else 5
    cpuExceptionValue fault === 0x20000000
    cpuCountRetirement fault === False
    faultWrite === Nothing
    faultOutput === (0, Nothing, Nothing)
    cpuPhase completed === Fetch
    cpuPc completed === 8
    cpuCountRetirement completed === True
    registerWrite === if write then Nothing else Just (1, value)
    ) [False, True]
  P.mapM_ (\phase -> do
    let before = initialMachine {cpuPhase = BusWait, cpuPc = 0x8000_0040, cpuInRom = False,
          cpuBusRequest = Just (BusRequest uartTxData 0 True 0x41),
          systemBus = initialBus {busPhase = phase, busTarget = UartTxTarget,
            busRequest = BusRequest 0x100 2 True value, busValue = value}}
        (cancelled, output) = machineStep before (0, Just 3, True)
    cpuPc cancelled === 0
    bootCause cancelled === causeBreak
    cpuBusRequest cancelled === Nothing
    cpuBusResponse cancelled === Nothing
    busPhase (systemBus cancelled) === BusIdle
    output === (0, Nothing, Nothing)
    ) [BusDecode, BusDispatch, BusLoadIssue, BusLoadWait, BusStoreIssue,
       BusStoreMerge, BusRamWrite, BusTx, BusReply]

prop_sdram_waits_preserve_full_address :: Property
prop_sdram_waits_preserve_full_address = property $ do
  offset <- forAll (Gen.integral (Range.linear 0 0x1fffff))
  value <- forAll (Gen.integral Range.constantBounded)
  let address = 0x80000000 + (resize offset `shiftL` 2)
      bus = initialBus {busPhase = BusRamWrite, busRequest = BusRequest address 2 True value, busValue = value}
      m = initialMachine {cpuPhase = BusWait, systemBus = bus}
      input ready done = (0, ready, done, Nothing, True, False, 0)
      (queued, _) = S.step S.initialState {S.machine = m} (input False False)
      (held, (req, wr, actualAddress, actualValue, _, _)) = S.step queued (input False False)
      (accepted, _) = S.step held (input True False)
      (waiting, (secondReq, _, _, _, _, _)) = S.step accepted (input False False)
      (completed, _) = S.step waiting (input False True)
  req === True
  wr === True
  actualAddress === offset
  actualValue === value
  secondReq === False
  S.phase completed === S.Normal
  cpuPc (S.machine waiting) === cpuPc (S.machine queued)
  counterLo (cpuCycle (S.machine waiting)) === counterLo (cpuCycle (S.machine queued)) + 3

-- All 8 MiB of SDRAM is RAM at 0x8000_0000; address 0 is the boot ROM, read
-- from block RAM without an SDRAM request.
prop_sdram_map_and_boot_rom_reads :: Property
prop_sdram_map_and_boot_rom_reads = withTests 1 . property $ do
  P.mapM_ (\address -> S.decodeTarget address === RamTarget)
    [0x80000000, 0x8000ffff, 0x80010000, 0x807fffff]
  P.mapM_ (\address -> S.decodeTarget address === RomTarget) [0, 0x7fc]
  P.mapM_ (\address -> S.decodeTarget address === UnmappedTarget)
    [0x800, 0xffff, 0x10000, 0x7fffffff, 0x80800000, 0xffffffff]
  S.decodeTarget exitDevice === ExitTarget
  S.decodeTarget bootCauseDevice === BootCauseTarget
  -- Power-on fetch of the reset vector.
  let input romWord = (0, False, False, Nothing, True, False, romWord)
      (issued, (firstReq, _, _, _, _, _)) = S.step S.initialState (input 0)
      (addressed, (secondReq, _, _, _, _, romAddressOut)) = S.step issued (input 0)
      (captured, _) = S.step addressed (input 0x1234_5678)
  firstReq === False
  secondReq === False
  S.phase issued === S.RomRead
  S.romAddress issued === 0
  romAddressOut === 0
  S.phase addressed === S.RomCapture
  S.phase captured === S.Normal
  S.word captured === 0x1234_5678

prop_sdram_reset_drains_pending_memory :: Property
prop_sdram_reset_drains_pending_memory = withTests 1 . property $ do
  let s=S.initialState {S.machine=initialMachine {cpuPc = 0x8000_0100, cpuInRom = False, bootCause = causeNone}, S.phase=S.AwaitDone}
      (cancelled, _) = S.step s (0,False,False,Just 3,True,True,0)
      (drained, _) = S.step cancelled (123,False,True,Nothing,True,False,0)
  cpuPc (S.machine cancelled) === 0
  bootCause (S.machine cancelled) === causeBreak
  S.phase cancelled === S.AwaitDone
  S.phase drained === S.Normal
  cpuBusRequest (S.machine drained) === Nothing
  cpuPendingWrite (S.machine drained) === Nothing

simpleRiscGroup :: Group
simpleRiscGroup =
  Group
    "SimpleRisc"
    [ ("Bus completes once and preserves concurrent RX", prop_bus_completion_and_rx_race),
      ("Bus waits, precise faults and cancellation", prop_bus_wait_faults_and_cancel),
      ("Registered memory target selection", prop_registered_memory_decode),
      ("Finisher exits reset into the ROM; BOOTCAUSE reads and clears", prop_finisher_commands),
      ("Split counters match a 64-bit reference", prop_split_counter_matches_64_bit_reference),
      ("Counter CSR reads wait for carry", prop_counter_csr_reads_wait_for_carry),
      ("Precise retirement and UART stalls", prop_counters_retire_precisely),
      ("Counter writes override implicit increments", prop_counter_write_overrides_retirement),
      ("All six CSR read/modify/write forms", prop_csr_read_modify_write),
      ("CSR access rules and WARL masks", prop_csr_access_rules_and_masks),
      ("Guest trap handler resumes with mret", prop_guest_handler_resumes_with_mret),
      ("MRET restores machine status", prop_mret_restores_status),
      ("mtip compares all 64 bits", prop_mtip_compares_64_bits),
      ("Timer interrupt wakes WFI", prop_timer_interrupt_wakes_wfi),
      ("UART interrupt claims through the PLIC", prop_uart_interrupt_claims_through_plic),
      ("Reset quietens interrupt sources", prop_reset_quietens_devices),
      ("SPI exchanges bytes in mode 0", prop_spi_exchanges_bytes_in_mode_0),
      ("FIFO is a bounded queue", prop_fifo_is_a_bounded_queue),
      ("A program reaches the I/O complex", prop_program_reaches_io_complex),
      ("SPI DMA moves blocks", prop_spi_dma_moves_blocks),
      ("Core matches the reference model on random programs", prop_core_matches_reference_model),
      ("MRET takes a pending interrupt at once", prop_mret_takes_pending_interrupt_at_once),
      ("Boot ROM quietens interrupt sources before R", prop_boot_rom_quietens_devices_before_run),
      ("Precise staged exception entry", prop_precise_exception_entry),
      ("Unhandled traps return to the boot ROM; Ctrl-C resets", prop_unhandled_trap_returns_to_boot_rom),
      ("Faulting store never writes RAM", prop_faulting_store_never_writes_ram),
      ("Every register bank and x0 survives decode/writeback", prop_register_banks_and_writeback),
      ("RV32I arithmetic result reaches BRAM", prop_arithmetic_instruction_stores_expected_result),
      ("Boot ROM loads programs with a guard word", prop_boot_rom_loads_programs),
      ("Boot ROM clears RAM", prop_boot_rom_clears_memory),
      ("RV32M matches the reference semantics", prop_muldiv_matches_reference),
      ("Test encoders match known instructions", prop_encoders_match_known_instructions),
      ("Backward branch in final word loops", prop_backward_branch_in_final_word_loops),
      ("SDRAM holds full word addresses across waits", prop_sdram_waits_preserve_full_address),
      ("SDRAM map and boot ROM reads", prop_sdram_map_and_boot_rom_reads),
      ("SDRAM reset drains pending memory", prop_sdram_reset_drains_pending_memory),
      ("Registered CSR selection matches reference", prop_registered_csr_selection_matches_reference),
      ("Registered execution matches compute", prop_registered_execution_matches_compute),
      ("Split comparisons match signed/unsigned references", prop_split_comparisons_match_reference),
      ("Simple instructions take nine cycles each", prop_simple_instructions_take_nine_cycles_each),
      ("Two-stage shifts cover every distance", prop_shift_stages_all_distances),
      ("Store into next instruction is fetched fresh", prop_store_into_next_instruction_is_fetched_fresh),
      ("UART TX store waits for ready", prop_uart_tx_store_waits_for_ready),
      ("Circuit programs and runs over UART", prop_circuit_programs_and_runs_over_uart),
      ("UART TX timing and byte latch", prop_uart_tx_is_cycle_exact_and_latches_byte),
      ("UART RX 8-N-1 sampling", prop_uart_rx_samples_115200_8n1),
      ("UART RX holding register", prop_uart_rx_holding_register_is_one_byte_deep)
    ]
