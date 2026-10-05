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

-- | The 64 KiB memory as a sparse map: copying a 16384-entry vector on every
-- write would make the simulations far too slow.  Unwritten words read as
-- 'ramDefault'.
data Ram = Ram {ramDefault :: Word32, ramWords :: IntMap.IntMap Word32}

blankRam :: Word32 -> Ram
blankRam fill = Ram fill IntMap.empty

ramRead :: Ram -> MemAddr -> Word32
ramRead (Ram fill words) address = IntMap.findWithDefault fill (fromIntegral address) words

ramWrite :: MemAddr -> Word32 -> Ram -> Ram
ramWrite address value (Ram fill words) = Ram fill (IntMap.insert (fromIntegral address) value words)

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
      nextRamOutput = ramRead simRam simReadCommand
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

-- | Step with no UART input until the CPU stops, returning the cycles taken.
runUntilHalt :: Int -> Sim -> (Int, Sim)
runUntilHalt fuel = go 0
  where
    go cycles sim
      | not (cpuRunning (simMachine sim)) || cycles >= fuel = (cycles, sim)
      | otherwise = go (cycles + 1) (stepSim sim Nothing)

runningMachine :: [(Index 32, Word32)] -> Word32 -> Machine
runningMachine registers end =
  initialMachine
    { cpuRegs = P.foldr (P.uncurry replace) (repeat 0) registers,
      cpuRunning = True,
      cpuPhase = Fetch,
      cpuPc = 0,
      programEnd = end
    }

doneBytes :: [Byte]
doneBytes = [0x44, 0x4f, 0x4e, 0x45]

wordBytes :: Word32 -> [Byte]
wordBytes word =
  [ resize word,
    resize (word `shiftR` 8),
    resize (word `shiftR` 16),
    resize (word `shiftR` 24)
  ]

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

  let registers = replace (2 :: Index 32) b (replace (1 :: Index 32) a (repeat 0))
      machine =
        initialMachine
          { cpuRegs = registers,
            cpuRunning = True,
            cpuPhase = Fetch,
            cpuPc = 0,
            programEnd = 8
          }
      -- op x3,x1,x2; sw x3,256(x0)
      ram = ramFromList [encodeArithmetic operation, 0x1030_2023]
      finished = runInputs (startSim machine ram) (P.replicate 30 Nothing)
      expected = arithmeticResult operation a b

  cpuRegs (simMachine finished) !! (3 :: Index 32) === expected
  ramRead (simRam finished) 64 === expected
  assert (not (cpuRunning (simMachine finished)))
  simTransmitted finished === [0x44, 0x4f, 0x4e, 0x45]

prop_program_loads_up_to_fifty_words :: Property
prop_program_loads_up_to_fifty_words = property $ do
  wordsToProgram <- forAll (Gen.list (Range.linear 1 50) (Gen.integral Range.constantBounded))
  let count = P.length wordsToProgram
      countLo = fromIntegral count :: Byte
      countHi = fromIntegral (count `shiftR` 8) :: Byte
      frame = Just 0x50 : Just countLo : Just countHi : P.map Just (P.concatMap wordBytes wordsToProgram)
      programmed = runInputs (startSim initialMachine (blankRam 0)) (frame P.++ [Nothing])
      actual = P.map (ramRead (simRam programmed) . fromIntegral) [0 .. count - 1]

  actual === wordsToProgram
  programEnd (simMachine programmed) === fromIntegral (count * 4)
  hostState (simMachine programmed) === HostIdle

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
      (_, halted) = runUntilHalt 1000 (startSim (runningMachine [] 12) ram)
      finished = runInputs halted (P.replicate 4 Nothing)

  assert (not (cpuRunning (simMachine halted)))
  cpuRegs (simMachine finished) !! (1 :: Index 32) === 0
  cpuPc (simMachine finished) === 12
  simTransmitted finished === doneBytes

-- Exercise every bank boundary and x0 through real decode/writeback stages.
prop_register_banks_and_writeback :: Property
prop_register_banks_and_writeback = property $ do
  salt <- forAll (Gen.integral Range.constantBounded)
  let value i = salt + fromIntegral i * 0x1020304
      registers = [(fromIntegral i, value i) | i <- [0..31 :: Int]]
      program = P.concat [[addi r r 1, sw r 0 (256 + 4*r)] | r <- [0..31]]
      (_, halted) = runUntilHalt 2000 (startSim (runningMachine registers 256) (ramFromList program))
  assert (not (cpuRunning (simMachine halted)))
  P.mapM_ (\i -> ramRead (simRam halted) (fromIntegral (64+i)) === if i == 0 then 0 else value i + 1) [0..31 :: Int]

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
          machine = runningMachine [(1, a), (2, n)] 4
          (cycles, halted) = runUntilHalt 50 (startSim machine (ramFromList [instruction]))
      cpuRegs (simMachine halted) !! (3 :: Index 32) === arithmeticResult op a n
      assert (not (cpuRunning (simMachine halted)))
      cycles === 9 + 5

prop_simple_instructions_take_eight_cycles_each :: Property
prop_simple_instructions_take_eight_cycles_each = property $ do
  n <- forAll (Gen.int (Range.linear 1 50))
  let ram = ramFromList (P.replicate n (addi 1 1 1))
      (cycles, halted) = runUntilHalt 1000 (startSim (runningMachine [] (fromIntegral (4 * n))) ram)

  -- Registered RAM commands add FetchDelay, including the final end check.
  cycles === 8 * n + 5
  cpuRegs (simMachine halted) !! (1 :: Index 32) === fromIntegral n

prop_store_into_next_instruction_is_fetched_fresh :: Property
prop_store_into_next_instruction_is_fetched_fresh = withTests 1 . property $ do
  -- addi x1,x0,1; sw x5,8(x0) where x5 holds "addi x6,x0,42"; <word 2 overwritten>
  let ram = ramFromList [addi 1 0 1, sw 5 0 8, 0]
      (_, halted) = runUntilHalt 1000 (startSim (runningMachine [(5, addi 6 0 42)] 12) ram)

  cpuRegs (simMachine halted) !! (6 :: Index 32) === 42
  cpuPc (simMachine halted) === 12

prop_uart_tx_store_waits_for_ready :: Property
prop_uart_tx_store_waits_for_ready = property $ do
  bytes <- forAll (Gen.list (Range.singleton 3) (Gen.integral Range.constantBounded))
  readiness <- forAll (Gen.list (Range.linear 0 60) Gen.bool)
  let registers = (2, uartTxData) : P.zip [1, 3, 4] (P.map resize bytes)
      -- sw x1,0(x2); sw x3,0(x2); sw x4,0(x2)
      ram = ramFromList [sw 1 2 0, sw 3 2 0, sw 4 2 0]
      schedule = readiness P.++ P.replicate 40 True
      finished = P.foldl (\sim ready -> stepSimReady ready sim Nothing) (startSim (runningMachine registers 12) ram) schedule

  simTransmitted finished === bytes P.++ doneBytes
  assert (not (cpuRunning (simMachine finished)))

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
      hostBytes = [0x50, fromIntegral (P.length program), 0] P.++ P.concatMap wordBytes program P.++ [0x52]
      idle k = P.replicate k high
      waveform =
        idle 20
          P.++ P.concatMap (\byte -> uartFrame byte P.++ idle 20) hostBytes
          P.++ idle (8 * 10 * clocksPerBit)
      output = simulateN @System (P.length waveform) simpleRisc waveform
      received = [value | Just value <- rxTrace output]

  received === [0x48, 0x69] P.++ doneBytes

prop_reset_memory_takes_exactly_16384_clear_cycles :: Property
prop_reset_memory_takes_exactly_16384_clear_cycles = withTests 1 . property $ do
  let dirtyRam = blankRam 0xdead_beef
      commandAccepted = stepSim (startSim initialMachine dirtyRam) (Just 0x4d)
      beforeLastWrite = runInputs commandAccepted (P.replicate 16383 Nothing)
      finished = stepSim beforeLastWrite Nothing
      drained = stepSim finished Nothing

  hostState (simMachine commandAccepted) === ClearMemory 0
  hostState (simMachine beforeLastWrite) === ClearMemory 16383
  ramRead (simRam beforeLastWrite) 16383 === 0xdead_beef
  hostState (simMachine finished) === HostIdle
  ramRead (simRam finished) 16383 === 0xdead_beef
  assert (P.all (\address -> ramRead (simRam drained) address == 0) [minBound .. maxBound])

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
  -- op x3,x1,x2; sw x3,256(x0)
  let ram = ramFromList [encodeMulDiv operation, sw 3 0 256]
      (_, halted) = runUntilHalt 200 (startSim (runningMachine [(1, a), (2, b)] 8) ram)
      expected = mulDivResult operation a b

  assert (not (cpuRunning (simMachine halted)))
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
  let running = initialMachine {cpuRunning = True, programEnd = 64, cpuPhase = Fetch}
      (withFirst, _) = runningStep running 0 (Just first) True
      -- Keep this latch-focused property from advancing into an instruction
      -- decode between the two simulated receive cycles.
      (afterSecond, _) = runningStep withFirst {cpuPhase = Fetch} 0 (Just second) True
      (status, afterStatus) = readUart uartStatus True afterSecond
      (value, consumed) = readUart uartRxData True afterStatus
      (emptyStatus, _) = readUart uartStatus True consumed

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
        { cpuRunning = True, cpuPhase = Commit, cpuPc = faultPc,
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
        (queued, write, output) = commitInstruction m True
        (entered, trapWrite, trapOutput) = cpuStep queued 0 Nothing True
        regs = cpuTrapRegisters entered
    cpuPhase queued === Trap
    cpuPc queued === faultPc
    cpuTrapRegisters queued === cpuTrapRegisters base
    write === Nothing
    output === (0, Nothing, Nothing)
    cpuPhase entered === Fetch
    cpuPc entered === 0x100
    cpuRunning entered === True
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

prop_trap_compatibility_and_reset :: Property
prop_trap_compatibility_and_reset = withTests 1 $ property $ do
  let m = (runningMachine [] 4) {cpuInstruction = 0x00000073, cpuPhase = Commit}
      (queued, _, _) = commitInstruction m True
      (stopped, _, _) = cpuStep queued 0 Nothing True
      resumed = runInputs (startSim stopped (blankRam 0)) (P.replicate 4 Nothing)
      reset = resetCpu stopped
  cpuRunning stopped === False
  trapCause (cpuTrapRegisters stopped) === 11
  simTransmitted resumed === doneBytes
  cpuTrapRegisters reset === initialTrapRegisters
  let (notTaken, write, _) = commitInstruction
        m {cpuInstruction = 0x00000063, cpuComputed = Computed Nothing 4 6 (Just False) 0} True
  cpuPhase notTaken === Fetch
  cpuPc notTaken === 4
  write === Nothing

prop_faulting_store_never_writes_ram :: Property
prop_faulting_store_never_writes_ram = withTests 1 $ property $ do
  let sim = startSim (runningMachine [(1, 0x100), (2, 0xdeadbeef)] 8)
            (ramFromList [sw 2 1 1, 0x00000073])
      (_, stopped) = runUntilHalt 100 sim
      drained = runInputs stopped (P.replicate 5 Nothing)
      regs = cpuTrapRegisters (simMachine drained)
  trapCause regs === 6
  trapValue regs === 0x101
  trapEpc regs === 0
  ramRead (simRam drained) 64 === 0
  simTransmitted drained === doneBytes

simpleRiscGroup :: Group
simpleRiscGroup =
  Group
    "SimpleRisc"
    [ ("Precise staged exception entry", prop_precise_exception_entry),
      ("Trap DONE compatibility and reset", prop_trap_compatibility_and_reset),
      ("Faulting store never writes RAM", prop_faulting_store_never_writes_ram),
      ("Every register bank and x0 survives decode/writeback", prop_register_banks_and_writeback),
      ("RV32I arithmetic result reaches BRAM", prop_arithmetic_instruction_stores_expected_result),
      ("PROGRAM accepts up to 50 words", prop_program_loads_up_to_fifty_words),
      ("RESET-MEM is exactly 16384 clear cycles", prop_reset_memory_takes_exactly_16384_clear_cycles),
      ("RV32M matches the reference semantics", prop_muldiv_matches_reference),
      ("Test encoders match known instructions", prop_encoders_match_known_instructions),
      ("Backward branch in final word loops", prop_backward_branch_in_final_word_loops),
      ("Simple instructions take eight cycles each", prop_simple_instructions_take_eight_cycles_each),
      ("Two-stage shifts cover every distance", prop_shift_stages_all_distances),
      ("Store into next instruction is fetched fresh", prop_store_into_next_instruction_is_fetched_fresh),
      ("UART TX store waits for ready", prop_uart_tx_store_waits_for_ready),
      ("Circuit programs and runs over UART", prop_circuit_programs_and_runs_over_uart),
      ("UART TX timing and byte latch", prop_uart_tx_is_cycle_exact_and_latches_byte),
      ("UART RX 8-N-1 sampling", prop_uart_rx_samples_115200_8n1),
      ("UART RX holding register", prop_uart_rx_holding_register_is_one_byte_deep)
    ]
