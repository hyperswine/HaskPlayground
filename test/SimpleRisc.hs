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
import qualified Data.List as List
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import "haskplayground" SimpleRisc
import qualified Prelude as P

type Ram = Vec 1024 Word32

-- This mirrors blockRamPow2: the read address selected in one cycle produces
-- the input word for the following machineStep, and writes happen on the edge.
data Sim = Sim
  { simMachine :: Machine,
    simRam :: Ram,
    simRamOutput :: Word32,
    simTransmitted :: [Byte]
  }

stepSim :: Sim -> Maybe Byte -> Sim
stepSim = stepSimReady True

stepSimReady :: Bool -> Sim -> Maybe Byte -> Sim
stepSimReady txReady Sim {..} received =
  let (machine', (readAddress, writeCommand, txByte)) =
        machineStep simMachine (simRamOutput, received, txReady)
      nextRamOutput = simRam !! readAddress
      ram' = case writeCommand of
        Nothing -> simRam
        Just (writeAddress, value) -> replace writeAddress value simRam
      transmitted' = case txByte of
        Nothing -> simTransmitted
        Just byte -> simTransmitted P.++ [byte]
   in Sim machine' ram' nextRamOutput transmitted'

runInputs :: Sim -> [Maybe Byte] -> Sim
runInputs = P.foldl stepSim

startSim :: Machine -> Ram -> Sim
startSim machine ram = Sim machine ram 0 []

ramFromList :: [Word32] -> Ram
ramFromList = go 0 (repeat 0)
  where
    go :: Index 1024 -> Ram -> [Word32] -> Ram
    go _ ram [] = ram
    go address ram (word : rest) = go (address + 1) (replace address word ram) rest

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
      finished = runInputs (startSim machine ram) (P.replicate 20 Nothing)
      expected = arithmeticResult operation a b

  cpuRegs (simMachine finished) !! (3 :: Index 32) === expected
  simRam finished !! (64 :: Index 1024) === expected
  assert (not (cpuRunning (simMachine finished)))
  simTransmitted finished === [0x44, 0x4f, 0x4e, 0x45]

prop_program_loads_up_to_fifty_words :: Property
prop_program_loads_up_to_fifty_words = property $ do
  wordsToProgram <- forAll (Gen.list (Range.linear 1 50) (Gen.integral Range.constantBounded))
  let count = P.length wordsToProgram
      countLo = fromIntegral count :: Byte
      countHi = fromIntegral (count `shiftR` 8) :: Byte
      frame = Just 0x50 : Just countLo : Just countHi : P.map Just (P.concatMap wordBytes wordsToProgram)
      programmed = runInputs (startSim initialMachine (repeat 0)) frame
      actual = P.map (\i -> simRam programmed !! (fromIntegral i :: Index 1024)) [0 .. count - 1]

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

prop_simple_instructions_take_four_cycles_each :: Property
prop_simple_instructions_take_four_cycles_each = property $ do
  n <- forAll (Gen.int (Range.linear 1 50))
  let ram = ramFromList (P.replicate n (addi 1 1 1))
      (cycles, halted) = runUntilHalt 1000 (startSim (runningMachine [] (fromIntegral (4 * n))) ram)

  -- One initial Fetch cycle; FetchWait, Decode, Execute and Commit per
  -- instruction; then FetchWait and Decode notice the PC has left the program.
  cycles === 4 * n + 3
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

prop_reset_memory_takes_exactly_1024_clear_cycles :: Property
prop_reset_memory_takes_exactly_1024_clear_cycles = withTests 1 . property $ do
  let dirtyRam = repeat 0xdead_beef
      commandAccepted = stepSim (startSim initialMachine dirtyRam) (Just 0x4d)
      beforeLastWrite = runInputs commandAccepted (P.replicate 1023 Nothing)
      finished = stepSim beforeLastWrite Nothing

  hostState (simMachine commandAccepted) === ClearMemory 0
  hostState (simMachine beforeLastWrite) === ClearMemory 1023
  simRam beforeLastWrite !! (1023 :: Index 1024) === 0xdead_beef
  hostState (simMachine finished) === HostIdle
  simRam finished === repeat 0

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

simpleRiscGroup :: Group
simpleRiscGroup =
  Group
    "SimpleRisc"
    [ ("RV32I arithmetic result reaches BRAM", prop_arithmetic_instruction_stores_expected_result),
      ("PROGRAM accepts up to 50 words", prop_program_loads_up_to_fifty_words),
      ("RESET-MEM is exactly 1024 clear cycles", prop_reset_memory_takes_exactly_1024_clear_cycles),
      ("Test encoders match known instructions", prop_encoders_match_known_instructions),
      ("Backward branch in final word loops", prop_backward_branch_in_final_word_loops),
      ("Simple instructions take four cycles each", prop_simple_instructions_take_four_cycles_each),
      ("Store into next instruction is fetched fresh", prop_store_into_next_instruction_is_fetched_fresh),
      ("UART TX store waits for ready", prop_uart_tx_store_waits_for_ready),
      ("Circuit programs and runs over UART", prop_circuit_programs_and_runs_over_uart),
      ("UART TX timing and byte latch", prop_uart_tx_is_cycle_exact_and_latches_byte),
      ("UART RX 8-N-1 sampling", prop_uart_rx_samples_115200_8n1),
      ("UART RX holding register", prop_uart_rx_holding_register_is_one_byte_deep)
    ]
