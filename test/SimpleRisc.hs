{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
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
stepSim Sim {..} received =
  let (machine', (readAddress, writeCommand, txByte)) =
        machineStep simMachine (simRamOutput, received, True)
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
      finished = runInputs (startSim machine ram) (P.replicate 8 Nothing)
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
  where
    wordBytes word =
      [ resize word,
        resize (word `shiftR` 8),
        resize (word `shiftR` 16),
        resize (word `shiftR` 24)
      ]

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
clocksPerBit = 434

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
      ("UART TX timing and byte latch", prop_uart_tx_is_cycle_exact_and_latches_byte),
      ("UART RX 8-N-1 sampling", prop_uart_rx_samples_115200_8n1),
      ("UART RX holding register", prop_uart_rx_holding_register_is_one_byte_deep)
    ]
