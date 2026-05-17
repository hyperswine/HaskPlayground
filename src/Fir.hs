-- F-IR: Functional IR Interpreter
-- Registers r0..r15, flat instruction set, port-based I/O
-- Ports are resolved by the host via a PortTable
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Fir where

import Control.Monad (when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- ─────────────────────────────────────────────
-- Values
-- ─────────────────────────────────────────────

data Val
  = VInt Int
  | VBool Bool
  | VUnit
  | VBytes String -- simple string stand-in for byte data
  | VPack Int [Val] -- tag + fields (for PACK/UNPACK)
  | VErr String
  deriving (Eq)

instance Show Val where
  show (VInt n) = show n
  show (VBool b) = if b then "true" else "false"
  show VUnit = "()"
  show (VBytes s) = show s
  show (VPack t fs) = "pack(" ++ show t ++ ", " ++ show fs ++ ")"
  show (VErr e) = "ERR:" ++ e

-- ─────────────────────────────────────────────
-- Instructions
-- ─────────────────────────────────────────────

type Reg = Int -- 0..15

type PortId = Int

type Offset = Int -- relative jump, in instructions

data Instr
  = LOAD Reg Val -- dst <- immediate
  | MOVE Reg Reg -- dst <- src
  | ADD Reg Reg Reg
  | SUB Reg Reg Reg
  | AND Reg Reg Reg
  | OR Reg Reg Reg
  | SHR Reg Reg Int
  | SHL Reg Reg Int
  | ISEQUAL Reg Reg Reg -- dst <- a == b
  | LT Reg Reg Reg -- dst <- a <  b
  | NOT Reg Reg
  | JUMP Offset
  | JUMPIF Reg Offset
  | CALL String -- named function (label)
  | RETURN
  | READ Reg PortId -- dst  <- port
  | WRITE PortId Reg -- port <- src
  | WAIT Reg -- yield src ms  (simulated: just log)
  | YIELD -- safe swap point
  | PACK Reg Int [Reg] -- dst <- pack(tag, regs...)
  | UNPACK Reg Reg Int -- dst <- src.field[i]
  | HALT
  deriving (Show)

-- ─────────────────────────────────────────────
-- Port table - host provides read/write handlers
-- ─────────────────────────────────────────────

data PortHandler = PortHandler
  { portRead :: IO Val,
    portWrite :: Val -> IO (),
    portName :: String
  }

type PortTable = Map PortId PortHandler

-- ─────────────────────────────────────────────
-- Module: named functions (label -> instruction list)
-- ─────────────────────────────────────────────

type Label = String

type Module = Map Label [Instr]

-- ─────────────────────────────────────────────
-- Interpreter state
-- ─────────────────────────────────────────────

data Frame = Frame
  { frameRegs :: IORef (Map Reg Val),
    frameInstrs :: [Instr],
    framePC :: IORef Int,
    frameLabel :: Label
  }

data InterpState = InterpState
  { stateModule :: Module,
    statePorts :: PortTable,
    stateLog :: IORef [String],
    stateStack :: IORef [Frame] -- call stack
  }

-- ─────────────────────────────────────────────
-- Helpers
-- ─────────────────────────────────────────────

getReg :: Frame -> Reg -> IO Val
getReg f r = do
  m <- readIORef (frameRegs f)
  case Map.lookup r m of
    Just v -> return v
    Nothing -> return (VInt 0) -- default uninitialized = 0

setReg :: Frame -> Reg -> Val -> IO ()
setReg f r v = modifyIORef' (frameRegs f) (Map.insert r v)

logMsg :: InterpState -> String -> IO ()
logMsg st msg = do
  modifyIORef' (stateLog st) (++ [msg])
  putStrLn $ "  [interp] " ++ msg

asInt :: Val -> Int
asInt (VInt n) = n
asInt (VBool b) = if b then 1 else 0
asInt _ = 0

asBool :: Val -> Bool
asBool (VInt n) = n /= 0
asBool (VBool b) = b
asBool _ = False

-- ─────────────────────────────────────────────
-- Execute a single instruction
-- Returns False if HALT or RETURN with empty stack
-- ─────────────────────────────────────────────

step :: InterpState -> Frame -> IO Bool
step st frame = do
  pc <- readIORef (framePC frame)
  let instrs = frameInstrs frame
  if pc >= length instrs
    then do
      logMsg st $ "pc overrun in '" ++ frameLabel frame ++ "'"
      return False
    else do
      let instr = instrs !! pc
      modifyIORef' (framePC frame) (+ 1)
      execInstr st frame instr

execInstr :: InterpState -> Frame -> Instr -> IO Bool
execInstr _ _ HALT = return False
execInstr _ _ RETURN = return False -- caller handles stack pop
execInstr st frame (LOAD dst val) = do
  setReg frame dst val
  return True
execInstr st frame (MOVE dst src) = do
  v <- getReg frame src
  setReg frame dst v
  return True
execInstr st frame (ADD dst a b) = do
  va <- getReg frame a
  vb <- getReg frame b
  setReg frame dst (VInt (asInt va + asInt vb))
  return True
execInstr st frame (SUB dst a b) = do
  va <- getReg frame a
  vb <- getReg frame b
  setReg frame dst (VInt (asInt va - asInt vb))
  return True
execInstr st frame (AND dst a b) = do
  va <- getReg frame a
  vb <- getReg frame b
  setReg frame dst (VInt (asInt va .&. asInt vb))
  return True
execInstr st frame (OR dst a b) = do
  va <- getReg frame a
  vb <- getReg frame b
  setReg frame dst (VInt (asInt va .|. asInt vb))
  return True
execInstr st frame (SHR dst a n) = do
  va <- getReg frame a
  setReg frame dst (VInt (asInt va `shiftR` n))
  return True
execInstr st frame (SHL dst a n) = do
  va <- getReg frame a
  setReg frame dst (VInt (asInt va `shiftL` n))
  return True
execInstr st frame (ISEQUAL dst a b) = do
  va <- getReg frame a
  vb <- getReg frame b
  setReg frame dst (VBool (va == vb))
  return True
execInstr st frame (Fir.LT dst a b) = do
  va <- getReg frame a
  vb <- getReg frame b
  setReg frame dst (VBool (asInt va < asInt vb))
  return True
execInstr st frame (NOT dst src) = do
  v <- getReg frame src
  setReg frame dst (VBool (not (asBool v)))
  return True
execInstr st frame (JUMP offset) = do
  -- offset is relative to *current* pc (already incremented past this instr)
  modifyIORef' (framePC frame) (+ (offset - 1))
  return True
execInstr st frame (JUMPIF cond offset) = do
  v <- getReg frame cond
  when (asBool v) $
    modifyIORef' (framePC frame) (+ (offset - 1))
  return True
execInstr st frame (CALL lbl) = do
  case Map.lookup lbl (stateModule st) of
    Nothing -> do
      logMsg st $ "unknown function: " ++ lbl
      return False
    Just body -> do
      regs <- newIORef Map.empty
      newPC <- newIORef 0
      let callee = Frame regs body newPC lbl
      -- run the callee to completion
      runFrame st callee
      return True
execInstr st frame (READ dst portId) = do
  case Map.lookup portId (statePorts st) of
    Nothing -> do
      logMsg st $ "READ unknown port " ++ show portId
      setReg frame dst (VErr ("no-port-" ++ show portId))
    Just ph -> do
      logMsg st $ "READ  port." ++ portName ph ++ " (#" ++ show portId ++ ")"
      v <- portRead ph
      logMsg st $ "  -> " ++ show v
      setReg frame dst v
  return True
execInstr st frame (WRITE portId src) = do
  v <- getReg frame src
  case Map.lookup portId (statePorts st) of
    Nothing ->
      logMsg st $ "WRITE unknown port " ++ show portId ++ " val=" ++ show v
    Just ph -> do
      logMsg st $ "WRITE port." ++ portName ph ++ " (#" ++ show portId ++ ") <- " ++ show v
      portWrite ph v
  return True
execInstr st frame (WAIT reg) = do
  v <- getReg frame reg
  logMsg st $ "WAIT " ++ show (asInt v) ++ "ms  (simulated yield)"
  return True
execInstr _ _ YIELD = return True
execInstr st frame (PACK dst tag srcs) = do
  vals <- mapM (getReg frame) srcs
  setReg frame dst (VPack tag vals)
  return True
execInstr st frame (UNPACK dst src i) = do
  v <- getReg frame src
  case v of
    VPack _ fields ->
      if i < length fields
        then setReg frame dst (fields !! i)
        else setReg frame dst (VErr "unpack-oob")
    _ -> setReg frame dst (VErr "unpack-non-pack")
  return True

-- ─────────────────────────────────────────────
-- Run a frame until HALT/RETURN/overrun
-- ─────────────────────────────────────────────

runFrame :: InterpState -> Frame -> IO ()
runFrame st frame = do
  continue <- step st frame
  when continue $ runFrame st frame

-- ─────────────────────────────────────────────
-- Run a named function as entry point
-- ─────────────────────────────────────────────

runFunction :: InterpState -> Label -> IO ()
runFunction st lbl =
  case Map.lookup lbl (stateModule st) of
    Nothing -> putStrLn $ "ERROR: entry point '" ++ lbl ++ "' not found"
    Just body -> do
      regs <- newIORef Map.empty
      pc <- newIORef 0
      let frame = Frame regs body pc lbl
      runFrame st frame

-- ─────────────────────────────────────────────
-- Host port implementations
-- ─────────────────────────────────────────────

-- Port backed by a file on disk (simulates "10.port" etc.)
filePort :: PortId -> FilePath -> IO PortHandler
filePort pid path = do
  return $
    PortHandler
      { portName = path,
        portRead = do
          content <- readFile path
          let trimmed = reverse . dropWhile (== '\n') . reverse $ content
          return (VBytes trimmed),
        portWrite = \val ->
          appendFile path (show val ++ "\n")
      }

-- Port backed by an IORef (simulates GPIO / in-memory register)
refPort :: PortId -> String -> Val -> IO PortHandler
refPort pid name initial = do
  ref <- newIORef initial
  return $
    PortHandler
      { portName = name,
        portRead = readIORef ref,
        portWrite = \v -> writeIORef ref v
      }

-- Counter port: each read increments an internal counter
counterPort :: PortId -> String -> IO PortHandler
counterPort pid name = do
  ref <- newIORef (0 :: Int)
  return $
    PortHandler
      { portName = name,
        portRead = do
          n <- readIORef ref
          modifyIORef' ref (+ 1)
          return (VInt n),
        portWrite = \_ -> return ()
      }

-- Stdout port: writes go to console
stdoutPort :: PortId -> IO PortHandler
stdoutPort pid =
  return $
    PortHandler
      { portName = "stdout",
        portRead = return VUnit,
        portWrite = \v -> putStrLn $ "  [port.stdout] " ++ show v
      }

-- ─────────────────────────────────────────────
-- Test programs
-- ─────────────────────────────────────────────

-- Program 1: Read from file port 10, write to stdout port 0
prog_readFile :: [Instr]
prog_readFile =
  [ READ 1 10, -- r1 <- port 10 (file)
    WRITE 0 1, -- port 0 (stdout) <- r1
    HALT
  ]

-- Program 2: Read a number from a ref port (simulated sensor),
--            compare against threshold, write alert to stdout
prog_threshold :: [Instr]
prog_threshold =
  [ LOAD 0 (VInt 75), -- r0 = threshold
    READ 1 20, -- r1 <- port 20 (sensor value)
    Fir.LT 2 0 1, -- r2 = threshold < reading
    JUMPIF 2 2, -- if over threshold, skip past JUMP
    JUMP 3, -- jump to "ok" (past alert)
    -- alert branch
    LOAD 3 (VBytes "ALERT: threshold exceeded"),
    WRITE 0 3, -- write to stdout
    JUMP 2,
    -- ok branch
    LOAD 3 (VBytes "OK: within threshold"),
    WRITE 0 3,
    HALT
  ]

-- Program 3: Counter loop - read from counter port 5 times, sum, write result
prog_counter :: [Instr]
prog_counter =
  [ LOAD 0 (VInt 0), -- r0 = accumulator
    LOAD 1 (VInt 5), -- r1 = iterations remaining
    -- loop top
    ISEQUAL 2 1 (error "unused"), -- placeholder, use LT instead
    HALT -- replaced below
  ]

-- cleaner counter loop:
prog_counter2 :: [Instr]
prog_counter2 =
  -- r0 = accumulator, r1 = loop counter (counts down)
  [ LOAD 0 (VInt 0), -- r0 <- 0
    LOAD 1 (VInt 5), -- r1 <- 5
    LOAD 4 (VInt 0), -- r4 <- 0  (zero constant)
    LOAD 5 (VInt 1), -- r5 <- 1  (one constant)
    -- loop:  pc=4
    ISEQUAL 2 1 4, -- r2 <- (r1 == 0)
    JUMPIF 2 5, -- if done, jump to end (+5 from next pc)
    READ 3 30, -- r3 <- port 30 (counter port)
    ADD 0 0 3, -- r0 <- r0 + r3
    SUB 1 1 5, -- r1 <- r1 - 1
    JUMP (-5), -- back to loop top
    -- end:  pc=10
    WRITE 0 0, -- write sum to stdout
    HALT
  ]

-- Program 4: PACK/UNPACK - build a command, send to port, read ack
prog_pack :: [Instr]
prog_pack =
  [ LOAD 0 (VInt 42), -- r0 = payload value
    PACK 1 7 [0], -- r1 = pack(tag=7, r0)  -> cmd struct
    WRITE 40 1, -- send to port 40 (command port)
    READ 2 40, -- read ack from same port
    UNPACK 3 2 0, -- r3 = ack.field[0]
    WRITE 0 3, -- print to stdout
    HALT
  ]

-- Program 5: Read file, write modified content back, read again to verify
prog_readWriteVerify :: [Instr]
prog_readWriteVerify =
  [ READ 0 10, -- r0 <- original file content
    WRITE 0 0, -- print original
    LOAD 1 (VBytes "updated by F-IR program"),
    WRITE 10 1, -- write new content to file port
    READ 2 10, -- r2 <- read back
    WRITE 0 2, -- print readback
    HALT
  ]

-- ─────────────────────────────────────────────
-- Test runner
-- ─────────────────────────────────────────────

runTest :: String -> [Instr] -> PortTable -> IO ()
runTest name prog ports = do
  putStrLn $ "\n======================================"
  putStrLn $ "TEST: " ++ name
  putStrLn "======================================"
  logRef <- newIORef []
  stackRef <- newIORef []
  let mdl = Map.singleton "main" prog
  let st = InterpState mdl ports logRef stackRef
  runFunction st "main"
  putStrLn $ "-- done --"

-- ─────────────────────────────────────────────
-- Command port: echoes back a tagged ack
-- ─────────────────────────────────────────────

cmdPort :: PortId -> String -> IO PortHandler
cmdPort pid name = do
  lastCmd <- newIORef VUnit
  return $
    PortHandler
      { portName = name,
        portRead = do
          v <- readIORef lastCmd
          case v of
            VPack tag fields ->
              return (VPack (tag + 100) [VInt (length fields), VBool True])
            _ -> return (VPack 0 [VInt 0, VBool False]),
        portWrite = \v -> writeIORef lastCmd v
      }

-- ─────────────────────────────────────────────
-- Main
-- ─────────────────────────────────────────────

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- set up port files
  writeFile "10.port" "hello from the filesystem\n"

  -- build ports
  p0 <- stdoutPort 0
  p10 <- filePort 10 "10.port"
  p20 <- refPort 20 "sensor.temp" (VInt 82) -- over threshold (>75)
  p21 <- refPort 21 "sensor.temp.low" (VInt 60) -- under threshold
  p30 <- counterPort 30 "counter.tick"
  p40 <- cmdPort 40 "cmd.actuator"

  let basePorts =
        Map.fromList
          [ (0, p0),
            (10, p10),
            (30, p30),
            (40, p40)
          ]

  -- test 1: read file port -> stdout
  runTest
    "Read from file port 10 -> stdout port 0"
    prog_readFile
    basePorts

  -- test 2a: threshold check (sensor over threshold)
  p20' <- refPort 20 "sensor.temp.high" (VInt 82)
  runTest
    "Threshold check - sensor HIGH (82 > 75)"
    prog_threshold
    (Map.insert 20 p20' basePorts)

  -- test 2b: threshold check (sensor under threshold)
  p20l <- refPort 20 "sensor.temp.low" (VInt 60)
  runTest
    "Threshold check - sensor LOW (60 < 75)"
    prog_threshold
    (Map.insert 20 p20l basePorts)

  -- test 3: counter loop
  p30' <- counterPort 30 "counter.tick"
  runTest
    "Counter loop - sum 5 reads from counter port 30"
    prog_counter2
    (Map.insert 30 p30' basePorts)

  -- test 4: pack/unpack command
  runTest
    "PACK command -> port 40 -> READ ack -> UNPACK -> stdout"
    prog_pack
    basePorts

  -- test 5: read/write/verify on file port
  writeFile "10.port" "original file content"
  p10' <- filePort 10 "10.port"
  runTest
    "Read -> Write -> Verify on file port 10"
    prog_readWriteVerify
    (Map.insert 10 p10' basePorts)

  putStrLn "\n== all tests complete ==\n"
