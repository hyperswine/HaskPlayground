{-# LANGUAGE OverloadedStrings #-}

module FPRunner where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)
import FPDevices (defaultVFS)
import FPInterpreter
import FParser

-- ─────────────────────────────────────────────────────────────────────────────
-- AST EXAMPLES  (unchanged from before)
-- ─────────────────────────────────────────────────────────────────────────────

example1 :: Expr
example1 =
  Seq
    [ Let "x" (Lit (VTagged "Num" [VInt 5])) $
        Let "t" (TypeOf (Var "x")) $
          Seq
            [ App (Var "println") [Var "t"],
              Let "isNum" (App (Var "typeEq") [Var "t", TypeOf (Lit (VInt 42))]) $
                App (Var "println") [Var "isNum"],
              App (Var "typeEq") [TypeOf (Lit (VInt 42)), TypeOf (Lit (VInt 99))]
            ]
    ]

example2 :: Expr
example2 =
  Seq
    [ Let "ref" (Alloc (Lit (VTagged "Num" [VInt 42]))) $
        Seq
          [ App (Var "println") [GetRef (Var "ref")],
            Dealloc (Var "ref"),
            App (Var "println") [Lit (VStr "deallocated")]
          ]
    ]

example3 :: Expr
example3 =
  Let "workerAddr" (Spawn "worker" (Lam [] workerBody) []) $
    Seq
      [ Send
          (Lit VUnit)
          (Lit (VStr "worker"))
          (Tag "Task" [Lit (VInt 21), Lit (VStr "main")]),
        Receive
          [ ( PTagged "Result" [PVar "r"],
              App
                (Var "println")
                [ App
                    (Var "strConcat")
                    [Lit (VStr "main received: "), App (Var "intToStr") [Var "r"]]
                ]
            )
          ]
      ]
  where
    workerBody =
      Receive
        [ ( PTagged "Task" [PVar "n", PVar "replyTo"],
            Let "doubled" (App (Var "intMul") [Var "n", Lit (VInt 2)]) $
              Seq
                [ App
                    (Var "println")
                    [ App
                        (Var "strConcat")
                        [ Lit (VStr "worker doubling "),
                          App (Var "intToStr") [Var "n"]
                        ]
                    ],
                  Send (Lit VUnit) (Var "replyTo") (Tag "Result" [Var "doubled"])
                ]
          )
        ]

example4 :: Expr
example4 =
  Let "f" (Lam ["x"] (App (Var "intAdd") [Var "x", Lit (VInt 1)])) $
    Seq
      [ App (Var "println") [FnOf (Var "f")],
        App (Var "println") [FnOf (Var "println")]
      ]

example5 :: Expr
example5 =
  Let
    "fact"
    ( Fix
        "fact"
        ["n"]
        ( If
            (App (Var "intEq") [Var "n", Lit (VInt 0)])
            (Lit (VInt 1))
            ( App
                (Var "intMul")
                [ Var "n",
                  App (Var "fact") [App (Var "intSub") [Var "n", Lit (VInt 1)]]
                ]
            )
        )
    )
    $ Seq
      [ App
          (Var "println")
          [ App
              (Var "strConcat")
              [Lit (VStr "fact 6 = "), App (Var "intToStr") [App (Var "fact") [Lit (VInt 6)]]]
          ],
        App
          (Var "println")
          [ App
              (Var "strConcat")
              [Lit (VStr "fact 0 = "), App (Var "intToStr") [App (Var "fact") [Lit (VInt 0)]]]
          ]
      ]

example6 :: Expr
example6 =
  Let
    "unwrapMyList"
    ( Fix
        "unwrapMyList"
        ["v"]
        ( If
            (App (Var "typeEq") [TypeOf (Var "v"), Lit (VType "MyList")])
            (App (Var "tagPayload") [Var "v"])
            (App (Var "showVal") [Var "v"])
        )
    )
    $ Let
      "_"
      ( IsoDecl
          "MyList"
          "List"
          (Lam ["v"] (App (Var "unwrapMyList") [Var "v"]))
          (Lam ["xs"] (Tag "MyList" [Var "xs"]))
      )
    $ Let "isoResult" (LookupIso "MyList" "List")
    $ Let "myVal" (Tag "MyList" [Lit (VList [VInt 1, VInt 2, VInt 3])])
    $ Seq
      [ App
          (Var "println")
          [ App
              (Var "strConcat")
              [Lit (VStr "original:   "), App (Var "showVal") [Var "myVal"]]
          ],
        App (Var "withIso") [Var "isoResult", Var "myVal"],
        Let "missing" (LookupIso "Foo" "Bar") $
          App
            (Var "println")
            [ App
                (Var "strConcat")
                [Lit (VStr "iso Foo Bar: "), App (Var "showVal") [Var "missing"]]
            ]
      ]

-- ─────────────────────────────────────────────────────────────────────────────
-- PARSER TEST CASES
-- Each entry is (description, source, expected-to-parse-cleanly)
-- ─────────────────────────────────────────────────────────────────────────────

data ParseTest = ParseTest
  { testName :: String,
    testSrc :: String
  }

parserTests :: [ParseTest]
parserTests =
  [ ParseTest
      "integer literal"
      "42",
    ParseTest
      "string literal"
      "\"hello world\"",
    ParseTest
      "bool literals"
      "{ true; false }",
    ParseTest
      "variable"
      "x",
    ParseTest
      "application"
      "intAdd(x, y)",
    ParseTest
      "nested application"
      "intMul(intAdd(a, b), c)",
    ParseTest
      "lambda"
      "fn(x, y) => intAdd(x, y)",
    ParseTest
      "fix factorial"
      "fix fact(n) => if intEq(n, 0) then 1 else intMul(n, fact(intSub(n, 1)))",
    ParseTest
      "let without in"
      "let x = 42",
    ParseTest
      "let with in"
      "let x = 10 in intAdd(x, 5)",
    ParseTest
      "let chain"
      "let x = 1\nlet y = 2\nintAdd(x, y)",
    ParseTest
      "if then else"
      "if intEq(x, 0) then \"zero\" else \"nonzero\"",
    ParseTest
      "block sequence"
      "{ let x = 1; let y = 2; intAdd(x, y) }",
    ParseTest
      "Tag nullary"
      "Tag Unit",
    ParseTest
      "Tag with args"
      "Tag Pair(1, 2)",
    ParseTest
      "type()"
      "type(someValue)",
    ParseTest
      "function()"
      "function(println)",
    ParseTest
      "alloc/getref/dealloc"
      "{ let r = alloc(42); let v = getref(r); dealloc(r) }",
    ParseTest
      "send"
      "send \"worker\" Tag Task(21, \"main\")",
    ParseTest
      "receive"
      "receive { Result(r) => println(r), Nothing => println(\"nope\") }",
    ParseTest
      "spawn"
      "spawn \"worker\" fn() => receive { Task(n, r) => send r Tag Done }",
    ParseTest
      "self"
      "self",
    ParseTest
      "iso declaration"
      "iso MyList List fn(v) => tagPayload(v) fn(xs) => Tag MyList(xs)",
    ParseTest
      "iso lookup"
      "iso?(MyList, List)",
    ParseTest
      "pattern wildcard"
      "receive { Result(r) => r, _ => () }",
    ParseTest "full program" $
      unlines
        [ "let fact = fix fact(n) =>",
          "  if intEq(n, 0) then 1",
          "  else intMul(n, fact(intSub(n, 1)))",
          "println(intToStr(fact(6)))"
        ]
  ]

runParserTests :: IO ()
runParserTests = do
  putStrLn "── Parser tests ──"
  let results = map runTest parserTests
      passed = length (filter fst results)
      total = length results
  mapM_ printResult (zip parserTests results)
  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " passed"
  where
    runTest t = case parseFile (testName t) (testSrc t) of
      Right _ -> (True, "")
      Left e -> (False, e)

    printResult (t, (ok, err))
      | ok = putStrLn $ "  [OK]   " ++ testName t
      | otherwise = do
          putStrLn $ "  [FAIL] " ++ testName t
          putStrLn $ "         " ++ firstLine err

    firstLine = head . lines

-- ─────────────────────────────────────────────────────────────────────────────
-- PARSE-AND-RUN DEMO
-- Show that parsed source produces the same output as hand-written AST
-- ─────────────────────────────────────────────────────────────────────────────

factorialSrc :: String
factorialSrc =
  unlines
    [ "let fact = fix fact(n) =>",
      "  if intEq(n, 0) then 1",
      "  else intMul(n, fact(intSub(n, 1)))",
      "println(strConcat(\"fact 10 = \", intToStr(fact(10))))"
    ]

isoSrc :: String
isoSrc =
  unlines
    [ "-- Define MyList <-> List isomorphism",
      "let unwrap = fix unwrap(v) =>",
      "  if typeEq(type(v), type(Tag MyList)) then tagPayload(v)",
      "  else v",
      "iso MyList List fn(v) => unwrap(v) fn(xs) => Tag MyList(xs)",
      "let result = iso?(MyList, List)",
      "println(showVal(result))"
    ]

actorSrc :: String
actorSrc =
  unlines
    [ "-- Spawn an echo actor and talk to it",
      "let echo = spawn \"echo\" fn() =>",
      "  receive {",
      "    Msg(txt, replyTo) => send replyTo Tag Reply(txt)",
      "  }",
      "send \"echo\" Tag Msg(\"hello\", \"main\")",
      "receive {",
      "  Reply(txt) => println(strConcat(\"echo replied: \", txt))",
      "}"
    ]

runParseAndRun :: IO ()
runParseAndRun = do
  putStrLn "\n── Parse-and-run: factorial ──"
  runSrc "factorial" factorialSrc

  putStrLn "\n── Parse-and-run: iso ──"
  runSrc "iso" isoSrc

  putStrLn "\n── Parse-and-run: actor echo ──"
  runSrc "actor" actorSrc
  threadDelay 100000

runSrc :: String -> String -> IO ()
runSrc name src =
  case parseFile name src of
    Left err -> putStrLn $ "Parse error:\n" ++ err
    Right ast -> do
      _ <- runProgram primEnv emptyVFSMap ast
      return ()

-- ─────────────────────────────────────────────────────────────────────────────
-- TEST MAIN (original self-contained demo)
-- ─────────────────────────────────────────────────────────────────────────────

testMain :: IO ()
testMain = do
  -- Original AST-level examples
  putStrLn "── Example 1: type/1 reflection ──"
  _ <- runProgram primEnv emptyVFSMap example1

  putStrLn "\n── Example 2: RC allocator ──"
  _ <- runProgram primEnv emptyVFSMap example2

  putStrLn "\n── Example 3: actor send/receive ──"
  _ <- runProgram primEnv emptyVFSMap example3
  threadDelay 100000

  putStrLn "\n── Example 4: function/1 reflection ──"
  _ <- runProgram primEnv emptyVFSMap example4

  putStrLn "\n── Example 5: fix combinator ──"
  _ <- runProgram primEnv emptyVFSMap example5

  putStrLn "\n── Example 6: iso registry ──"
  _ <- runProgram primEnv emptyVFSMap example6

  -- Parser test suite
  putStrLn ""
  runParserTests

  -- Parse real source and run it
  runParseAndRun

-- ─────────────────────────────────────────────────────────────────────────────
-- MAIN  (reads a .fplang file and runs it)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Run a .fplang source file against the default VFS
runSrcWithVFS :: String -> String -> IO ()
runSrcWithVFS name src =
  case parseFile name src of
    Left err -> putStrLn $ "Parse error:\n" ++ err
    Right ast -> do
      _ <- runProgram primEnv defaultVFS ast
      return ()

-- | Run several VFS example files in sequence
vfsMain :: [String] -> IO ()
vfsMain paths = mapM_ runOne paths
  where
    runOne path = do
      putStrLn $ "\n── " ++ path ++ " ──"
      src <- readFile path
      runSrcWithVFS path src

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  args <- getArgs
  case args of
    ("--vfs" : paths) -> vfsMain paths
    [path] -> do
      src <- readFile path
      case parseFile path src of
        Left err -> putStrLn ("Parse error:\n" ++ err) >> exitFailure
        Right ast -> do
          _ <- runProgram primEnv defaultVFS ast
          return ()
    _ -> do
      putStrLn "Usage: fplang [--vfs file1.fplang ...] <file.fplang>"
      putStrLn "Running built-in tests instead..."
      testMain
