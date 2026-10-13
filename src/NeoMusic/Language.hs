-- | Line-oriented .neomusic authoring, with acyclic, first-order definitions.
--
-- Values are music (scores) and numbers (exact rationals). Definitions take
-- music parameters and, marked @#@ in the signature, number parameters. A
-- prelude of ordinary definitions ("NeoMusic.Prelude") is read before every
-- document; a document's own definitions shadow it. See neomusic-prelude.md.
module NeoMusic.Language
  ( Document(..), SheetView(..), Staff(..), parseDocument, prettyScore, pitchNumber ) where

import Control.Monad (foldM, unless, void, when)
import Data.Char (isSpace)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as M
import Data.Ratio ((%), numerator, denominator)
import qualified NeoMusic.Audio as A
import qualified NeoMusic.Pitch as A
import NeoMusic.Prelude (preludeSource)
import NeoMusic.Score
import Text.Parsec hiding (Line, label, token)
import Text.Parsec.String (Parser)

data Staff = Treble | Bass deriving (Eq, Ord, Show)
-- | 'numbers' prints each note's Neo step under it (chords stacked), so the
-- staff view carries the numeric notation alongside the traditional one.
data SheetView = SheetView { meter :: Int, key :: String, staves :: M.Map Name Staff, numbers :: Bool }
  deriving (Eq, Show)
data Document = Document { performance :: Performance, sheetView :: SheetView, score :: Score }
  deriving (Eq, Show)

-- | A signal expression, kept exact until it is evaluated. A number is a
-- constant signal.
data NumE = NLit Rational | NVar Name | NBin Char NumE NumE | NNeg NumE
  | NPoints [(NumE, NumE)] | NOsc Wave NumE | NCents NumE | NArrow NumE NumE
  deriving (Eq, Show)
data Param = MusicParam Name | NumberParam Name deriving (Eq, Show)
data Arg = MusicArg Expr | NumberArg NumE deriving (Eq, Show)
-- | A tempo shape as written: a signal (a constant, or a -> b), or a cycle of
-- segments (written beats, shape).
data ShapeE = SignalShape NumE | CycleShape [(NumE, ShapeE)] deriving (Eq, Show)
data Expr = Notes [Int] | NoteE NumE | Ref Name [Arg] | SeqE [Expr] | ParE Expr Expr
  | Scale NumE Expr | Trans NumE Expr | Repeat NumE Expr | Rev Expr | Inv Expr
  | Gate NumE Expr | Pedal Expr | TempoE ShapeE Expr
  | PitchE NumE Expr | LevelE NumE Expr | GlideE NumE Expr
  deriving (Eq, Show)
-- | Parameters, body, and whether it came from the prelude (whose bodies only
-- see prelude definitions, so a document can't change what they mean).
data Definition = Definition [Param] Expr Bool deriving (Eq, Show)
data Value = MusicV Score | SignalV Signal

paramName :: Param -> Name
paramName (MusicParam p) = p
paramName (NumberParam p) = p

isNumberParam :: Param -> Bool
isNumberParam (NumberParam _) = True
isNumberParam _ = False

white :: Parser ()
white = skipMany (oneOf " \t\r")
lexeme :: Parser a -> Parser a
lexeme p = p <* white
symbol :: String -> Parser String
symbol = lexeme . string
ident :: Parser String
ident = lexeme ((:) <$> letter <*> many (alphaNum <|> oneOf "_-"))
integerValue :: Parser Integer
integerValue = lexeme $ do
  sign <- option id (char '-' >> pure negate)
  digits <- many1 digit
  notFollowedBy (letter <|> char '.')
  pure (sign (read digits))
integer :: Parser Int
integer = do
  x <- integerValue
  if x < toInteger (minBound :: Int) || x > toInteger (maxBound :: Int)
    then fail "integer out of range" else pure (fromInteger x)
number :: Parser Double
number = lexeme $ do
  sign <- option "" (string "-")
  a <- many1 digit
  b <- option "" ((:) <$> char '.' <*> many1 digit)
  exponentPart <- option "" (do
    e <- oneOf "eE"
    signE <- option "" ((:[]) <$> oneOf "+-")
    ds <- many1 digit
    pure (e:signE++ds))
  let x = read (sign++a++b++exponentPart)
  if isInfinite x then fail "number overflow" else pure x

-- | An exact decimal literal: @3@, @-14@, @0.25@ (= 1/4), @1.0e-2@ (= 1/100).
decimal :: Parser Rational
decimal = lexeme $ do
  sign <- option id (char '-' >> pure negate)
  whole <- many1 digit
  fraction <- option "" (try (char '.' *> many1 digit))
  exponentPart <- option 0 (try (do
    _ <- oneOf "eE"
    s <- option id ((char '-' >> pure negate) <|> (char '+' >> pure id))
    ds <- many1 digit
    pure (s (read ds :: Integer))))
  notFollowedBy (letter <|> char '.')
  let mantissa = read (whole ++ fraction) % (10 ^ length fraction)
      scaled = if exponentPart >= 0 then mantissa * 10 ^ exponentPart else mantissa / 10 ^ negate exponentPart
  pure (sign scaled)

-- | A full signal expression: @a -> b@ over @+ -@ over @* /@ over atoms. A @-@
-- only subtracts when followed by a space, so @-2@ is always a negative number.
numExpr :: Parser NumE
numExpr = do
  a <- chainl1 numTerm addOp
  option a (NArrow a <$> (try (symbol "->") *> chainl1 numTerm addOp))
  where
    addOp = (NBin '+' <$ symbol "+") <|> (NBin '-' <$ try (lexeme (char '-' <* lookAhead (oneOf " \t"))))
    numTerm = chainl1 numAtom (NBin '*' <$ symbol "*" <|> NBin '/' <$ symbol "/")

-- | One atom: a literal, a name, a parenthesised expression, a negation,
-- breakpoints @{u:value ...}@ over the note (u from 0 to 1), or a wave
-- (@sine r@, @square r@, r cycles per beat), or @cents c@. A literal fraction
-- @a/b@ counts as one atom, so @x:1/2@ still reads as before.
numAtom :: Parser NumE
numAtom = between (symbol "(") (symbol ")") numExpr
  <|> try (do a <- decimal; b <- optionMaybe (try (symbol "/" *> decimal)); pure (maybe (NLit a) (NBin '/' (NLit a) . NLit) b))
  <|> (NPoints <$> between (symbol "{") (symbol "}") (many1 ((,) <$> numAtom <* symbol ":" <*> numAtom)))
  <|> (keyword "sine" *> (NOsc Sine <$> numAtom))
  <|> (keyword "square" *> (NOsc Square <$> numAtom))
  <|> (keyword "cents" *> (NCents <$> numAtom))
  <|> (NVar <$> ident)
  <|> (NNeg <$> (try (char '-' <* lookAhead (letter <|> char '(')) *> numAtom))

keyword :: String -> Parser ()
keyword word = void (try (lexeme (string word <* notFollowedBy (alphaNum <|> oneOf "_-"))))

-- | Parse music, knowing each function's parameter kinds (True = number) and
-- which names in scope are number parameters.
expression :: M.Map Name [Bool] -> [Name] -> Parser Expr
expression kinds numberNames = parallel
  where
    parallel = chainl1 sequenceP (symbol "&" >> pure ParE)
    sequenceP = SeqE <$> many1 term
    term = do
      base <- prefix <|> atom
      suffix base
    prefix = (keyword "stretch" *> (Scale <$> numExpr <*> term))
      <|> (keyword "rev" *> (Rev <$> term))
      <|> (keyword "inv" *> (Inv <$> term))
      <|> (keyword "gate" *> (Gate <$> numExpr <*> term))
      <|> (keyword "pedal" *> (Pedal <$> term))
      -- speed r: constant; speed a -> b: a ramp (rit. or accel.).
      -- speed r; speed a -> b (rit. or accel.); speed cycle {len:shape ...}.
      <|> (keyword "speed" *> (TempoE <$> shapeP <*> term))
      <|> (keyword "pitch" *> (PitchE <$> numExpr <*> term))
      <|> (keyword "level" *> (LevelE <$> numExpr <*> term))
      <|> (keyword "glide" *> (GlideE <$> numExpr <*> term))
    atom = (Notes . pure <$> integer)
      <|> (symbol "_" >> pure (Notes []))
      <|> between (symbol "(") (symbol ")") (Notes <$> many integer)
      <|> between (symbol "[") (symbol "]") parallel
      <|> do
        name <- ident
        if name `elem` numberNames
          then pure (NoteE (NVar name))  -- a number in a music position is a note
          else Ref name <$> mapM argument (M.findWithDefault [] name kinds)
    argument isNumber = if isNumber then NumberArg <$> numExpr else MusicArg <$> term
    suffix e = (symbol ":" *> numAtom >>= suffix . (`Scale` e))
      <|> (symbol "+" *> numAtom >>= suffix . (`Trans` e))
      -- '-2' starts a negative note; '- 2' is a transpose operator.
      <|> (try (lexeme (char '-' <* lookAhead (oneOf " \t"))) *> numAtom >>= suffix . (\k -> Trans (NNeg k) e))
      <|> (symbol "*" *> numAtom >>= suffix . (`Repeat` e))
      <|> pure e

-- | A tempo shape: @cycle {len:shape ...}@, or a signal (a constant or @a -> b@).
-- Inside a cycle, a segment's shape is a number atom, or a parenthesised
-- @(a -> b)@ or @(cycle {...})@.
shapeP :: Parser ShapeE
shapeP = (keyword "cycle" *> (CycleShape <$> between (symbol "{") (symbol "}") (many1 segment)))
  <|> (SignalShape <$> numExpr)
  where
    segment = (,) <$> numAtom <* symbol ":" <*> segmentShape
    segmentShape = try (between (symbol "(") (symbol ")") shapeP) <|> (SignalShape <$> numAtom)

runLine :: Int -> Parser a -> String -> Either String a
runLine line p input = either (Left . show) Right (parse (white *> p <* eof) (".neomusic line "++show line) input)

-- | C4 = MIDI 60. Accidentals are accepted without imposing a key on the score.
pitchNumber :: String -> Either String Int
pitchNumber = runLine 0 $ do
  c <- oneOf "CDEFGAB"
  accidental <- option 0 ((char '#' >> pure 1) <|> (char 'b' >> pure (-1)))
  octave <- integer
  let pc = M.fromList (zip "CDEFGAB" [0,2,4,5,7,9,11]) M.! c
      result = 12 * (toInteger octave+1) + pc + accidental
  if result < 0 || result > 127 then fail "pitch name outside MIDI 0..127" else pure (fromInteger result)

presets :: M.Map String A.Patch
presets = M.fromList
  [("piano",A.pianoPatch),("sine",A.sinePatch),("saw",A.sawPatch),("square",A.squarePatch)
  ,("pluck",A.pluck),("supersaw",A.supersaw),("reeseBass",A.reeseBass),("subBass",A.subBass)
  ,("kick",A.kick),("snare",A.snare),("hat",A.hat),("noiseSweep",A.noiseSweep)]

reserved :: [String]
reserved = ["tempo","tuning","meter","key","instrument","stretch","rev","inv","gate","pedal","speed","cycle","pitch","level","glide","sine","square"]

-- | Source lines without comments or blank lines, numbered from 1.
sourceLines :: String -> [(Int, String)]
sourceLines input = [(i, s') | (i, s) <- zip [1..] (lines input), let s' = stripComment s, not (all isSpace s')]
  where
    stripComment [] = []
    stripComment ('-':'-':_) = []
    stripComment (x:xs) = x : stripComment xs

takeWord :: String -> String
takeWord = takeWhile (not . isSpace) . dropWhile isSpace

isBinding :: String -> Bool
isBinding s = '=' `elem` s && takeWord s /= "instrument"

-- | Parse a file's definitions. @outer@ gives the kinds of functions defined
-- outside it (the prelude, for a document); the file's own names shadow them.
parseDefinitions :: Bool -> M.Map Name [Bool] -> [(Int, String)] -> Either String (M.Map Name Definition, M.Map Name [Bool])
parseDefinitions fromPrelude outer ls = do
  let bindings = [(i,s) | (i,s) <- ls, isBinding s]
      paramP = (NumberParam <$> (char '#' *> ident)) <|> (MusicParam <$> ident)
  signatures <- mapM (\(i,s) -> runLine i ((,) <$> ident <*> many paramP) (takeWhile (/='=') s)) bindings
  unless (all (\(name, ps) -> let xs = name : map paramName ps in length xs == length (nub xs) && all (`notElem` reserved) xs) signatures)
    (Left "duplicate parameters or reserved binding name")
  let names = map fst signatures
  unless (length names == length (nub names)) (Left "duplicate binding")
  let kinds = M.union (M.fromList [(name, map isNumberParam ps) | (name, ps) <- signatures]) outer
  definitions <- mapM (\((i,s),(name,ps)) -> do
      let local = foldr (M.delete . paramName) kinds ps
          numberNames = [p | NumberParam p <- ps]
      expr <- runLine i (expression local numberNames) (drop 1 (dropWhile (/='=') s))
      pure (name, Definition ps expr fromPrelude)) (zip bindings signatures)
  pure (M.fromList definitions, kinds)

-- | The prelude's definitions, parsed once.
prelude :: Either String (M.Map Name Definition, M.Map Name [Bool])
prelude = either (Left . ("prelude: " ++)) Right (parseDefinitions True M.empty (sourceLines preludeSource))

parseDocument :: String -> Either String Document
parseDocument input = do
  (fromPreludeDefs, preludeKinds) <- prelude
  let ls = sourceLines input
  (defs, kinds) <- parseDefinitions False preludeKinds ls
  let nonbindings = [(i,s) | (i,s) <- ls, not (isBinding s)]
      initial = (defaultPerformance, SheetView 4 "C" M.empty True, tuning defaultInstrument, [], [])
      env = Env defs fromPreludeDefs False M.empty []
  (perf,view,_,voices,_) <- foldM (readStatement kinds) initial nonbindings
  scores <- mapM (\(name,expr) -> evaluate env name expr) voices
  let result = foldr (:||:) (Silence 0) scores
  validate perf result
  pure (Document perf view result)
  where
    readStatement kinds (p,v,t,vs,seen) (line,s) = do
      let first = takeWord s
          singleton label = unless (label `notElem` seen) (Left ("duplicate header: "++label))
          unchanged = (p,v,t,vs,seen)
      case first of
        "tempo" -> do
          singleton first
          speed <- runLine line (keyword "tempo" *> number) s
          pure (p {bpm=speed},v,t,vs,first:seen)
        "meter" -> do
          singleton first
          beats <- runLine line (keyword "meter" *> integer) s
          unless (beats > 0) (Left "meter must be positive")
          pure (p,v {meter=beats},t,vs,first:seen)
        "key" -> do
          singleton first
          k <- runLine line (keyword "key" *> lexeme (many1 (oneOf "ABCDEFGb#"))) s
          unless (k `elem` ["C","G","D","A","E","B","F#","C#","F","Bb","Eb","Ab","Db","Gb","Cb"])
            (Left "unsupported major key hint")
          pure (p,v {key=k},t,vs,first:seen)
        "tuning" -> do
          singleton first
          unless (M.null (instruments p)) (Left "global tuning must precede instrument declarations")
          t' <- runLine line (keyword "tuning" *> tuningParser) s >>= id
          pure (p,v,t',vs,first:seen)
        "instrument" -> do
          (name,preset,attrs) <- runLine line instrumentParser s
          unless (M.notMember name (instruments p)) (Left ("duplicate instrument: "++name))
          patch <- maybe (Left ("unknown preset: "++preset)) Right (M.lookup preset presets)
          (inst,staff) <- foldM applyAttr (defaultInstrument {tuning=t,timbre=patch},Treble) attrs
          unless (length (map fst attrs) == length (nub (map fst attrs))) (Left "duplicate instrument attribute")
          pure (p {instruments=M.insert name inst (instruments p)},v {staves=M.insert name staff (staves v)},t,vs,seen)
        _ -> do
          (name,expr) <- runLine line ((,) <$> ident <* symbol ":" <*> expression kinds []) s
          let (p0,v0,t0,vs0,seen0) = unchanged
          pure (p0,v0,t0,vs0++[(name,expr)],seen0)
    tuningParser = try (do
      edo <- integer
      keyword "at"
      name <- lexeme (many1 (alphaNum <|> oneOf "#-"))
      pure $ do
        midi <- pitchNumber name
        A.equalTemperament (440 * 2 ** (fromIntegral (midi-69)/12)) edo)
      <|> (do base <- number; keyword "hz"; r <- number; pure (Right (A.Tuning base r)))
    instrumentParser = do
      keyword "instrument"
      name <- ident
      _ <- symbol "="
      preset <- ident
      attrs <- many ((,) <$> ident <*> lexeme (many1 (noneOf " \t\r")))
      pure (name,preset,attrs)
    applyAttr (i,staff) (attr,value) = case attr of
      "staff" -> case value of
        "treble" -> Right (i,Treble)
        "bass" -> Right (i,Bass)
        _ -> Left "staff must be treble or bass"
      "gain" -> do x <- runLine 0 number value; pure (i {gain=x},staff)
      "pan" -> do x <- runLine 0 number value; pure (i {pan=x},staff)
      "send" -> do x <- runLine 0 number value; pure (i {send=x},staff)
      _ -> Left ("unknown instrument attribute: "++attr)

-- | What evaluation can see: the document's and the prelude's definitions,
-- whether it is inside a prelude body, the parameters bound here, and the
-- definitions being expanded (to reject recursion).
data Env = Env
  { userDefs :: M.Map Name Definition, preludeDefs :: M.Map Name Definition, inPrelude :: Bool
  , locals :: M.Map Name Value, active :: [Name] }

-- | The value of a signal expression.
signalValue :: Env -> NumE -> Either String Signal
signalValue env e = case e of
  NLit r -> Right (Const r)
  NVar v -> case M.lookup v (locals env) of
    Just (SignalV sig) -> Right sig
    Just (MusicV _) -> Left (v ++ " is music, not a number")
    Nothing -> Left ("unknown number: " ++ v)
  NNeg a -> negateSignal <$> recur a
  NBin op a b -> do
    x <- recur a
    y <- recur b
    case op of
      '+' -> Right (plus x y)
      '-' -> Right (plus x (negateSignal y))
      '*' -> Right (times x y)
      _ -> do
        k <- constantNumber "a divisor" y
        if k == 0 then Left "division by zero" else Right (times x (Const (1 / k)))
  NPoints ps -> Points <$> mapM (\(u, v) -> (,) <$> (recur u >>= constantNumber "a point") <*> (recur v >>= constantNumber "a point")) ps
  NOsc w r -> Osc w <$> (recur r >>= constantNumber "a wave's rate")
  NCents a -> do
    x <- recur a
    pure (maybe (Cents x) (Cents . Const) (constantValue x))
  NArrow a b -> do
    x <- recur a >>= constantNumber "the start of a -> b"
    y <- recur b >>= constantNumber "the end of a -> b"
    pure (Points [(0, x), (1, y)])
  where recur = signalValue env

-- | A signal that must be a plain constant here.
constantNumber :: String -> Signal -> Either String Rational
constantNumber what sig = maybe (Left (what ++ " must be a constant number")) Right (constantValue sig)

-- | The value of an expression that must be a plain number.
numberValue :: Env -> NumE -> Either String Rational
numberValue env e = signalValue env e >>= constantNumber "this value"

ratText :: Rational -> String
ratText x = show (numerator x) ++ if denominator x == 1 then "" else "/" ++ show (denominator x)

-- | A number that must be whole (repeats, transposition, notes).
wholeNumber :: String -> Rational -> Either String Int
wholeNumber what r
  | denominator r /= 1 = Left (what ++ " must be a whole number, not " ++ ratText r)
  | numerator r < toInteger (minBound :: Int) || numerator r > toInteger (maxBound :: Int) = Left (what ++ " is out of range")
  | otherwise = Right (fromInteger (numerator r))

positiveNumber :: String -> Rational -> Either String Rational
positiveNumber what r = if r > 0 then Right r else Left (what ++ " must be positive, not " ++ ratText r)

evaluate :: Env -> Name -> Expr -> Either String Score
evaluate env name expr = case expr of
  Notes xs -> pure (Line name (chord xs))
  NoteE e -> do
    x <- num e >>= wholeNumber "a number used as a note"
    pure (Line name (chord [x]))
  SeqE xs -> foldr (:>>:) (Silence 0) <$> mapM recur xs
  ParE a b -> (:||:) <$> recur a <*> recur b
  Scale k e -> do k' <- num k >>= positiveNumber "a duration factor"; scaleScore k' <$> recur e
  Trans k e -> do k' <- num k >>= wholeNumber "a transposition"; mapPhrases (transpose k') <$> recur e
  Repeat k e -> do
    k' <- num k >>= wholeNumber "a repeat count"
    when (k' < 0) (Left "repeat count must be nonnegative")
    x <- recur e
    pure (mconcat (replicate k' x))
  Rev e -> reverseScore <$> recur e
  Inv e -> mapPhrases invert <$> recur e
  Gate g e -> do g' <- num g >>= positiveNumber "a gate"; mapPhrases (scaleGate g') <$> recur e
  Pedal e -> pedal <$> recur e
  TempoE shape e -> do
    s <- tempoShape shape
    Tempo s <$> recur e
  PitchE p e -> do sig <- signalValue env p; mapPhrases (addPitch sig) <$> recur e
  LevelE l e -> do sig <- signalValue env l; mapPhrases (scaleLevel sig) <$> recur e
  GlideE f e -> do
    f' <- num f >>= positiveNumber "a glide fraction"
    when (f' > 1) (Left "glide fraction must be within 0..1")
    glide f' <$> recur e
  Ref ref args -> case M.lookup ref (locals env) of
    Just (MusicV x) | null args -> Right x
    Just (SignalV sig) | null args -> do
      x <- constantNumber "a number used as a note" sig >>= wholeNumber "a number used as a note"
      pure (Line name (chord [x]))
    Just _ -> Left (ref ++ " is a parameter and cannot be called")
    Nothing -> do
      unless (ref `notElem` active env) (Left ("recursive definition: "++intercalate " -> " (reverse (ref:active env))))
      let visible = if inPrelude env then preludeDefs env else M.union (userDefs env) (preludeDefs env)
      Definition params body fromPrelude <- maybe (Left ("unknown binding: "++ref)) Right (M.lookup ref visible)
      unless (length params == length args) (Left ("wrong arity: "++ref))
      values <- mapM argument args
      let inner = env {inPrelude = fromPrelude, locals = M.fromList (zip (map paramName params) values), active = ref : active env}
      either (Left . (("in " ++ ref ++ ": ") ++)) Right (evaluate inner name body)
  where
    recur = evaluate env name
    num = numberValue env
    tempoShape shape = case shape of
      SignalShape r -> do
        sig <- signalValue env r
        case (constantValue sig, sig) of
          (Just k, _) -> steady <$> positiveNumber "a speed" k
          (_, Points [(0, a), (1, b)]) -> Ramp <$> positiveNumber "a speed" a <*> positiveNumber "a speed" b
          _ -> Left "speed takes a constant, a -> b, or cycle {...}"
      CycleShape segs -> Cycle <$> mapM (\(l, sub) -> (,) <$> (num l >>= positiveNumber "a cycle segment's length") <*> tempoShape sub) segs
    argument (MusicArg e) = MusicV <$> recur e
    argument (NumberArg e) = SignalV <$> signalValue env e

scaleScore :: Rational -> Score -> Score
scaleScore k (Silence d) = Silence (k*d)
scaleScore k (Line name xs) = Line name (stretch k xs)
scaleScore k (a :>>: b) = scaleScore k a :>>: scaleScore k b
scaleScore k (a :||: b) = scaleScore k a :||: scaleScore k b
scaleScore k (Tempo shape s) = Tempo (scaled shape) (scaleScore k s)
  where
    scaled (Cycle segs) = Cycle [(k * l, scaled sub) | (l, sub) <- segs]
    scaled other = other
reverseScore :: Score -> Score
reverseScore (Line name xs) = Line name (retro xs)
reverseScore (Silence d) = Silence d
reverseScore (a :>>: b) = reverseScore b :>>: reverseScore a
-- A ramp runs backwards; a cycle's segments run in reverse order, each mirrored,
-- so swing becomes short-long (exact when the extent is a whole number of
-- periods, since periods are counted from the node's start).
reverseScore (Tempo shape s) = Tempo (mirrored shape) (reverseScore s)
  where
    mirrored (Ramp a b) = Ramp b a
    mirrored (Cycle segs) = Cycle (reverse [(l, mirrored sub) | (l, sub) <- segs])
reverseScore (a :||: b) =
  let da = scoreLength a
      db = scoreLength b
      total = max da db
  in (Silence (total-da) :>>: reverseScore a) :||: (Silence (total-db) :>>: reverseScore b)

-- | Canonical expanded source. Instruments/headers are supplied by the caller;
-- each structural leaf is a voice with an explicit initial delay. No bindings
-- need to survive expansion. Parallel structure preserves exact beat onsets.
prettyScore :: Score -> Either String String
prettyScore s = do
  (total,events) <- flatten s
  let carrier = case events of e:_ -> instrument e; [] -> "silent"
  -- The clock is shared, so each tempo node can stand alone over rests.
  let tempos = [ carrier ++ ": " ++ silence t0 ++ "speed " ++ shapeSource shape ++ " [_:" ++ rat len ++ "]"
               | (t0,len,shape) <- tempoNodes s, len > 0 ]
  pure (unlines ([instrument e ++ ": " ++ silence (onset e) ++ token (event e)
                  ++ silence (total-onset e-dur (event e)) | e <- events] ++
                (if null events then ["instrument silent = sine", "silent: "++if total == 0 then "[0]*0" else "_:"++rat total] else [])
                ++ tempos))
  where
    rat = ratText
    -- Every compound is parenthesised, so it reads back as one atom.
    signalText sig = case sig of
      Const k -> rat k
      Points ps -> "{"++unwords [rat u++":"++rat y | (u, y) <- ps]++"}"
      Osc w r -> "("++(case w of Sine -> "sine "; Square -> "square ")++rat r++")"
      a :+: b -> "("++signalText a++" + "++signalText b++")"
      a :*: b -> "("++signalText a++" * "++signalText b++")"
      Cents a -> "(cents "++signalText a++")"
    silence 0 = ""
    silence d = " _:"++rat d++" "
    token e = "["++leveled e (pitched e (gated e (pitch (pitches e)++":"++rat (dur e))))++"]"
    leveled e body = if level e == Const 1 then body else "level "++signalText (level e)++" ["++body++"]"
    pitched e body = if pitchShift e == Const 0 then body else "pitch "++signalText (pitchShift e)++" ["++body++"]"
    gated e body = if gate e == 1 then body else "gate "++rat (gate e)++" ["++body++"]"
    pitch [] = "_"
    pitch [A.Step x] = show x
    pitch xs = "("++unwords [show x | A.Step x <- xs]++")"
