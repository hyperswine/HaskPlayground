-- | Line-oriented .neomusic authoring, with acyclic, first-order definitions.
module NeoMusic.Language
  ( Document(..), SheetView(..), Staff(..), parseDocument, prettyScore, pitchNumber ) where

import Control.Monad (foldM, unless, void)
import Data.Char (isSpace)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as M
import Data.Ratio ((%), numerator, denominator)
import qualified NeoMusic.Audio as A
import qualified NeoMusic.Pitch as A
import NeoMusic.Score
import Text.Parsec hiding (Line, label, token)
import Text.Parsec.String (Parser)

data Staff = Treble | Bass deriving (Eq, Ord, Show)
data SheetView = SheetView { meter :: Int, key :: String, staves :: M.Map Name Staff }
  deriving (Eq, Show)
data Document = Document { performance :: Performance, sheetView :: SheetView, score :: Score }
  deriving (Eq, Show)
data Expr = Notes [Int] | Ref Name [Expr] | SeqE [Expr] | ParE Expr Expr
  | Scale Rational Expr | Trans Int Expr | Repeat Int Expr | Rev Expr | Inv Expr | Vel Double Expr
  deriving (Eq, Show)
data Definition = Definition [Name] Expr deriving (Eq, Show)

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
rational :: Parser Rational
rational = do
  a <- integerValue
  b <- option 1 (symbol "/" *> integerValue)
  if a <= 0 || b <= 0 then fail "duration must be positive" else pure (a % b)
keyword :: String -> Parser ()
keyword word = void (try (lexeme (string word <* notFollowedBy (alphaNum <|> oneOf "_-"))))

expression :: M.Map Name Int -> Parser Expr
expression arities = parallel
  where
    parallel = chainl1 sequenceP (symbol "&" >> pure ParE)
    sequenceP = SeqE <$> many1 term
    term = do
      base <- prefix <|> atom
      suffix base
    prefix = (keyword "stretch" *> (Scale <$> rational <*> term))
      <|> (keyword "rev" *> (Rev <$> term))
      <|> (keyword "inv" *> (Inv <$> term))
      <|> (keyword "vel" *> (Vel <$> number <*> term))
    atom = (Notes . pure <$> integer)
      <|> (symbol "_" >> pure (Notes []))
      <|> between (symbol "(") (symbol ")") (Notes <$> many integer)
      <|> between (symbol "[") (symbol "]") parallel
      <|> do
        name <- ident
        args <- count (M.findWithDefault 0 name arities) term
        pure (Ref name args)
    suffix e = (symbol ":" *> rational >>= suffix . (`Scale` e))
      <|> (symbol "+" *> integer >>= suffix . (`Trans` e))
      -- '-2' starts a negative note; '- 2' is a transpose operator.
      <|> (try (lexeme (char '-' <* lookAhead (oneOf " \t"))) *> integer >>= suffix . (\k -> Trans (negate k) e))
      <|> (symbol "*" *> integer >>= \k -> if k < 0 then fail "repeat count must be nonnegative" else suffix (Repeat k e))
      <|> pure e

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

parseDocument :: String -> Either String Document
parseDocument input = do
  let ls = [(i,stripComment s) | (i,s) <- zip [1..] (lines input), not (all isSpace (stripComment s))]
      isBinding s = '=' `elem` s && takeWord s /= "instrument"
      bindings = [(i,s) | (i,s) <- ls, isBinding s]
      reserved = ["tempo","tuning","meter","key","instrument","stretch","rev","inv","vel"]
  signatures <- mapM (\(i,s) -> runLine i (many1 ident) (takeWhile (/='=') s)) bindings
  unless (all (\xs -> length xs == length (nub xs) && all (`notElem` reserved) xs) signatures)
    (Left "duplicate parameters or reserved binding name")
  let names = [name | name:_ <- signatures]
  unless (length names == length (nub names)) (Left "duplicate binding")
  let arities = M.fromList [(name,length params) | name:params <- signatures]
  definitions <- mapM (\((i,s),(name,params)) -> do
      expr <- runLine i (expression (foldr M.delete arities params)) (drop 1 (dropWhile (/='=') s))
      pure (name, Definition params expr)) (zip bindings [(name,params) | name:params <- signatures])
  let defs = M.fromList definitions
      nonbindings = [(i,s) | (i,s) <- ls, not (isBinding s)]
      initial = (defaultPerformance, SheetView 4 "C" M.empty, tuning defaultInstrument, [], [])
  (perf,view,_,voices,_) <- foldM (readStatement arities) initial nonbindings
  scores <- mapM (\(name,expr) -> evaluate defs M.empty [] name expr) voices
  let result = foldr (:||:) (Silence 0) scores
  validate perf result
  pure (Document perf view result)
  where
    stripComment [] = []
    stripComment ('-':'-':_) = []
    stripComment (x:xs) = x : stripComment xs
    takeWord = takeWhile (not . isSpace) . dropWhile isSpace
    readStatement arities (p,v,t,vs,seen) (line,s) = do
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
          (name,expr) <- runLine line ((,) <$> ident <* symbol ":" <*> expression arities) s
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

evaluate :: M.Map Name Definition -> M.Map Name Score -> [Name] -> Name -> Expr -> Either String Score
evaluate defs locals active name expr = case expr of
  Notes xs -> pure (Line name (chord xs))
  SeqE xs -> foldr (:>>:) (Silence 0) <$> mapM recur xs
  ParE a b -> (:||:) <$> recur a <*> recur b
  Scale k e -> scaleScore k <$> recur e
  Trans k e -> mapPhrases (transpose k) <$> recur e
  Repeat k e -> do x <- recur e; pure (mconcat (replicate k x))
  Rev e -> reverseScore <$> recur e
  Inv e -> mapPhrases invert <$> recur e
  Vel v e -> mapPhrases (scaleVelocity v) <$> recur e
  Ref ref args -> case M.lookup ref locals of
    Just x -> if null args then Right x else Left "parameters cannot be called"
    Nothing -> do
      unless (ref `notElem` active) (Left ("recursive definition: "++intercalate " -> " (reverse (ref:active))))
      Definition params body <- maybe (Left ("unknown binding: "++ref)) Right (M.lookup ref defs)
      values <- mapM recur args
      unless (length params == length values) (Left ("wrong arity: "++ref))
      evaluate defs (M.fromList (zip params values)) (ref:active) name body
  where recur = evaluate defs locals active name

scaleScore :: Rational -> Score -> Score
scaleScore k (Silence d) = Silence (k*d)
scaleScore k (Line name xs) = Line name (stretch k xs)
scaleScore k (a :>>: b) = scaleScore k a :>>: scaleScore k b
scaleScore k (a :||: b) = scaleScore k a :||: scaleScore k b
reverseScore :: Score -> Score
reverseScore (Line name xs) = Line name (retro xs)
reverseScore (Silence d) = Silence d
reverseScore (a :>>: b) = reverseScore b :>>: reverseScore a
reverseScore (a :||: b) =
  let da = scoreLength a
      db = scoreLength b
      total = max da db
  in (Silence (total-da) :>>: reverseScore a) :||: (Silence (total-db) :>>: reverseScore b)

scoreLength :: Score -> Rational
scoreLength (Line _ xs) = sum (map dur xs)
scoreLength (Silence d) = d
scoreLength (a :>>: b) = scoreLength a + scoreLength b
scoreLength (a :||: b) = max (scoreLength a) (scoreLength b)

-- | Canonical expanded source. Instruments/headers are supplied by the caller;
-- each structural leaf is a voice with an explicit initial delay. No bindings
-- need to survive expansion. Parallel structure preserves exact beat onsets.
prettyScore :: Score -> Either String String
prettyScore s = do
  (total,events) <- flatten s
  pure (unlines ([instrument e ++ ": " ++ silence (onset e) ++ token (event e)
                  ++ silence (total-onset e-dur (event e)) | e <- events] ++
                if null events then ["instrument silent = sine", "silent: "++if total == 0 then "[0]*0" else "_:"++rat total] else []))
  where
    rat x = show (numerator x) ++ if denominator x == 1 then "" else "/"++show (denominator x)
    silence 0 = ""
    silence d = " _:"++rat d++" "
    token e = "vel "++show (vel e)++" ["++pitch (pitches e)++":"++rat (dur e)++"]"
    pitch [] = "_"
    pitch [A.Step x] = show x
    pitch xs = "("++unwords [show x | A.Step x <- xs]++")"
