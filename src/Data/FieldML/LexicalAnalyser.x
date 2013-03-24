{
{-# OPTIONS_GHC -funbox-strict-fields -w  #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- Warning suppression is for an Alex-triggered warning -
-- see https://github.com/simonmar/alex/issues/2

module Data.FieldML.LexicalAnalyser where
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Internal as BSI
import Data.Char
import Data.Word
import Data.Int
import Data.Maybe
import Data.Data
-- import qualified Debug.Trace
}

tokens :-
  [\ \t\r\n]+ / [^\#\ \t\r\n] { \(pn@(AlexPn _ _ _ col), _, s') l ->
    (let possiblyCloseBlock = do
          bi <- alexGetBlockIndent
          if finalCol < bi then
            do
              alexPopBlockIndent
              pcb <- possiblyCloseBlock
              return $ (TokCloseBlock pn):pcb
            else return []
         s = LBS.take (fromIntegral l) s'
         sParts = LBSC.splitWith (\c->c=='\r'||c=='\n') s
         finalCol = case sParts of
           [x] -> col + fromIntegral (LBS.length x)
           [] -> col -- Impossible?
           l -> 1 + fromIntegral (LBS.length (last l))
     in possiblyCloseBlock)
  }
  [\ \t\r\n]+ { \_ _ -> return []}
  "#".* { \_ _ -> return [] }
  append { returnP TokAppend }
  as { returnP TokAs }
  case { returnP TokCase }
  class { returnP TokClass }
  clone { returnP TokClone }
  connect { returnP TokConnect }
  dimensionless { returnP TokDimensionless }
  domain { returnP TokDomain }
  ensemble { returnP TokEnsemble }
  fcase { returnP TokFCase }
  from { returnP TokFrom }
  "=>" { returnP TokHeadSep }
  hiding { returnP TokHiding }
  import { returnP TokImport }
  instance { returnP TokInstance }
  let { returnP TokLet }
  lookup { returnP TokLookup }
  val { returnP TokVal }
  namespace { returnP TokNamespace }
  newbase { returnP TokNewbase }
  of { returnP TokOf }
  "::" { returnP TokPathSep }
  "->" { returnP TokRightArrow }
  subset { returnP TokSubset }
  unit { returnP TokUnit }
  using { returnP TokUsing }
  where { returnP TokWhere }
  \\ { returnP TokForwardSlash}
  \= / ([^\~ \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|]|[\r\n]) { returnP TokEqual }
  \, { returnP TokComma }
  \(\< { openBracket TokOpenProductBracket }
  \>\) { openBracket TokCloseProductBracket }
  \( { openBracket TokOpenBracket }
  \) { closeBracket TokCloseBracket }
  \[ { openBracket TokOpenSqBracket }
  \] { closeBracket TokCloseSqBracket }
  \{ { openBracket TokOpenCurlyBracket }
  \} { closeBracket TokCloseCurlyBracket }
  \| / ([^ \~ \. \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|]|[\r\n]) { returnP TokPipe }
  (\-|\+)[0-9]+ / ([^Ee\.]|[\r\n]) { \(p, _, s) l -> return [TokSignedInt (p, fst . fromJust . LBSC.readInt $ s)] }
  [0-9]+ / ([^Ee\.]|[\r\n]) { \(p, _, s) l -> return [TokInt (p, fst . fromJust . LBSC.readInt $ s)] }
  (\-|\+)?[0-9]+(\.[0-9]+)?((E|e)(\+|\-)?[0-9]+)? {
    \(p, _, s) l -> return [TokReal (p, read . LBSC.unpack . LBS.take (fromIntegral l) $ s)] }
  \/ / ([^\~ \. \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|]|[\r\n]) { returnP TokSlash }
  \~ / ([^\~ \. \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|]|[\r\n]) { returnP TokTilde }
  ":" { returnP TokColon }
  R / ([^A-Za-z0-9_']|[\r\n]) { returnP TokR }
  \_[A-Za-z0-9_']+ { \(p, _, s) l -> return [TokScopedSymbol (p, LBS.toStrict $ LBS.take (fromIntegral l) s)] }
  \_ / ([^\~ \. \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|A-Za-z0-9_']|[\r\n]) { returnP TokUnderscore }
  [A-Za-z][A-Za-z0-9_']* { \(p, _, s) l -> return [TokNamedSymbol (p, LBS.toStrict $ LBS.take (fromIntegral l) s)] }
  [ \. \~ \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|]+ { \(p, _, s) l -> return [TokNamedSymbol (p, LBS.toStrict $ LBS.take (fromIntegral l) s)] }
  \"([^\\\"]*(\\[\"rntf\\]))*[^\\\"]*\" { \(p, _, s) l -> return [TokString (p, LBS.toStrict $ LBS.tail $ LBS.take (fromIntegral (l - 1)) s)] }

{
type Byte = Word8
type AlexInput = (AlexPosn, -- current position,
                  Char, -- previous char
                  LBS.ByteString) -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,_,cs) | LBS.null cs = Nothing
                     | otherwise = let b = LBS.head cs
                                       cs' = LBS.tail cs
                                       c = BSI.w2c b
                                       p' = alexMove p c
                                    in p' `seq` cs' `seq` Just (b, (p', c, cs'))

data AlexPosn = AlexPn !String !Int !Int !Int
        deriving (Eq, Ord, Data, Typeable)
instance Show AlexPosn where
  showsPrec _ (AlexPn f _ r c) = showString f . showString " line " . shows r . showString ", column " . shows c

alexStartPos :: String -> AlexPosn
alexStartPos fn = AlexPn fn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn f a l c) '\t' = AlexPn f (a+1) l (((c+7) `div` 8)*8+1)
alexMove (AlexPn f a l c) '\n' = AlexPn f (a+1) (l+1) 1
alexMove (AlexPn f a l c) _ = AlexPn f (a+1) l (c+1)

data AlexState = AlexState {
        alex_pos :: !AlexPosn, -- position at current input location
        alex_inp :: LBS.ByteString, -- the current input
        alex_chr :: !Char, -- the character before the input
        alex_scd :: !Int, -- the current startcode
        alex_block_indent :: [(Int, Int)], -- block indent levels and bracket level (with an infinite tail of zeros).
        alex_token_queue :: [Token], -- any tokens that were generated and need to be consumed.
        alex_last_token :: Maybe Token
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> LBS.ByteString -> Alex a -> Either String a
runAlex filename input (Alex f)
   = case f (AlexState {alex_pos = alexStartPos filename,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_scd = 0,
                        alex_block_indent = repeat (0, 1),
                        alex_token_queue = [],
                        alex_last_token = Nothing
                       }) of
       Left msg -> Left msg
       Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)
  fail e = Alex $ \s -> Left e

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} ->
        Right (s, (pos,c,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexGetBlockIndent :: Alex Int
alexGetBlockIndent = Alex $ \(s@AlexState{alex_block_indent=abi}) -> Right (s, fst $ head abi)

alexWithBracketLevel :: (Int -> Int) -> Alex Int
alexWithBracketLevel f = Alex $ \(s@AlexState{alex_block_indent=abi}) ->
  let
    oldBracketLevel = snd (head abi)
    newBracketLevel = f oldBracketLevel
  in
    Right (s{alex_block_indent=(fst (head abi), newBracketLevel):(tail abi)}, newBracketLevel)

alexSetLastToken :: Token -> Alex Token
alexSetLastToken t = Alex $ \s -> Right (s{alex_last_token=Just t}, t)
alexGetLastToken :: Alex (Maybe Token)
alexGetLastToken = Alex $ \(s@AlexState{alex_last_token=t}) -> Right (s, t)

alexIsTokenOpenBracket (TokOpenBracket _) = True
alexIsTokenOpenBracket (TokOpenSqBracket _) = True
alexIsTokenOpenBracket (TokOpenCurlyBracket _) = True
alexIsTokenOpenBracket _ = False

alexFixLookaheadBracket :: Alex ()
alexFixLookaheadBracket = do
  mt <- alexGetLastToken
  -- Debug.Trace.trace ("alexGetLastToken returns " ++ show mt) (return ())
  case mt of
    Just t | alexIsTokenOpenBracket t ->
      Alex $ \(s@AlexState{alex_block_indent=(ind1, bc1):(ind2, bc2):abi}) ->
        Right (s{alex_block_indent=(ind1, bc1 + 1):(ind2, bc2 - 1):abi}, ())
    _ -> return ()

alexPopBlockIndent :: Alex ()
alexPopBlockIndent = Alex $ \(s@AlexState{alex_block_indent=abi}) -> Right (s{alex_block_indent=tail abi}, ())

alexPushBlockIndent :: Int -> Alex ()
alexPushBlockIndent h = do
  -- Debug.Trace.trace ("alexPushBlockIndent " ++ (show h)) $!
  Alex $ \(s@AlexState{alex_block_indent=abi}) -> Right (s{alex_block_indent=(h, 0):abi}, ())
  alexFixLookaheadBracket

alexPutTokenQueue :: [Token] -> Alex ()
alexPutTokenQueue tqend = Alex $
  \(s@AlexState{alex_token_queue=tq}) -> Right (s{alex_token_queue=tq++tqend},())

alexPopTokenQueue :: Alex (Maybe Token)
alexPopTokenQueue = Alex $
  \(s@AlexState{alex_token_queue=tq}) -> case tq of
    [] -> Right (s,Nothing)
    (h:t) -> Right (s{alex_token_queue=t}, Just h)

alexWithContinuation cont = alexMonadScan' >>= cont

alexMonadScan' = do
  ret <- alexMonadScan
  -- Debug.Trace.trace ("alexMonadScan: Returning " ++ (show ret)) (return ())
  return ret
  
alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  mt <- alexPopTokenQueue
  case mt of
    Just t -> alexSetLastToken t
    Nothing -> do
      case (alexScan inp sc) :: (AlexReturn (AlexInput -> Int -> Alex [Token])) of
        AlexEOF -> do
          bi <- alexGetBlockIndent
          if bi == 0
             then alexSetLastToken $ TokEOF ((\(p, _, _) -> p) inp)
             else do
               alexPopBlockIndent
               alexSetLastToken $ TokCloseBlock ((\(p, _, _) -> p) inp)
        AlexError inp' -> alexError $ "Lexical error at " ++ ((show . (\(pn,_,_) -> pn)) $ inp')
        AlexSkip inp' len -> do
          alexSetInput inp'
          alexMonadScan
        AlexToken inp' len action -> do
          alexSetInput inp'
          r <- action inp len
          case r of
            (h:[]) -> alexSetLastToken h
            [] -> alexMonadScan
            (h:t) -> alexPutTokenQueue t >> alexSetLastToken h

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

token :: (AlexInput -> Int -> token) -> AlexAction token
token t input len = return (t input len)

data Token = -- Straight keywords and multi-char symbols
             TokAppend AlexPosn |
             TokAs AlexPosn |
             TokCase AlexPosn |
             TokClass AlexPosn |
             TokClone AlexPosn |
             TokConnect AlexPosn |
             TokDimensionless AlexPosn |
             TokDomain AlexPosn |
             TokEnsemble AlexPosn |
             TokFCase AlexPosn |
             TokFrom AlexPosn |
             TokHeadSep AlexPosn | -- ^ The sequence =>
             TokHiding AlexPosn |
             TokImport AlexPosn |
             TokInstance AlexPosn |
             TokLet AlexPosn |
             TokLookup AlexPosn |
             TokVal AlexPosn |
             TokNamespace AlexPosn |
             TokNewbase AlexPosn |
             TokPathSep AlexPosn | -- ^ The sequence ::
             TokRightArrow AlexPosn | -- ^ The sequence ->
             TokSubset AlexPosn |
             TokUnit AlexPosn |
             TokUsing AlexPosn |
             TokWhere AlexPosn |
             -- Meaningful single character punctuation
             TokCloseBracket AlexPosn |
             TokCloseCurlyBracket AlexPosn |
             TokCloseSqBracket AlexPosn |
             TokColon AlexPosn |
             TokComma AlexPosn |
             TokEqual AlexPosn |
             TokForwardSlash AlexPosn |
             TokOf AlexPosn |
             TokOpenBracket AlexPosn |
             TokOpenCurlyBracket AlexPosn |
             TokOpenSqBracket AlexPosn |
             TokOpenProductBracket AlexPosn |
             TokCloseProductBracket AlexPosn |
             TokPipe AlexPosn |
             TokR AlexPosn |
             TokSlash AlexPosn |
             TokTilde AlexPosn |
             -- Parsed values
             TokInt (AlexPosn, Int) |
             TokNamedSymbol (AlexPosn, BS.ByteString) |
             TokReal (AlexPosn, Double) |
             TokScopedSymbol (AlexPosn, BS.ByteString) |
             TokSignedInt (AlexPosn, Int) |
             TokUnderscore AlexPosn |
             -- Special tokens
             -- | Hit when a token is encountered at a lower level of indent
             --   than required for the current block
             TokCloseBlock AlexPosn |
             TokEOF AlexPosn | -- ^ End of file.
             TokString (AlexPosn, BS.ByteString)
    deriving (Eq, Ord, Data, Show, Typeable)

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,bs) = (p,c,bs)

openBracket x (pos, _, _) _ = do
  alexWithBracketLevel (+1)
  return [x pos]

closeBracket x ai@(pos, _, _) l = do
  v <- alexWithBracketLevel (+(-1))
  if v < 0
    then do
      alexPopBlockIndent
      ret <- closeBracket x ai l
      return $ (TokCloseBlock pos):ret
    else
      return [x pos]

returnP x (pos, _, _) _ = return [x pos]
}