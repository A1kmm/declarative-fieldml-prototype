{
{-# OPTIONS_GHC -funbox-strict-fields -w  #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- Warning suppression is for an Alex-triggered warning -
-- see https://github.com/simonmar/alex/issues/2

module Data.FieldML.LexicalAnalyser where
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Internal as BSI
import Data.Char
import Data.Word
import Data.Int
import Data.Maybe
import Data.Data
import qualified Debug.Trace
}

tokens :-
  [\ \t\r\n]+ / [^\#\ \t\r\n] { \(AlexPn _ _ col, _, s') l ->
    (let possiblyCloseBlock = do
          bi <- alexGetBlockIndent
          if finalCol < bi then
            do
              alexPopBlockIndent
              pcb <- possiblyCloseBlock
              return $ TokCloseBlock:pcb
            else return []
         s = LBS.take (fromIntegral l) s'
         sParts = LBSC.splitWith (\c->c=='\r'||c=='\n') s
         finalCol = case sParts of
           [x] -> col + fromIntegral (LBS.length x)
           [] -> col -- Impossible?
           l -> 1 + fromIntegral (LBS.length (last l))
     in possiblyCloseBlock)
  }
  [\ \t\r\n]+ ;
  "#".* ;
  append { returnP TokAppend }
  case { returnP TokCase }
  connect { returnP TokConnect }
  domain { returnP TokDomain }
  "=>" { returnP TokHeadSep }
  hiding { returnP TokHiding }
  import { returnP TokImport }
  lookup { returnP TokLookup }
  namespace { returnP TokNamespace }
  of { returnP TokOf }
  "->" { returnP TokRightArrow }
  subset { returnP TokSubset }
  unit { returnP TokUnit }
  using { returnP TokUsing }
  where { returnP TokWhere }
  \= / [^\~ \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|] { returnP TokEqual }
  \, { returnP TokComma }
  \( { returnP TokOpenBracket }
  \) { returnP TokCloseBracket }
  \[ { returnP TokOpenSqBracket }
  \] { returnP TokCloseSqBracket }
  \{ { returnP TokOpenCurlyBracket }
  \} { returnP TokCloseCurlyBracket }
  (\-|\+)[0-9]+ \ [^Ee\.] { \(_, _, s) l -> return [TokSignedInt . fst . fromJust . LBSC.readInt $ s] }
  [0-9]+ \ [^Ee\.] { \(_, _, s) l -> return [TokInt . fst . fromJust . LBSC.readInt $ s] }
  (\-|\+)?[0-9]+(\.[0-9]+)?((E|e)(\+|\-)?[0-9]+)? {
    \(_, _, s) l -> return [TokReal . read . LBSC.unpack . LBS.take (fromIntegral l) $ s] }
  \~ / [^\~ \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|] { returnP TokTilde }
  [A-Za-z][A-Za-z0-9_']* { \(_, _, s) l -> return [NamedSymbol (LBS.take (fromIntegral l) s)] }
  [ \~ \` \! \@ \$ \% \^ \& \* \- \+ \= \< \> \? \|]+ { \(_, _, s) l -> return [NamedSymbol (LBS.take (fromIntegral l) s)] }

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

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1) l (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1) 1
alexMove (AlexPn a l c) _ = AlexPn (a+1) l (c+1)

data AlexState = AlexState {
        alex_pos :: !AlexPosn, -- position at current input location
        alex_inp :: LBS.ByteString, -- the current input
        alex_chr :: !Char, -- the character before the input
        alex_scd :: !Int, -- the current startcode
        alex_block_indent :: [Int], -- block indent levels (with an infinite tail of zeros).
        alex_token_queue :: [Token] -- any tokens that were generated and need to be consumed.
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: LBS.ByteString -> Alex a -> Either String a
runAlex input (Alex f)
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_scd = 0,
                        alex_block_indent = repeat 0,
                        alex_token_queue = []
                       }) of
       Left msg -> Left msg
       Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Monad Alex where
  m >>= k = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

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
alexGetBlockIndent = Alex $ \(s@AlexState{alex_block_indent=abi}) -> Right (s, head abi)

alexPopBlockIndent :: Alex ()
alexPopBlockIndent = Alex $ \(s@AlexState{alex_block_indent=abi}) -> Right (s{alex_block_indent=tail abi}, ())

alexPushBlockIndent :: Int -> Alex ()
alexPushBlockIndent h = Alex $ \(s@AlexState{alex_block_indent=abi}) -> Right (s{alex_block_indent=h:abi}, ())

alexPutTokenQueue :: [Token] -> Alex ()
alexPutTokenQueue tqend = Alex $
  \(s@AlexState{alex_token_queue=tq}) -> Right (s{alex_token_queue=tq++tqend},())

alexPopTokenQueue :: Alex (Maybe Token)
alexPopTokenQueue = Alex $
  \(s@AlexState{alex_token_queue=tq}) -> case tq of
    [] -> Right (s,Nothing)
    (h:t) -> Right (s{alex_token_queue=t}, Just h)

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  mt <- alexPopTokenQueue
  case mt of
    Just t -> return t
    Nothing -> do
      case (alexScan inp sc) :: (AlexReturn (AlexInput -> Int -> Alex [Token])) of
        AlexEOF -> return TokCloseBlock
        AlexError inp' -> alexError "lexical error"
        AlexSkip inp' len -> do
          alexSetInput inp'
          alexMonadScan
        AlexToken inp' len action -> do
          alexSetInput inp'
          r <- action inp len
          case r of
            (h:[]) -> return h
            [] -> alexMonadScan
            (h:t) -> alexPutTokenQueue t >> return h

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

data Token = NamedSymbol LBS.ByteString |
             TokAppend | TokAs |
             TokCase | TokClone |
             TokConnect | TokDimensionless |
             TokDomain | TokFrom |
             TokHeadSep | -- ^ The sequence =>
             TokHiding |
             TokImport |
             TokLookup |
             TokNamespace |
             TokOf |
             TokRightArrow | -- ^ The sequence ->
             TokSubset |
             TokUnit |
             TokUsing |
             TokWhere |
             TokEqual |
             TokComma |
             TokTilde |
             TokOpenBracket |
             TokCloseBracket |
             TokOpenSqBracket |
             TokCloseSqBracket |
             TokOpenCurlyBracket |
             TokCloseCurlyBracket |
             -- | Hit when a token is encountered at a lower level of indent
             --   than required for the current block (including at EOF)
             TokCloseBlock |
             TokReal Double |
             TokSignedInt Int |
             TokInt Int
    deriving (Eq, Ord, Data, Show, Typeable)

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,bs) = (p,c,bs)

returnP x = \_ _ -> return [x]
}