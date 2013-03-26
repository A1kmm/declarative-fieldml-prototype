{
{-# LANGUAGE OverloadedStrings #-}
module Data.FieldML.Parser (parseFieldML) where
import Data.FieldML.LexicalAnalyser
import Data.FieldML.Level1Structure
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
}
%monad { Alex }
%lexer { alexWithContinuation } { TokEOF _ }
%tokentype { Token }
%token Append { TokAppend $$ }
       As { TokAs $$ }
       Case { TokCase $$ }
       Class { TokClass $$ }
       Clone { TokClone $$ }
       Closed { TokClosed $$ }
       Colon { TokColon $$ }
       Connect { TokConnect $$ }
       Dimensionless { TokDimensionless $$ }
       Domain { TokDomain $$ }
       DomainList { TokDomainList $$ }
       Ensemble { TokEnsemble $$ }
       FCase { TokFCase $$ }
       From { TokFrom $$ }
       HeadSep { TokHeadSep $$ }
       Hiding { TokHiding $$ }
       Import { TokImport $$ }
       Instance { TokInstance $$ }
       Let { TokLet $$ }
       Lookup { TokLookup $$ }
       Val { TokVal $$ }
       Namespace { TokNamespace $$ }
       Newbase { TokNewbase $$ }
       PathSep { TokPathSep $$ }
       RightArrow { TokRightArrow $$ }
       Subset { TokSubset $$ }
       Unit { TokUnit $$ }
       Using { TokUsing $$ }
       Where { TokWhere $$ }
       CloseBracket { TokCloseBracket $$ }
       CloseCurlyBracket { TokCloseCurlyBracket $$ }
       CloseProductBracket { TokCloseProductBracket $$ }
       CloseSqBracket { TokCloseSqBracket $$ }
       Comma { TokComma $$ }
       Equal { TokEqual $$ }
       Of { TokOf $$ }
       OpenBracket { TokOpenBracket $$ }
       OpenCurlyBracket { TokOpenCurlyBracket $$ }
       OpenProductBracket { TokOpenProductBracket $$ }
       OpenSqBracket { TokOpenSqBracket $$ }
       Pipe { TokPipe $$ }
       R { TokR $$ }
       Slash { TokSlash $$ }
       ForwardSlash { TokForwardSlash $$ }
       Tilde { TokTilde $$ }
       Underscore { TokUnderscore $$ }
       Int { TokInt $$ }
       NamedSymbol { TokNamedSymbol $$ }
       Real { TokReal $$ }
       ScopedSymbol { TokScopedSymbol $$ }
       SignedInt { TokSignedInt $$ }
       String { TokString $$ }
       CloseBlock { TokCloseBlock $$ }

%left lowerEmpty
%left lowerSep
%left HeadSep
%left expressionSig
%left expressionCombineLambda
%left PathSep
%left expressionCombine OpenCurlyBracket OpenProductBracket As R Slash ScopedSymbol Where Int SignedInt Append Case FCase Lookup String
%left ForwardSlash
%left Comma Pipe
%left OpenBracket
%left preferOpenSqBracket
%left OpenSqBracket
%left kindSpec RightArrow
%left NamedSymbol expression
%right unitExpression
%right unitExprEnding
%left highestEmpty
%left highestSep

%name happyParseFieldML
%error { happyError }
%%

main : namespaceContents { $1 }

namespaceContents
  : many(namespaceStatement) { L1NamespaceContents $1 }

namespaceStatement
  : startBlock(Import) maybe(fromURL) relOrAbsPath maybe(identList) maybe(hidingList) maybe(asId) closeBlock {
      L1NSImport (twoPosToSpan $1 $7) $2 $3 $4 $5 $6
    }
  | startBlock(Namespace) identifier Where namespaceContents closeBlock {
      L1NSNamespace (twoPosToSpan $1 $5) $2 $4
    }
  | startBlock(Domain) identifier Equal domainDefinition orEmptyNSContents(whereNamespaceContents) closeBlock {
      L1NSDomain (twoPosToSpan $1 $6) $2 $4 $5
    }
  | startBlock(Let) expression closeBlock { L1NSAssertion (twoPosToSpan $1 $3) $2 }
  | startBlock(Val) identifier maybe(domainTypeAnnotation) closeBlock { L1NSNamedValue (twoPosToSpan $1 $4) $2 $3 }
  | startBlock(Class) identifier classParameters maybe(classContents) closeBlock {
      L1NSClass (twoPosToSpan $1 $5) $2 $3 (maybe [] fst $4) (maybe [] snd $4)
    }
  | startBlock(Ensemble) OpenCurlyBracket sepBy1(identifier, Comma) CloseCurlyBracket maybe(asId) closeBlock {
      L1NSEnsemble (twoPosToSpan $1 $6) $3 $5
    }
  | startBlock(Unit) identifier Equal unitDefinition closeBlock { L1NSUnit (twoPosToSpan $1 $5) $2 $4 }
  | startBlock(Instance) relOrAbsPath OpenBracket sepBy(domainExpression,Comma) CloseBracket maybe(instanceContents) closeBlock {
      L1NSInstance (twoPosToSpan $1 $7) $2 $4 (maybe [] fst $6) (maybe [] snd $6)
    }

classParameters : OpenBracket sepBy(classParameter,Comma) CloseBracket { $2 }
classParameter :: { (L1ScopedID, L1Kind) } : scopedId maybe(kindAnnotation) { ($1, fromMaybe (L1Kind [] []) $2) }

kindAnnotation
  :: { L1Kind }
  : PathSep OpenBracket kindSpec CloseBracket { $3 }
kindSpec
  :: { L1Kind }
  : maybe(kindDomainList) maybe(kindUnitList) { L1Kind (fromMaybe [] $1) (fromMaybe [] $2) }
kindDomainList :: { [(L1ScopedID, L1Kind)] } : Domain sepBy1(kindDomain, Comma) { $2 }
kindUnitList :: { [L1ScopedID] } : Unit sepBy1(scopedId,Comma) { $2 }
kindDomain :: { (L1ScopedID, L1Kind) } : scopedId kindAnnotation { ($1, $2) }

classContents : Where classDomainFunctions classValues { ($2, $3) }
classDomainFunctions : many(classDomainFunction) { $1 }
classDomainFunction : startBlock(Domain) identifier OpenBracket sepBy1(scopedId,Comma) CloseBracket closeBlock {
   ($2, length $4)
  }
classValues : many(classValue) { $1 }
classValue : identifier domainTypeAnnotation { ($1, $2) }

instanceContents : Where many(instanceDomainFunction) many(instanceValue) { ($2, $3) }
instanceDomainFunction : startBlock(Domain) identifier OpenBracket sepBy1(domainExpression, Comma) CloseBracket Equal domainExpression closeBlock {
    ($2, $4, $7)
  }
instanceValue : startBlock(Let) expression closeBlock { $2 }
domainTypeAnnotation : PathSep domainExpression { $2 }

fromURL : From String { snd $2 }
identList :: { [L1Identifier] }
          : OpenBracket sepBy(identifier,Comma) CloseBracket { $2 }
hidingList : Hiding identList { $2 }
whereNamespaceContents : Where namespaceContents { $2 }

relOrAbsPath : Slash relPath0 { L1RelOrAbsPath (twoPosToSpan (alexPosToSrcPoint $1) (l1RelPathSS $2)) True $2 }
             | relPath { L1RelOrAbsPath (l1RelPathSS $1) False $1 }
relPath0 : {- empty -} {% do
                           (pos, _, _) <- alexGetInput                        
                           return $ L1RelPath (alexPosToSrcPoint pos) []
                       }
  | PathSep sepBy1(identifier, PathSep) %prec highestSep { L1RelPath (twoPosToSpan (alexPosToSrcPoint $1) (l1IdSS (last $2))) $2 }
relPath : sepBy1(identifier,PathSep) { L1RelPath (twoPosToSpan (l1IdSS $ head $1) (l1IdSS $ last $1)) $1 }

relOrAbsPathPossiblyIntEnd
  :: { L1RelOrAbsPathPossiblyIntEnd }
  : Slash relPath0PossiblyIntEndRev %prec lowerSep {
      case $2 of
        Left v -> L1RelOrAbsPathNoInt (if null v then alexPosToSrcPoint $1
                                                 else twoPosToSpan (alexPosToSrcPoint $1)
                                                                   (l1IdSS $ head v))
                                      True (L1RelPath (SrcSpan "-" 0 0 0 0) $ reverse v)
        Right (v, (ss, i)) ->
          L1RelOrAbsPathInt (twoPosToSpan (alexPosToSrcPoint $1)
                                          ss) True (L1RelPath (SrcSpan "-" 0 0 0 0) $ reverse v) i
     }
  | relPathPossiblyIntEndRev %prec lowerSep {
    case $1 of
      Left v -> L1RelOrAbsPathNoInt (twoPosToSpan (l1IdSS $ last v) (l1IdSS $ head v))
                                    False (L1RelPath (SrcSpan "-" 0 0 0 0) (reverse v))
      Right (v, (ss, i)) -> L1RelOrAbsPathInt (if null v then ss else twoPosToSpan (l1IdSS $ last v) ss)
                                              False (L1RelPath (SrcSpan "-" 0 0 0 0) (reverse v)) i
    }
relPath0PossiblyIntEndRev
  :: { Either [L1Identifier] ([L1Identifier], (SrcSpan, Int)) }
  : PathSep relPathPossiblyIntEndRev %prec highestSep { $2 }
  | {- empty -} %prec lowerSep { Left [] }
relPathPossiblyIntEndRev : relPathPossiblyIntEndRev PathSep identifierOrInteger %prec highestSep {%
  case ($1, $3) of
    (Right _, _) -> happyError (TokPathSep $2)
    (Left vl, Left v)  -> return $ Left (v:vl)
    (Left vl, Right i) -> return $ Right (vl, i)
                                                                                }
  | identifierOrInteger { either (Left . (:[])) (\i -> Right ([], i)) $1 }
identifierOrInteger : identifier { Left $1 }
                    | integer { Right $1 }

identifier : NamedSymbol { L1Identifier (alexPosToSrcPoint $ fst $1) (snd $1) }
scopedId : ScopedSymbol { L1ScopedID (alexPosToSrcPoint $ fst $1) (snd $1) }
asId : As identifier { $2 }

domainDefinition : Clone domainExpression { L1CloneDomain (alexPosToSrcPoint $1) $2 }
                 | startBlock(Subset) domainExpression Using expression closeBlock { L1SubsetDomain (twoPosToSpan $1 $5) $2 $4 }
                 | startBlock(Connect) domainExpression Using expression closeBlock { L1ConnectDomain (twoPosToSpan $1 $5) $2 $4 }
                 | domainExpression { L1DomainDefDomainType (l1DomainExpressionSS $1) $1 }

classExpression : relOrAbsPath {
    L1ClassExpressionReference ((\(L1RelOrAbsPath ss _ _) -> ss) $1) $1
  } | startBlock(OpenBracket) Underscore Pipe labelledDomains(Pipe) CloseBracket closeBlock {
    L1ClassExpressionOpenDisjointUnion (twoPosToSpan $1 $6) $4
  } | startBlock(DomainList) OpenBracket sepBy(classExpression,Comma) CloseBracket closeBlock {
    L1ClassExpressionList (twoPosToSpan $1 $5) $3
  }

domainHeadMember : Unit unitExpression domainHeadUnit {% $3 $2 }
                    | Domain scopedId kindAnnotation { L1DHMScopedDomain $2 $3 }
                    | Class classExpression OpenBracket sepBy(domainExpression,Comma) CloseBracket { L1DHMRelation $2 $4 }
                    | domainExpression Tilde domainExpression { L1DHMEquality $1 $3 }

domainHeadUnit : Tilde unitExpression {
       \uex -> return (L1DHMUnitConstraint uex $2)
    } | {- empty -} {
        \uex -> case uex of
                   L1UnitScopedVar _ sv -> return (L1DHMScopedUnit sv)
                   _ -> fail $ "Malformed domain head unit; expected either "
    }

domainExpression
  :: { L1DomainExpression }
  : OpenProductBracket labelledDomains(Comma) CloseProductBracket {
      L1DomainExpressionProduct (twoPosToSpan (alexPosToSrcPoint $1) (alexPosToSrcPoint $3)) $2 
    }
  | OpenBracket domainExpression bracketDomainExpression CloseBracket {%
      $3 $2 (twoPosToSpan (alexPosToSrcPoint $1) (alexPosToSrcPoint $4))
                                                                      }
| OpenSqBracket sepBy(domainHeadMember,Comma) CloseSqBracket HeadSep domainExpression {
  L1DomainExpressionLambda (twoPosToSpan (alexPosToSrcPoint $1) (l1DomainExpressionSS $5)) $2 $5
    }
  | domainExpression RightArrow domainExpression { L1DomainExpressionFieldSignature (twoPosToSpan (l1DomainExpressionSS $1) (l1DomainExpressionSS $3))
                                                                                    $1 $3 }
  | domainExpression OpenSqBracket sepBy1(domainApplyArg,Comma) CloseSqBracket %prec OpenSqBracket {
    let ss = twoPosToSpan (l1DomainExpressionSS $1) (alexPosToSrcPoint $4) in
      L1DomainExpressionApply ss $3 $1
    }
  | R maybeBracketedUnits {
      L1DomainExpressionReal (alexPosToSrcPoint $1) $2
    }
  | relOrAbsPathPossiblyIntEnd domainExprStartsWithPath {% $2 $1 }
  | scopedId { L1DomainVariableRef (l1ScopedIdSS $1) $1 }

domainExprStartsWithPath
  : OpenBracket sepBy1(domainExpression,Comma) CloseBracket %prec highestSep {
      \path' -> case path' of
        L1RelOrAbsPathNoInt ss ra p -> return $ L1DomainFunctionEvaluate (twoPosToSpan ss (alexPosToSrcPoint $3))
                                                                         (L1RelOrAbsPath ss ra p) $2
        L1RelOrAbsPathInt ss _ _ _ -> fail $ "Unexpected number label at " ++ show ss
    }
  | {- empty -} %prec lowerSep { \path -> return $ L1DomainReference (l1RelOrAbsPIESS path) path }

maybeBracketedUnits : bracketedUnits %prec OpenSqBracket { $1 }
                    | {- empty -} %prec preferOpenSqBracket { L1UnitExDimensionless (SrcSpan "built-in" 0 0 0 0) }
bracketedUnits : OpenSqBracket unitExpression CloseSqBracket { $2 }
domainApplyArg :: { (L1ScopedID, Either L1DomainExpression L1UnitExpression) }
               : Unit scopedId Equal unitExpression { ($2, Right $4)}
               | scopedId Equal domainExpression { ($1, Left $3) }

bracketDomainExpression : {- empty -} { \ex _ -> return ex } -- Just a bracketed expression.
                        | Colon domainExpression Pipe labelledDomains(Pipe) {\shouldBeLabel ss ->
                            -- We parse the more general case and fail if it doesn't make sense...
                            let L1LabelledDomains lTail = $4 in
                              case shouldBeLabel of
                                (L1DomainReference _ label) -> return $
                                   L1DomainExpressionDisjointUnion ss
                                         (L1LabelledDomains ((label, $2):lTail))
                                _ -> happyError (TokColon $1)
                          }
                        | Pipe labelledDomains(Pipe) {\shouldBeLabel ss ->
                            let L1LabelledDomains lTail = $2 in
                              case shouldBeLabel of
                                (L1DomainReference _ label) -> return $
                                   L1DomainExpressionDisjointUnion ss (L1LabelledDomains ((label, shouldBeLabel):lTail))
                                _ -> happyError (TokPipe $1)
                                               }

labelledDomains(sep) :: { L1LabelledDomains }
                     : sepBy(labelledDomain,sep) { L1LabelledDomains $1 }
labelledDomain :: { (L1RelOrAbsPathPossiblyIntEnd, L1DomainExpression) }
               : relOrAbsPathPossiblyIntEnd Colon domainExpression { ($1, $3) }
               | relOrAbsPathPossiblyIntEnd { ($1, L1DomainReference (l1RelOrAbsPIESS $1) $1) }

unitExpression
  : Dimensionless { L1UnitExDimensionless (alexPosToSrcPoint $1) }
  | relOrAbsPath { L1UnitExRef ((\(L1RelOrAbsPath ss _ _) -> ss) $1) $1 }
  | double NamedSymbol unitExpression %prec unitExpression {%
     do
       (when (snd $2 /= "*") . fail $ "Expected * " ++ " at " ++ (show $3))
       return $ L1UnitScalarMup (twoPosToSpan (fst $1) (l1UnitExSS $3)) (snd $1) $3
                                                           }
  | unitExpression NamedSymbol unitExprEnding {%
     case $3 of
       (Left ex) | snd $2 == "*" -> return $ L1UnitExTimes (twoPosToSpan (l1UnitExSS $1) (l1UnitExSS ex)) $1 ex
       (Right (ss, d)) | snd $2 == "**" -> return $ L1UnitPow (twoPosToSpan (l1UnitExSS $1) ss) $1 d
       otherwise -> happyError (TokNamedSymbol $2)
                                              }
  | scopedId { L1UnitScopedVar (l1ScopedIdSS $1) $1 }

unitExprEnding : double %prec highestSep { Right (fst $1, snd $1) }
               | unitExpression %prec unitExprEnding { Left $1 }
double : SignedInt { (alexPosToSrcPoint $ fst $1, fromIntegral (snd $1)) }
       | Int { (alexPosToSrcPoint $ fst $1, fromIntegral (snd $1)) }
       | Real { (alexPosToSrcPoint $ fst $1, snd $1) }
integer : SignedInt { (alexPosToSrcPoint $ fst $1, fromIntegral (snd $1)) }
        | Int { (alexPosToSrcPoint $ fst $1, fromIntegral (snd $1)) }

expression
  :: { L1Expression }
  : expression applyOrWhereOrAs %prec expressionCombine { $2 $1 }
  | relOrAbsPathPossiblyIntEnd %prec expressionCombine {
      L1ExReference (l1RelOrAbsPIESS $1) $1
    }
  | scopedId %prec expressionCombine { L1ExBoundVariable (l1ScopedIdSS $1) $1 }
  | OpenBracket expression CloseBracket { $2 }
  | R maybe(bracketedUnits) PathSep double %prec expressionCombine {
      L1ExLiteralReal (twoPosToSpan (alexPosToSrcPoint $1) (fst $4))
                      (fromMaybe (L1UnitExDimensionless (alexPosToSrcPoint $1)) $2) (snd $4) }
  | OpenProductBracket sepBy(labelledExpression,Comma) CloseProductBracket %prec expressionCombine {
      L1ExMkProduct (twoPosToSpan (alexPosToSrcPoint $1) (alexPosToSrcPoint $3)) $2
     }
  | Lookup relOrAbsPathPossiblyIntEnd %prec expressionCombine { 
      L1ExProject (twoPosToSpan (alexPosToSrcPoint $1)
                  (l1RelOrAbsPIESS $2)) $2
     }
  | Append relOrAbsPathPossiblyIntEnd %prec expressionCombine {
      L1ExAppend (twoPosToSpan (alexPosToSrcPoint $1)
                               (l1RelOrAbsPIESS $2)) $2
     }
  | ForwardSlash many(pattern) RightArrow expression %prec expressionCombineLambda {
      let ss = twoPosToSpan (alexPosToSrcPoint $1) (l1ExSS $4)
        in foldl' (\ex sv -> L1ExLambda ss sv ex) $4 $2
    }
  | startBlock(Case) expression possibleClosed Of many(expressionCase) closeBlock {% do
      let ss = (twoPosToSpan $1 $6)
      caseContents <- if $3 then liftM Left (mapM (patternToClosedUnionMember ss) $5)
                            else return (Right $5)
      return $ L1ExCase (twoPosToSpan $1 $6) $2 caseContents
    }
  | startBlock(FCase) possibleClosed many(expressionCase) closeBlock {% do
      let ss = (twoPosToSpan $1 $4)
      caseContents <- if $2 then liftM Left (mapM (patternToClosedUnionMember ss) $3)
                            else return (Right $3)
      return $ L1ExFCase (twoPosToSpan $1 $4) $2 caseContents
    }
  | String {
      L1ExString (alexPosToSrcPoint $ fst $1) (snd $1)
    }
  | expression PathSep domainExpression %prec expressionSig {
      L1ExSignature (twoPosToSpan (l1ExSS $1) (l1DomainExpressionSS $3)) $1 $3
    }

possibleClosed :: { Bool }
possibleClosed : {- empty -} { False }
               | Closed { True }

pattern :: { L1Pattern }
        : Underscore { L1PatternIgnore (alexPosToSrcPoint $1) }
        | scopedId { L1PatternBind (l1ScopedIdSS $1) $1 }
        | pattern As relOrAbsPathPossiblyIntEnd {
            L1PatternAs (twoPosToSpan (l1PatternSS $1) (l1RelOrAbsPIESS $3)) $3 $1
          }
        | relOrAbsPathPossiblyIntEnd {
          L1PatternAs (l1RelOrAbsPIESS $1) $1 (L1PatternIgnore (l1RelOrAbsPIESS $1))
          }
        | startBlock(OpenProductBracket) sepBy1(patternProductArg,Comma) CloseProductBracket closeBlock {
          L1PatternProduct (twoPosToSpan $1 $4) $2
          }
patternProductArg :: { (L1RelOrAbsPathPossiblyIntEnd, L1Pattern) }
                  : relOrAbsPathPossiblyIntEnd Colon pattern { ($1, $3) }

expressionCase : startBlockPattern RightArrow expression closeBlock {
    ($1, $3)
  }

applyOrWhereOrAs : Where namespaceContents %prec expressionCombine {
    \expr -> L1ExLet (twoPosToSpan (l1ExSS expr) (alexPosToSrcPoint $1)) expr $2 }
  | expression %prec expressionCombine { \expr -> L1ExApply (twoPosToSpan (l1ExSS expr) (l1ExSS $1)) expr $1 }
  | As relOrAbsPathPossiblyIntEnd %prec expressionCombine { \expr -> L1ExMkUnion (twoPosToSpan (l1ExSS expr) (l1RelOrAbsPIESS $2)) $2 expr }

unitDefinition :: { L1UnitDefinition }
  : Newbase { L1UnitDefNewBase (alexPosToSrcPoint $1) }
  | unitExpression { L1UnitDefUnitExpr (l1UnitExSS $1) $1 }

labelledExpression
  : relOrAbsPathPossiblyIntEnd Colon expression { ($1, $3) }

startBlock(t) : t {% do
                      let (AlexPn _ _ _ col) = $1
                      alexPushBlockIndent (col + 1)
                      return $ alexPosToSrcPoint $1
                  }
startBlockPattern
  : pattern {%
      do
        let col = srcStartColumn (l1PatternSS $1)
        alexPushBlockIndent (col + 1)
        return $1
                               }
closeBlock : CloseBlock { alexPosToSrcPoint $1 }

maybe(x) : x           { Just $1 }
         | {- empty -} { Nothing }

many(x) : manyRev(x) { reverse $1 }
manyRev(x) : manyRev(x) x { $2:$1 }
           | {- empty -} %prec lowerSep { [] }

sepBy(x,sep) : orEmpty(sepBy1(x,sep)) { $1 }
sepBy1(x,sep) : sepBy1Rev(x,sep) { reverse ($1) }
sepBy1Rev(x,sep) : sepBy1Rev(x,sep) sep x %prec highestSep { $3:$1 }
                 | x %prec lowerSep { [$1] }
orEmpty(x) : x %prec highestEmpty { $1 }
           | {- empty -} %prec lowerEmpty { [] }
orEmptyNSContents(x) : x { $1 }
                     | {- empty -} { L1NamespaceContents [] }

{
twoPosToSpan :: SrcSpan -> SrcSpan -> SrcSpan
twoPosToSpan (SrcSpan { srcFile = f, srcStartRow = r1, srcStartColumn = c1 }) (SrcSpan { srcEndRow = r2, srcEndColumn = c2 }) =
  SrcSpan { srcFile = f, srcStartRow = r1, srcStartColumn = c1, srcEndRow = r2, srcEndColumn = c2 }

happyError failTok = do
  (pn, _, _) <- alexGetInput
  fail $ "Parse error; unexpected token " ++ (show failTok) ++ ", at " ++ (show pn)

parseFieldML :: String -> LBS.ByteString -> Either String L1NamespaceContents
parseFieldML srcFile bs = runAlex srcFile bs happyParseFieldML

alexPosToSrcPoint (AlexPn fn _ row col) = SrcSpan { srcFile = fn,
                                                    srcStartRow = row,
                                                    srcStartColumn = col,
                                                    srcEndRow = row,
                                                    srcEndColumn = col
  }
                                          
patternToClosedUnionMember :: Monad m => SrcSpan -> (L1Pattern, L1Expression) -> m (L1RelOrAbsPathPossiblyIntEnd,
                                                                                    L1Expression)
patternToClosedUnionMember ss (L1PatternAs ssP label (L1PatternBind ssB scopedId), ex) =
  return (label, L1ExLambda ssP (L1PatternBind ssB scopedId) ex)
patternToClosedUnionMember ss _ =
  fail ("Case with closed keyword can only contain members in the form \"label as _bvar\", at " ++ show ss)
}
