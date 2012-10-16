{
{-# LANGUAGE OverloadedStrings #-}
module Data.FieldML.Parser (parseFieldML) where
import Data.FieldML.LexicalAnalyser
import Data.FieldML.Level1Structure
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy as LBS
}
%monad { Alex }
%lexer { alexWithContinuation } { TokEOF }
%tokentype { Token }
%token Append { TokAppend }
       As { TokAs }
       Case { TokCase }
       Class { TokClass }
       Clone { TokClone }
       Colon { TokColon }
       Connect { TokConnect }
       Dimensionless { TokDimensionless }
       Domain { TokDomain }
       Ensemble { TokEnsemble }
       From { TokFrom }
       HeadSep { TokHeadSep }
       Hiding { TokHiding }
       Import { TokImport }
       Instance { TokInstance }
       Let { TokLet }
       Lookup { TokLookup }
       My { TokMy }
       Namespace { TokNamespace }
       Newbase { TokNewbase }
       PathSep { TokPathSep }
       RightArrow { TokRightArrow }
       Subset { TokSubset }
       Unit { TokUnit }
       Using { TokUsing }
       Where { TokWhere }
       CloseBracket { TokCloseBracket }
       CloseCurlyBracket { TokCloseCurlyBracket }
       CloseSqBracket { TokCloseSqBracket }
       Comma { TokComma }
       Equal { TokEqual }
       OpenBracket { TokOpenBracket }
       OpenCurlyBracket { TokOpenCurlyBracket }
       OpenSqBracket { TokOpenSqBracket }
       Pipe { TokPipe }
       R { TokR }
       Slash { TokSlash }
       ForwardSlash { TokForwardSlash }
       Tilde { TokTilde }
       Int { TokInt $$ }
       NamedSymbol { TokNamedSymbol $$ }
       Real { TokReal $$ }
       ScopedSymbol { TokScopedSymbol $$ }
       SignedInt { TokSignedInt $$ }
       String { TokString $$ }
       CloseBlock { TokCloseBlock }

%left lowerEmpty
%left lowerSep
%left expressionCombine OpenCurlyBracket As R Slash ScopedSymbol Where Int SignedInt Append Case Lookup
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
%left getPos

%name happyParseFieldML
%error { happyError }
%%

main : namespaceContents { $1 }

namespaceContents : many(namespaceStatement) { L1NamespaceContents $1 }

namespaceStatement : Import startBlock maybe(fromURL) relOrAbsPath maybe(identList) maybe(hidingList) maybe(asId) closeBlock {
    L1NSImport (twoPosToSpan $2 $8) $3 $4 $5 $6 $7
  }
                   | Namespace startBlock identifier Where namespaceContents closeBlock {
    L1NSNamespace (twoPosToSpan $2 $6) $3 $5
  }
                   | Domain startBlock identifier Equal domainDefinition orEmptyNSContents(whereNamespaceContents) closeBlock {
    L1NSDomain (twoPosToSpan $2 $7) $3 $5 $6
  }
                   | Let startBlock expression closeBlock { L1NSAssertion $2 $3 }
                   | My startBlock identifier maybe(domainTypeAnnotation) closeBlock { L1NSNamedValue $2 $3 $4 }
                   | Class startBlock identifier classParameters maybe(classContents) closeBlock {
                       L1NSClass $2 $3 $4 (maybe [] fst $5) (maybe [] snd $5)
                                                                                                          }
                   | Ensemble startBlock OpenCurlyBracket sepBy1(identifier, Comma) CloseCurlyBracket maybe(asId) closeBlock {
                       L1NSEnsemble $2 $4 $6
                       }
                   | Unit startBlock unitDefinition closeBlock { L1NSUnit $2 $3 }
                   | Instance startBlock relOrAbsPath OpenBracket sepBy(domainType,Comma) CloseBracket maybe(instanceContents) closeBlock {
                       L1NSInstance (twoPosToSpan $2 $8) $3 $5 (maybe [] fst $7) (maybe [] snd $7)
                     }

classParameters : OpenBracket sepBy(classParameter,Comma) CloseBracket { $2 }
classParameter : scopedId maybe(kindAnnotation) { ($1, fromMaybe (L1Kind []) $2) }

kindAnnotation : PathSep kindSpec { $2 }
kindSpec
  : NamedSymbol %prec kindSpec
       {% if $1 == "*"
            then return (L1Kind [])
            else happyError (TokNamedSymbol $1)
       }
  | OpenBracket kindSpec CloseBracket %prec kindSpec { $2 }
  | kindSpec RightArrow kindSpec %prec kindSpec { (\(L1Kind k) -> L1Kind ($1:k)) $3 }

classContents : Where classDomainFunctions classValues { ($2, $3) }
classDomainFunctions : many(classDomainFunction) { $1 }
classDomainFunction : Domain startBlock identifier OpenBracket sepBy1(scopedId,Comma) CloseBracket closeBlock {
   ($3, length $5)
  }
classValues : many(classValue) { $1 }
classValue : identifier domainTypeAnnotation { ($1, $2) }

instanceContents : Where many(instanceDomainFunction) many(instanceValue) { ($2, $3) }
instanceDomainFunction : Domain startBlock identifier OpenBracket sepBy1(domainType, Comma) CloseBracket Equal domainExpression closeBlock {
    ($3, $5, $8)
  }
instanceValue : Let startBlock expression closeBlock { $3 }
domainTypeAnnotation : PathSep domainType { $2 }

fromURL : From String { $2 }
identList :: { [L1Identifier] }
          : OpenBracket sepBy(identifier,Comma) CloseBracket { $2 }
hidingList : Hiding identList { $2 }
whereNamespaceContents : Where namespaceContents { $2 }

relOrAbsPath : Slash relPath0 { L1RelOrAbsPath True $2 }
             | relPath { L1RelOrAbsPath False $1 }
relPath0 : {- empty -} { L1RelPath []}
  | PathSep sepBy1(identifier, PathSep) { L1RelPath $2 }
relPath : sepBy1(identifier,PathSep) { L1RelPath $1 }

relOrAbsPathPossiblyIntEnd
  :: { L1RelOrAbsPathPossiblyIntEnd }
  : Slash relPath0PossiblyIntEndRev {
    case $2 of
       Left v -> L1RelOrAbsPathNoInt True (L1RelPath $ reverse v)
       Right (v, i) -> L1RelOrAbsPathInt True (L1RelPath $ reverse v) i }
  | relPathPossiblyIntEndRev {
    case $1 of
      Left v -> L1RelOrAbsPathNoInt False (L1RelPath . reverse $  v)
      Right (v, i) -> L1RelOrAbsPathInt False (L1RelPath . reverse $ v) i
    }
relPath0PossiblyIntEndRev
  :: { Either [L1Identifier] ([L1Identifier], Int) }
  : PathSep relPathPossiblyIntEndRev { $2 }
  | {- empty -} { Left [] }
relPathPossiblyIntEndRev : relPathPossiblyIntEndRev PathSep identifierOrInteger {%
  case ($1, $3) of
    (Right _, _) -> happyError TokPathSep
    (Left vl, Left v)  -> return $ Left (v:vl)
    (Left vl, Right i) -> return $ Right (vl, i)
                                                                                }
  | identifierOrInteger { either (Left . (:[])) (\i -> Right ([], i)) $1 }
identifierOrInteger : identifier { Left $1 }
                    | integer { Right $1 }

identifier : NamedSymbol { L1Identifier $1 }
scopedId : ScopedSymbol { L1ScopedID $1 }
asId : As identifier { $2 }

domainDefinition : Clone getPos domainType getPos { L1CloneDomain (twoPosToSpan $2 $4) $3 }
                 | Subset startBlock getPos domainType Using expression getPos closeBlock { L1SubsetDomain (twoPosToSpan $3 $7) $4 $6 }
                 | Connect startBlock getPos domainType Using expression getPos closeBlock { L1ConnectDomain (twoPosToSpan $3 $7) $4 $6 }
                 | domainType getPos { L1DomainDefDomainType $2 $1 }

domainType : getPos orEmpty(domainHead) domainExpression getPos { L1DomainType (twoPosToSpan $1 $4) $2 $3 }
domainHead : OpenSqBracket sepBy(domainClassRelation,Comma) CloseSqBracket HeadSep { $2 }
domainClassRelation : Unit unitExpression Tilde unitExpression { L1DCRUnitConstraint $2 $4 }
                    | Class relOrAbsPath OpenBracket sepBy(domainExpression,Comma) CloseBracket { L1DCRRelation $2 $4 }
                    | domainExpression Tilde domainExpression { L1DCREquality $1 $3 }

domainExpression :: { L1DomainExpression }
                 : OpenCurlyBracket getPos labelledDomains(Comma) getPos CloseCurlyBracket { L1DomainExpressionProduct (twoPosToSpan $2 $4) $3 }
                 | OpenBracket getPos domainExpression bracketDomainExpression getPos CloseBracket {% $4 $3 (twoPosToSpan $2 $5) }
                 | domainExpression RightArrow getPos domainExpression { L1DomainExpressionFieldSignature $3 $1 $4 }
                 | domainExpression OpenSqBracket getPos sepBy1(domainApplyArg,Comma) CloseSqBracket getPos %prec OpenSqBracket {
                     let ss = twoPosToSpan $3 $6 in
                       foldl (\d (sv,ex) -> L1DomainExpressionApply ss d sv ex) $1 $4
                   }
                 | R maybeBracketedUnits getPos {
                   L1DomainExpressionReal $3 $2 }
                 | relOrAbsPathPossiblyIntEnd domainExprStartsWithPath getPos {% $2 $3 $1 }
                 | scopedId getPos { L1DomainVariableRef $2 $1 }

domainExprStartsWithPath
  : OpenBracket sepBy1(domainExpression,Comma) CloseBracket getPos {
      \startPos path' -> case path' of
                             L1RelOrAbsPathNoInt ra p -> return $ L1DomainFunctionEvaluate (twoPosToSpan startPos $4) (L1RelOrAbsPath ra p) $2
                             L1RelOrAbsPathInt _ _ _ -> fail $ "Unexpected number label at " ++ show $4
    }
  | {- empty -} { \startPos path -> return $ L1DomainReference startPos path }

maybeBracketedUnits : bracketedUnits %prec OpenSqBracket { $1 }
                    | {- empty -} %prec preferOpenSqBracket { L1UnitExDimensionless (SrcSpan "built-in" 0 0 0 0) }
bracketedUnits : OpenSqBracket unitExpression CloseSqBracket { $2 }
domainApplyArg :: { (L1ScopedID, L1DomainExpression) }
               : scopedId Equal domainExpression { ($1, $3) }

bracketDomainExpression : {- empty -} { \ex _ -> return ex } -- Just a bracketed expression.
                        | Colon domainExpression Pipe labelledDomains(Pipe) {\shouldBeLabel ss ->
                            -- We parse the more general case and fail if it doesn't make sense...
                            let (L1LabelledDomains lTail) = $4 in
                              case shouldBeLabel of
                                (L1DomainReference _ label) -> return $
                                   L1DomainExpressionDisjointUnion ss (L1LabelledDomains ((label, $2):lTail))
                                _ -> happyError TokColon
                          }
                        | Pipe labelledDomains(Pipe) {\shouldBeLabel ss ->
                            let (L1LabelledDomains lTail) = $2 in
                              case shouldBeLabel of
                                (L1DomainReference _ label) -> return $
                                   L1DomainExpressionDisjointUnion ss (L1LabelledDomains ((label, shouldBeLabel):lTail))
                                _ -> happyError TokPipe
                                               }

labelledDomains(sep) :: { L1LabelledDomains }
                     : sepBy(labelledDomain,sep) { L1LabelledDomains $1 }
labelledDomain :: { (L1RelOrAbsPathPossiblyIntEnd, L1DomainExpression) }
               : relOrAbsPathPossiblyIntEnd Colon domainExpression { ($1, $3) }
               | relOrAbsPathPossiblyIntEnd getPos { ($1, L1DomainReference $2 $1) }

unitExpression : Dimensionless getPos { L1UnitExDimensionless $2 }
               | relOrAbsPath getPos { L1UnitExRef $2 $1 }
               | double NamedSymbol getPos unitExpression %prec unitExpression {% do
                 (when ($2 /= "*") . fail $ "Expected * " ++ " at " ++ (show $3))
                 return $ L1UnitScalarMup $3 $1 $4
                                                          }
               | unitExpression NamedSymbol unitExprEnding getPos {%
                 case $3 of
                   (Left ex) | $2 == "*" -> return $ L1UnitExTimes $4 $1 ex
                   (Right d) | $2 == "**" -> return $ L1UnitPow $4 $1 d
                   otherwise -> happyError (TokNamedSymbol $2)
                 }
               | scopedId getPos { L1UnitScopedVar $2 $1 }

unitExprEnding : double %prec highestSep { Right $1 }
               | unitExpression %prec unitExprEnding { Left $1 }
double : SignedInt { fromIntegral $1 }
       | Int { fromIntegral $1 }
       | Real { $1 }
integer : SignedInt { fromIntegral $1 }
        | Int { fromIntegral $1 }

expression :: { L1Expression }
           : expression applyOrWhereOrAs %prec expressionCombine { $2 $1 }
           | relOrAbsPathPossiblyIntEnd getPos %prec expressionCombine {
               L1ExReference $2 $1
             }
           | scopedId getPos %prec expressionCombine { L1ExBoundVariable $2 $1 }
           | OpenBracket expression CloseBracket { $2 }
           | R getPos maybe(bracketedUnits) PathSep double %prec expressionCombine { L1ExLiteralReal $2 (fromMaybe (L1UnitExDimensionless $2) $3) $5 }
           | OpenCurlyBracket getPos sepBy(labelledExpression,Comma) CloseCurlyBracket getPos  %prec expressionCombine { L1ExMkProduct (twoPosToSpan $2 $5) $3 }
           | Lookup getPos relOrAbsPathPossiblyIntEnd %prec expressionCombine { L1ExProject $2 $3 }
           | Append getPos relOrAbsPathPossiblyIntEnd %prec expressionCombine { L1ExAppend $2 $3 }
           | ForwardSlash getPos many(scopedId) RightArrow expression %prec expressionCombine {
               foldl' (\ex sv -> L1ExLambda $2 sv ex) $5 $3
             }
           | Case startBlock expression many(expressionCase) closeBlock {
               L1ExCase $2 $3 $4
             }

expressionCase : relOrAbsPathPossiblyIntEnd RightArrow startBlock expression closeBlock {
    ($1, $4)
  }

applyOrWhereOrAs : Where getPos namespaceContents %prec expressionCombine { \expr -> L1ExLet $2 expr $3 }
  | expression getPos %prec expressionCombine { \expr -> L1ExApply $2 expr $1 }
  | As getPos relOrAbsPathPossiblyIntEnd %prec expressionCombine { \expr -> L1ExMkUnion $2 $3 expr }

unitDefinition :: { L1UnitDefinition }
  : Newbase getPos { L1UnitDefNewBase $2 }
  | unitExpression getPos { L1UnitDefUnitExpr $2 $1 }

labelledExpression : relOrAbsPathPossiblyIntEnd Colon expression { ($1, $3) }

getPos : {- empty -} %prec getPos {% (\((AlexPn _ row col):_) -> SrcSpan { srcFile = "input",
                                                                           srcStartRow = row,
                                                                           srcStartColumn = col,
                                                                           srcEndRow = row,
                                                                           srcEndColumn = col
                                                                         }) `liftM` alexGetLastPos }

startBlock : {- empty -} {% do
                               (_:(AlexPn _ row col):_) <- alexGetLastPos
                               alexPushBlockIndent (col + 1)
                               return $ SrcSpan { srcFile = "input",
                                                  srcStartRow = row,
                                                  srcStartColumn = col,
                                                  srcEndRow = row,
                                                  srcEndColumn = col
                                                }
                         }
closeBlock : getPos CloseBlock { $1}

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

parseFieldML :: LBS.ByteString -> Either String L1NamespaceContents
parseFieldML bs = runAlex bs happyParseFieldML
}
