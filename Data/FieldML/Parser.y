{
{-# LANGUAGE OverloadedStrings #-}
module Data.FieldML.Parser (parseFieldML) where
import Data.FieldML.LexicalAnalyser
import Data.FieldML.Level1Structure
import Control.Monad
import Data.Maybe
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
       Lookup { TokLookup }
       My { TokMy }
       Namespace { TokNamespace }
       Newbase { TokNewbase }
       Of { TokOf }
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
       Tilde { TokTilde }
       Int { TokInt $$ }
       NamedSymbol { TokNamedSymbol $$ }
       Real { TokReal $$ }
       ScopedSymbol { TokScopedSymbol $$ }
       SignedInt { TokSignedInt $$ }
       String { TokString $$ }
       CloseBlock { TokCloseBlock }
%left As
%left NamedSymbol expression
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

fromURL : From String { $2 }
identList :: { [L1Identifier] }
          : OpenBracket sepBy(identifier,Comma) CloseBracket { $2 }
hidingList : Hiding identList { $2 }
whereNamespaceContents : Where namespaceContents { $2 }

relOrAbsPath : PathSep relPath0 { L1RelOrAbsPath True $2 }
             | relPath { L1RelOrAbsPath False $1 }
relPath0 : sepBy(identifier,PathSep) { L1RelPath $1 }
relPath : sepBy1(identifier,PathSep) { L1RelPath $1 }

identifier : NamedSymbol { L1Identifier $1 }
scopedId : ScopedSymbol { L1ScopedID $1 }
asId : As identifier { $2 }

domainDefinition : Clone getPos domainType getPos { L1CloneDomain (twoPosToSpan $2 $4) $3 }
                 | Subset getPos domainType Using expression getPos { L1SubsetDomain (twoPosToSpan $2 $6) $3 $5 }
                 | Connect getPos domainType Using expression getPos { L1ConnectDomain (twoPosToSpan $2 $6) $3 $5 }
                 | getPos domainType getPos { L1DomainDefDomainType (twoPosToSpan $1 $3) $2 }

domainType : getPos orEmpty(domainHead) domainExpression getPos { L1DomainType (twoPosToSpan $1 $4) $2 $3 }
domainHead : OpenSqBracket many(domainClassRelation) CloseSqBracket HeadSep { $2 }
domainClassRelation : Unit unitExpression Tilde unitExpression { L1DCRUnitConstraint $2 $4 }
                    | Class relOrAbsPath many(domainExpression) { L1DCRRelation $2 $3 }
                    | domainExpression Tilde domainExpression { L1DCREquality $1 $3 }

domainExpression :: { L1DomainExpression }
                 : OpenSqBracket getPos labelledDomains(Comma) getPos CloseSqBracket { L1DomainExpressionProduct (twoPosToSpan $2 $4) $3 }
                 | OpenBracket getPos domainExpression bracketDomainExpression getPos CloseBracket {% $4 $3 (twoPosToSpan $2 $5) }
                 | domainExpression getPos RightArrow domainExpression getPos { L1DomainExpressionFieldSignature (twoPosToSpan $2 $5) $1 $4 }
                 | R getPos maybe(bracketedUnits) getPos {
                     L1DomainExpressionReal (twoPosToSpan $2 $4) (fromMaybe (L1UnitExDimensionless $2) $3) }
                 | domainExpression getPos OpenSqBracket sepBy1(domainApplyArg,Comma) CloseSqBracket getPos {
                     let ss = twoPosToSpan $2 $6 in
                       foldl (\d (sv,ex) -> L1DomainExpressionApply ss d sv ex) $1 $4
                   }
                 | relOrAbsPath getPos OpenBracket sepBy1(domainExpression,Comma) CloseBracket getPos {
                     L1DomainFunctionEvaluate (twoPosToSpan $2 $6) $1 $4
                   }
                 | scopedId getPos { L1DomainVariableRef $2 $1 }
                 | relOrAbsPath getPos { L1DomainReference $2 $1 }

bracketedUnits : OpenSqBracket unitExpression CloseSqBracket { $2 }
domainApplyArg :: { (L1ScopedID, L1DomainExpression) }
               : scopedId Equal domainExpression { ($1, $3) }

bracketDomainExpression : domainExpression { const . const  . return $ $1 } -- Just a bracketed expression.
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
labelledDomain :: { (L1RelOrAbsPath, L1DomainExpression) }
               : relOrAbsPath Colon domainExpression { ($1, $3) }
               | relOrAbsPath getPos { ($1, L1DomainReference $2 $1) }

unitExpression : Dimensionless getPos { L1UnitExDimensionless $2 }
               | relOrAbsPath getPos { L1UnitExRef $2 $1 }
               | double getPos NamedSymbol unitExpression getPos {% do
                 (when ($3 /= "*") . fail $ "Expected * " ++ " at " ++ (show $2))
                 return $ L1UnitScalarMup (twoPosToSpan $2 $5) $1 $4
                                                          }
               | unitExpression getPos NamedSymbol unitExprEnding getPos {%
                 case $4 of
                   (Left ex) | $3 == "*" -> return $ L1UnitExTimes (twoPosToSpan $2 $5) $1 ex
                   (Right d) | $3 == "**" -> return $ L1UnitPow (twoPosToSpan $2 $5) $1 d
                   otherwise -> happyError (TokNamedSymbol $3)
                 }
               | scopedId getPos { L1UnitScopedVar $2 $1 }

unitExprEnding : double { Right $1 }
               | unitExpression { Left $1 }
double : SignedInt { fromIntegral $1 }
       | Int { fromIntegral $1 }
       | Real { $1 }
integer : SignedInt { fromIntegral $1 }
        | Int { fromIntegral $1 }

expression :: { L1Expression }
           : expression getPos expression getPos { L1ExApply (twoPosToSpan $2 $4) $1 $3 }
           | relOrAbsPath getPos { L1ExReference $2 $1 }
           | relOrAbsPath getPos PathSep integer { L1ExLiteralInt $2 $1 $4 }
           | scopedId getPos { L1ExBoundVariable $2 $1 }
           | R getPos maybe(bracketedUnits) PathSep double { L1ExLiteralReal $2 (fromMaybe (L1UnitExDimensionless $2) $3) $5 }
           | OpenCurlyBracket getPos sepBy(labelledExpression,Comma) CloseCurlyBracket getPos { L1ExMkProduct (twoPosToSpan $2 $5) $3 }
           | expression getPos As relOrAbsPath { L1ExMkUnion $2 $4 $1 }

labelledExpression : relOrAbsPath Colon expression { ($1, $3) }

getPos : {- empty -} {% (\((AlexPn _ row col):_) -> SrcSpan { srcFile = "input",
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
           | {- empty -} { [] }

sepBy(x,sep) : orEmpty(sepBy1(x,sep)) { $1 }
sepBy1(x,sep) : sepBy1Rev(x,sep) { reverse ($1) }
sepBy1Rev(x,sep) : sepBy1Rev(x,sep) sep x { $3:$1 }
                 | x { [$1] }
orEmpty(x) : x { $1 }
           | {- empty -} { [] }
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
