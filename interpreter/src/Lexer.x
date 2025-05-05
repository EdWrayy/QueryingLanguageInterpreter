{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]


tokens :-
  $white+                ;   -- Ignore whitespace
  "--".*                 ;   -- Ignore comments

  -- Keywords (case sensitive for simplicity)
  from                   { \p s -> PT p TokenFrom}
  to                     { \p s -> PT p TokenTo}
  do                     { \p s -> PT p TokenDo}
  select                 { \p s -> PT p TokenSelect}
  filter                 { \p s -> PT p TokenFilter}
  leftMerge              { \p s -> PT p TokenLeftMerge}
  rightMerge             { \p s -> PT p TokenRightMerge}
  innerMerge             { \p s -> PT p TokenInnerMerge}
  outerMerge             { \p s -> PT p TokenOuterMerge}
  cartesianProduct      { \p s -> PT p TokenCartesianProduct }
  coalesceColumns      { \p s -> PT p TokenCoalesceColumns }

  rename                 { \p s -> PT p TokenRename}
  drop                   { \p s -> PT p TokenDrop}
  sort                   { \p s -> PT p TokenSort}
  sortLex                { \p s -> PT p TokenSortLex}
  addColumn              { \p s -> PT p TokenAddColumn}
  appendRow              { \p s -> PT p TokenAppendRow}
  removePadding          { \p s -> PT p TokenRemovePadding}
  set                    { \p s -> PT p TokenSet }
  map                    { \p s -> PT p TokenMap }
  asc                    { \p s -> PT p TokenAsc}
  desc                   { \p s -> PT p TokenDesc}
  raw                    { \p s -> PT p TokenRaw }
  

  -- Operators and symbols
  "->"                   { \p s -> PT p TokenPipe}
  "=="                   { \p s -> PT p TokenEquals}
  "!="                   { \p s -> PT p TokenNotEquals}
  "!"                    { \p s -> PT p TokenNot}
  "&&"                   { \p s -> PT p TokenAnd}
  "||"                   { \p s -> PT p TokenOr}
  "++"                   { \p s -> PT p TokenConcatOp}
  ","                    { \p s -> PT p TokenComma}


  --Aggregation
  groupBy                { \p s -> PT p TokenGroupBy}
  count                  { \p s -> PT p TokenCount}
  concat                 { \p s -> PT p TokenConcat}
  concatDist             { \p s -> PT p TokenConcatDist}

 
  --Primitives
  $digit+ { \p s -> PT p (TokenInt (read s)) }
  \"[^\"]*\"             { \p s -> PT p (TokenString (init (tail s))) }





{
data PosnToken = PT AlexPosn Token deriving (Eq, Show)

data Token =
  -- Keywords
    TokenFrom
  | TokenTo
  | TokenDo
  | TokenSelect
  | TokenFilter
  | TokenLeftMerge
  | TokenRightMerge
  | TokenInnerMerge
  | TokenOuterMerge 
  | TokenCartesianProduct
  | TokenCoalesceColumns
  | TokenRaw
  | TokenRename
  | TokenDrop
  | TokenSort
  | TokenSortLex
  | TokenSet
  | TokenMap
  | TokenAddColumn
  | TokenAppendRow
  | TokenRemovePadding
  | TokenAsc
  | TokenDesc

  -- Operators and symbols
  | TokenPipe
  | TokenEquals
  | TokenNotEquals
  | TokenGreaterThan
  | TokenLessThan
  | TokenGreaterThanEquals
  | TokenLessThanEquals
  | TokenNot
  | TokenAnd
  | TokenOr
  | TokenConcatOp
  | TokenComma



  --Aggregation
  | TokenGroupBy
  | TokenCount
  | TokenConcat
  | TokenConcatDist

  --Primitives
  | TokenString String
  | TokenInt Int

  deriving (Eq, Show)


tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}