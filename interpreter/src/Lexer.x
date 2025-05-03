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
  raw                    { \p s -> PT p TokenRaw }
  
  -- Operators and symbols
  "->"                   { \p s -> PT p TokenPipe}
  "=="                   { \p s -> PT p TokenEquals}
  "!="                   { \p s -> PT p TokenNotEquals}
  ">"                    { \p s -> PT p TokenGreaterThan} 
  "<"                    { \p s -> PT p TokenLessThan}
  ">="                   { \p s -> PT p TokenGreaterThanEquals}
  "<="                   { \p s -> PT p TokenLessThanEquals}
  "!"                    { \p s -> PT p TokenNot}
  "&&"                   { \p s -> PT p TokenAnd}
  "||"                   { \p s -> PT p TokenOr}
  "++"                   { \p s -> PT p TokenConcatOp}
  ","                    { \p s -> PT p TokenComma}

  --Aggregation
  groupBy                { \p s -> PT p TokenGroupBy}
  sum                    { \p s -> PT p TokenSum}
  avg                    { \p s -> PT p TokenAvg}
  count                  { \p s -> PT p TokenCount}
  max                    { \p s -> PT p TokenMax}
  min                    { \p s -> PT p TokenMin}
  concat                 { \p s -> PT p TokenConcat}
  concatDist             { \p s -> PT p TokenConcatDist}

  
  
  --Primitives
  $digit+                { \p s -> PT p (TokenInt (read s)) }
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
  | TokenRaw
  
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
  | TokenSum
  | TokenAvg
  | TokenCount
  | TokenMax
  | TokenMin
  | TokenConcat
  | TokenConcatDist

  --Primitives
  | TokenString String
  | TokenInt Int
  
  deriving (Eq, Show)


tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}