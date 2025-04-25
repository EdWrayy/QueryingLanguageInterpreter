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
  
  -- Operators and symbols
  "->"                    { \p s -> PT p TokenPipe}
  "="                    { \p s -> PT p TokenEquals}
  "!="                   { \p s -> PT p TokenNotEquals}
  ","                    { \p s -> PT p TokenComma}
  
  
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
  
  -- Operators and symbols
  | TokenPipe 
  | TokenEquals 
  | TokenNotEquals 
  | TokenComma 

  --Primitives
  | TokenString String
  | TokenInt Int
  
  deriving (Eq, Show)


tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}