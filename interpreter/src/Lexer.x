{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]

@identifier = $alpha ($alphaNum | '_')*

tokens :-
  $white+                ;   -- Ignore whitespace
  "--".*                 ;   -- Ignore comments
  
  -- Keywords (case sensitive for simplicity)
  from                   { \p s -> PT p TokenFrom}
  where                  { \p s -> PT p TokenWhere}
  do                     { \p s -> PT p TokenDo}
  select                 { \p s -> PT p TokenSelect}
  filter                 { \p s -> PT p TokenFilter}
  
  -- Operators and symbols
  ">"                    { \p s -> PT p TokenPipe}
  "="                    { \p s -> PT p TokenEquals}
  "!="                   { \p s -> PT p TokenNotEquals}
  ","                    { \p s -> PT p TokenComma}
  
  
  --String literals
  \"[^\"]*\"   { \p s -> PT p (TokenString (init (tail s))) }
  
  -- Identifiers
  @identifier            { \p s -> PT p (TokenIdentifier s) }



{
data PosnToken = PT AlexPosn Token deriving (Eq, Show)

data Token = 
  -- Keywords
    TokenFrom  
  | TokenWhere 
  | TokenDo 
  | TokenSelect 
  | TokenFilter 
  
  -- Operators and symbols
  | TokenPipe 
  | TokenEquals 
  | TokenNotEquals 
  | TokenComma 

  --Primitives
  | TokenString String
  
  -- Identifiers
  | TokenIdentifier String
  deriving (Eq, Show)


tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}