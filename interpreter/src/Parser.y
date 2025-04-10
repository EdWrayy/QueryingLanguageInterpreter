{
module Parser where

import Lexer
}

%name parse
%tokentype { PosnToken }
%error { parseError }

%token
  from        { PT _ TokenFrom}
  do          { PT _ TokenDo}
  select      { PT _ TokenSelect}
  filter      { PT _ TokenFilter}
  '>'         { PT _ TokenPipe}
  '='         { PT _ TokenEquals}
  '!='        { PT _ TokenNotEquals}
  ','         { PT _ TokenComma}
  string      { PT _ (TokenString $$) }
  int         { PT _ (TokenInt $$) }


%%
-- Main query structure
Query : From Operations  { Query $1 $2 }

-- From clause with file identifiers
From : from string    { From $2 }
 
-- Operations section
Operations : do OperationList  { $2 }

-- List of operations, possibly separated by pipes
OperationList : Operation                   { [$1] }
              | Operation '>' OperationList { $1 : $3 }

-- Different types of operations
Operation : select IntList   { Select $2 }
          | filter Condition   { Filter $2 }

-- Integer list for column indices
IntList : int               { [$1] }
        | int ',' IntList   { $1 : $3 }

-- Condition expressions
Condition : int '=' string    { Equals $1 $3 }
          | int '!=' string   { NotEquals $1 $3 }

{
data Query = Query FromClause [Operation]
  deriving (Show, Eq)

data FromClause = From String
  deriving (Show, Eq)

data Condition
  = Equals Int String
  | NotEquals Int String
  deriving (Show, Eq)

data Operation
  = Select [Int]
  | Filter Condition
  deriving (Show, Eq)

-- === Error Handling ===
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at unknown location"
parseError (PT pos _ : _) = error $ "Parse error at " ++ show pos
}