{
module Parser where

import Lexer
}

%name parse
%tokentype { PosnToken }
%error { parseError }

%token
  from        { PT _ TokenFrom}
  where       { PT _ TokenWhere}
  do          { PT _ TokenDo}
  select      { PT _ TokenSelect}
  filter      { PT _ TokenFilter}
  '>'         { PT _ TokenPipe}
  '='         { PT _ TokenEquals}
  '!='        { PT _ TokenNotEquals}
  ','         { PT _ TokenComma}
  string      { PT _ (TokenString $$) }
  identifier  { PT _ (TokenIdentifier $$) }


%%
-- Main query structure
Query : From OptWhere Operations  { Query $1 $2 $3 }

-- From clause with file identifiers
From : from StringList    { From $2 }

StringList : string               { [$1] }
           | string ',' StringList { $1 : $3 }

IdentList : identifier               { [$1] }
          | identifier ',' IdentList { $1 : $3 }

-- Optional Where clause
OptWhere : where Condition { Just $2 }
         | {- empty -}     { Nothing }

-- Condition expressions
Condition : identifier '=' identifier    { Equals $1 $3 }
          | identifier '!=' identifier   { NotEquals $1 $3 }

-- Operations section
Operations : do OperationList  { $2 }

-- List of operations, possibly separated by pipes
OperationList : Operation                { [$1] }
              | Operation '>' OperationList { $1 : $3 }

-- Different types of operations
Operation : select IdentList   { Select $2 }
          | filter Condition   { Filter $2 }


 

{
data Query = Query FromClause (Maybe Condition) [Operation]
  deriving (Show, Eq)

data FromClause = From [String]
  deriving (Show, Eq)

data Condition
  = Equals String String
  | NotEquals String String
  deriving (Show, Eq)

data Operation
  = Select [String]
  | Filter Condition
  deriving (Show, Eq)

-- === Error Handling ===
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at unknown location"
parseError (PT pos _ : _) = error $ "Parse error at " ++ show pos
}