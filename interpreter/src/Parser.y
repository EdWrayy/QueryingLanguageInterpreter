{
module Parser where

import Lexer
}

%name parse
%tokentype { PosnToken }
%error { parseError }

%token
  from        { PT _ TokenFrom}
  to          { PT _ TokenTo}
  do          { PT _ TokenDo}
  select      { PT _ TokenSelect}
  filter      { PT _ TokenFilter}
  leftMerge   { PT _ TokenLeftMerge}
  '->'         { PT _ TokenPipe}
  '='         { PT _ TokenEquals}
  '!='        { PT _ TokenNotEquals}
  ','         { PT _ TokenComma}
  string      { PT _ (TokenString $$) }
  int         { PT _ (TokenInt $$) }


%%
-- Main query structure
Query : FromClause ToClause do OperationList  { Query $1 $2 $4 }

--From clause, can be a single file or a pair of files
FromClause : from string    { From $2 }
           | from string ',' string {FromPair $2 $4}

-- Optional To clause
ToClause : {- empty -}    { Nothing }
         | to string      { Just $2 }

-- Operations section
Operations : do OperationList  { $2 }

-- List of operations, possibly separated by pipes
OperationList : Operation                   { [$1] }
              | Operation '->' OperationList { $1 : $3 }

-- Different types of operations
Operation : select IntList   { Select $2 }
          | filter Condition   { Filter $2 }
          | leftMerge          { LeftMerge }

-- Integer list for column indices
IntList : int               { [$1] }
        | int ',' IntList   { $1 : $3 }

-- Condition expressions
Condition : int '=' string    { Equals $1 $3 }
          | int '!=' string   { NotEquals $1 $3 }

{
data Query = Query FromClause (Maybe String) [Operation]
  deriving (Show, Eq)


data FromClause = From String
                | FromPair String String
                deriving (Show, Eq)

data Condition
  = Equals Int String
  | NotEquals Int String
  deriving (Show, Eq)

data Operation
  = Select [Int]
  | Filter Condition
  | LeftMerge
  deriving (Show, Eq)

-- === Error Handling ===
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at unknown location"
parseError (PT pos _ : _) = error $ "Parse error at " ++ show pos
}