{
module Parser where

import Lexer
}

%name parse
%tokentype { PosnToken }
%error { parseError }

%token
  from        { PT _ TokenFrom }
  to          { PT _ TokenTo }
  do          { PT _ TokenDo }
  select      { PT _ TokenSelect }
  filter      { PT _ TokenFilter }
  leftMerge   { PT _ TokenLeftMerge }
  drop        { PT _ TokenDrop }
  rename      { PT _ TokenRename }
  sort        { PT _ TokenSort }
  asc         { PT _ TokenAsc }
  desc        { PT _ TokenDesc }
  addColumn   { PT _ TokenAddColumn }
  appendRow   { PT _ TokenAppendRow }
  '->'        { PT _ TokenPipe }
  '='         { PT _ TokenEquals }
  '!='        { PT _ TokenNotEquals }
  ','         { PT _ TokenComma }
  string      { PT _ (TokenString $$) }
  int         { PT _ (TokenInt $$) }

%%

-- Main query structure
Query : FromClause ToClause do OperationList  { Query $1 $2 $4 }

-- From clause
FromClause : from string                   { From $2 }
           | from string ',' string       { FromPair $2 $4 }

-- Optional To clause
ToClause : {- empty -}                    { Nothing }
         | to string                      { Just $2 }

-- Operation list
OperationList : Operation                 { [$1] }
              | Operation '->' OperationList { $1 : $3 }

-- Operations
Operation
  : select IntList                        { Select $2 }
  | filter Condition                      { Filter $2 }
  | leftMerge                             { LeftMerge }
  | drop IntList                          { Drop $2 }
  | rename int string                     { Rename $2 $3 }
  | sort int SortOrder                    { Sort $2 $3 }
  | addColumn string string               { AddColumn $2 $3 }
  | appendRow StringList                  { AppendRow $2 }

-- Integer list
IntList : int                             { [$1] }
        | int ',' IntList                 { $1 : $3 }

-- String list
StringList : string                       { [$1] }
           | string ',' StringList        { $1 : $3 }

-- Sort direction
SortOrder : asc                           { Asc }
          | desc                          { Desc }

-- Conditions
Condition : int '=' string                { Equals $1 $3 }
          | int '!=' string               { NotEquals $1 $3 }

{
-- === AST Definitions ===
data Query = Query FromClause (Maybe String) [Operation]
  deriving (Show, Eq)

data FromClause
  = From String
  | FromPair String String
  deriving (Show, Eq)

data Condition
  = Equals Int String
  | NotEquals Int String
  deriving (Show, Eq)

data SortOrder = Asc | Desc
  deriving (Show, Eq)

data Operation
  = Select [Int]
  | Filter Condition
  | LeftMerge
  | Drop [Int]
  | Rename Int String
  | Sort Int SortOrder
  | AddColumn String String
  | AppendRow [String]
  deriving (Show, Eq)

-- === Error Handling ===
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at unknown location"
parseError (PT pos _ : _) = error $ "Parse error at " ++ show pos
}
