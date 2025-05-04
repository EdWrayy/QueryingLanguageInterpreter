{
module Parser (parse, Query(..), FromClause(..), Operation(..), Condition(..), AggregateFunc(..), Condition) where

import Lexer
}

%name parse
%tokentype { PosnToken }
%error { parseError }

%token
  from        { PT _ TokenFrom}
  to          { PT _ TokenTo}
  do          { PT _ TokenDo}
  raw         { PT _ TokenRaw}
  select      { PT _ TokenSelect}
  filter      { PT _ TokenFilter}
  leftMerge   { PT _ TokenLeftMerge}
  '->'         { PT _ TokenPipe}
  '=='         { PT _ TokenEquals}
  '!='        { PT _ TokenNotEquals}
  ','         { PT _ TokenComma}
  '>'         { PT _ TokenGreaterThan }
  '<'         { PT _ TokenLessThan }
  '>='        { PT _ TokenGreaterThanEquals }
  '<='        { PT _ TokenLessThanEquals }
  '!'         { PT _ TokenNot }
  '&&'        { PT _ TokenAnd }
  '||'        { PT _ TokenOr }
  '++'        { PT _ TokenConcatOp }
  string      { PT _ (TokenString $$) }
  int         { PT _ (TokenInt $$) }
  groupBy    { PT _ TokenGroupBy }
  sum        { PT _ TokenSum }
  count      { PT _ TokenCount }
  avg        { PT _ TokenAvg }
  min        { PT _ TokenMin }
  max        { PT _ TokenMax }
  concat     { PT _ TokenConcat }
  concatDist { PT _ TokenConcatDist }


%%
-- Main query structure
Query : FromClause ToClause do OperationList  { Query $1 $2 $4 }

--From clause, can be a single file or a pair of files
FromClause : from string                { From $2 True}
           | from string raw            { From $2 False }
           | from string ',' string     {FromPair $2 $4 True}
           | from string ',' string raw {FromPair $2 $4 False}

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
          | groupBy int AggregateFunc { GroupBy $2 $3 }


AggregateFunc : sum   { Sum }
              | count { Count }
              | avg   { Avg }
              | min   { Min }
              | max   { Max }
              | concat { Concat}
              | concatDist {ConcatDist}

-- Integer list for column indices
IntList : int               { [$1] }
        | int ',' IntList   { $1 : $3 }

-- Condition expressions
Condition : int '==' string    { Equals $1 $3 }
          | int '!=' string   { NotEquals $1 $3 }
          | int '>' int               { GreaterThan $1 $3 }
          | int '<' int               { LessThan $1 $3 }
          | int '>=' int              { GreaterThanEq $1 $3 }
          | int '<=' int              { LessThanEq $1 $3 }
          | '!' Condition             { Not $2 }
          | Condition '&&' Condition  { And $1 $3 }
          | Condition '||' Condition  { Or $1 $3 }

{
data Query = Query FromClause (Maybe String) [Operation]
  deriving (Show, Eq)


data FromClause = From String Bool
                | FromPair String String Bool
                deriving (Show, Eq)

data Condition
  = Equals Int String
  | NotEquals Int String
  | GreaterThan Int Int
  | LessThan Int Int
  | GreaterThanEq Int Int
  | LessThanEq Int Int
  | Not Condition
  | And Condition Condition
  | Or Condition Condition
  deriving (Show, Eq)

data Operation
  = Select [Int]
  | Filter Condition
  | LeftMerge
  | GroupBy Int AggregateFunc
  deriving (Show, Eq)

data AggregateFunc = Sum | Count | Avg | Min | Max | Concat | ConcatDist  
  deriving (Show, Eq)

-- === Error Handling ===
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at unknown location"
parseError (PT pos _ : _) = error $ "Parse error at " ++ show pos
}