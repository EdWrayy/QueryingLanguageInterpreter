{
module Parser (parse, Query(..), FromClause(..), Operation(..), Condition(..), AggregateFunc(..), Condition(..), SortOrder(..)) where

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
  rightMerge   {PT _ TokenRightMerge}
  innerMerge   {PT _ TokenInnerMerge}
  outerMerge   {PT _ TokenOuterMerge}
  cartesianProduct { PT _ TokenCartesianProduct }
  coalesceColumns  { PT _ TokenCoalesceColumns }
  drop        { PT _ TokenDrop }
  rename      { PT _ TokenRename }
  sort        { PT _ TokenSort }
  sortLex   { PT _ TokenSortLex }
  set        { PT _ TokenSet }
  map        { PT _ TokenMap }
  asc         { PT _ TokenAsc }
  desc        { PT _ TokenDesc }
  addColumn   { PT _ TokenAddColumn }
  appendRow   { PT _ TokenAppendRow }
  removePadding { PT _ TokenRemovePadding }
  '->'         { PT _ TokenPipe}
  '=='         { PT _ TokenEquals}
  '!='        { PT _ TokenNotEquals}
  ','         { PT _ TokenComma}
  '!'         { PT _ TokenNot }
  '&&'        { PT _ TokenAnd }
  '||'        { PT _ TokenOr }
  '++'        { PT _ TokenConcatOp }
  string      { PT _ (TokenString $$) }
  int         { PT _ (TokenInt $$) }
  groupBy    { PT _ TokenGroupBy }
  count      { PT _ TokenCount }
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
ToClause : {- empty -}                    { Nothing }
         | to string                      { Just $2 }

-- Operation list
OperationList : Operation                 { [$1] }
              | Operation '->' OperationList { $1 : $3 }


-- Operations
Operation
  : select IntList                        { Select $2 }
  | filter Condition                      { Filter $2 }
  | leftMerge Condition                   { LeftMerge $2}
  | rightMerge Condition                  { RightMerge $2 }
  | innerMerge Condition                  { InnerMerge $2 }
  | outerMerge Condition                  { OuterMerge $2 }
  | cartesianProduct                      { CartesianProduct }
  | coalesceColumns int   { CoalesceColumns $2 }
  | drop IntList                          { Drop $2 }
  | rename int string                     { Rename $2 $3 }
  | sort int SortOrder                    { Sort $2 $3 }
  | sortLex                               { SortLex}
  | set int int string                    { Set $2 $3 $4 }
  | map int string                        { Map $2 $3 }
  | addColumn string string               { AddColumn $2 $3 }
  | appendRow StringList                  { AppendRow $2 }
  | removePadding                         { RemovePadding }
  | groupBy int AggregateFunc             { GroupBy $2 $3 }
  

AggregateFunc : count { Count }
              | concat { Concat}
              | concatDist {ConcatDist}


-- Integer list
IntList : int                             { [$1] }
        | int ',' IntList                 { $1 : $3 }

-- String list
StringList : string                       { [$1] }
           | string ',' StringList        { $1 : $3 }

-- Sort direction
SortOrder : asc                           { Asc }
          | desc                          { Desc }

-- Condition expressions
Condition : int '==' string    { Equals $1 $3 }
          | int '!=' string   { NotEquals $1 $3 }
          | '!' Condition             { Not $2 }
          | Condition '&&' Condition  { And $1 $3 }
          | Condition '||' Condition  { Or $1 $3 }
          | int '==' int       { EqualsCol $1 $3} -- conditions for equal columns
          | int '!=' int      { NotEqualsCol $1 $3 }

{
-- === AST Definitions ===
data Query = Query FromClause (Maybe String) [Operation]
  deriving (Show, Eq)

data FromClause = From String Bool
                | FromPair String String Bool
                deriving (Show, Eq)


data Condition
  = Equals Int String
  | NotEquals Int String
  | Not Condition
  | And Condition Condition
  | Or Condition Condition
  | EqualsCol Int Int
  | NotEqualsCol Int Int
  deriving (Show, Eq)

data SortOrder = Asc | Desc
  deriving (Show, Eq)

data Operation
  = Select [Int]
  | Filter Condition
  | LeftMerge Condition
  | RightMerge Condition 
  | InnerMerge Condition
  | OuterMerge Condition
  | CartesianProduct
  | CoalesceColumns Int
  | Drop [Int]
  | Rename Int String
  | Sort Int SortOrder
  | SortLex
  | Set Int Int String
  | Map Int String
  | AddColumn String String
  | AppendRow [String]
  | RemovePadding
  | GroupBy Int AggregateFunc
  deriving (Show, Eq)

data AggregateFunc = Count | Concat | ConcatDist  
  deriving (Show, Eq)

-- === Error Handling ===
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at unknown location"
parseError (PT pos _ : _) = error $ "Parse error at " ++ show pos
}
