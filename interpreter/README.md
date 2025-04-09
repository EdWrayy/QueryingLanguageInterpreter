# Query Language Interpreter

This project implements a simple query language interpreter that can process data from CSV files according to query operations.

## Language Grammar

The language syntax:

```
Query : From OptWhere Operations

From : from StringList
StringList : string | string ',' StringList

OptWhere : where Condition | ε
Condition : identifier '=' identifier | identifier '!=' identifier

Operations : do OperationList
OperationList : Operation | Operation '>' OperationList
Operation : select IdentList | filter Condition
IdentList : identifier | identifier ',' IdentList
```

## Example Queries

Valid query example:

```
from "data.csv"
where name != unknown
do select name, age, city > filter age = thirty
```

This query:
1. Loads data from "data.csv"
2. Filters out rows where name equals "unknown"
3. Selects the name, age, and city columns
4. Further filters to only include rows where age equals "thirty"

## File Structure

The project consists of:

- `Lexer.x` - Alex specification for lexical analysis
- `Parser.y` - Happy specification for parsing
- `Interpreter.hs` - Implementation of the interpreter
- `Main.hs` - Entry point of the program

## How It Works

The interpreter works in several steps:

1. **Lexical Analysis**: Converts the text of a query into tokens
2. **Parsing**: Builds an abstract syntax tree (AST) from tokens
3. **Interpretation**: Executes the AST against the data

## Data Handling

The interpreter:

- Loads CSV files with headers
- Represents data as maps from column names to values
- Processes the data according to query operations

## Building and Running

### Prerequisites

- GHC (Glasgow Haskell Compiler)
- Alex lexer generator
- Happy parser generator
- Stack build tool

### Build Instructions

Using Stack:

```bash
stack build
```

### Running

```bash
stack exec interpreter-exe -- <query-file>
```

Example:

```bash
stack exec interpreter-exe -- input.cql
```

## Implementation Details

### Environment

The interpreter maintains an environment that maps identifiers to values. This allows identifiers in the query to be treated as variables or literals.

### Conditionals

The `where` and `filter` clauses evaluate conditions against rows, determining which rows to include in the result.

### Operations

Operations like `select` transform the data by choosing which columns to include.

### Pipelining

Operations can be chained using the `>` operator, with the output of one operation becoming the input to the next.

## Extending the Interpreter

This interpreter provides a basic foundation. Potential extensions include:

- Support for more operations (GROUP BY, ORDER BY, etc.)
- Support for more complex conditions (AND, OR, etc.)
- Support for joins across multiple files
- Support for more data types (numbers, booleans, etc.)
- Addition of functions (SUM, AVG, COUNT, etc.)