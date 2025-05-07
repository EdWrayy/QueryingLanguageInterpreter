# Query Language Interpreter

This project implements a simple query language interpreter that can process data from CSV files according to query operations.

## File Structure

The project consists of:

- `Lexer.x` - Alex specification for lexical analysis
- `Parser.y` - Happy specification for parsing
- `Interpreter.hs` - Implementation of the interpreter
- `Main.hs` - Entry point of the program

### Build Instructions

Using Stack:

```bash
stack build
```

### Running

```bash
stack exec interpreter-exe <query-file>
```

Example:

```bash
stack exec interpreter-exe input.cql
```
