# Satisfiability Checker for Symbolic Predicates

This Haskell project provides a tool for checking the satisfiability of symbolic predicates using multi-core processing. The program reads a list of logical expressions from a file, evaluates whether each expression is satisfiable, and reports the results.

## Table of Contents

- [Introduction](#introduction)
- [Components](#components)
  - [Tokenization](#tokenization)
  - [Parsing](#parsing)
  - [Evaluation](#evaluation)
  - [Multi-core Processing](#multi-core-processing)
- [Usage](#usage)

## Introduction

The Satisfiability Checker allows you to determine whether given logical expressions (predicates) can be satisfied by some assignment of truth values to variables. The program supports common logical operations such as AND, OR, NOT, IMPLIES, and IAOI (if and only if).

## Components

### Tokenization

- **Tokenizer**: Converts a string representing a logical expression into a stream of tokens. These tokens include variables, operators (`&`, `|`, `~`, `->`, `<->`), and parentheses.
  
### Parsing

- **Parser**: Transforms a stream of tokens into a predicate data structure. The parser supports nested expressions and enforces proper use of parentheses.

### Evaluation

- **Predicate Evaluation**: The `eval` function evaluates the truth of a predicate given a specific assignment of truth values to variables.
- **Satisfiability Checking**: The `isSatisfiable` function checks whether there exists any assignment of truth values that makes the predicate true.

### Multi-core Processing

- **Parallel Processing**: The program utilizes Haskell's concurrency features to evaluate multiple predicates in parallel, improving performance on multi-core processors.

## Usage

To use this project, follow these steps:

1. Ensure you have GHC (Glasgow Haskell Compiler) installed on your system.
2. Clone or download this repository.
3. Compile the project using `ghc -o satisfiabilityChecker satisfiabilityChecker.hs` or similar commands.
4. Prepare a text file containing one or more symbolic predicates separated by commas.
5. Run the executable `./satisfiabilityChecker` and provide the path to your file when prompted.
6. The program will evaluate each predicate and display whether it is satisfiable.

### Example Input File

```
a & b -> c, ~(a | b), a <-> b
```

### Example Output

```
Enter a symbolic predicate file path:
example.txt
Satisfiability of each predicate: [True, False, True]
Are all predicates satisfiable? False
```
