# MicroCaml Interpreter

This OCaml file implements a simple interpreter for the MicroCaml programming language. MicroCaml is a small subset of the OCaml language, focusing on expressions and mutop directives.

## Table of Contents

- [Introduction](#introduction)
- [Usage](#usage)
- [File Structure](#file-structure)
- [Provided Functions](#provided-functions)
- [How to Run](#how-to-run)
- [Examples](#examples)
- [License](#license)

## Introduction

MicroCaml Interpreter is designed to evaluate MicroCaml expressions and mutop directives. It handles basic operations, conditionals, and function calls within the MicroCaml language.

## Usage

This interpreter can be used to evaluate MicroCaml expressions and mutop directives. It provides functions for extending environments, looking up variables, and updating mappings.

## File Structure

- `MicroCamlTypes`: Module containing types used in MicroCaml expressions.
- `Utils`: Module containing utility functions.
- `Interpreter`: Main implementation of the MicroCaml interpreter.

## Provided Functions

### `extend`
```ocaml
val extend: environment -> string -> value -> environment
```
Adds a mapping [x:v] to the environment [env].

### `lookup`
```ocaml
val lookup: environment -> string -> value
```
Returns [v] if [x:v] is a mapping in [env].

### `extend_tmp`
```ocaml
val extend_tmp: environment -> string -> environment
```
Creates a placeholder mapping for [x] in [env].

### `update`
```ocaml
val update: environment -> string -> value -> unit
```
Updates the (most recent) mapping in [env] for [x] to [v].

### eval_expr
```ocaml
val eval_expr: environment -> expr -> value
```
Evaluates MicroCaml expression [e] in environment [env], returning a value, or throwing an exception on error.

### eval_mutop
```ocaml
val eval_mutop: environment -> mutop -> environment * value option
```
Evaluates MicroCaml mutop directive [m] in environment [env], returning a possibly updated environment paired with a value option; throws an exception on error.

## How to Run

To run the interpreter, load the file in an OCaml interpreter and use the provided functions in your code.

## Examples

```ocaml
(* Example usage of the interpreter *)

let env = [] in
let expression = Bin
```

## License

This MicroCaml Interpreter is released under the MIT License.

```css
Feel free to replace `[MIT License](LICENSE)` with the actual path or URL to your license file.
```