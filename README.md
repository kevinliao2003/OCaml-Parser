(* MicroCaml Interpreter *)

(* Introduction *)
(* This OCaml file implements a simple interpreter for the MicroCaml programming language. *)
(* MicroCaml is a small subset of the OCaml language, focusing on expressions and mutop directives. *)

(* Table of Contents *)
let () =
  print_endline "# Introduction";
  print_endline "- [Introduction](#introduction)";
  print_endline "- [Usage](#usage)";
  print_endline "- [File Structure](#file-structure)";
  print_endline "- [Provided Functions](#provided-functions)";
  print_endline "- [How to Run](#how-to-run)";
  print_endline "- [Examples](#examples)";
  print_newline ()

(* Usage *)
let () =
  print_endline "# Usage";
  print_endline "This interpreter can be used to evaluate MicroCaml expressions and mutop directives.";
  print_endline "It provides functions for extending environments, looking up variables, and updating mappings.";
  print_newline ()

(* File Structure *)
let () =
  print_endline "# File Structure";
  print_endline "- `MicroCamlTypes`: Module containing types used in MicroCaml expressions.";
  print_endline "- `Utils`: Module containing utility functions.";
  print_endline "- `Interpreter`: Main implementation of the MicroCaml interpreter.";
  print_newline ()

(* Provided Functions *)
let () =
  print_endline "# Provided Functions";
  print_endline "### `extend`";
  print_endline "```ocaml";
  print_endline "val extend: environment -> string -> value -> environment";
  print_endline "```";
  print_endline "Adds a mapping [x:v] to the environment [env].";
  print_endline "";
  print_endline "### `lookup`";
  print_endline "```ocaml";
  print_endline "val lookup: environment -> string -> value";
  print_endline "```";
  print_endline "Returns [v] if [x:v] is a mapping in [env].";
  print_endline "";
  print_endline "### `extend_tmp`";
  print_endline "```ocaml";
  print_endline "val extend_tmp: environment -> string -> environment";
  print_endline "```";
  print_endline "Creates a placeholder mapping for [x] in [env].";
  print_endline "";
  print_endline "### `update`";
  print_endline "```ocaml";
  print_endline "val update: environment -> string -> value -> unit";
  print_endline "```";
  print_endline "Updates the (most recent) mapping in [env] for [x] to [v].";
  print_endline "";
  print_endline "### `eval_expr`";
  print_endline "```ocaml";
  print_endline "val eval_expr: environment -> expr -> value";
  print_endline "```";
  print_endline "Evaluates MicroCaml expression [e] in environment [env], returning a value, or throwing an exception on error.";
  print_endline "";
  print_endline "### `eval_mutop`";
  print_endline "```ocaml";
  print_endline "val eval_mutop: environment -> mutop -> environment * value option";
  print_endline "```";
  print_endline "Evaluates MicroCaml mutop directive [m] in environment [env], returning a possibly updated environment paired with a value option; throws an exception on error.";
  print_newline ()

(* How to Run *)
let () =
  print_endline "# How to Run";
  print_endline "To run the interpreter, load the file in an OCaml interpreter and use the provided functions in your code.";
  print_newline ()

(* Examples *)
let () =
  print_endline "# Examples";
  print_endline "```ocaml";
  print_endline "(* Example usage of the interpreter *)";
  print_endline "";
  print_endline "let env = [] in";
  print_endline "let expression = Binop(Add, Value(Int 2), Value(Int 3)) in";
  print_endline "let result = eval_expr env expression in";
  print_endline "print_endline (string_of_value result)";
  print_endline "```";
  print_newline ()