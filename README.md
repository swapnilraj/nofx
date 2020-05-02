# NoFx

`NoFx` is a untyped lazy functional language, with the main aim of being used a
language to help learn laziness. `NoFx` features a novel visualisation tool,
which shows the execution of a G-Machine.

## Pre-requisites

  * yarn

## Setup

Run the following after cloning the repository, to install the dependencies:

```sh
  yarn install
```

To run the project, run:

```sh
  yarn build
  yarn serve
```

## Syntax

The syntax of `NoFx` is intentionally kept simple, and described in detail
below:

```
  lowercase = ? ascii lower case characters ? ;
  uppercase = ? ascii upper case characters ? ;
  digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
  integer = [ "-" ], digit, { digit } ;
  boolean = "true" | "false" ;
  var = lowercase, { lowercase | "_" | " ' "} ;
  var_list = var, { var } ;
  binding = var, "=", expr ;
  bindings = binding, { binding } ;
  let = "let", [ "rec" ],
               bindings,
         "in", expr ;
  constructor = upper_case, { var } ;
  alters = constructor, [ var_list ], "=>", expr ;
  case = "case", expr, "of",
          ["|"], alters,
          { ( "|", alters) } ;
  ifThenElse = "if", expr,
               "then", expr,
               "else", expr  ;
  lambda = "\", var_list, "->", expr ;
  expr = var
       | "(", expr, ")"
       | expr expr
       | let
       | case
       | ifThenElse
       | lambda
       | number
       | boolean
       | constructor
       ;
  sc = "fun", var, [ var_list ],
        "=", expr ;
  program = sc { sc } ;
```
