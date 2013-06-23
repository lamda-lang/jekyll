# jekyll

A compiler for Lamda, a prefix untyped functional language. See doc/grammar for the EBNF definition the parser and compiler uses (free to invent your own grammar if you like). The compiler transforms the AST to Clojure and then compiles this to bytecode for our VM hyde (WIP).

## Usage

Best is to clone the repository and play around with the clojure code from a REPL. Have a look at `semantic.clj` to follow the compilation pipeline.

## License

Copyright © 2013 Judith Massa, Konrad Kühne, Christian Weilbach

Distributed under the Eclipse Public License, the same as Clojure.
