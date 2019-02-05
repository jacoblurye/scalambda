# scalambda

An interpreter for Church's lambda calculus extended with `let` expressions.

Currently, only a call-by-name substitution semantics is supported. Call-by-value to come.

## Installation
1) Download the project and cd into it:
    ```bash
    $ git clone git@github.com:jacoblurye/scalambda.git
    $ cd scalambda
    ```
2) Build the package and install the `scalambda` command-line command for accessing the REPL:
    ```
    $ sbt install
    ```

## Usage
Run the REPL:
```bash
$ scalambda
λ > (/x.(x x) y)
y y
λ > let ident = /u.u in (ident x)
x
```
Load definitions from `.lmb` files. A library of useful definitions, `core.lmb`, 
comes with the package (but you can make your own!):
```bash
λ > :load core.lmb
Loading definitions from core.lmb
λ > (plus 1 1)
2
λ > (fact 4)
24
```
Print all available REPL commands with `:help`.
