# True False Check

Verify that the logical constants are always written in full, `TRUE` or
`FALSE`, and never abbreviated as `T` or `F`, when used as values inside
a function: in argument values, like `f(log = T)` or `f(T)`, in
condition values, like `if(T)` or `while(T)`, in comparison values, like
`x == T` (both sides), and in indexing values, like `n[T]`. The safer-r
rule requires full logical names, because `T` and `F` can be overwritten
by the user (e.g., `T <- 1`), contrary to `TRUE` and `FALSE`.

Warning: the function first checks that all the functions used inside
`x` are preceded by a package attribution (like
[`colons_check()`](colons_check.md) would do): if not, an error is
returned, asking to run [`colons_check()`](colons_check.md) first (like
[`all_args_here()`](all_args_here.md) does). This pre-check must pass
before the `T` or `F` scanning is performed.

## Usage

``` r
true_false_check(
  x,
  export = FALSE,
  out_path = ".",
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- x:

  a function name, written without quotes and brackets.

- export:

  Single logical value. Export the modified function (see the `@returns`
  and `@details` sections) into a `.R` file? If `TRUE`, the modified
  code is written in a file named `<function name>_safer.R` (the
  function name is the last component of the submitted name, so that
  `pkg::fun()` gives `fun_safer.R`) in the directory indicated by the
  `out_path` argument, and `NULL` is returned: the modified function is
  only available in the file. The file contains an assignment of the
  modified function: `<function name>_safer <- function(...) ...`, so
  that [`base::source()`](https://rdrr.io/r/base/source.html)ing it
  recreates the modified function. An error is returned if this file
  already exists, and if the tested function is not designated by a
  function name (like `test` or `package::test`), i.e., written by an
  inline definition (like `function(x){x}`) or by a component writing
  (like `FUN_LIST$fun`). Nothing is written if no modification is
  performed (everything clean). If `FALSE` (default), the modified
  function is returned by
  [`return()`](https://rdrr.io/r/base/function.html) (`NULL` if no
  modification is performed).

- out_path:

  Single character string indicating the absolute pathway of the folder
  where to export the modified function when the `export` argument is
  `TRUE`. `out_path = "."` (default) means the R working directory set
  by the user. Ignored if the `export` argument is `FALSE`.

- safer_check:

  Single logical value. Perform some "safer" checks? If `TRUE`,
  checkings are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Must be set to
  `FALSE` if this function is used inside another "safer" function to
  avoid pointless multiple checkings.

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `unique(c(new, .Library.site, .Library))`. Warning:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored to
  the initial paths, after function execution. Ignored if `NULL`
  (default) or if the `safer_check` argument is `FALSE`: only the
  pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

A table-like text message indicating the `T` or `F` symbols that must be
written `TRUE` or `FALSE` or a message saying that everything seems
fine, printed by
[`base::message()`](https://rdrr.io/r/base/message.html) (always,
whatever the value of the `export` argument).

Table-like: column 1, the line number in the function code (starting at
the `"<- function"` line, i.e., without counting the `#'` header lines);
column 2, the symbol (`T` or `F`); column 3, the code preceding the
symbol.

In addition, the returned value depends on the `export` argument. If
`export` is `FALSE`, the submitted function with all the reported
symbols replaced (`T` by `TRUE` and `F` by `FALSE`) is returned by
[`return()`](https://rdrr.io/r/base/function.html), and `NULL` is
returned if no modification was performed (everything clean). If
`export` is `TRUE`, `NULL` is returned (invisibly), and the modified
code is only available in the exported file, whose location is indicated
by a message.

## Details

The reported symbols are used to modify the code of the tested function
like this: replace `T` by `TRUE` and `F` by `FALSE` (see the `@returns`
section). The replacement is performed at the exact positions of the
reported tokens in the recovered code, so that comments, strings and the
formatting of the original code are preserved (in particular, symbols
written as [`base::T`](https://rdrr.io/r/base/logical.html) are replaced
by `TRUE`, because the `::` operator cannot precede a reserved
constant). When `export` is `FALSE`, the modified function is returned
by [`return()`](https://rdrr.io/r/base/function.html) in a new
environment whose parent is the environment of the submitted function:
please, check the returned function before replacing the original one,
notably because its environment is not necessarily the same. When
`export` is `TRUE`, the modified code is written in the file
`<function name>_safer.R` of the directory indicated by the `out_path`
argument (the function name is the last component of the submitted name,
so that `pkg::fun()` gives `fun_safer.R`), and nothing is written if no
modification was performed. The exported file contains an assignment of
the modified function (`<function name>_safer <- function(...) ...`), so
that [`base::source()`](https://rdrr.io/r/base/source.html)ing it
recreates the modified function. Warning: the tested function must be
designated by a function name (like `test` or `package::test`) when
`export = TRUE`: an inline definition (like `function(x){x}`) or a
component writing (like `FUN_LIST$fun`) returns an error, because the
name of the exported file could not be determined reliably. Warning: the
exported file is a standalone R script containing the modified function
code, and its potential pre-existing version is never overwritten (an
error is returned instead).

More precisely, `true_false_check()` reports every symbol `T` or `F`
used as a value (see the `@description` section), but not the symbols
used as names, because these are user-defined objects: definition
target, like `T <- 1`, `T = 1` or `y -> T`; assignment value, like
`x <- T` or `T -> x` (deliberately not reported, because such a symbol
refers to a user-defined object); formal name, like `function(T)`;
named-argument name, like `f(T = 1)`; component after `$` or slot after
`@`, like `df$T` or `df@F`; loop variable, like `for(T in 1:3)`;
function name, like [`T()`](https://rdrr.io/r/base/logical.html); help
topic, like [`?T`](https://rdrr.io/r/base/logical.html), `?(T)`,
`?((T))` or `??T`, at any parenthesis depth; formula symbol, like
`y ~ T`, `T ~ y` or `y ~ x + T`, wherever it is located in the formula.

If a name `T` or `F` is defined anywhere in the checked function code
(formal name, loop variable, definition target), then all the value
usages of this symbol are not reported, whatever the position in the
code.

The verification is performed on the R parser output
([`base::parse()`](https://rdrr.io/r/base/parse.html) +
[`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html)),
so that strings (like `"T"`), comments and backquoted names (like
`` `T` ``) are never reported. Contrary to
[`curl_bracket_check()`](curl_bracket_check.md), function bodies reduced
to a single `.Primitive(...)` call do not need to be skipped, because
such bodies do not contain any `T` or `F` symbol.

The package-association pre-check verifies that all the functions called
with brackets (like [`mean()`](https://rdrr.io/r/base/mean.html)) in the
code of the tested function are preceded by `::` or `:::`. Like
[`colons_check()`](colons_check.md), it does not check function names
written without brackets, like in the `FUN` argument of some functions,
e.g., `sapply(1:3, FUN = as.character)`, and function names preceded by
`$` are not considered. Contrary to [`colons_check()`](colons_check.md),
the pre-check is performed on the R parser output, and functions without
any function call (like `function(x){ x == TRUE }`) are accepted,
instead of returning the [`colons_check()`](colons_check.md) "EMPTY OF
FUNCTION" error. Functions like
[`base::mean`](https://rdrr.io/r/base/mean.html) (body made of
`UseMethod(...)`) can never pass the pre-check, exactly like they never
pass [`colons_check()`](colons_check.md). Warning: internal functions
defined with the arrow writing `function(...)` `->` `name` are not
detected by the pre-check (the detection only supports
`name <- function(...)` and `name <- utils::getFromNamespace(...)`),
exactly like in [`colons_check()`](colons_check.md) where the arrow
writing is also flagged: please, use the standard writing.

Warning: `true_false_check()` cannot check symbols written with
backquotes, like `` `T` ``. Please, use the standard writing.

The returned line numbers are indicative, depending on which source is
checked. For instance, [`saferDev::colons_check`](colons_check.md)
(compiled) has not the same line numbers as its source file. Notably,
compiled functions do not have comments anymore, compared to the same
source function sourced into the working environment. In addition, the
counting starts at the `"<- function"` line, i.e., without counting the
`#'` header lines potentially present in source files.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/true_false_check.html
fun1 <- function(x){ if(x > 1){ base::return(TRUE) } }
saferDev::true_false_check(fun1)
#> 
#> 
#> AFTER RUNNING saferDev::true_false_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
#> NULL
fun2 <- function(x){ if(x == T){ base::return(x) } }
saferDev::true_false_check(fun2)
#> 
#> 
#> 
#> INSIDE fun2(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
#> 
#> LINE SYMBOL      CODE
#> 3    T       if (x == T) {
#> 
#> MODIFIED FUNCTION RETURNED BY return().
#> 
#> function (x) 
#> {
#>     if (x == TRUE) {
#>         base::return(x)
#>     }
#> }
#> <environment: 0x56311a38bcd0>
fun3 <- function(x){ x } # no function call: accepted by the package-association pre-check
saferDev::true_false_check(fun3)
#> 
#> 
#> AFTER RUNNING saferDev::true_false_check().
#> INSIDE fun3(), EVERYTHING SEEMS CLEAN.
#> 
#> NULL
fun4 <- function(x){ if(x == T){ base::return(x) } } ; saferDev::true_false_check(fun4)
#> 
#> 
#> 
#> INSIDE fun4(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
#> 
#> LINE SYMBOL      CODE
#> 3    T       if (x == T) {
#> 
#> MODIFIED FUNCTION RETURNED BY return().
#> 
#> function (x) 
#> {
#>     if (x == TRUE) {
#>         base::return(x)
#>     }
#> }
#> <environment: 0x56311e3b5f90>
fun4_bis <- saferDev::true_false_check(x = fun4, export = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "") # modified function returned (T replaced by TRUE)
#> 
#> 
#> 
#> INSIDE fun4(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
#> 
#> LINE SYMBOL      CODE
#> 3    T       if (x == T) {
#> 
#> MODIFIED FUNCTION RETURNED BY return().
#> 
fun4_safer <- fun4_bis ; base::rm(list = base::c("fun4_bis")) # keep the original fun4 untouched
saferDev::true_false_check(x = fun4, export = TRUE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = "") # fun4_safer.R written in the working directory, NULL returned
#> 
#> 
#> 
#> INSIDE fun4(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
#> 
#> LINE SYMBOL      CODE
#> 3    T       if (x == T) {
#> 
#> MODIFIED CODE EXPORTED IN:
#> ./fun4_safer.R
#> 
#> NULL
```
