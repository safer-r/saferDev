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
true_false_check(x, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- x:

  a function name, written without quotes and brackets.

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
fine.

Table-like: column 1, the line number in the function code (starting at
the `"<- function"` line, i.e., without counting the `#'` header lines);
column 2, the symbol (`T` or `F`); column 3, the code preceding the
symbol.

## Details

The result is used to modify the code inside the tested function like
this: replace `T` by `TRUE` and `F` by `FALSE`.

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
fun3 <- function(x){ x } # no function call: accepted by the package-association pre-check
saferDev::true_false_check(fun3)
#> 
#> 
#> AFTER RUNNING saferDev::true_false_check().
#> INSIDE fun3(), EVERYTHING SEEMS CLEAN.
#> 
```
