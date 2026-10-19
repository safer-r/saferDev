# For Loop Number Check

Verify that the sequence expression of every `for` construct of a
function is written with hard numbers, i.e., only integer-valued numeric
literals joined by `:` (like `1:3`, `0:0`, `-3:-1` or `1e3`, each of
them possibly reduced to a single literal like `3`), each literal
possibly negated by a unary minus, or a `base::seq_len(...)` or
`base::seq_along(...)` call. Any other writing is reported, notably
`1:length(x)` (recommended replacement: `base::seq_along(x = x)`), `1:n`
with `n` a variable, `1:(n - 1)`, `1:2.5`, `1:NA`, `1:Inf` or `1:TRUE`,
and plain variables like `for(i in x)` or calls like `base::rev(x = x)`,
`base::names(x = df)` or `base::seq(x = x)`.

Indeed, for instance `x <- 0 ; for(i in 0:0){y <- i}` makes one loop
while it should a priori not be necessary, and
`x <- character() ; for(i in 1:length(x)){y <- i}` returns `y = 0`,
because `1:length(x)` gives `c(1, 0)` when `x` is empty. Thus,
`base::seq_along(x = ...)` should be used instead of `length(x)`.

## Usage

``` r
for_loop_nb_check(x, safer_check = TRUE, lib_path = NULL, error_text = "")
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

A table-like text message indicating the `for` sequences not written
with hard numbers or a message saying that everything seems fine.

Table-like: column 1, the line number in the function code (starting at
the `"<- function"` line, i.e., without counting the `#'` header lines);
column 2, the construct (`for`); column 3, the code preceding the
construct body.

## Details

The result is used to modify the code inside the tested function like
this: `for(i in <HARD NUMBERS>)` or
`for(i in base::seq_along(x = <OBJECT>))`.

More precisely, `for_loop_nb_check()` verifies that the sequence
expression written between `in` and the closing parenthesis of the
condition of each `for` construct is exactly a hard-number writing or a
`seq_len(...)` or `seq_along(...)` call. The hard-number writing is: an
optional unary minus, then a numeric literal, then optionally `:`, then
an optional unary minus, then a numeric literal, with nothing else
(e.g., `1:3`, `0:0`, `-3:-1`, `1e3`, `1L:3L`, `0x10:0x11`, or single
literals like `3` or `-3`). Each numeric literal must be finite and
integer-valued: `2.5`, `1e-3`, `NA`, `NaN`, `Inf` and `TRUE`/`FALSE` are
reported (they are silently truncated toward 0, or run for a very long
time, at execution). A `seq_len(...)` or `seq_along(...)` call
(namespaced or not, whatever the arguments, because these functions are
the recommended safe replacements) is clean only if it occupies the
whole sequence expression: `seq_along(x) - 1` is reported, because it
gives `c(0, 1)` when `x` has 2 elements. Any other writing is reported:
variables like `for(i in n)` or `for(i in x)`, function calls like
`1:length(x)`, `1:nrow(df)`, `1:(n - 1)`, `1:3 - 1` (which gives
`c(-2, -1)` at execution), `2 - 1`, `1:3:1`, `base::rev(x = x)`,
`base::seq(x = x)` or `base::names(x = df)`. Comments and strings are
ignored during this verification.

Deliberately NOT reported, because the hard-number writings are checked
by a specific checking line added in the function bodies (which warns at
execution when the computed sequence is equal like `0:0` or `1:1`, both
negative like `-3:-1`, or decreasing like `1:0`): the equal-bounds,
both-negative and decreasing hard-number writings.

Warning: `for_loop_nb_check()` cannot check constructs written with
backquotes, like `` `for`(i in 1:3, base::print(i)) ``. Please, use the
standard writing.

Warning: aliased bounds like `n <- length(x) ; for(i in 1:n)` cannot be
detected, because `n` is a plain variable in the `for` writing. The
checking must then be done on the function body.

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
# See also https://safer-r.github.io/saferDev/articles/for_loop_nb_check.html
fun1 <- function(x){ for(i in 1:3){ base::print(x = i) } }
saferDev::for_loop_nb_check(fun1)
#> 
#> 
#> AFTER RUNNING saferDev::for_loop_nb_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
fun2 <- function(x){ for(i in 1:length(x)){ base::print(x = i) } } ; saferDev::for_loop_nb_check(fun2)
#> 
#> 
#> 
#> INSIDE fun2(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
#> 
#> LINE CONSTRUCT       CODE
#> 3    for     for (i in 1:length(x)) {
#> 
fun3 <- function(x){ for(i in base::seq_along(x = x)){ base::print(x = i) } } ; saferDev::for_loop_nb_check(fun3)
#> 
#> 
#> AFTER RUNNING saferDev::for_loop_nb_check().
#> INSIDE fun3(), EVERYTHING SEEMS CLEAN.
#> 
```
