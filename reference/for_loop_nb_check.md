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
for_loop_nb_check(
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
  performed (everything clean, or no checking line inserted). If `FALSE`
  (default), the modified function is returned by
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

A table-like text message indicating the `for` sequences not written
with hard numbers or a message saying that everything seems fine,
printed by [`base::message()`](https://rdrr.io/r/base/message.html)
(always, whatever the value of the `export` argument).

Table-like: column 1, the line number in the function code (starting at
the `"<- function"` line, i.e., without counting the `#'` header lines);
column 2, the construct (`for`); column 3, the code preceding the
construct body.

In addition, the returned value depends on the `export` argument and on
the presence of reported `for` sequences. If `export` is `FALSE` and at
least one checking line has been inserted (see the `@details` section),
the modified function is returned by
[`return()`](https://rdrr.io/r/base/function.html). If `export` is
`TRUE`, `NULL` is returned (invisibly), and the modified code is only
available in the exported file, whose location is indicated by a
message. `NULL` is also returned when no modification is performed
(everything clean, or no checking line inserted).

## Details

The reported `for` sequences are the ones whose computed controller
depends on objects or function results, i.e., whose number of iterations
is not predictable from the writing itself. The modification consists in
adding a checking line before each reported `for` construct (see below).

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

Deliberately NOT reported, because these writings are fully determined
by the code itself: their control is the responsibility of the writer,
and no checking line is added for them: the equal-bounds (like `0:0` or
`1:1`), both-negative (like `-3:-1`) and decreasing (like `1:0`)
hard-number writings.

Each reported `for` construct is modified in the recovered code: the
sequence expression is evaluated once in a specific object named
`tempo_for_seqN` (N being the rank of the report), this object is
checked, and then used as the `for` controller, giving
`tempo_for_seqN <- <SEQUENCE> ; <CHECKS> ; for(<VARIABLE> in tempo_for_seqN)<BODY>`.
Evaluating the sequence expression once in the `tempo_for_seqN` object
guarantees that the checking and the `for` controller are strictly
identical. At execution,
[`base::warning()`](https://rdrr.io/r/base/warning.html) is issued when
the computed sequence is empty (0 iteration), contains `NA` values, has
a single value (equal bounds), contains infinite values, or is
decreasing (like `1:0`). The errors of the sequence evaluation itself
(like `1:NA` or `1:Inf`) are propagated as they are. Warning: no endless
loop is possible with `Inf`, because R fully evaluates the `for`
controller before iterating: `1:Inf` gives an error at evaluation, and
an `Inf` element just gives one iteration with `i = Inf`, whatever a
potential [`break`](https://rdrr.io/r/base/Control.html) in the loop
body. The modification is performed at the exact positions of the
reported `for` constructs in the recovered code, so that comments,
strings and the formatting of the original code are preserved outside
the modified spans. When the reported `for` construct is not preceded by
a statement boundary (unbraced body of
`if`/`else`/`for`/`while`/`repeat`, assignment value, function argument,
operator operand, etc.), the whole modified construct is wrapped in
curly brackets, because a statement insertion would otherwise break the
binding of the enclosing writing. When `export` is `FALSE`, the modified
function is returned by
[`return()`](https://rdrr.io/r/base/function.html) in a new environment
whose parent is the environment of the submitted function: please, check
the returned function before replacing the original one, notably because
its environment is not necessarily the same. When `export` is `TRUE`,
the modified code is written in the file `<function name>_safer.R` of
the directory indicated by the `out_path` argument, and nothing is
written if no modification was performed. Warning: the exported file is
a standalone R script containing the modified function code, and its
potential pre-existing version is never overwritten (an error is
returned instead). Warning: the `x` argument must be designated by a
function name, not by an inline function definition (like
`function(x){ x }`), when `export` is `TRUE`, otherwise the name of the
exported file cannot be determined.

Warning: the checking lines are not inserted when the tempo object name
(`tempo_for_seqN`) is already used anywhere in the checked function code
(including in comments and strings), or when a reported `for` construct
is located inside the sequence expression of another reported `for`
construct (overlapping insertion spans): the concerned `for` constructs
are listed in the returned message, and remain unmodified. The checking
lines are also not inserted, for the same reason (listed in the
message), when a `for` construct that requires the wrap in curly
brackets (see above) contains comments in its span (the wrap collapses
the construct on a single line, which would merge the comments with the
code), or when such a construct shares the end of its body with a `for`
construct written after it and also requiring the wrap (as in
`for(a in 1:n) for(b in 1:length(x)){...}`, where the two bodies end at
the same position: the wrap of the latter, applied first, moves the
shared closing token away from the position where the former would end
its own body).

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
#> NULL
fun2 <- function(x){ for(i in 1:length(x)){ base::print(x = i) } } ; saferDev::for_loop_nb_check(fun2)
#> 
#> 
#> 
#> INSIDE fun2(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
#> 
#> LINE CONSTRUCT       CODE
#> 3    for     for (i in 1:length(x)) {
#> 
#> MODIFIED FUNCTION RETURNED BY return().
#> 
#> function (x) 
#> {
#>     tempo_for_seq1 <- 1:length(x)
#>     if (base::length(x = tempo_for_seq1) == 0) {
#>         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
#>             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
#>     }
#>     else if (base::anyNA(x = tempo_for_seq1)) {
#>         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
#>             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
#>     }
#>     else if (base::length(x = tempo_for_seq1) == 1) {
#>         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
#>             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
#>             domain = NULL)
#>     }
#>     else if (base::is.atomic(x = tempo_for_seq1) && base::any(base::is.infinite(x = tempo_for_seq1), 
#>         na.rm = TRUE)) {
#>         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
#>             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
#>             domain = NULL)
#>     }
#>     else if (base::is.atomic(x = tempo_for_seq1) && base::typeof(x = tempo_for_seq1) %in% 
#>         base::c("double", "integer") && base::all(x = tempo_for_seq1 < 
#>         0, na.rm = FALSE)) {
#>         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
#>             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
#>             domain = NULL)
#>     }
#>     else if (base::is.atomic(x = tempo_for_seq1) && base::typeof(x = tempo_for_seq1) %in% 
#>         base::c("double", "integer", "character", "complex") && 
#>         base::length(x = tempo_for_seq1) >= 2 && tempo_for_seq1[1] > 
#>         tempo_for_seq1[2]) {
#>         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
#>             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
#>     }
#>     for (i in tempo_for_seq1) {
#>         base::print(x = i)
#>     }
#> }
#> <environment: 0x555f06abaa50>
fun3 <- function(x){ for(i in base::seq_along(x = x)){ base::print(x = i) } } ; saferDev::for_loop_nb_check(fun3)
#> 
#> 
#> AFTER RUNNING saferDev::for_loop_nb_check().
#> INSIDE fun3(), EVERYTHING SEEMS CLEAN.
#> 
#> NULL
fun4 <- function(x){ for(i in 1:length(x)){ base::print(x = i) } } ; fun4_safer <- saferDev::for_loop_nb_check(x = fun4, export = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "") # modified function returned (checking line added)
#> 
#> 
#> 
#> INSIDE fun4(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
#> 
#> LINE CONSTRUCT       CODE
#> 3    for     for (i in 1:length(x)) {
#> 
#> MODIFIED FUNCTION RETURNED BY return().
#> 
fun5 <- function(x){ for(i in 0:0){ base::print(x = i) } } ; saferDev::for_loop_nb_check(x = fun5, export = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "") # NULL returned: the degenerate hard-number writings are the responsibility of the writer, no checking line inserted
#> 
#> 
#> AFTER RUNNING saferDev::for_loop_nb_check().
#> INSIDE fun5(), EVERYTHING SEEMS CLEAN.
#> 
#> NULL
```
