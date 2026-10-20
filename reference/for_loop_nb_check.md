# For Loop Number Check

Check the sequence expression of every `for` loop of a function. If the
number of iterations is not fixed by the writing itself (e.g.,
`1:length(x)`), a checking line is added before the loop, issuing a
warning at execution if the sequence is empty, contains `NA` or infinite
values, has a single value, or is decreasing. Return a table-like
message of the problematic lines and a corrected version of the
submitted function.

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

  A function name, written without quotes and brackets.

- export:

  Single logical value. Export the modified function into a `.R` file
  (see the `out_path` argument)? If `TRUE`, the file is named
  `<function name>_safer.R` (`pkg::fun` gives `fun_safer.R`) and
  contains `<function name>_safer <- function(...) ...`, so that
  [`base::source()`](https://rdrr.io/r/base/source.html) recreates it.
  An error is returned if the file already exists, or if `x` is not a
  function name (inline definition like `function(x){x}`, or component
  like `FUN_LIST$fun`). Nothing is written if no modification is
  performed. See the `@returns` section for the returned value.

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
with hard numbers, or a message saying that everything seems fine,
printed by [`base::message()`](https://rdrr.io/r/base/message.html).

Table-like: column 1, the line number in the function code (starting at
the `"<- function"` line, i.e., without counting the `#'` header lines);
column 2, the construct (`for`); column 3, the code preceding the
construct body.

In addition:

If `export = FALSE`: the modified function (checking lines added, see
the `@details` section), or `NULL` if no modification was performed
(everything clean, or no checking line inserted).

If `export = TRUE`: `NULL` (invisibly), the modified function being
written into the `<function name>_safer.R` file (see the `export`
argument).

## Details

Clean writings (not reported): hard numbers, i.e., an optional unary
minus and a finite integer-valued numeric literal, optionally followed
by `:` and another such literal, with nothing else (e.g., `3`, `-3`,
`1:3`, `1e3`, `1L:3L`, `0x10:0x11`), or a `seq_len(...)` or
`seq_along(...)` call (namespaced or not) occupying the whole sequence
expression. Any other writing is reported, e.g., variables
(`for(i in x)`, `1:n`), calls (`1:length(x)`, `1:nrow(df)`,
`base::rev(x = x)`), arithmetic (`1:(n - 1)`, `1:3 - 1`,
`seq_along(x) - 1`) and non-integer or non-finite literals (`1:2.5`,
`1:NA`, `1:Inf`, `1:TRUE`). Comments and strings are ignored. Rationale:
`x <- character() ; for(i in 1:length(x)){y <- i}` loops twice and ends
with `y = 0`, because `1:length(x)` gives `c(1, 0)` when `x` is empty:
`base::seq_along(x = x)` should be used instead.

Not reported either, because fully determined by the code itself (their
control is the responsibility of the writer): the equal-bounds (`0:0`,
`1:1`), both-negative (`-3:-1`) and decreasing (`1:0`) hard-number
writings.

Modification: each reported construct becomes
`tempo_for_seqN <- <SEQUENCE> ; <CHECKS> ; for(<VARIABLE> in tempo_for_seqN)<BODY>`
(N being the rank of the report), so that the checked sequence and the
`for` controller are strictly identical. At execution,
[`base::warning()`](https://rdrr.io/r/base/warning.html) is issued when
the sequence is empty, contains `NA` or infinite values, has a single
value, or is decreasing. Errors of the sequence evaluation itself (like
`1:NA`) are propagated as they are. Only the reported spans are
modified: comments, strings and formatting are preserved elsewhere. A
construct not preceded by a statement boundary (unbraced body of
`if`/`else`/`for`/`while`/`repeat`, assignment value, function argument,
operand, etc.) is wrapped in curly brackets. The returned function is in
a new environment whose parent is the environment of the submitted
function: check it before replacing the original one.

Warning: the checking lines are not inserted (the concerned constructs
are listed in the message and remain unmodified) when:

- the name `tempo_for_seqN` is already used in the function code
  (including comments and strings);

- the reported construct is inside the sequence expression of another
  reported construct;

- a construct requiring the curly bracket wrap contains comments (the
  wrap collapses it on a single line);

- a construct requiring the wrap shares the end of its body with a later
  construct also requiring it (e.g.,
  `for(a in 1:n) for(b in 1:length(x)){...}`).

Warning: with aliased bounds like `n <- length(x) ; for(i in 1:n)`, only
`1:n` is analyzed (reported as a plain variable), not the origin of `n`.

Line numbers are indicative: compiled functions (like
[`saferDev::colons_check`](colons_check.md)) have no comments, hence
line numbers different from their source file.

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
#> <environment: 0x55eddec26820>
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
