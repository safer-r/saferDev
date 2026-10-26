# `for` Loop Number Check

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

  Single logical value. Write the corrected function into a file instead
  of returning it? If `TRUE`, the file `<function name>_safer.R` is
  written in `out_path` (e.g., `fun_safer.R` for `x = fun` or
  `x = pkg::fun`). It contains `fun_safer <- function(...) ...`, so that
  [`base::source()`](https://rdrr.io/r/base/source.html) recreates the
  corrected function. Nothing is written if nothing has to be corrected.
  An error is returned if the file already exists, or if `x` is not a
  function name (e.g., `function(x){x}` or `FUN_LIST$fun`).

- out_path:

  Single character string indicating the path of the folder where to
  write the file when `export = TRUE`. `"."` (default) means the R
  working directory. Ignored if `export = FALSE`.

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

A table-like message is always printed, with one row per reported loop
(column 1: line number; column 2: construct; column 3: code before the
body), or a message saying that everything seems fine.

If `export = FALSE`: the corrected function, or `NULL` if nothing has
been corrected.

If `export = TRUE`: `NULL`, returned invisibly. The corrected function
is written in a file (see the `export` argument).

## Details

Not reported: hard numbers, i.e., an integer-valued number, possibly
negative, or two such numbers joined by `:` (e.g., `3`, `1:3`, `1e3`,
`1L:3L`, `0:0`, `-3:-1`, `1:0`), and a single `seq_len(...)` or
`seq_along(...)` call. These writings fully fix the iterations, and
their control is left to the writer.

Reported: everything else, e.g., `for(i in x)`, `1:n`, `1:length(x)`,
`1:nrow(df)`, `1:(n - 1)`, `seq_along(x) - 1`, `1:2.5`, `1:NA` or
`1:Inf`. Comments and strings are ignored.

Why: with `x <- character()`, `for(i in 1:length(x))` runs twice,
because `1:length(x)` is `c(1, 0)`. Use
`base::seq_along(along.with = x)` instead.

Correction: each reported loop becomes
`tempo_for_seqN <- <SEQUENCE> ; <CHECKS> ; for(i in tempo_for_seqN)<BODY>`,
N being the rank of the report. The checks warn if the sequence is
empty, contains `NA` or infinite values, has a single value, has only
negative values, or is decreasing (these last two checks only for
numeric sequences, i.e., not for factors, dates, characters or complex
numbers). The loop thus runs on the checked sequence. Errors of the
sequence itself (like `1:NA`) are not caught. If the loop is not a
standalone statement (e.g., unbraced body of an `if`, or argument
value), the whole is enclosed in curly brackets. The rest of the code is
unchanged. The comments are kept in the exported file, but can be lost
in the returned function. The returned function is evaluated in a new
environment whose parent is the environment of the submitted function:
check it before replacing the original function.

Warning: a reported loop is left unchanged, and listed in the message,
when:

- the name `tempo_for_seqN` is already used in the function code
  (including comments and strings);

- the loop is inside the sequence expression of another reported loop;

- the loop must be enclosed in curly brackets and contains comments;

- the loop must be enclosed in curly brackets and ends with another loop
  that must also be (e.g., `for(a in 1:n) for(b in 1:length(x)){...}`).

Warning: only the writing is analyzed. In
`n <- length(x) ; for(i in 1:n)`, `1:n` is reported as a variable,
whatever `n`.

Line numbers are indicative. They start at the `<- function` line (`#'`
header lines not counted). Compiled functions, like
[`saferDev::colons_check`](colons_check.md), have no comments: their
line numbers differ from their source file.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/for_loop_nb_check.html
fun1 <- function(x){ for(i in base::seq_along(along.with = x)){ base::print(x = i) } }
saferDev::for_loop_nb_check(fun1) # everything fine: NULL returned
#> 
#> 
#> AFTER RUNNING saferDev::for_loop_nb_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
#> NULL
fun2 <- function(x){ for(i in 1:length(x)){ base::print(x = i) } }
fun2_safer <- saferDev::for_loop_nb_check(fun2) # 1:length(x) reported
#> 
#> 
#> 
#> INSIDE fun2(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
#> 
#> LINE CONSTRUCT       CODE
#> 3    for     for (i in 1:length(x)) {
#> 
fun2_safer # corrected function
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
#>     else if (base::is.numeric(x = tempo_for_seq1) && base::all(tempo_for_seq1 < 
#>         0, na.rm = FALSE)) {
#>         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
#>             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
#>             domain = NULL)
#>     }
#>     else if (base::is.numeric(x = tempo_for_seq1) && base::length(x = tempo_for_seq1) >= 
#>         2 && tempo_for_seq1[1] > tempo_for_seq1[2]) {
#>         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
#>             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
#>     }
#>     for (i in tempo_for_seq1) {
#>         base::print(x = i)
#>     }
#> }
#> <environment: 0x55e99cfae318>
fun2_safer(x = base::character()) # warning: 1:length(x) is c(1, 0), a decreasing sequence
#> Warning: FOR SEQUENCE DECREASING
#> [1] 1
#> [1] 0
# Write the corrected function in the fun2_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::for_loop_nb_check(x = fun2, export = TRUE, out_path = tempo_dir)
#> 
#> 
#> 
#> INSIDE fun2(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
#> 
#> LINE CONSTRUCT       CODE
#> 3    for     for (i in 1:length(x)) {
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpSIxeUR/dir13d122e488b6/fun2_safer.R
#> 
```
