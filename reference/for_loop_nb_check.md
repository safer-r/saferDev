# Check and Guard the Sequences of `for` Loops

Checks the sequence of every `for` loop of a function. If the number of
iterations is not fixed by the writing itself (e.g., `1:length(x)`), a
checking line is added before the loop, which warns at execution if the
sequence is empty, contains `NA` or infinite values, has a single value,
has only negative values, or starts decreasing (first value greater than
the second). Prints the reported loops and returns a corrected version
of the function.

## Usage

``` r
for_loop_nb_check(
  x,
  export = FALSE,
  out_path = NULL,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- x:

  Function to check, written without quotes and brackets (e.g., `fun1`
  or `pkg::fun1`).

- export:

  Single logical value. If `TRUE`, the corrected function is written
  into the `<name>_safer.R` file of `out_path` instead of being returned
  (e.g., `fun1_safer.R` for `x = fun1` or `x = pkg::fun1`), as
  `fun1_safer <- function(...)`, which
  [`base::source()`](https://rdrr.io/r/base/source.html) recreates.
  Nothing is written if nothing has to be corrected, but an error is
  returned if the file already exists, or if `x` is not a function name
  (e.g., `function(x){x}` or `FUN_LIST$fun`).

- out_path:

  Single character string: path of an existing folder where the file is
  written. Required when `export` is `TRUE`; otherwise not used, but a
  non-`NULL` value must still be a single non-empty character string.
  `NULL` by default.

- safer_check:

  Single logical value. If `TRUE` (default), checks before running that
  the required packages and functions are installed and that the
  classical R operators (like `<-`) are not masked by another package or
  by a user-defined object of the global environment. These checks take
  time at each call: set `FALSE` in loops and inside other safer-r
  functions, to avoid repeating them.

- lib_path:

  Vector of character strings: paths of existing extra library folders
  containing the required packages (e.g., packages installed without
  admin rights). They are searched after the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) during the call
  (the current libraries keep priority), and
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored on
  exit. If `NULL` (default), only the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used.

- error_text:

  Single character string added to the error messages (e.g.,
  `" INSIDE pkg1::fun1()"`) to name the calling functions. `NULL` is
  converted into `""`.

## Value

The corrected function, which keeps the environment of `x`, or `NULL`,
invisibly, if nothing has to be corrected or if `export` is `TRUE`.

A message is also printed: a table with one row per reported loop
(column 1: line number; column 2: construct; column 3: code of the line)
followed by the loops left unchanged, or a sentence saying that
everything seems clean.

## Details

Not reported: hard numbers, i.e., an integer-valued number, possibly
negative, or two such numbers joined by `:` (e.g., `3`, `1:3`, `1e3`,
`1L:3L`, `0:0`, `-3:-1`, `1:0`), whose iterations are fixed by the
writing and left to the control of the writer, and a single
`seq_len(...)` or `seq_along(...)` call, which gives no iteration for a
zero length or an empty object.

Reported: everything else, e.g., `for(i in x)`, `1:n`, `1:length(x)`,
`1:nrow(df)`, `1:(n - 1)`, `seq_along(x) - 1`, `1:2.5`, `1:NA` or
`1:Inf`. Comments, strings and the loops inside formulas,
[`quote()`](https://rdrr.io/r/base/substitute.html),
[`bquote()`](https://rdrr.io/r/base/bquote.html),
[`expression()`](https://rdrr.io/r/base/expression.html),
[`alist()`](https://rdrr.io/r/base/list.html) and
[`substitute()`](https://rdrr.io/r/base/substitute.html), whose code R
does not evaluate as written (e.g., `quote(for(i in 1:n) x)`), are
ignored, except in the parts that R evaluates: `.()` in
[`bquote()`](https://rdrr.io/r/base/bquote.html) (`..()` too if
`splice = TRUE` is written), and the `where` and `splice` arguments of
[`bquote()`](https://rdrr.io/r/base/bquote.html) and `env` of
[`substitute()`](https://rdrr.io/r/base/substitute.html).

Why: with `x <- character()`, `for(i in 1:length(x))` runs twice,
because `1:length(x)` is `c(1, 0)`. Use
`base::seq_along(along.with = x)` instead.

Correction: each reported loop becomes
`tempo_for_seqN <- <SEQUENCE> ; <CHECKS> ; for(i in tempo_for_seqN)<BODY>`,
N being the rank of the report and `<SEQUENCE>` the sequence as written
in the code, on a single line. The checks of negative and decreasing
values only concern numeric sequences (not factors, dates, characters or
complex numbers). Errors of the sequence itself (like `1:NA`) are not
caught. If the loop is not a standalone statement (e.g., unbraced body
of an `if`, or argument value), it is enclosed in curly brackets. The
rest of the code, comments and indentation included, is unchanged.

Warning: a reported loop is left unchanged, and listed in the message,
when the name `tempo_for_seqN` is already used in the code (comments and
strings included), when its sequence contains another reported loop, or
when its sequence cannot be written on a single line with the same
meaning (e.g., a sequence written on several lines that contains a
comment, a string written on several lines, or statements on separate
lines).

Warning: only the writing is analyzed. In
`n <- length(x) ; for(i in 1:n)`, `1:n` is reported, whatever the value
of `n`.

Source references: line 1 is the line of the `function` keyword.
Comments and indentation are kept only if `x` carries its source
references (attribute `srcref`). Functions created with
`options(keep.source = FALSE)` (default of Rscript) and functions of
installed packages (e.g., [`saferDev::colons_check`](colons_check.md))
have none: their deparsed code is analyzed, without comments and with
line numbers that differ from the source file.

## See also

The code checkers of saferDev, in the recommended order of use:
[`super_assign_check`](super_assign_check.md),
[`lambda_check`](lambda_check.md), [`colons_check`](colons_check.md),
[`all_args_here`](all_args_here.md),
[`curly_bracket_check`](curly_bracket_check.md),
[`true_false_check`](true_false_check.md), `for_loop_nb_check` (see
[`vignette("saferDev")`](../articles/saferDev.md)).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Without source references ("Run examples" link, Rscript), line numbers and comments differ.
# See also https://safer-r.github.io/saferDev/articles/for_loop_nb_check.html
fun1 <- function(x){ for(i in base::seq_along(along.with = x)){ base::print(x = i) } }
saferDev::for_loop_nb_check(x = fun1, export = FALSE, out_path = NULL, safer_check = TRUE,
    lib_path = NULL, error_text = "") # nothing to correct: NULL returned, invisibly
#> 
#> 
#> AFTER RUNNING saferDev::for_loop_nb_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
fun2 <- function(x){ for(i in 1:length(x)){ base::print(x = i) } }
fun2_safer <- saferDev::for_loop_nb_check(x = fun2, export = FALSE, out_path = NULL,
    safer_check = TRUE, lib_path = NULL, error_text = "") # 1:length(x) reported
#> 
#> 
#> 
#> INSIDE fun2(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
#> 
#> LINE CONSTRUCT       CODE
#> 3    for     for (i in 1:length(x)) {
#> 
fun2_safer(x = base::character(length = 0)) # 1:length(x) is c(1, 0): the added checking line warns
#> Warning: FOR SEQUENCE STARTING DECREASING
#> [1] 1
#> [1] 0
# Write the corrected function in the fun2_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::for_loop_nb_check(x = fun2, export = TRUE, out_path = tempo_dir, safer_check = TRUE,
    lib_path = NULL, error_text = "")
#> 
#> 
#> 
#> INSIDE fun2(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
#> 
#> LINE CONSTRUCT       CODE
#> 3    for     for (i in 1:length(x)) {
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpQNRq2A/dir11041c765204/fun2_safer.R
#> 
base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
```
