# Check and Rewrite the `\(x)` Lambda Functions

Checks that the anonymous functions are written `function(x)` and not
with the `\(x)` shorthand (e.g., `function(x) x * 2` and not
`\(x) x * 2`). Prints a table of the reported lambdas and returns a
corrected version of the function.

## Usage

``` r
lambda_check(
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

A message is also printed: a table with one row per reported lambda
(column 1: line number; column 2: symbol; column 3: code of the line),
or a sentence saying that everything seems clean.

## Details

Source references: line 1 is the line of the `function` keyword. The
`\(x)` writing is only kept in a function that carries its source
references (attribute `srcref`), e.g., parsed with `keep.source = TRUE`
([`base::parse()`](https://rdrr.io/r/base/parse.html),
[`base::source()`](https://rdrr.io/r/base/source.html)). Functions
created with `options(keep.source = FALSE)` (default of Rscript) and
functions of installed packages (e.g.,
[`saferDev::colons_check`](colons_check.md)) have none: their deparsed
code shows `function(x)`, so that nothing can be reported.

Reported: the lambdas written `\(x)`, like `\(x) x * 2`, `\(x){ x }` or
`\() base::mean(1:3)`, wherever they are written in the function. In the
printed table, the symbol is `\()` when the formals of the lambda span
several lines.

Not reported: the standard `function(x)` writing (an unbraced body like
`function(x) x` is reported by
[`curly_bracket_check()`](curly_bracket_check.md)). A `\(` inside a
string, a comment or a backquoted name is ignored.

Correction: the `\` of each reported lambda is replaced by `function`
(e.g., `\(x) x * 2` becomes `function(x) x * 2`). The rest of the code,
comments, indentation and lambda bodies included, is unchanged.

## See also

The code checkers of saferDev, in the recommended order of use:
[`super_assign_check`](super_assign_check.md), `lambda_check`,
[`colons_check`](colons_check.md), [`all_args_here`](all_args_here.md),
[`curly_bracket_check`](curly_bracket_check.md),
[`true_false_check`](true_false_check.md),
[`for_loop_nb_check`](for_loop_nb_check.md) (see
[`vignette("saferDev")`](../articles/saferDev.md)).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

## Examples

``` r
# See also https://safer-r.github.io/saferDev/articles/lambda_check.html
# The \(x) writing is only kept in a function carrying its source references: hence
# keep.source = TRUE, and srcfile left empty (srcfile = NULL would remove them).
code1 <- "function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) }"
fun1 <- base::eval(expr = base::parse(file = "", n = NULL, text = code1, prompt = "?",
    keep.source = TRUE, srcfile = , encoding = "unknown"), envir = , enclos = )
saferDev::lambda_check(x = fun1, export = FALSE, out_path = NULL, safer_check = TRUE,
    lib_path = NULL, error_text = "") # nothing to correct: NULL returned, invisibly
#> 
#> 
#> AFTER RUNNING saferDev::lambda_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
code2 <- "function(x){ base::sapply(X = 1:3, FUN = \\(y){ y * 2 }) }"
fun2 <- base::eval(expr = base::parse(file = "", n = NULL, text = code2, prompt = "?",
    keep.source = TRUE, srcfile = , encoding = "unknown"), envir = , enclos = )
fun2_safer <- saferDev::lambda_check(x = fun2, export = FALSE, out_path = NULL,
    safer_check = TRUE, lib_path = NULL, error_text = "") # lambda writing reported
#> 
#> 
#> 
#> INSIDE fun2(), SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function():
#> 
#> LINE SYMBOL      CODE
#> 1    \(y)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) }
#> 
fun2_safer # corrected function
#> function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) }
#> <environment: 0x6235c1d0ef78>
# A backslash followed by ( in a string, a raw string, a comment or a backquoted name
# is not a lambda writing: not reported, not modified.
code3 <- base::paste0(
    "function(x){ y <- \"the text \\\\(x) is inside a string\" ; ",
    "z <- r\"(raw \\(x))\" ; # a \\(x) in a comment\n    `\\\\(w)` <- 1 ; x }",
    collapse = NULL, recycle0 = FALSE
)
fun3 <- base::eval(expr = base::parse(file = "", n = NULL, text = code3, prompt = "?",
    keep.source = TRUE, srcfile = , encoding = "unknown"), envir = , enclos = )
saferDev::lambda_check(x = fun3, export = FALSE, out_path = NULL, safer_check = TRUE,
    lib_path = NULL, error_text = "") # nothing to correct: NULL returned, invisibly
#> 
#> 
#> AFTER RUNNING saferDev::lambda_check().
#> INSIDE fun3(), EVERYTHING SEEMS CLEAN.
#> 
# Write the corrected function in the fun2_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::lambda_check(x = fun2, export = TRUE, out_path = tempo_dir, safer_check = TRUE,
    lib_path = NULL, error_text = "")
#> 
#> 
#> 
#> INSIDE fun2(), SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function():
#> 
#> LINE SYMBOL      CODE
#> 1    \(y)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) }
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpQNRq2A/dir11045221b37e/fun2_safer.R
#> 
base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
```
