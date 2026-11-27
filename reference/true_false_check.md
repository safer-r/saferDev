# Check and Write `TRUE` and `FALSE` in Full

Checks that the logical constants `TRUE` and `FALSE` are written in full
when used as values, and not abbreviated as `T` or `F`, which are
ordinary objects that can be overwritten (e.g., `T <- 0`). Prints the
reported symbols and returns a corrected version of the function.

## Usage

``` r
true_false_check(
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

A message is also printed: a table with one row per reported symbol
(column 1: line number; column 2: symbol; column 3: code of the line),
or a sentence saying that everything seems clean.

## Details

Reported: `T` or `F` used as a value, e.g., `f(log = T)`, `f(T)`,
`if(T)`, `while(T)`, `x == T`, `x[T]` or `z <- T & y`.

Not reported: `T` or `F` used as a name or as the whole assigned value,
e.g., `T <- 1`, `x <- T`, `function(T)`, `f(T = 1)`, `df$T`,
`for(T in 1:3)`, [`T()`](https://rdrr.io/r/base/logical.html),
[`?T`](https://rdrr.io/r/base/logical.html) or `y ~ T`, and the `T` and
`F` inside [`quote()`](https://rdrr.io/r/base/substitute.html),
[`bquote()`](https://rdrr.io/r/base/bquote.html),
[`expression()`](https://rdrr.io/r/base/expression.html),
[`alist()`](https://rdrr.io/r/base/list.html) and
[`substitute()`](https://rdrr.io/r/base/substitute.html), whose code R
does not evaluate as written (e.g., the plotmath label
`expression(T[max])`), except the parts that R evaluates: `.()` in
[`bquote()`](https://rdrr.io/r/base/bquote.html) (`..()` too if
`splice = TRUE` is written), and the `where` and `splice` arguments of
[`bquote()`](https://rdrr.io/r/base/bquote.html) and `env` of
[`substitute()`](https://rdrr.io/r/base/substitute.html). In the other
functions that use the code of their arguments, `T` and `F` are
replaced, even when they are names (e.g., a column named `T` in
`base::subset(x = d, subset = T > 30)`, which becomes `TRUE > 30`).
Strings, comments and backquoted names (`` `T` ``) are ignored. If `T`
or `F` is defined anywhere in the function (argument, loop variable or
assignment), none of its uses is reported.

Correction: each reported `T` is replaced by `TRUE` and each `F` by
`FALSE` ([`base::T`](https://rdrr.io/r/base/logical.html) also becomes
`TRUE`). The rest of the code, comments and indentation included, is
unchanged.

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
[`curly_bracket_check`](curly_bracket_check.md), `true_false_check`,
[`for_loop_nb_check`](for_loop_nb_check.md) (see
[`vignette("saferDev")`](../articles/saferDev.md)).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Without source references ("Run examples" link, Rscript), line numbers and comments differ.
# See also https://safer-r.github.io/saferDev/articles/true_false_check.html
fun1 <- function(x){ if(x == TRUE){ base::return(x) } }
saferDev::true_false_check(x = fun1, export = FALSE, out_path = NULL, safer_check = TRUE,
    lib_path = NULL, error_text = "") # nothing to correct: NULL returned, invisibly
#> 
#> 
#> AFTER RUNNING saferDev::true_false_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
fun2 <- function(x){ if(x == T){ base::return(x) } }
fun2_safer <- saferDev::true_false_check(x = fun2, export = FALSE, out_path = NULL,
    safer_check = TRUE, lib_path = NULL, error_text = "") # T reported
#> 
#> 
#> 
#> INSIDE fun2(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
#> 
#> LINE SYMBOL      CODE
#> 3    T       if (x == T) {
#> 
fun2_safer # corrected function
#> function (x) 
#> {
#>     if (x == TRUE) {
#>         base::return(x)
#>     }
#> }
#> <environment: 0x6235c62e0e80>
# Write the corrected function in the fun2_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::true_false_check(x = fun2, export = TRUE, out_path = tempo_dir, safer_check = TRUE,
    lib_path = NULL, error_text = "")
#> 
#> 
#> 
#> INSIDE fun2(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
#> 
#> LINE SYMBOL      CODE
#> 3    T       if (x == T) {
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpQNRq2A/dir11047667486d/fun2_safer.R
#> 
base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
```
