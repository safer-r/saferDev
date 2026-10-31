# True False Check

Verify that the `TRUE` and `FALSE` logical constants are always written
in full when used as values (argument values, conditions, comparisons,
indexing) and never abbreviated as `T` or `F`, which can be overwritten
by the user. Return table message of the line with problems and a
corrected version of the submitted function.

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

  Single logical value. Perform the "safer" checks? If `TRUE`, checkings
  are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) required functions and
  related packages effectively present in local R libraries and 2) R
  classical operators (like `"<-"`) not overwritten by another package
  because of the R scope. Warning: must be set to `FALSE` if this
  function is used inside another "safer" function to avoid pointless
  multiple checkings.

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
  (default): only the pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

A table-like message is always printed, with one row per reported symbol
(column 1: line number; column 2: symbol; column 3: code before the
symbol), or a message saying that everything seems fine.

If `export = FALSE`: the corrected function, or `NULL` if nothing has to
be corrected.

If `export = TRUE`: `NULL`, returned invisibly. The corrected function
is written in a file (see the `export` argument).

## Details

Warning: `true_false_check()` only checks the `T` and `F` symbols used
as values: the package association of the function calls is not checked
(run [`saferDev::colons_check()`](colons_check.md) separately for that).
Function names written without brackets (e.g., `FUN = as.character`) or
used as function names (e.g.,
[`T()`](https://rdrr.io/r/base/logical.html)) are not reported.

Reported: `T` or `F` used as a value, e.g., `f(log = T)`, `f(T)`,
`if(T)`, `while(T)`, `x == T` or `x[T]`.

Not reported: `T` or `F` used as a name or assigned, e.g., `T <- 1`,
`x <- T`, `function(T)`, `f(T = 1)`, `df$T`, `for(T in 1:3)`,
[`T()`](https://rdrr.io/r/base/logical.html),
[`?T`](https://rdrr.io/r/base/logical.html) or `y ~ T`. Strings,
comments and backquoted names (`` `T` ``) are ignored. If `T` or `F` is
defined anywhere in the function (argument, loop variable or
assignment), none of its uses is reported.

Correction: each reported `T` is replaced by `TRUE` and each `F` by
`FALSE` ([`base::T`](https://rdrr.io/r/base/logical.html) also becomes
`TRUE`). The rest of the code is unchanged. The comments and the
indentation of the submitted function are kept, in the returned function
and in the exported file. Warning: comments are only available when the
submitted function carries its source references (attribute srcref).
Otherwise (e.g., compiled package functions, or functions created with
`options(keep.source = FALSE)`, default of Rscript), the code is the
deparsed one: no comments and standard indentation. The returned
function is evaluated in a new environment whose parent is the
environment of the submitted function: check it before replacing the
original function.

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
# See also https://safer-r.github.io/saferDev/articles/true_false_check.html
fun1 <- function(x){ if(x == TRUE){ base::return(x) } }
saferDev::true_false_check(fun1) # everything fine: NULL returned
#> 
#> 
#> AFTER RUNNING saferDev::true_false_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
#> NULL
fun2 <- function(x){ if(x == T){ base::return(x) } }
fun2_safer <- saferDev::true_false_check(fun2) # T reported
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
#> <environment: 0x561e4a12d720>
# Write the corrected function in the fun2_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::true_false_check(x = fun2, export = TRUE, out_path = tempo_dir)
#> 
#> 
#> 
#> INSIDE fun2(), SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:
#> 
#> LINE SYMBOL      CODE
#> 3    T       if (x == T) {
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/Rtmpbw7R90/dir172f96eaf75/fun2_safer.R
#> 
```
