# Curly Brackets Check

Verify that all the bodies of the `if`, `else`, `for`, `while`, `repeat`
and `function` are delimited by curly brackets
[`{ }`](https://rdrr.io/r/base/Paren.html). For instance:
`if(x > 1){ x }` and not `if(x > 1) x`. Return table message of the line
with problems and a corrected version of the submitted function.

## Usage

``` r
curly_bracket_check(
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

A table-like message is always printed, with one row per missing
[`{ }`](https://rdrr.io/r/base/Paren.html) (column 1: line number;
column 2: construct; column 3: code before the body), or a message
saying that everything seems fine.

If `export = FALSE`: the corrected function, or `NULL` if nothing has to
be corrected.

If `export = TRUE`: `NULL`, returned invisibly. The corrected function
is written in a file (see the `export` argument).

## Details

Comments are ignored. In `else if` chains, the `else` is not reported:
the chained `if` is checked on its own.

Bodies reduced to `.Primitive(...)`, like in
[`` base::`+` ``](https://rdrr.io/r/base/Arithmetic.html), are skipped,
because they cannot be enclosed in curly brackets. Bodies made of
`UseMethod(...)`, like in
[`base::mean`](https://rdrr.io/r/base/mean.html), are reported.

Correction: a `{` is added before each reported body and a `}` after it,
separated from the body by a space. The rest of the code is unchanged.
The corrected code is checked by parsing it. Lines can become longer
than 80 characters. The returned function keeps the environment of the
submitted function.

The comments and the indentation of the submitted function are kept, in
the returned function and in the exported file. Warning: comments are
only available when the submitted function carries its source references
(attribute srcref). Otherwise (e.g., compiled package functions, or
functions created with `options(keep.source = FALSE)`, default of
Rscript), the code is the deparsed one: no comments and standard
indentation.

Constructs written with backquotes, like `` `if`(x > 1, x, 0) ``, may
not be checked: use the standard writing.

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
# See also https://safer-r.github.io/saferDev/articles/curly_bracket_check.html
fun1 <- function(x){ if(x > 1){ base::return(x) } }
saferDev::curly_bracket_check(fun1) # everything fine: NULL returned
#> 
#> 
#> AFTER RUNNING saferDev::curly_bracket_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
fun2 <- function(x){ if(x > 1) base::return(x) }
fun2_safer <- saferDev::curly_bracket_check(fun2) # missing { } reported
#> 
#> 
#> 
#> INSIDE fun2(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
#> 
#> LINE CONSTRUCT       CODE
#> 3    if      if (x > 1) 
#> 
fun2_safer # corrected function
#> function (x) 
#> {
#>     if (x > 1) 
#>         { base::return(x) }
#> }
#> <environment: 0x55e99f2c0bc0>
# Write the corrected function in the fun2_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::curly_bracket_check(x = fun2, export = TRUE, out_path = tempo_dir)
#> 
#> 
#> 
#> INSIDE fun2(), SOME CURLY BRACKETS {} ARE MISSING AFTER:
#> 
#> LINE CONSTRUCT       CODE
#> 3    if      if (x > 1) 
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpSIxeUR/dir13d1302278c8/fun2_safer.R
#> 
```
