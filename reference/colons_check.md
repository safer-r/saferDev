# Colons Check

Verify that all the functions used inside a function are preceded by a
package attribution. For instance:
[`base::mean()`](https://rdrr.io/r/base/mean.html) and not
[`mean()`](https://rdrr.io/r/base/mean.html), or
`saferDev:::.base_op_check()` and not
[`.base_op_check()`](dot-base_op_check.md). Warning: does not check that
the package is the good one. Use [`all_args_here()`](all_args_here.md)
for that.

## Usage

``` r
colons_check(
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

  Single logical value. Export the modified code into a `.R` file? If
  `TRUE`, the modified function is not returned but only exported. The
  name of the file is the name of the tested function, with `_safer`
  added at the end (e.g., `test_safer.R`). The written code is the code
  of the submitted function, with its comments and indentation, only
  modified by the added package attributions (see the `Details`
  section). An error is returned if the file already exists in
  `out_path`, and if the tested function is not designated by a function
  name (like `test` or `package::test`), i.e., written by an inline
  definition (like `function(x){x}`) or by a component writing (like
  `FUN_LIST$fun`). If `FALSE` (default), the modified function is not
  written but returned.

- out_path:

  Single character string indicating the pathway of the folder where to
  export the modified code. `out_path = "."` (default) means the R
  working directory set by the user. A non-existing directory, `NULL` or
  an empty string (`""`) returns an error. Ignored if `export` is
  `FALSE`.

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

A table-like text message indicating the missing `::` or `:::` or a
message saying that everything seems fine.

The message is always printed (see the `Details` section).

A warning and `NULL` are returned if the submitted function is empty of
function, or only made of comments, or only made of `function`, `if`,
`for`, `while`, `repeat`, `else`.

In addition, if the `export` argument is `FALSE` (default), the function
returns `NULL` if no basic function required a package attribution, or
the modified function if basic functions were modified (see the
`Details` section).

If the `export` argument is `TRUE`, the modified code is exported in the
`.R` file specified by the `out_path` argument, instead of being
returned, and a message indicating where the modified code has been
exported is printed.

Table-like: column 1, the line number in the function code (starting at
the `"<- function"` line, i.e., without counting the `#'` header lines);
column 2, the function name; column 3, the code preceding the function
name.

With missing `::` or `:::`, the message also indicates if internal
functions are created inside the checked function code, since these
functions cannot have `::` or `:::`.

## Details

The result is used to modify the code inside the tested function like
this: `<PACKAGE>::<FUNCTION>` (or `<PACKAGE>:::<FUNCTION>` for function
names starting by a dot).

Since `saferDev` v3.0.0, the modified function is returned: the package
attributions of the basic R functions, as indicated in the
`"INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS"`
section of the returned message, are added to the submitted function
code, and the modified function is returned using
[`return()`](https://rdrr.io/r/base/function.html). The package
attributions of the other functions, as indicated in the
`"INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE OTHER FUNCTIONS"`
section of the returned message, are NOT added automatically, because
the correct package cannot be determined reliably: a
[`base::warning()`](https://rdrr.io/r/base/warning.html) is issued
instead (in addition to the printed message). `NULL` is returned if no
basic function required a package attribution.

The modification is only performed on the code lines indicated in the
`"INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS"`
section of the message, which is the same analysis as the one performed
by `colons_check()` itself: functions written between quotes or preceded
by `$` are not modified, and internal functions (created inside the
tested function) are not modified either. The analysis is performed on
the code with comments removed and multiple spaces collapsed, but the
package attributions are added to the code of the submitted function as
printed: the returned (and exported) function therefore keeps the
comments and the indentation of the submitted function. Warning:
comments are only present when the submitted function carries its source
references (attribute `srcref`, e.g., a function defined in the console
or in a file loaded with `keep.source = TRUE`). Otherwise, as for
compiled package functions, the code printed by `R` (without comments
and reindented) is used. The modified function is returned with the
environment of the submitted function, so that the functions used inside
it still resolve the same way. If the modified code does not parse (a
rare situation, e.g., a function name split across lines in a way the
analyzer could not handle correctly), this is an internal bug: an
internal error is returned when `export = TRUE`, and the submitted
function is returned unchanged, with no error, when `export = FALSE`.

The name of the exported file (when `export = TRUE`) is the name of the
submitted function, as written by the user, with `_safer` added at the
end, e.g., `test_safer.R` for `colons_check(x = test)`. If the submitted
function was written as `package::function`, like
`colons_check(x = saferDev::test)`, the name is `test_safer.R` (the
package name is not part of the file name). The exported file contains
an assignment of the modified function:
`test_safer <- function(...) ...`, so that
[`base::source()`](https://rdrr.io/r/base/source.html)ing it recreates
the modified function. Warning: the tested function must be designated
by a function name (like `test` or `package::test`) when
`export = TRUE`: an inline definition (like `function(x){x}`) or a
component writing (like `FUN_LIST$fun`) returns an error, because the
name of the exported file could not be determined reliably.

Pre-check: the submitted function is first checked by
[`lambda_check()`](lambda_check.md): if some lambda functions are
written with the `\(x)` shorthand, an error is returned asking to run
[`saferDev::lambda_check()`](lambda_check.md) first, because the `\`
writings are not managed by the text-based detection of
`colons_check()`. As in [`lambda_check()`](lambda_check.md), the `\(x)`
writing is only detectable in a function carrying its source references
(attribute `srcref`): a compiled or loaded package function prints
`function(x)`, so that the pre-check silently passes in that case.

More precisely, `colons_check()` verifies that all the strings before an
opening bracket `(` are preceded by `::`.

`:::` are not checked per se, because incorrect writing, like
[`saferDev::.colons_check_message()`](dot-colons_check_message.md)
returns an error when executing the tested function, and because
`base:::sum()` is as ok as
[`base::sum()`](https://rdrr.io/r/base/sum.html). In the same manner,
more than three colons are not checked because it returns an error when
executing the tested function.

Warning: `colons_check()` cannot check function names written without
brackets, like in the `FUN` argument of some functions, e.g.,
`sapply(1:3, FUN = as.character)`.

The Perl regex used to detect a function name is:
`"([a-zA-Z]|\.[a-zA-Z._])[a-zA-Z0-9._]*\s*\("`.

Currently, `colons_check()` cannot detect functions written between
quotes, like [`"+"()`](https://rdrr.io/r/base/Arithmetic.html) or
`"rownames<-"(x, "a")`.

Function names preceded by `$` are not considered.

The following R functions are skipped: `"function"`, `"if"`, `"for"`,
`"while"`, `"repeat"` and `"else"`.

Most of the time, `colons_check()` does not check inside comments, but
some unexpected writing could dupe `colons_check()`. Please, report
[here](https://github.com/safer-r/saferDev/issues) if it is the case.

The returned line numbers are indicative, depending on which source is
checked. For instance, [`saferDev::report`](report.md) (compiled) has
not the same line numbers as its source file ([source
file](https://github.com/safer-r/saferDev/blob/main/R/report.R)).
Notably, compiled functions do not have comments anymore, compared to
the same source function sourced into the working environment. In
addition, the counting starts at the `"<- function"` line, i.e., without
counting the `#'` header lines potentially present in source files.

Of note, during package creation, the
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
command tells which functions where wrongly attributed to package.
Example:

    checking dependencies in R code ... WARNING
      '::' or ':::' import not declared from: 'sbase'
      Missing or unexported objects:
        'base::dev.off' 'base::graphics.off' 'base::hcl' 'base::par' 'base::read.table' 'saferGG::report'

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/colons_check.html
fun1 <- function(x){
    y <- mean(x) # mean() written without base::
    base::return(y)
}
fun1_safer <- saferDev::colons_check(fun1) # base:: added before mean()
#> 
#> 
#> INSIDE fun1(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
#> 
#> LINE FUN     STRING_BEFORE
#> 3    mean        y <- 
#> 
fun1_safer # corrected function
#> function (x) 
#> {
#>     y <- base::mean(x)
#>     base::return(y)
#> }
#> <environment: 0x558e6e86bea0>
# Write the corrected function in the fun1_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::colons_check(x = fun1, export = TRUE, out_path = tempo_dir)
#> 
#> 
#> INSIDE fun1(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
#> 
#> LINE FUN     STRING_BEFORE
#> 3    mean        y <- 
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpBNefhH/dir176213966162/fun1_safer.R
#> 
```
