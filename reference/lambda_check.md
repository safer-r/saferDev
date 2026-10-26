# Lambda Check

Verify if lambda functions are written with the shorthand `\(x)` (like
`\(x) x * 2`). Return table message of the line with problems and a
corrected version of the submitted function.

## Usage

``` r
lambda_check(
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
  written in `out_path` (e.g., `fun_safer.R` for `x = fun`). Nothing is
  returned or exported if nothing has to be corrected. An error is
  returned if exported and the file already exists.

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

A table-like message is always printed, with one row per reported lambda
(column 1: line number; column 2: symbol; column 3: code before the
lambda), or a message saying that everything seems fine.

If `export = FALSE`: the corrected function, or `NULL` if nothing has to
be corrected.

If `export = TRUE`: `NULL`, returned invisibly. The corrected function
is written in a file (see the `export` argument).

## Details

Warning: the `\(x)` writing of a lambda function is only kept in the
code of the submitted function when the function carries its source
references (attribute `srcref`, e.g., after a
`base::parse(keep.source = TRUE)` or a function defined in a file loaded
with `keep.source = TRUE`). Otherwise, `R` prints the lambda as
`function(x)`, so that nothing can be reported: run `lambda_check()` on
the source code itself, not on a compiled or on a loaded package
function. The `base::getOption("keep.source")` default is `TRUE` in
interactive sessions and `FALSE` in `Rscript`.

Reported: lambdas written `\(x)`, like `\(x) x * 2`, `\(x){ x }` or
`\() base::mean(1:3)`, wherever they are written in the submitted
function (right side of an assignment, argument value of a function
call, etc.). In the message table, the SYMBOL column shows the lambda
writing with its formals only when they are written on a single line:
for a lambda whose formals span several lines, the fallback writing
`\()` is shown (the whole writing is still replaced correctly in the
code, because only the `\` token is replaced).

Not reported: the `function(x)` standard writing, and lambda writings of
the past (e.g., `function(x) x`) reported by other functions. Strings,
comments and backquoted names (like a backquoted backslash, the
deprecated `function()` writing of R versions before 4.1.0) are ignored:
a backslash followed by `(` inside a string, a raw string, a comment or
a backquoted name is never reported nor modified, whatever the rest of
the written text (the detection is token-based, not text-based). A
lambda body itself is not modified: run
[`curly_bracket_check()`](curly_bracket_check.md) and
[`true_false_check()`](true_false_check.md) afterwards to check the
corrected function further.

Correction: each reported `\` lambda writing is replaced by `function`
(e.g., `\(x) x * 2` becomes `function(x) x * 2`). The rest of the code
is unchanged. The corrected code is checked by parsing it. Lines can
become longer than 80 characters. The returned function keeps the
environment of the submitted function.

Warning: the comments of the submitted function are lost in the returned
function (its code is re-parsed). They are kept in the exported file,
which contains the recovered code verbatim, only modified by the `\` -\>
`function` replacements.

Line numbers are indicative. They start at the `<- function` line (`#'`
header lines not counted). Compiled functions, like
[`saferDev::colons_check`](colons_check.md), have no comments: their
line numbers differ from their source file.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/lambda_check.html
# The \(x) writing is only kept in a function carrying its source references,
# hence the parse(keep.source = TRUE) writing used here:
code1 <- "function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) }"
fun1 <- eval(parse(text = code1, keep.source = TRUE))
saferDev::lambda_check(fun1) # everything fine: NULL returned
#> 
#> 
#> AFTER RUNNING saferDev::lambda_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
code2 <- "function(x){ base::sapply(X = 1:3, FUN = \\(y){ y * 2 }) }"
fun2 <- eval(parse(text = code2, keep.source = TRUE))
fun2_safer <- saferDev::lambda_check(fun2) # lambda writing reported
#> 
#> 
#> 
#> INSIDE fun2(), SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function():
#> 
#> LINE SYMBOL      CODE
#> 1    \(y)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) }
#> 
fun2_safer # corrected function
#> function (x) 
#> {
#>     base::sapply(X = 1:3, FUN = function(y) {
#>         y * 2
#>     })
#> }
#> <environment: 0x558e6f310030>
# A backslash followed by ( in a string, a raw string, a comment or a backquoted name
# is not a lambda writing (token-based detection): not reported, not modified.
code3 <- paste0(
    "function(x){ y <- \"the text \\\\(x) is inside a string\" ; ",
    "z <- r\"(raw \\(x))\" ; # a \\(x) in a comment\n    base::`\\\\`(x) ; x }"
)
fun3 <- eval(parse(text = code3, keep.source = TRUE))
saferDev::lambda_check(fun3) # everything fine: NULL returned
#> 
#> 
#> AFTER RUNNING saferDev::lambda_check().
#> INSIDE fun3(), EVERYTHING SEEMS CLEAN.
#> 
# Write the corrected function in the fun2_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::lambda_check(x = fun2, export = TRUE, out_path = tempo_dir)
#> 
#> 
#> 
#> INSIDE fun2(), SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function():
#> 
#> LINE SYMBOL      CODE
#> 1    \(y)        function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) }
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpBNefhH/dir17627aff41c8/fun2_safer.R
#> 
```
