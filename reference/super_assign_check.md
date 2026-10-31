# Superassignment Check

Verify that the superassignment operators `<<-` and `->>` are not used
in a function. Return an error with a table of the lines with problems
if any, since these operators are not allowed in safer-r functions.

## Usage

``` r
super_assign_check(x, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- x:

  A function name, written without quotes and brackets.

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

If no superassignment operator is detected: a message saying that
everything seems fine, and `NULL` returned invisibly.

Otherwise: an error, with a table-like message made of one row per
reported operator (column 1: line number; column 2: symbol; column 3:
code of the line).

## Details

Why an error: the superassignment operators `<<-` and `->>` assign in
the first environment, among the parent environments of the evaluation
environment, where the name already exists, and in the global
environment if the name exists nowhere. What is modified thus depends on
the environments where the function is defined and called, not on the
code of the function itself: it is difficult to guess what the writer
had in mind, and nothing can be corrected automatically. The code must
be rewritten, for instance by returning the value, or by using
[`base::assign()`](https://rdrr.io/r/base/assign.html) with an explicit
`envir` argument.

Reported: every `<<-` and `->>` operator written in the submitted
function, including inside the functions defined in the submitted
function (e.g., a `FUN` argument of
[`base::lapply()`](https://rdrr.io/r/base/lapply.html) or the handlers
of [`base::tryCatch()`](https://rdrr.io/r/base/conditions.html)).

Not reported: the `<-`, `->` and `=` assignment operators, and the `<<-`
or `->>` writings inside strings, comments or backquoted names (the
detection is token-based, not text-based).

Line numbers are indicative. They start at the `<- function` line (`#'`
header lines not counted). Compiled functions, like
[`saferDev::colons_check`](colons_check.md), have no comments: their
line numbers differ from their source file. A `->>` operator is printed
`<<-` in the code of a function without source references (attribute
`srcref`).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/super_assign_check.html
fun1 <- function(x){ y <- x ; base::return(y) }
saferDev::super_assign_check(fun1) # everything fine: NULL returned
#> 
#> 
#> AFTER RUNNING saferDev::super_assign_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
fun2 <- function(x){ y <<- x ; base::return(y) }
try(saferDev::super_assign_check(fun2)) # error: <<- reported
#> Error : 
#> 
#> ================
#> 
#> ERROR IN saferDev::super_assign_check().
#> 
#> INSIDE fun2(), THE SUPERASSIGNMENT OPERATORS <<- AND ->> ARE NOT ALLOWED IN SAFER-R FUNCTIONS:
#> PLEASE, REWRITE THE CODE ACCORDINGLY.
#> 
#> LINE SYMBOL      CODE
#> 3    <<-       y <<- x
#> 
#> ================
#> 
#> 
# A <<- written in a string or in a comment is not an operator: not reported.
fun3 <- function(x){ y <- "the <<- writing is just a string" ; x } # a <<- in a comment
saferDev::super_assign_check(fun3) # everything fine: NULL returned
#> 
#> 
#> AFTER RUNNING saferDev::super_assign_check().
#> INSIDE fun3(), EVERYTHING SEEMS CLEAN.
#> 
```
