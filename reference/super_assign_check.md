# Check That No Superassignment Is Used

Checks that the superassignment operators `<<-` and `->>` are not used
in a function. Returns an error with a table of the lines to rewrite if
any, since these operators are not allowed in safer-r functions.

## Usage

``` r
super_assign_check(x, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- x:

  Function to check, written without quotes and brackets (e.g., `fun1`
  or `pkg::fun1`).

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

If no superassignment operator is detected: a message saying that
everything seems clean, and `NULL`, invisibly.

Otherwise: an error, with a table-like message made of one row per
reported operator (column 1: line number; column 2: symbol; column 3:
code of the line).

## Details

Why an error: `<<-` and `->>` assign into the first enclosing
environment of the function where the name exists, or into the global
environment. What they modify thus depends on where the function is
defined, not on its code, and cannot be corrected automatically: rewrite
the code, for instance by returning the value, or by using
[`base::assign()`](https://rdrr.io/r/base/assign.html) with an explicit
`envir` argument.

Reported: every `<<-` and `->>` operator written in the submitted
function, also when called as a function (e.g., `` `<<-`(y, 1) `` or
`` base::`<<-`(y, 1) ``), including inside the functions defined in the
submitted function (e.g., a `FUN` argument of
[`base::lapply()`](https://rdrr.io/r/base/lapply.html) or the handlers
of [`base::tryCatch()`](https://rdrr.io/r/base/conditions.html)).

Not reported: the `<-`, `->` and `=` assignment operators, and the `<<-`
or `->>` writings inside strings, comments or backquoted names (e.g.,
`c("<<-")` or `` `a<<-b` ``), apart from the calls above.

Source references: line 1 is the line of the `function` keyword.
Functions created with `options(keep.source = FALSE)` (default of
Rscript) and functions of installed packages (e.g.,
[`saferDev::colons_check`](colons_check.md)) have no source references
(attribute `srcref`): their deparsed code is analyzed, without comments,
with line numbers that differ from the source file, and with `->>`
reported as `<<-`.

## See also

The code checkers of saferDev, in the recommended order of use:
`super_assign_check`, [`lambda_check`](lambda_check.md),
[`colons_check`](colons_check.md), [`all_args_here`](all_args_here.md),
[`curly_bracket_check`](curly_bracket_check.md),
[`true_false_check`](true_false_check.md),
[`for_loop_nb_check`](for_loop_nb_check.md) (see
[`vignette("saferDev")`](../articles/saferDev.md)).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

## Examples

``` r
# Without source references ("Run examples" link, Rscript), line numbers and comments differ.
# See also https://safer-r.github.io/saferDev/articles/super_assign_check.html
fun1 <- function(x){ y <- x ; base::return(y) }
saferDev::super_assign_check(x = fun1, safer_check = TRUE, lib_path = NULL,
    error_text = "") # no superassignment: NULL returned, invisibly
#> 
#> 
#> AFTER RUNNING saferDev::super_assign_check().
#> INSIDE fun1(), EVERYTHING SEEMS CLEAN.
#> 
fun2 <- function(x){ y <<- x ; base::return(y) }
base::try(expr = saferDev::super_assign_check(x = fun2, safer_check = TRUE, lib_path = NULL,
    error_text = ""), silent = FALSE,
    outFile = base::getOption(x = "try.outFile", default = base::stderr())) # error: <<- reported
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
fun3 <- function(x){
    y <- "the <<- writing is just a string" # a <<- in a comment
    x
}
saferDev::super_assign_check(x = fun3, safer_check = TRUE, lib_path = NULL,
    error_text = "") # no superassignment: NULL returned, invisibly
#> 
#> 
#> AFTER RUNNING saferDev::super_assign_check().
#> INSIDE fun3(), EVERYTHING SEEMS CLEAN.
#> 
```
