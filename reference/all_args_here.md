# All Arguments Here

Verify that all the functions used inside a function are written with
all their arguments. For instance:
`base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE)` and not
`paste0(letters[1:2])`.

## Usage

``` r
all_args_here(
  x,
  table_export = FALSE,
  fun_export = FALSE,
  out_path = ".",
  df_name = "res.tsv",
  overwrite = FALSE,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- x:

  Function name, written without quotes and brackets.

- table_export:

  Single logical value. Export the result data frame into a `.tsv` file?
  If `TRUE`, the data frame is not returned by the function but only
  exported.

- fun_export:

  Single logical value. Export the modified function into a `.R` file?
  If `TRUE`, the corrected code (missing arguments added into the
  detected function calls, as proposed in the `$STATUS` column) is not
  returned by the function but only exported into a file made of the
  name of the submitted function with `_safer` added at the end (e.g.,
  `test_safer.R`). The file contains an assignment of the modified
  function: `test_safer <- function(...) ...`, so that
  [`base::source()`](https://rdrr.io/r/base/source.html)ing it recreates
  the modified function. An error is returned if the file already exists
  in `out_path` and `overwrite` is `FALSE`, and if the tested function
  is not designated by a function name (like `test` or `package::test`),
  i.e., written by an inline definition (like `function(x){x}`) or by a
  component writing (like `FUN_LIST$fun`). Ignored if no correction has
  been applied.

- out_path:

  Single character string indicating the absolute pathway of the folder
  where to export the data frame or the modified function.
  `out_path = "."` (default) means the R working directory set by the
  user. Ignored if `table_export` and `fun_export` are `FALSE`.

- df_name:

  Single character string indicating the name of the exported data frame
  file. Ignored if `table_export` is `FALSE`.

- overwrite:

  Single logical value. Overwrite potential files already existing in
  `out_path` (`df_name` for the table export, and the function name with
  `_safer` added at the end for the `fun_export`)? Ignored if
  `table_export` and `fun_export` are `FALSE`.

- safer_check:

  Single logical value. Perform the "safer" checks? If `TRUE`, checkings
  are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Warning: must
  be set to `FALSE` if this function is used inside another "safer"
  function to avoid pointless multiple checkings.

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `c(lib_path, .libPaths())`. Warning:
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

A data frame indicating the missing arguments or a message saying that
everything seems fine. If `table_export` argument is `TRUE`, then the
data frame is exported as `res.tsv` instead of being returned. If
`fun_export` argument is `TRUE`, the modified function is exported as
`<FUNCTION_NAME>_safer.R` instead of being returned (ignored if no
correction has been applied). Otherwise, the modified function (an R
function object rebuilt from the corrected code, with its comments
removed) is returned, or `NULL` if no modification has been applied. The
returned value is invisible when nothing is exported. Column
descriptions:

- `$LINE_NB`: the line number in the function code (starting at the
  `"<- function"` line, i.e., without counting the `#'` header lines).

- `$FUN_NAME`: the function name.

- `$FUN_ARGS`: the written arguments of `FUN_NAME`. `"NOT_CONSIDERED"`
  means that the function is between quotes or after `$`.

- `$FUN_POS`: the position of the first character of the function name
  in the `$LINE_NB` line.

- `$DEF_ARGS`: the defaults arguments of `FUN_NAME`. `"NO_ARGS"` means
  that the function has no arguments. `"INTERNAL_FUNCTION"` means that
  the function has been created inside the checked function. `"SKIPPED"`
  means that the function is not analyzed for the reason indicated in
  the "details" section.

- `$MISSING_ARG_NAMES`: the missing argument names in `FUN_ARGS`.

- `$MISSING_ARGS`: the missing arguments with their values in
  `FUN_ARGS`.

- `$STATUS`: either `"GOOD"`, meaning that all the arguments are already
  written, or a new proposal of arguments writing, or indicates if some
  arguments are not fully written (abbreviation is discouraged), or
  nothing.

An additional message `"EVERYTHING SEEMS CLEAN"` if the `$STATUS` column
is only made of `""` and `"GOOD"`.

## Details

More precisely, `all_args_here()` verifies that all the strings before
an opening bracket `(` are written with all their arguments. Thus, it
cannot check function names written without brackets, like in the `FUN`
argument of some functions, e.g., `sapply(1:3, FUN = as.character)`.

The Perl regex used to detect a function name is:
`"([a-zA-Z]|\.[a-zA-Z._])[a-zA-Z0-9._]*\s*\("`.

Currently, `all_args_here()` cannot detect functions written between
quotes, like [`"+"()`](https://rdrr.io/r/base/Arithmetic.html) or
`"rownames<-"(x, "a")`.

Function names preceded by `$` are not considered.

The following R functions are skipped: `function`, `if`, `for`, `while`,
`repeat` and `else`.

Most of the time, `all_args_here()` does not check inside comments, but
some unexpected writing could dupe `all_args_here()`. Please, report
[here](https://github.com/safer-r/saferDev/issues) if it is the case.

The returned line numbers are indicative, depending on which source is
checked. For instance, [`saferDev::report`](report.md) (compiled) has
not the same line numbers as its [source
file](https://github.com/safer-r/saferDev/blob/main/R/report.R).
Notably, compiled functions do not have comments anymore, compared to
the same source function sourced into the working environment. In
addition, the counting starts at the `"<- function"` line, i.e., without
counting the `#'` header lines potentially present in source files.

The function works first by replacing in the code: 1) the three
consecutive characters `'"'` or `'"'` by three spaces, 2) `"\"'"` and
`'\'"'` by four spaces and 3) escape quotes like `\"` or `\'` by two
spaces.

See more examples
[here](https://safer-r.github.io/saferDev/articles/all_args_here.html).

Warnings:

1.  The following R functions are also skipped (as indicated by
    `"SKIPPED"` in the `DEF_ARGS` column of the returned data frame):
    [`as.environment()`](https://bugs.r-project.org/show_bug.cgi?id=18849).
    Click to see the explanation.

2.  Some functions, like
    [`rownames()`](https://rdrr.io/r/base/colnames.html), have different
    arguments depending on whether something is assigned to it (e.g.,
    `rownames(x) <- "a"`) or not. The `all_args_here()` function always
    proposes the arguments defined in the help page without assignment,
    meaning that it cannot detect the assignment. A way to bypass this
    is to use the "exact" writing of the function. For instance,
    `base::"rownames<-"(x, "a")` instead of using
    `base::rownames(x) <- "a"`, and use `getAnywhere("rownames<-")` to
    see the arguments of the function.

3.  The function could not properly work if any comma is present in
    default argument values. Please, report
    [here](https://github.com/safer-r/saferDev/issues) if it is the
    case.

4.  Proposals in the `STATUS` column are only suggestions, as it is
    difficult to anticipate all the exceptions with arguments writing.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/all_args_here.html
if (FALSE) { # \dontrun{
# Example that returns an error
saferDev::all_args_here(mean) # Example that returns an error
source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R")
saferDev::all_args_here(test2) # the checked function must be executable
} # }

FUN2 <- function(x, y){
    middle_bracket2 <- base::do.call(
        what = base::c, 
        args = code_for_col, 
        quote = FALSE, 
        envir = base::parent.frame()
    )
}
saferDev::all_args_here(FUN2, safer_check = FALSE)
#> 
#> INSIDE FUN2(), ARGUMENTS ARE MISSING.
#> 

if (FALSE) { # \dontrun{
# Example that creates a file/folder in the working directory
source("https://raw.githubusercontent.com/safer-r/.github/refs/heads/main/profile/backbone.R")
saferDev::all_args_here(BACKBONE, table_export = TRUE, fun_export = TRUE, safer_check = FALSE)
} # }

FUN2 <- function(x, y){
    middle_bracket2 <- base::do.call(
        what = base::c,
        args = code_for_col,
        quote = FALSE,
        envir = base::parent.frame()
    )
}
FUN2_safer <- saferDev::all_args_here(FUN2, fun_export = FALSE, safer_check = FALSE) # returns the modified function (missing arguments added) or NULL if nothing was modified
#> 
#> INSIDE FUN2(), ARGUMENTS ARE MISSING.
#> 
```
