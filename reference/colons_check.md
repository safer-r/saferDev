# Check and Add Package Prefixes to Function Calls

Checks that the functions called inside a function are preceded by their
package (e.g., [`base::mean()`](https://rdrr.io/r/base/mean.html) and
not [`mean()`](https://rdrr.io/r/base/mean.html)), and adds the missing
`<package>::` prefixes before the basic R functions (functions of the
base, stats, graphics, grDevices, utils, datasets and methods packages).

## Usage

``` r
colons_check(
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

The corrected function, which keeps the environment of `x`: `x` with the
`<package>::` prefixes added before its calls of basic R functions.

`NULL`, invisibly, if nothing has to be corrected, if `x` contains no
function call, as a primitive function like
[`base::sum`](https://rdrr.io/r/base/sum.html) (with a warning), or if
`export` is `TRUE`.

In addition, except when `x` contains no function call, a report is
printed with [`message()`](https://rdrr.io/r/base/message.html): either
a message saying that everything seems clean, or tables of the calls
without prefix, one for the basic R functions and one for the other
functions (column 1: line number; column 2: function name; column 3:
code before the function name). The internal functions called (see the
Details section) are then also listed.

## Details

A call of a basic R function gets the prefix of its package (e.g.,
[`mean()`](https://rdrr.io/r/base/mean.html) becomes
[`base::mean()`](https://rdrr.io/r/base/mean.html)) only if the function
found by R when running `x`, from the environment of `x` (functions
defined by the user, imported functions and attached packages), is this
basic function. Otherwise, the name is masked (e.g.,
[`filter()`](https://rdrr.io/r/stats/filter.html) with the dplyr package
attached, or a [`filter()`](https://rdrr.io/r/stats/filter.html)
function defined in the global environment): the call is not modified
and is reported with the other functions. Under
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
([`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)),
[`system.file()`](https://rdrr.io/r/base/system.file.html),
[`help()`](https://rdrr.io/r/utils/help.html),
[`library.dynam()`](https://rdrr.io/r/base/library.dynam.html) and
[`library.dynam.unload()`](https://rdrr.io/r/base/library.dynam.html)
can thus be reported as masked by the pkgload shims. In code evaluated
in another environment (e.g., `expr` of
[`with()`](https://rdrr.io/r/base/with.html), or `.()` of
[`bquote()`](https://rdrr.io/r/base/bquote.html) with `where`), a
function of that environment named like a basic function is not
detected: its calls get the basic prefix. The stats, graphics,
grDevices, utils, datasets and methods packages must be attached, as in
a default R session, otherwise an error is returned.

The calls of internal functions, written without `::` or `:::`, are not
modified, nor reported as missing a prefix. The internal functions are
the functions created inside `x` (names assigned a function definition
or a
[`utils::getFromNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
result), the arguments of `x` and of all the functions defined in `x`,
anonymous ones included (e.g., `FUN` in `FUN(y)`), but not the arguments
and local functions of those written in quoted code (e.g., `c` in
`bquote(function(c) .(c(y)))`), and `x` itself when written as a name,
like `fun1` or `pkg::fun1` (recursive calls). These names are recognized
in the function that defines them (the whole code of `x` for its
arguments, `x` itself and `<<-`): an argument `t` of a function defined
in `x` does not prevent the check of
[`t()`](https://rdrr.io/r/base/t.html) outside this function. A name
assigned another value is not an internal function: after `c <- 3`,
`c(y)` calls [`base::c()`](https://rdrr.io/r/base/c.html) and gets its
prefix. A local name that holds a function obtained otherwise is not
detected: after `rev <- base::get(x = "sort")`, `rev(y)` becomes
`base::rev(y)`.

The calls of the other functions are reported in the message and in a
warning, but not modified, because their package cannot be determined
reliably: add their prefixes manually.

Not checked: function names written between quotes (like
[`"+"()`](https://rdrr.io/r/base/Arithmetic.html)), preceded by `$` or
`@`, or written without brackets (like `as.character` in
`sapply(1:3, FUN = as.character)`), and `function`, `if`, `for`,
`while`, `repeat` and `else`. Neither are the calls inside formulas,
including the one-sided formulas used as lambdas (e.g.,
[`log()`](https://rdrr.io/r/base/Log.html) and
[`offset()`](https://rdrr.io/r/stats/offset.html) in
`y ~ log(x) + offset(z)`, or
[`mean()`](https://rdrr.io/r/base/mean.html) in `~ mean(.x)`), because
modeling functions use their text, the calls inside
[`quote()`](https://rdrr.io/r/base/substitute.html),
[`bquote()`](https://rdrr.io/r/base/bquote.html),
[`expression()`](https://rdrr.io/r/base/expression.html),
[`alist()`](https://rdrr.io/r/base/list.html) and
[`substitute()`](https://rdrr.io/r/base/substitute.html), whose
arguments are code used as data (e.g.,
[`paste()`](https://rdrr.io/r/base/paste.html) in the plotmath label
`expression(paste("Temp (", degree, "C)"))`), except the code that R
evaluates (`.()` and spliced `..()` of
[`bquote()`](https://rdrr.io/r/base/bquote.html), its `where` argument,
and the `env` argument of
[`substitute()`](https://rdrr.io/r/base/substitute.html)), nor the
function called by [`.Internal()`](https://rdrr.io/r/base/Internal.html)
(e.g., `toupper` in `.Internal(toupper(x))`). The other functions that
use the code of their arguments get the corrected code (e.g., the
default axis label of `ggplot2::aes(x = log(y))` becomes
`base::log(y)`). Comments are not checked, but some unexpected writing
could dupe `colons_check()`: please report it
[here](https://github.com/safer-r/saferDev/issues).

Names already preceded by `::` or `:::` are not checked (R CMD check
reports the wrong ones, e.g., `base:::sum()` or `base::dev.off()`). The
added prefixes are always `::`.

Source references: line 1 is the line of the `function` keyword.
Comments and indentation are kept only if `x` carries its source
references (attribute `srcref`). Functions created with
`options(keep.source = FALSE)` (default of Rscript) and functions of
installed packages (e.g., `saferDev::colons_check`) have none: their
deparsed code is analyzed, without comments and with line numbers that
differ from the source file.

If the corrected code does not parse (a writing that the analysis does
not handle, e.g., a function name at the beginning of a line, after a
`$` or `@` ending the previous line), `x` is returned unchanged with a
warning, or an error is returned if `export` is `TRUE`.

An error is returned if `x` contains `\(x)` lambda functions (only
detected with source references): run
[`lambda_check()`](lambda_check.md) first.

## See also

The code checkers of saferDev, in the recommended order of use:
[`super_assign_check`](super_assign_check.md),
[`lambda_check`](lambda_check.md), `colons_check`,
[`all_args_here`](all_args_here.md),
[`curly_bracket_check`](curly_bracket_check.md),
[`true_false_check`](true_false_check.md),
[`for_loop_nb_check`](for_loop_nb_check.md) (see
[`vignette("saferDev")`](../articles/saferDev.md)).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Without source references ("Run examples" link, Rscript), line numbers and comments differ.
# See also https://safer-r.github.io/saferDev/articles/colons_check.html
fun1 <- function(x){
    y <- mean(x) # mean() written without base::
    base::return(y)
}
fun1_safer <- saferDev::colons_check(x = fun1, export = FALSE, out_path = NULL,
    safer_check = TRUE, lib_path = NULL, error_text = "") # base:: added before mean()
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
#> <environment: 0x6235c3377f88>
# Write the corrected function in the fun1_safer.R file of a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE),
    fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::colons_check(x = fun1, export = TRUE, out_path = tempo_dir, safer_check = TRUE,
    lib_path = NULL, error_text = "")
#> 
#> 
#> INSIDE fun1(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:
#> 
#> LINE FUN     STRING_BEFORE
#> 3    mean        y <- 
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpQNRq2A/dir1104737e2dfc/fun1_safer.R
#> 
base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
```
