# Check and Complete the Arguments of Function Calls

Checks that the function calls inside a function are written with all
their arguments, and returns the function with the missing arguments
added. For instance, `base::paste0(letters[1:2])` becomes
`base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE)`.

The calls must already be written with their package (e.g.,
[`base::paste0()`](https://rdrr.io/r/base/paste.html); run
[`colons_check`](colons_check.md) first), otherwise an error is
returned, except the calls not analyzed (see the Details section) and
the calls, without package, of the internal functions of `x`, as defined
in [`colons_check`](colons_check.md) (`"INTERNAL_FUNCTION"`).

## Usage

``` r
all_args_here(
  x,
  table_export = FALSE,
  fun_export = FALSE,
  out_path = NULL,
  df_name = "res.tsv",
  overwrite = FALSE,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- x:

  Function to check, written without quotes and brackets (e.g., `fun1`
  or `pkg::fun1`).

- table_export:

  Single logical value. Write the table (see the Value section) into the
  `df_name` file of `out_path`, instead of printing it?

- fun_export:

  Single logical value. If `TRUE`, the corrected function is written
  into the `<name>_safer.R` file of `out_path` instead of being returned
  (e.g., `fun1_safer.R` for `x = fun1` or `x = pkg::fun1`), as
  `fun1_safer <- function(...)`, which
  [`base::source()`](https://rdrr.io/r/base/source.html) recreates.
  Nothing is written if nothing has to be corrected, but an error is
  returned if the file already exists and `overwrite` is `FALSE`, or if
  `x` is not a function name (e.g., `function(x){x}` or `FUN_LIST$fun`).

- out_path:

  Single character string: path of an existing folder where the files
  are written. Required when `table_export` or `fun_export` is `TRUE`;
  otherwise not used, but a non-`NULL` value must still be a single
  non-empty character string. `NULL` by default.

- df_name:

  Single character string (not `""`): name of the file of the table,
  written in `out_path` when `table_export` is `TRUE`.

- overwrite:

  Single logical value. Overwrite the files already existing in
  `out_path` (`df_name` and `<name>_safer.R`) when `table_export` or
  `fun_export` is `TRUE`?

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
invisibly, if nothing has to be corrected, if `fun_export` is `TRUE`, or
if `x` contains no function call, as a primitive function like
[`base::sum`](https://rdrr.io/r/base/sum.html) (with a warning).

The table of the detected calls is printed as a message, or written into
a file if `table_export` is `TRUE`, but never returned. Its columns are:

- `$LINE_NB`: line number in the code of `x` (see the Details section).

- `$FUN_NAME`: function name.

- `$FUN_ARGS`: the call as written, without its package (e.g.,
  `sort(v)`). `"NOT_CONSIDERED"` for a name followed by `(` inside a
  string (e.g., `"a("` in `base::paste0("a(", x)`), which is not a call
  (the calls with a function name written between quotes or after `$` or
  `@` are not listed).

- `$FUN_POS`: position of the first character of the function name in
  the line, after removal of the leading and repeated spaces.

- `$DEF_ARGS`: arguments of the function with their default values (`""`
  for a function without arguments, e.g.,
  [`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html), and for
  `"NOT_CONSIDERED"` in `$FUN_ARGS`). `"NO_ARGS"`: primitive function
  whose arguments are not available (e.g.,
  [`base::return()`](https://rdrr.io/r/base/function.html));
  `"INTERNAL_FUNCTION"`: call, written without package, of an internal
  function inside the function that defines it (see the Description
  section); `"SKIPPED"`: call not analyzed (see the Details section).

- `$MISSING_ARG_NAMES`: names of the missing arguments, and of the
  arguments written with a partial name.

- `$MISSING_ARGS`: these arguments with their proposed values.

- `$STATUS`: `"GOOD"` if all the arguments are written, otherwise the
  proposed call. `""` if the call is not analyzed (see `$FUN_ARGS`,
  `$DEF_ARGS` and the warnings), or if the function has no argument
  other than `...` (e.g.,
  [`base::Sys.time()`](https://rdrr.io/r/base/Sys.time.html) or
  [`base::c()`](https://rdrr.io/r/base/c.html)).

Other messages: `"EVERYTHING SEEMS CLEAN"` if no argument is missing,
`"ARGUMENTS WITH EMPTY VALUE"` (empty values inserted into the corrected
function, see the Details section) and `"CORRECTIONS NOT APPLIED"`
(proposals not inserted into the corrected function, each with its
reason).

## Details

Analyzed calls: the names followed by `(`, except `function`, `if`,
`for`, `while`, `repeat`, `else` and the names preceded by `$` or `@`
(e.g., `x$fun()`). A function passed without brackets (e.g.,
`FUN = base::as.character`) or whose name is written between quotes
(e.g., `base::"rownames<-"(x, "a")`) is not checked. The calls inside
formulas, one-sided ones included (e.g.,
[`offset()`](https://rdrr.io/r/stats/offset.html) in
`y ~ x + offset(z)`, or
[`base::mean()`](https://rdrr.io/r/base/mean.html) in
`~ base::mean(.x)`), the calls inside
[`quote()`](https://rdrr.io/r/base/substitute.html),
[`bquote()`](https://rdrr.io/r/base/bquote.html),
[`expression()`](https://rdrr.io/r/base/expression.html),
[`alist()`](https://rdrr.io/r/base/list.html) and
[`substitute()`](https://rdrr.io/r/base/substitute.html), whose
arguments are code used as data (e.g.,
[`base::paste()`](https://rdrr.io/r/base/paste.html) in the plotmath
label `base::expression(base::paste("Temp (", degree, "C)"))`), except
the code that R evaluates (`.()` and spliced `..()` of
[`bquote()`](https://rdrr.io/r/base/bquote.html), its `where` argument,
and the `env` argument of
[`substitute()`](https://rdrr.io/r/base/substitute.html)), the function
called by [`.Internal()`](https://rdrr.io/r/base/Internal.html) (e.g.,
`toupper` in `base::.Internal(toupper(x))`) and
[`as.environment()`](https://rdrr.io/r/base/as.environment.html) are not
analyzed (`"SKIPPED"`). The other functions that use the code of their
arguments get the completed code (e.g., the default axis label of
`ggplot2::aes(x = base::log(y))` becomes
`base::log(x = y, base = base::exp(x = 1))`). A call written with more
arguments than its function has is not analyzed, with a warning (also
for a valid replacement call like `base::body(f, envir = e) <- v`). The
package of each `pkg::fun()` call must be installed (not only loaded
with
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)),
and the stats, graphics, grDevices, utils, datasets and methods packages
must be attached, as in a default R session, otherwise an error is
returned.

Missing arguments: a value written without argument name gets its name
(e.g., `base::sort(v)` becomes `base::sort(x = v, decreasing = FALSE)`),
a partial name is completed (only for the arguments before `...`, as in
R), and the other missing arguments are added with their default value,
written with packages and argument names (e.g., `base::log(x = 2)`
becomes `base::log(x = 2, base = base::exp(x = 1))`). An empty value
(e.g., `envir = `), which a function written in R treats as a missing
argument (except the first argument of an S3 generic function, which
selects the method), is written instead when the default value cannot be
copied safely: no default value (e.g., `new` in
[`base::.libPaths()`](https://rdrr.io/r/base/libPaths.html)); a default
value that depends on another argument, on a variable internal to the
called function, on the frame where it is evaluated (e.g.,
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html), a formula or
a function definition), or on an object that `x` does not find, or finds
differently (e.g., an internal function of a package, or a variable
masked by an argument of `x`); a default value whose function calls are
written `pkg::fun()`, lack some arguments (e.g., `getOption("digits")`)
or use the code of their arguments (e.g., `quote(exp(1))`); an argument
tested with [`missing()`](https://rdrr.io/r/base/missing.html) (e.g.,
`nrow` in `base::matrix(x, ncol = 2)`). An argument already written with
an empty value is accepted, except if its default value can be copied,
which is then added.

Corrected function: the missing arguments are inserted into the code of
`x`, whose line breaks are kept. A proposal is not inserted, and is
listed with its reason in the `"CORRECTIONS NOT APPLIED"` message, when
the call receives a value from a pipe (`|>`, `%>%`, `%<>%`, `%T>%` or
`%!>%`), forwards `...`, has an empty argument (e.g., `base::sum(1, )`),
is the target of an assignment (e.g., `base::rownames(x) <- "a"`), or
has a value written without name that is also proposed with its name;
when the called function counts its arguments with
[`nargs()`](https://rdrr.io/r/base/nargs.html) (e.g.,
[`base::diag()`](https://rdrr.io/r/base/diag.html)), or is a builtin
primitive that would receive an empty value (e.g., `PACKAGE = ` in
[`base::.Call()`](https://rdrr.io/r/base/CallExternal.html)); when the
called function is an S3 generic whose first argument is not written, or
is written without name while a method names it differently (e.g.,
`formula` in `t.test.formula()`), or when a method has neither `...` nor
an added argument (e.g., `digits` for
[`round.POSIXt()`](https://rdrr.io/r/base/round.POSIXt.html)), or has
arguments that an added argument matches partially (e.g., `y` matching
`ylim` and `ylab` of `plot.table()`); and when the call cannot be read,
located, parsed or matched to its `$STATUS` proposal.

An error is returned if `x` contains `\(x)` lambda functions (only
detected with source references): run [`lambda_check`](lambda_check.md)
first.

Source references: line 1 is the line of the `function` keyword.
Comments and indentation are kept only if `x` carries its source
references (attribute `srcref`). Functions created with
`options(keep.source = FALSE)` (default of Rscript) and functions of
installed packages (e.g., [`saferDev::colons_check`](colons_check.md))
have none: their deparsed code is analyzed, without comments and with
line numbers that differ from the source file.

Limitations: the detection is text-based, and some unusual writings
(e.g., in comments) can dupe it. The `$STATUS` proposals remain
suggestions to check. Please report unexpected results
[here](https://github.com/safer-r/saferDev/issues).

## See also

The code checkers of saferDev, in the recommended order of use:
[`super_assign_check`](super_assign_check.md),
[`lambda_check`](lambda_check.md), [`colons_check`](colons_check.md),
`all_args_here`, [`curly_bracket_check`](curly_bracket_check.md),
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
# See also https://safer-r.github.io/saferDev/articles/all_args_here.html
fun1 <- function(x){
    base::paste0(x, "a") # collapse and recycle0 arguments not written
}
fun1_safer <- saferDev::all_args_here(x = fun1, table_export = FALSE, fun_export = FALSE,
    out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE,
    lib_path = NULL, error_text = "") # arguments added
#> 
#> INSIDE fun1(), ARGUMENTS ARE MISSING.
#> 
#>   LINE_NB FUN_NAME       FUN_ARGS FUN_POS
#> 1       3   paste0 paste0(x, "a")       7
#>                                 DEF_ARGS  MISSING_ARG_NAMES
#> 1 ..., collapse = NULL, recycle0 = FALSE collapse, recycle0
#>                        MISSING_ARGS
#> 1 collapse = NULL, recycle0 = FALSE
#>                                              STATUS
#> 1 paste0(x, "a", collapse = NULL, recycle0 = FALSE)
#> 
fun1_safer # corrected function
#> function (x) 
#> {
#>     base::paste0(x, "a", collapse = NULL, recycle0 = FALSE)
#> }
#> <environment: 0x6235be69cd08>
# Write the table (res.tsv) and the corrected function (fun1_safer.R) in a new temporary folder:
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
saferDev::all_args_here(x = fun1, table_export = TRUE, fun_export = TRUE, out_path = tempo_dir,
    df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL,
    error_text = "")
#> 
#> INSIDE fun1(), ARGUMENTS ARE MISSING.
#> RESULT EXPORTED IN:
#> /tmp/RtmpQNRq2A/dir11043b5dd324/res.tsv
#> 
#> MODIFIED CODE EXPORTED IN:
#> /tmp/RtmpQNRq2A/dir11043b5dd324/fun1_safer.R
#> 
base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
```
