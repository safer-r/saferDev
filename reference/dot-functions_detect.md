# Functions Detection

Detect the names of the functions called in the code of a function.

## Usage

``` r
.functions_detect(x, skipped_base, arg_user_setting2, lib_path, error_text)
```

## Arguments

- x:

  Function to analyze.

- skipped_base:

  Vector of strings: names not reported (e.g., `"function"` or `"if"`).

- arg_user_setting2:

  List of the arguments written in the call of the calling function
  (`arg_user_setting` of the calling function). Its `x` compartment, the
  name of the analyzed function, is used in the messages and for the
  recursive calls (see `fun_names_internal` below).

- lib_path:

  Vector of character strings or `NULL`: the `lib_path` value of the
  calling exported function, only passed on to
  [`arg_check()`](arg_check.md) (directly or through the internal
  functions called). No default value.

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

## Value

A list:

- `code`: vector of strings, the lines of the printed code of `x`, with
  the runs of spaces collapsed into one space, and the leading space and
  the comments removed.

- `all_basic_funs`: vector of strings, names of all the objects of the
  basic R environments of the search path (packages stats, graphics,
  grDevices, utils, datasets, methods and base, and Autoloads), which
  must all be attached, otherwise an error is returned.

- `fun_names`: list of vectors of strings, the names followed by `(` in
  each line of `code`, and the names of called functions ending a line,
  their `(` being on a next line (names given as calls by the parse data
  or, if the printed code does not parse, any name ending a line
  followed by a line starting with `(`), except the names of
  `skipped_base`. Only lines with names are present, the compartment
  names indicating the line numbers (e.g., `"c3"` for line 3).

- `fun_names_pos`: list as `fun_names`, with the position of the first
  character of each name in its line.

- `code_line_nb`: vector of integers, the line numbers of the
  compartments of `fun_names`.

- `arg_user_setting`: `arg_user_setting2`, with `x` written
  `"pkg::fun()"` if `x` is written `pkg::fun` in the call.

- `fun_names_skip`: list as `fun_names`, with `TRUE` for the names not
  to check: names inside a formula or inside the arguments of a call of
  [`quote()`](https://rdrr.io/r/base/substitute.html),
  [`bquote()`](https://rdrr.io/r/base/bquote.html),
  [`expression()`](https://rdrr.io/r/base/expression.html),
  [`alist()`](https://rdrr.io/r/base/list.html) or
  [`substitute()`](https://rdrr.io/r/base/substitute.html) (written
  without package or after `base::` or `base:::`, possibly between
  quotes, also with the value piped into it), except the code that R
  evaluates (arguments of the `.()` and, if `splice` is `TRUE` or `T`,
  `..()` calls of [`bquote()`](https://rdrr.io/r/base/bquote.html),
  `where` of [`bquote()`](https://rdrr.io/r/base/bquote.html) and `env`
  of [`substitute()`](https://rdrr.io/r/base/substitute.html), none if R
  cannot match their arguments), and name of the function called by
  [`.Internal()`](https://rdrr.io/r/base/Internal.html) (e.g., `toupper`
  in `.Internal(toupper(x))`).

- `fun_names_internal`: list as `fun_names`, with `TRUE` for the calls
  of internal functions inside their scope: formal arguments of `x` and
  name of `x` (`x` compartment of `arg_user_setting2`, possibly written
  `pkg::name` or followed by `()`, if the function of this name found
  from the environment of `x` is `x`: recursive calls) in the whole code
  of `x`, formal arguments of a function defined in `x` inside this
  definition (not for a definition in quoted code, see
  `fun_names_skip`), and names assigned a function definition or a
  [`utils::getFromNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html)
  result (`<-`, `=`, `->`) inside the innermost function definition
  containing the assignment (the innermost one outside the quoted code
  if the assignment is not quoted, none if this definition is in quoted
  code, and the whole code of `x` for `<<-`, `->>`).

`NULL`, with a warning, if `x` is a primitive function (no R code), or
if the code of `x` has no name followed by `(` other than the names of
`skipped_base`.

## Details

The names between quotes (e.g., `"paste(x)"`) are reported. The comments
are removed beforehand, the quotes being read as the R parser does (see
[`.in_quotes_replacement()`](dot-in_quotes_replacement.md)).

The existence of the reported functions is not checked.

The internal functions, the formulas, the calls of
[`quote()`](https://rdrr.io/r/base/substitute.html),
[`bquote()`](https://rdrr.io/r/base/bquote.html),
[`expression()`](https://rdrr.io/r/base/expression.html),
[`alist()`](https://rdrr.io/r/base/list.html) and
[`substitute()`](https://rdrr.io/r/base/substitute.html), and the
[`.Internal()`](https://rdrr.io/r/base/Internal.html) calls are found in
the parse data of the printed code of `x`. If this code does not parse
(e.g., an environment object inserted in the body of `x`), the formulas,
these quoting calls and the
[`.Internal()`](https://rdrr.io/r/base/Internal.html) calls are not
detected, and the internal functions are only the name of `x` and the
names written `name <- function(` or `name <- utils::getFromNamespace(`
at the beginning of a line, in the whole code of `x`.

Warnings:

- Requires [`saferDev::arg_check`](arg_check.md),
  `.extract_all_fun_names`, `.in_quotes_replacement`.

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also these 3 functions when checking for the presence of
  `.functions_detect`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
