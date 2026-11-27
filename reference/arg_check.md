# Check the Expected Features of an Argument

Checks the expected features of an object, typically an argument of a
function: class, type, mode, length, allowed values, and for numeric
data, proportions, negative values, infinite values and integers stored
as double. The problems detected are returned, not raised as errors
(except for `data` made of `NA` only).

## Usage

``` r
arg_check(
  data,
  class = NULL,
  typeof = NULL,
  mode = NULL,
  length = NULL,
  prop = FALSE,
  options = NULL,
  double_as_integer_allowed = FALSE,
  all_options_in_data = FALSE,
  na_contain = FALSE,
  neg_values = TRUE,
  inf_values = TRUE,
  print = FALSE,
  data_name = NULL,
  data_arg = TRUE,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- data:

  Object to test. It cannot be made of `NA` only (error, as for every
  argument of the safer-r functions), whatever `na_contain`.

- class:

  Vector of character strings, compared exactly (same values, in the
  same order) to `class(data)`: e.g., `class = c("matrix", "array")` for
  matrices and `class = c("ordered", "factor")` for ordered factors.
  `NULL`: class not tested.

- typeof:

  Single character string, compared to `typeof(data)`. Several values
  always give a problem, since `typeof(data)` is a single string.
  `NULL`: type not tested.

- mode:

  Single character string, compared to `mode(data)`. Several values
  always give a problem, since `mode(data)` is a single string. `NULL`:
  mode not tested.

- length:

  Single integer value, compared to `length(data)`. `NULL`: length not
  tested.

- prop:

  Single logical value. If `TRUE`, `data` must be of type `"double"`,
  with all its values in \[-1, 1\], or in \[0, 1\] with
  `neg_values = FALSE`. To also check the class of a proportion object
  (a matrix for instance), use a second `arg_check()` call with
  `prop = FALSE`.

- options:

  Vector of character strings or integers: the allowed values of `data`.
  Numbers of type `"double"` (in `options` or `data`) are accepted if
  they are integer values, up to a tolerance of about 1.5e-8 (e.g.,
  `0.57 * 100`, slightly below 57). `NULL`: values not tested.

- double_as_integer_allowed:

  Single logical value. If `TRUE`, `class = "integer"` and
  `typeof = "integer"` also accept a `data` of type `"double"` made of
  integer values (e.g., `data = 1`). See the details for approximate
  integers and dates.

- all_options_in_data:

  Single logical value. If `FALSE`, `data` must be made of some of the
  `options` elements and nothing else (e.g., `data = c(1, 1, 2)` with
  `options = c(1, 2, 3)`). If `TRUE`, `data` must contain all the
  `options` elements at least once, and nothing else (e.g.,
  `data = c(1, 1, 2, 3)`). Ignored if `options` is `NULL`.

- na_contain:

  Single logical value. Can `data` contain `NA`?

- neg_values:

  Single logical value. Are negative values authorized? Warning: `TRUE`
  (default) means that no check is performed. `FALSE` requires the
  `typeof` argument set to `"double"` or `"integer"`, the `mode`
  argument set to `"numeric"`, or `prop = TRUE` (error otherwise). A
  factor tested with `typeof` or `mode` then gives the problem
  `"... MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR"`.

- inf_values:

  Single logical value. Are infinite values (`Inf`, `-Inf`) authorized?
  Warning: `TRUE` (default) means that no check is performed. `FALSE`
  requires the `typeof` argument set to `"double"` or the `mode`
  argument set to `"numeric"` (error otherwise). A factor then gives the
  problem `"... MUST BE MADE OF NON INFINITE VALUES BUT IS A FACTOR"`.

- print:

  Single logical value. If `TRUE`, the text of the problems is also
  displayed (with [`message()`](https://rdrr.io/r/base/message.html))
  when `$problem` is `TRUE`. `FALSE` by default, to let the calling
  function handle the problems (see the examples).

- data_name:

  Single character string: name of the tested object in the returned
  text. If `NULL`, the expression passed to `data` is used.

- data_arg:

  Single logical value. Is the tested object an argument of a function?
  `"ARGUMENT"` (`TRUE`, default) or `"OBJECT"` is written in the
  returned text.

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
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used. Only
  checked and used when `safer_check` is `TRUE`.

- error_text:

  Single character string added to the error messages and to the
  returned text (e.g., `" INSIDE pkg1::fun1()"`) to name the calling
  functions. `NULL` is converted into `""`.

## Value

A list:

- `problem`: single logical value, `TRUE` if a problem is detected.

- `text`: the problems detected (text starting with `"ERROR"`), or a
  text saying that no problem is detected.

- `object_name`: `data_name` if provided, otherwise the expression
  passed to `data` as a string (e.g., `"vec1"`).

## Details

At least one of `class`, `typeof`, `mode`, `length`, `options` and
`prop = TRUE` must be set. `options` and `prop = TRUE` exclude each
other, and require `class`, `typeof` and `mode` to be `NULL`. `length`
can be combined with all of them. Wrong argument values or combinations
raise an error.

The function tests what is written, even if incoherent:
`arg_check(data = factor(1), class = "factor", mode = "character")`
returns a problem, since factors are of mode `"numeric"`.

To accept several possibilities for the tested object, combine several
`arg_check()` calls:

    tempo1 <- saferDev::arg_check(data = data, mode = "character")
    tempo2 <- saferDev::arg_check(data = data, class = "factor")
    tempo3 <- saferDev::arg_check(data = data, class = "integer")
    if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE){
        print("data ARGUMENT MUST BE A VECTOR OF (1) STRINGS, (2) FACTORS OR (3) INTEGERS")
    }

Warning: `double_as_integer_allowed = TRUE` only accepts exact integer
values stored as double: approximate integers (e.g., `1 + 1e-10` or
`3 - 1e-10`) are problems. Objects of type `"double"` that are not
numeric (e.g., dates) are problems when tested with `options`, and are
not accepted as integers by `double_as_integer_allowed = TRUE`.

## See also

[`class`](https://rdrr.io/r/base/class.html),
[`typeof`](https://rdrr.io/r/base/typeof.html),
[`mode`](https://rdrr.io/r/base/mode.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# See also https://safer-r.github.io/saferDev/articles/arg_check.html
test <- base::matrix(data = 1:3, nrow = 3, ncol = 1, byrow = FALSE, dimnames = NULL)
# $problem is TRUE: class(test) is c("matrix", "array"), compared exactly to the class argument
saferDev::arg_check(data = test, class = "vector", typeof = NULL, mode = "numeric", length = NULL,
    prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE,
    na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = TRUE, data_name = NULL,
    data_arg = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
#> 
#> 
#> ================
#> 
#> ERROR
#> 
#> THE test ARGUMENT MUST BE CLASS vector
#> 
#> ================
#> 
#> $problem
#> [1] TRUE
#> 
#> $text
#> [1] "ERROR\n\nTHE test ARGUMENT MUST BE CLASS vector"
#> 
#> $object_name
#> [1] "test"
#> 
# $problem is TRUE as well, for the same reason
saferDev::arg_check(data = test, class = "matrix", typeof = NULL, mode = "numeric", length = NULL,
    prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE,
    na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = TRUE, data_name = NULL,
    data_arg = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
#> 
#> 
#> ================
#> 
#> ERROR
#> 
#> THE test ARGUMENT MUST BE CLASS matrix
#> 
#> ================
#> 
#> $problem
#> [1] TRUE
#> 
#> $text
#> [1] "ERROR\n\nTHE test ARGUMENT MUST BE CLASS matrix"
#> 
#> $object_name
#> [1] "test"
#> 
# $problem is FALSE (error_text is added to the returned text)
saferDev::arg_check(data = test, class = base::c("matrix", "array"), typeof = NULL,
    mode = "numeric", length = NULL, prop = FALSE, options = NULL,
    double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE,
    neg_values = TRUE, inf_values = TRUE, print = TRUE, data_name = NULL, data_arg = TRUE,
    safer_check = TRUE, lib_path = NULL, error_text = " using saferDev::arg_check()")
#> $problem
#> [1] FALSE
#> 
#> $text
#> [1] "NO PROBLEM DETECTED FOR THE test ARGUMENT using saferDev::arg_check()"
#> 
#> $object_name
#> [1] "test"
#> 
# Inside a function, with print = FALSE (default): the function stops with the returned text
fun1 <- function(x){
    tempo <- saferDev::arg_check(data = x, class = NULL, typeof = "double", mode = NULL,
        length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE,
        all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE,
        print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = TRUE, lib_path = NULL,
        error_text = "")
    if(tempo$problem == TRUE){
        base::stop(tempo$text, call. = FALSE, domain = NULL)
    }
    base::return(x)
}
fun1(x = 2)
#> [1] 2
base::try(expr = fun1(x = -2), silent = FALSE,
    outFile = base::getOption(x = "try.outFile", default = base::stderr()))
#> Error : ERROR
#> 
#> THE x ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES.
```
