# Argument Check

Check expected values of an argument of functions: class, type, mode,
length, restricted values panel, kind of numeric values (in addition to
the distinction between `'integer'` and `'double'`)? Proportion only?
Inf values authorized? negative values authorized? Integers of type
`'double'`?

## Usage

``` r
arg_check(
  data,
  class = NULL,
  typeof = NULL,
  mode = NULL,
  length = NULL,
  prop = FALSE,
  double_as_integer_allowed = FALSE,
  options = NULL,
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

  Object to test.

- class:

  Character string or vector of character strings, tested exactly (same
  values, in the same order) against `class(data)`. Thus,
  `class = c("matrix", "array")` is required for matrices since R 4.0.0
  and `class = c("ordered", "factor")` for ordered factors (see the
  details below). Write NULL to do not test the class.

- typeof:

  Character string or vector of character strings, tested exactly (same
  values, in the same order) against `typeof(data)`. Since
  `typeof(data)` always returns a single string, a vector of more than
  one value in the `typeof` argument always returns a problem. Write
  NULL to do not test the type.

- mode:

  Character string or vector of character strings, tested exactly (same
  values, in the same order) against `mode(data)`. Since `mode(data)`
  always returns a single string, a vector of more than one value in the
  `mode` argument always returns a problem. Write NULL to do not test
  the mode.

- length:

  Single numeric value indicating the length of the object. Not
  considered if `NULL`.

- prop:

  Single logical value. Are the numeric values between 0 and 1
  (proportion)? If `TRUE`, can be used alone, i.e., without necessarily
  checking the class, mode, etc., of the object to test.

- double_as_integer_allowed:

  Single logical value. If `TRUE`, no error is reported in the checking
  message if the `typeof` argument is set to `"integer"`, while the
  reality is type `"double"` but with zero as modulo (reminder of a
  division). This means that `i <- 1`, which is `typeof(i) == "double"`
  is considered as an integer when setting
  `double_as_integer_allowed = TRUE`. Warning:
  `double_as_integer_allowed = TRUE` uses `data %% 1 == 0L` but not
  `isTRUE(all.equal(data %% 1, 0))` is used here because the argument
  checks for integers stored as double (does not check for decimal
  numbers that are approximate integers).

- options:

  Vector of character strings or integers indicating all the possible
  option values for the `data` argument, or `NULL`. Numbers of type
  `"double"` are accepted if they have a 0 modulo (i.e., are integer
  like).

- all_options_in_data:

  Single logical value. If `FALSE`, the tested object must be made of at
  least one of the options and nothing else. Example returning no error:
  `data = c(1, 1, 2)` and `options = c(1, 2, 3)`. Example returning an
  error: `data = c(1, 4)` and `options = c(1, 2, 3)`. If `TRUE`, the
  tested object must contain all of the options, at least once, and
  nothing else. Example returning no error: `data = c(1, 1, 2, 3` and
  `options = c(1, 2, 3)`. Example returning an error (missing 3):
  `data = c(1, 1, 2` and `options = c(1, 2, 3)`. Example returning an
  error (unwanted 4): `data = c(1, 1, 4` and `options = c(1, 2, 3)`.
  Ignored if the `options` argument is `NULL`.

- na_contain:

  Single logical value. Can the `data` argument contain `NA`?

- neg_values:

  Single logical value. Are negative numeric values authorized? Warning:
  the default setting is `TRUE`, meaning that, in that case, no check is
  performed for the presence of negative values. The checking is
  activated only when set to `FALSE`. In addition, `FALSE` can only be
  used when the `typeof` argument is set to `"double"` or `"integer"`,
  or the `mode` argument to `"numeric"`. Otherwise an error message is
  returned. Finally, the presence of negative values is not checked with
  `FALSE` if the tested object is a factor and a message is returned:
  `"OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR"`.

- inf_values:

  Single logical value. Are infinite `Inf` or `-Inf` values authorized?
  Warning: the default setting is `TRUE`, meaning that, in that case, no
  check is performed for the presence of infinite values. The checking
  is activated only when set to `FALSE`. In addition, `FALSE` can only
  be used when the `typeof` argument is set to `"double"`, or the `mode`
  argument to `"numeric"`. Otherwise an error message is returned.
  Finally, the presence of infinite values is not checked with `FALSE`
  if the tested object is a factor and a message is returned:
  `"OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR"`.

- print:

  Single logical value. Print the message if the `$problem` output is
  `TRUE`? Warning: set by default to `FALSE`, which facilitates the
  control of the checking message output when using `arg_check()` inside
  functions. See the example section.

- data_name:

  Single character string indicating the name of the object to test. If
  `NULL`, use what is assigned to the `data` argument for the returned
  message.

- data_arg:

  Single logical value. Is the tested object a function argument? If
  `TRUE` (default), `"ARGUMENT"` is written in output messages,
  otherwise `"OBJECT"`.

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
  If `NULL`, converted into `""`. Of note, in `arg_check()`,
  `error_text` is also used at the end of the string returned when no
  problem is detected.

## Value

A list containing:

- `problem`: logical. Is there any problem detected?

- `text`: message indicating the details of the problem, or the absence
  of problem.

- `object.name`: value of the `data_name` argument (i.e., name of the
  checked object if provided, `NULL` otherwise).

## Details

If `options == NULL`, then at least `class` or `type` or `mode` or
`length` argument must be non-null.

If `options` is non-null, then `class`, `type` and `mode` must be
`NULL`, and `length` can be `NULL` or specified.

The function tests what is written in its arguments, even if what is
written is incoherent. For instance,
`arg_check(data = factor(1), class = "factor", mode = "character")` will
return a problem, whatever the object tested in the `data` argument,
because no object can be class `"factor"` and mode `"character"`
(factors are class `"factor"` and mode `"numeric"`). Of note, the length
of object of class `"environment"` is always 0.

If the tested object is `NULL`, then the function will always return a
checking problem.

The `class`, `typeof` and `mode` arguments are compared to
`class(data)`, `typeof(data)` and `mode(data)` exactly: the written
values must be identical, element per element, in the same order. Since
`typeof(data)` and `mode(data)` return a single string, passing several
values in the `typeof` or `mode` arguments always returns a problem. In
addition, since `class(data)` of objects of class `"matrix"` is
`c("matrix", "array")` since R 4.0.0, `class = "matrix"` alone always
returns a problem for matrices (use `class = c("matrix", "array")`).
Similarly, `class = "factor"` alone always returns a problem for ordered
factors (use `class = c("ordered", "factor")`), and `class = "vector"`
is no longer a shortcut for the classes `"numeric"`, `"integer"`,
`"character"`, `"logical"`, `"complex"` or `"expression"`: write the
exact class instead, or nothing.

## See also

[`class`](https://rdrr.io/r/base/class.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/arg_check.html
test <- matrix(1:3)
if (FALSE) { # \dontrun{
# Example that returns an error (since R 4.0.0, class(matrix(1:3)) is c("matrix", "array"))
saferDev::arg_check(data = test, print = TRUE, class = "vector", mode = "numeric")
# Example that returns an error as well, because class = "matrix" must match class(data) exactly
saferDev::arg_check(data = test, print = TRUE, class = "matrix", mode = "numeric")
} # }
saferDev::arg_check(data = test, print = TRUE, class = c("matrix", "array"), mode = "numeric")
#> $problem
#> [1] FALSE
#> 
#> $text
#> [1] "NO PROBLEM DETECTED FOR THE test ARGUMENT."
#> 
#> $object.name
#> [1] "test"
#> 
saferDev::arg_check(
    data = test, 
    print = TRUE, 
    class = c("matrix", "array"), 
    mode = "numeric", 
    error_text = " using saferDev::arg_check()"
)
#> $problem
#> [1] FALSE
#> 
#> $text
#> [1] "NO PROBLEM DETECTED FOR THE test ARGUMENT using saferDev::arg_check()"
#> 
#> $object.name
#> [1] "test"
#> 
```
