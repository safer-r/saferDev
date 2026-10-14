# Function Arguments Position

Return the positions of 1st letter of the first function name in a
string, as well as its own opening and closing parenthesis, as well as
positions of all the internal parenthesis (inside the own parenthesis).

## Usage

``` r
.fun_args_pos(text, pattern, lib_path, error_text)
```

## Arguments

- text:

  Single string.

- pattern:

  Single string of a Perl regex to extract function name and `()`, using
  generally `paste0(<FUNCTION_NAME>, "[\s\r\n]*\(")`.

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

A list containing positions:

- `begin_fun`: position of the 1st letter of the first function name in
  the tested string.

- `begin`: position of the opening `"("` of the first function.

- `end`: position of the closing `")"` of the first function.

- `middle_bracket_pos`: list of the positions of each couple of brackets
  in the middle of the `begin` and `end` positions. In each compartment,
  the first number is the position of `(` and the second the position of
  `)`. `NULL` if no inside brackets.

## Details

Warnings:

- The tested string must be cleaned form brackets between quotes. Use
  [`.in_quotes_replacement()`](dot-in_quotes_replacement.md) for that.

- Quotes in strings are escaped, so that position of `(` in `\"a(` is 3,
  not 4.

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.fun_args_pos`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
