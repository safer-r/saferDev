# Replacement Inside Parenthesis

Parse a string and replace any pattern present between brackets `()` by
another pattern.

## Usage

``` r
.in_parenthesis_replacement(
  string,
  pattern,
  no_regex_pattern,
  replacement,
  perl,
  open_pos,
  close_pos,
  lib_path,
  error_text
)
```

## Arguments

- string:

  Single string.

- pattern:

  Single string indicating the pattern to detect. Warning: must be very
  simple pattern, like `","`.

- no_regex_pattern:

  Same as `pattern` without escape characters or list, etc. Used for
  messages.

- replacement:

  Single string for `pattern` replacement. Is not regex.

- perl:

  Single logical value. Use Perl regex in `pattern` ?

- open_pos:

  Single positive and non zero integer indicating the position of the
  opening parenthesis.

- close_pos:

  Single positive and non zero integer indicating the position of the
  closing parenthesis.

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

A list containing:

- `string`: the input string with all pattern replaced by the
  replacement pattern.

- `pos`: the positions of the 1st character of the replaced pattern.
  `NULL` if no replaced pattern. In that case, `string` is identical to
  the input string

## Details

Warnings:

- Must be very simple pattern, like `"\("`.

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.in_parenthesis_replacement`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
