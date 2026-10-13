# Replacement Inside Quotes

Parse a string and replace any pattern inside simple or double quotes by
another replacement pattern and get the position of replacements.

## Usage

``` r
.in_quotes_replacement(
  string,
  pattern,
  no_regex_pattern,
  replacement,
  perl,
  lib_path,
  error_text
)
```

## Arguments

- string:

  Single string.

- pattern:

  Single string indicating the pattern to detect. Warning: must be very
  simple pattern, like `"\("`.

- no_regex_pattern:

  Same as `pattern` without escape characters or list, etc. Used for
  messages.

- replacement:

  Single string for `pattern` replacement. Must have the same number of
  character than `no_regex_pattern`. Is not regex.

- perl:

  Single logical value. Use Perl regex in `pattern` ?

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

A list containing:

- `string`: The input string with all pattern replaced by the
  replacement pattern.

- `pos`: vector of the positions of the 1st character of the replaced
  pattern. `NULL` if no replaced pattern. In that case, `string` is
  identical to the input string.

## Details

The function works first by replacing in the code: 1) the three
consecutive characters `'"'` or `'"'` by three spaces, 2) `"\"'"` and
`'\'"'` by four spaces and 3) escape quotes like `\"` or `\'` by two
spaces, before looking for the pattern inside quotes.

Warnings:

- Must be very simple pattern, like `"\("`.

- requires [`saferDev::arg_check`](arg_check.md),
  `.has_odd_number_of_quotes`.

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also these 2 functions when checking for the presence of
  `.in_quotes_replacement`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
