# Has Odd Number of Quotes

Determine if a string has a odd number of quotes.

## Usage

``` r
.has_odd_number_of_quotes(input_string, pattern, lib_path, error_text)
```

## Arguments

- input_string:

  Single string.

- pattern:

  Either `'"'` or `"'"`.

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

`TRUE` or `FALSE`.

## Details

The function considers any present `'` (`pattern = "'\''"`) or `"`
(`pattern = '"'`). Meaning that it does not treat special situation,
like escape quotes `\"`, or situation like this `"'"`. All these must be
removed from the `input_string` if necessary, using for instance:
`input_string <- gsub( x = input_string, pattern = '"\'"', replacement = ' ')`

Warnings:

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.has_odd_number_of_quotes`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
