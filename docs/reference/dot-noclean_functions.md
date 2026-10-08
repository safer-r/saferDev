# No-Clean Functions

Indicate if function names are inside quotes or after `$`.

## Usage

``` r
.noclean_functions(col1, col2, col3, ini, lib_path, error_text)
```

## Arguments

- col1:

  Vector of integers of line numbers in a code.

- col2:

  Vector of strings of the function names scrutinized.

- col3:

  Vector of strings of the code before the function name.

- ini:

  Vector of strings of the initial function code analyzed.

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

A logical vector indicating if function names of `col2` are inside
quotes or after `$` (`TRUE`) in `ini` or not (`FALSE`). Can be length 0.

## Details

Warnings:

- requires [`saferDev::arg_check`](arg_check.md),
  `.has_odd_number_of_quotes`.

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also these 2 functions when checking for the presence of
  `.noclean_functions`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
