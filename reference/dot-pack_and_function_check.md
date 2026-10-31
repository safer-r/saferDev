# Package and Function Check

Check if 1) required functions are present in required packages and 2)
required packages are installed locally. Simplified version of
[`saferDev::is_function_here()`](is_function_here.md), used as internal
function for the other functions of the saferDev package.

## Usage

``` r
.pack_and_function_check(fun, lib_path, error_text)
```

## Arguments

- fun:

  Character vector of the names of the required functions, preceded by
  the name of the package they belong to, separated by a double or
  triple colon. Example: `c("ggplot2::geom_point", "grid::gpar")`.

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

An error message if at least one of the checked packages is missing in
`lib_path`, or if at least one of the checked functions is missing in
the required package, nothing otherwise.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
