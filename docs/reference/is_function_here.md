# Is Function Here

Check if required functions are present in installed packages. This
controls the potential modifications of package contents.

## Usage

``` r
is_function_here(fun, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- fun:

  Character vector of the names of the required functions, preceded by
  the name of the package they belong to and a double or triple colon.
  Example: `c("ggplot2::geom_point", "grid::gpar")`. Warning: do not
  write `"()"` at the end of each tested function.

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
  If `NULL`, converted into `""`.

## Value

An error message if at least one of the checked packages is missing in
`lib_path`, or if at least one of the checked functions is missing in
the required package, nothing otherwise.

## See also

[`exists`](https://rdrr.io/r/base/exists.html) and `findFunction`

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/is_function_here.html
if (FALSE) { # \dontrun{
# Example that returns an error
saferDev::is_function_here(fun = "ggplot2::notgood", error_text = " INSIDE P1::F1")
saferDev::is_function_here(fun = c("ggplot2::geom_point", "grid::gpar"))
saferDev::is_function_here(fun = "c")
} # }
saferDev::is_function_here(fun = "base::c", error_text = " INSIDE P1::F1")
```
