# .pack_and_function_check

Check if 1) required functions are present in required packages and 2)
required packages are installed locally.

Check if critical operators of R are not present in other packages or in
the global env.

## Usage

``` r
.pack_and_function_check(fun, lib.path, external.function.name)

.base_function_check(external.function.name)
```

## Arguments

- fun:

  Character vector of the names of the required functions, preceded by
  the name of the package they belong to and a double colon. Example:
  c("ggplot2::geom_point", "grid::gpar").

- lib.path:

  Character vector specifying the absolute pathways of the directories
  containing the listed packages in the fun argument, if not in the
  default directories. If NULL, the function checks only in the
  .libPaths() default R library folders.

- external.function.name:

  Name of the function using the .pack_and_function_check() function.

## Value

An error message if at least one of the checked packages is missing in
lib.path, or if at least one of the checked functions is missing in the
required package, nothing otherwise.

An error message if at least one of the checked operator is present in
the R scope, nothing otherwise.

## Examples

``` r
# .pack_and_function_check(fun = "ggplot2::notgood", lib.path = .libPaths(), external.function.name = "fun1") # commented because this example returns an error
if (FALSE) {
# Example that shouldn't be run because this is an internal function
.pack_and_function_check(fun = c("ggplot2::geom_point", "grid::gpar"), lib.path = .libPaths(), external.function.name = "fun1")
}
if (FALSE) {
# Example that shouldn't be run because this is an internal function
.base_function_check(external.function.name = "fun1") # commented because this example returns an error
}
```
