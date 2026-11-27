# Package and Function Check

Check that the packages of `fun` are installed in `lib_path`, and that
the functions of `fun` exist in the namespace of their package (exported
or not). Simplified version of
[`saferDev::is_function_here()`](is_function_here.md), used by the other
functions of the saferDev package.

## Usage

``` r
.pack_and_function_check(fun, lib_path, error_text)
```

## Arguments

- fun:

  Character vector of the names of the required functions, preceded by
  the name of their package and `::` (or `:::` for function names
  starting with a dot). Example:
  `c("saferDev::arg_check", "saferDev:::.base_op_check")`.

- lib_path:

  Vector of character strings or `NULL`: the library folders where the
  packages are searched (the calling exported functions pass the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html), which already
  contain their `lib_path`). If `NULL`, the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used. No
  default value.

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

## Value

`NULL`, invisibly. An error if at least one of the packages is not
installed in `lib_path`, or if at least one of the functions is missing
in its package.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
