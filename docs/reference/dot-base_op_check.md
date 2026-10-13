# Base Operator Check

Check if critical operators of R are not present in other packages or in
the global env.

## Usage

``` r
.base_op_check(error_text)
```

## Arguments

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

An error message if at least one of the checked operator is present in
the R scope, nothing otherwise.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
