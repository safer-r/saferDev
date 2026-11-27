# Base Operator Check

Check that the critical operators and reserved words of R listed in the
error message (like `<-`, `[`, `+` or `if`) are not present in another
environment of the search path
([`search()`](https://rdrr.io/r/base/search.html): global environment,
attached packages and objects) than `package:base`.

## Usage

``` r
.base_op_check(error_text)
```

## Arguments

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

## Value

`NULL`, invisibly. An error if at least one of the checked objects is
present in another environment of the search path.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
