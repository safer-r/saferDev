# No-Clean Functions

Indicate if function names are between quotes or after `$`.

## Usage

``` r
.noclean_functions(col1, col2, col3, ini, lib_path, error_text)
```

## Arguments

- col1:

  Vector of integers: the line numbers of the function names in `ini`.

- col2:

  Vector of strings: the function names.

- col3:

  Vector of strings: the code of the line before each function name, so
  that the name starts at the position `nchar(col3) + 1` of its line.

- ini:

  Vector of strings: the lines of the analyzed code (`$code` of
  [`.functions_detect()`](dot-functions_detect.md)).

- lib_path:

  Vector of character strings or `NULL`: the `lib_path` value of the
  calling exported function, only passed on to
  [`arg_check()`](arg_check.md) (directly or through the internal
  functions called). No default value.

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

## Value

A logical vector: `TRUE` if the function name of `col2` is between
quotes (string literal or backquoted name) or after `$` in `ini`,
`FALSE` otherwise. Can be length 0.

## Details

A function name is between quotes if the `(` that follows it is, the
quotes being read over all the lines of `ini` as the R parser does (see
[`.in_quotes_replacement()`](dot-in_quotes_replacement.md)).

Warnings:

- requires [`saferDev::arg_check`](arg_check.md),
  `.in_quotes_replacement`.

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also these 2 functions when checking for the presence of
  `.noclean_functions`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
