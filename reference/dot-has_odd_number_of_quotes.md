# Has Odd Number of Quotes

Determine if a string has an odd number of a quote character.

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

  Vector of character strings or `NULL`: the `lib_path` value of the
  calling exported function, only passed on to
  [`arg_check()`](arg_check.md). No default value.

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

## Value

`TRUE` or `FALSE`.

## Details

Every occurrence of `pattern` is counted, including escaped quotes
(`\"`) and quotes inside strings delimited by the other quote
(`"it's"`). Thus, the result does not tell if the end of `input_string`
is between quotes: use
[`.in_quotes_replacement()`](dot-in_quotes_replacement.md) for that.

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
