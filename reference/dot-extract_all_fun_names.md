# Extract All Function Names

Extract all the function names of a string, with their positions.

## Usage

``` r
.extract_all_fun_names(text, pattern, lib_path, error_text)
```

## Arguments

- text:

  Single string.

- pattern:

  Single string: Perl regex, typically matching a function name followed
  by `(`, with a capture group starting at the first character of the
  name, like `"([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\("`.

- lib_path:

  Vector of character strings or `NULL`: the `lib_path` value of the
  calling exported function, only passed on to
  [`arg_check()`](arg_check.md). No default value.

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

## Value

A list containing:

- `string`: vector of strings, the matches of `pattern` in `text`,
  without their final `(`, if any, and the spaces before it.
  `character(0)` if no match.

- `pos`: vector of integers, the position in `text` of the first
  character of the first capture group of each match. `-1` if no match,
  `NULL` if `pattern` has no capture group.

## Details

The function names between quotes are also extracted.

Warnings:

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.extract_all_fun_names`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
