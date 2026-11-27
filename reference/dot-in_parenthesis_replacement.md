# Replacement Inside Parenthesis

Replace each match of a pattern located between two given brackets of a
string by a replacement character, and return the positions of the
replacements.

## Usage

``` r
.in_parenthesis_replacement(
  string,
  pattern,
  no_regex_pattern,
  replacement,
  perl,
  open_pos,
  close_pos,
  lib_path,
  error_text
)
```

## Arguments

- string:

  Single string.

- pattern:

  Single string: regex matching a single character, like `","`.

- no_regex_pattern:

  Single string: the character matched by `pattern`, without regex
  escape. Used to check the replaced characters.

- replacement:

  Single character replacing each match of `pattern`. Not a regex.

- perl:

  Single logical value. Use Perl regex in `pattern`?

- open_pos:

  Single integer: position of an opening bracket `(` in `string`.

- close_pos:

  Single integer: position of a closing bracket `)` in `string`.

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

- `string`: `string` with the matches of `pattern` between the positions
  `open_pos` and `close_pos` replaced by `replacement`.

- `pos`: vector of the positions of the replaced matches in `string`.
  `NULL` if no match. In that case, `string` is identical to the input
  string.

## Details

Warnings:

- `pattern` must match a single character.

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.in_parenthesis_replacement`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
