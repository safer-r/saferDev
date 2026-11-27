# Replacement Inside Quotes

Replace each match of a pattern located between quotes (string literals
and backquoted names of R code) by a replacement of the same length, and
return the positions of the replacements.

## Usage

``` r
.in_quotes_replacement(
  string,
  pattern,
  no_regex_pattern,
  replacement,
  perl,
  lib_path,
  error_text
)
```

## Arguments

- string:

  Single string, for instance R code, with lines separated by `"\n"`.

- pattern:

  Single string: regex matching a single character, like `"\\("`.

- no_regex_pattern:

  Single string: the character matched by `pattern`, without regex
  escape, like `"("`. Used to check the replaced characters.

- replacement:

  Single string replacing each match of `pattern` between quotes. Must
  have the same number of characters as `no_regex_pattern`. Not a regex.

- perl:

  Single logical value. Use Perl regex in `pattern`?

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

- `string`: `string` with the matches of `pattern` between quotes
  replaced by `replacement`.

- `pos`: vector of the positions of the replaced matches. `NULL` if no
  match is between quotes. In that case, `string` is identical to the
  input string.

## Details

`string` is read from left to right, as the R parser reads code: a quote
(`"`, `'` or `` ` ``) opens a quoted part that only the same quote
closes, a backslash inside quotes escapes the next character (e.g., `\"`
or `\\`), a raw string (e.g., `r"(...)"` or `R"--[...]--"`) is only
closed by its own closing delimiter, `#` outside quotes starts a
comment, up to the end of the line, and the `%...%` operators are
skipped. Thus, an apostrophe inside double quotes (`"it's"`), or a
double quote inside simple quotes, opens nothing. A quoted part that is
not closed goes up to the end of `string`. The matches inside comments
are not replaced.

Only the content of a quoted part is replaced, never its quotes or the
delimiters of a raw string, so that the output string can be passed
again to the function (e.g., to replace `(` after `)`).

Warnings:

- `pattern` must match a single character.

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.in_quotes_replacement`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
