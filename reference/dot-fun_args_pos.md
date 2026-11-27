# Function Arguments Position

Return the positions of the first function name found in a string, of
its opening and closing brackets, and of each pair of brackets between
them.

## Usage

``` r
.fun_args_pos(text, pattern, lib_path, error_text)
```

## Arguments

- text:

  Single string, whose brackets between quotes have been replaced (see
  [`.in_quotes_replacement()`](dot-in_quotes_replacement.md)).

- pattern:

  Single string: Perl regex matching the function name and its opening
  bracket, generally `paste0(<FUNCTION_NAME>, "[\\s\\r\\n]*\\(")`.

- lib_path:

  Vector of character strings or `NULL`: the `lib_path` value of the
  calling exported function, only passed on to
  [`arg_check()`](arg_check.md). No default value.

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

## Value

A list containing positions in `text`:

- `begin_fun`: position of the first character of the first match of
  `pattern`.

- `begin`: position of the opening bracket `(` of the function.

- `end`: position of the closing bracket `)` of the function.

- `middle_bracket_pos`: list of the positions of each pair of brackets
  between `begin` and `end`: in each compartment, the position of `(`
  and the position of the matching `)`. `NULL` if no bracket.

## Details

An internal error is returned if `pattern` is not found in `text`, if no
`(` follows it, or if its closing bracket is not found (e.g., bracket
between quotes not replaced). Its message contains an excerpt of `text`.
No file is written.

Warnings:

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.fun_args_pos`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
