# Find Construct Body

Locates the body expression of a construct (`if`, `else`, `for`,
`while`, `repeat`, `function` or lambda `\(x)`) in the parse data of the
checked code, for the construction of the modified function by
[`curly_bracket_check`](curly_bracket_check.md). The candidate children
of the construct expression are those whose token is `expr`, or
`expr_or_assign_or_help` for a body written with `=` (e.g.,
`if(x) y = 1`). The `forcond` of a `for`, which owns the parentheses, is
not a candidate. The body is the last candidate, except the `then` body
of an `if` with an `else`, which is the second-to-last.

## Usage

``` r
.find_construct_body(
  pd,
  kid,
  body_start,
  body_start_pos,
  term_rows,
  else_branch,
  else_tok,
  lib_path,
  error_text
)
```

## Arguments

- pd:

  Data frame returned by
  [`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html)
  on the code of the checked function.

- kid:

  Single integer: row index in `pd` of the terminal token of the
  construct keyword (`IF`, `FOR`, `WHILE`, `REPEAT`, `FUNCTION`,
  `LAMBDA` or `'\\'`).

- body_start:

  Single integer: row index in `pd` of the terminal token just before
  the body (the closing `)` of the condition or of the arguments, or the
  keyword itself for `repeat`).

- body_start_pos:

  Single integer: position in `term_rows` of the terminal token
  `body_start`.

- term_rows:

  Integer vector of the terminal rows (comments excluded) of `pd`,
  sorted by source position (as computed in
  [`curly_bracket_check`](curly_bracket_check.md)).

- else_branch:

  Single logical value. Is the requested body the `else` branch of the
  `if()` construct? If `TRUE`, the last candidate child of the construct
  expression (see Description) is returned. If `FALSE`, the body of the
  construct is returned: the second-to-last candidate for the `then`
  body of an `if` with an `else`, the last one otherwise.

- else_tok:

  Single integer or `NULL`: row index in `pd` of the `ELSE` token
  (required when `else_branch = TRUE`).

- lib_path:

  Vector of character strings or `NULL`: the `lib_path` value of the
  calling exported function, only passed on to
  [`arg_check()`](arg_check.md). No default value.

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

## Value

A list with: `row`, the row index in `pd` of the body expression
(`line1` and `col1`: first character of the body; `line2` and `col2`:
last character of the body); `line1` and `col1`, the line and the column
of the body start. The columns are those of
[`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html),
where a TAB counts as a jump to the next multiple of 8: the caller
converts them into character positions of the code line.

## Details

Warnings:

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.find_construct_body`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
