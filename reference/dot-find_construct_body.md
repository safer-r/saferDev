# Find Construct Body

Locate the body expression of a construct (`if`, `else`, `for`, `while`,
`repeat`, `function` or lambda `\(x)`) in the parse data of the checked
code, for the construction of the modified function by
[`curly_bracket_check`](curly_bracket_check.md). The body is the last
`expr` child of the construct expression: for `if`, the condition `expr`
comes first, the `then` body follows, and the `else` body (if any) is
the last `expr` child; for `for`, the `forcond` (which owns the
parentheses) comes before the body `expr`; for `while`, `repeat`,
`function` and lambdas, the body is also the last `expr` child.

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
  intern_error_text_start,
  intern_error_text_end,
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
  `if()` construct? If `TRUE`, the last `expr` child of the construct
  expression is returned; if `FALSE`, the `then` body of an `if` with an
  `else` (the second-to-last `expr` child) is returned.

- else_tok:

  Single integer or `NULL`: row index in `pd` of the `ELSE` token (used
  only when `else_branch = TRUE`, for the internal error messages).

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `unique(c(new, .Library.site, .Library))`. Warning:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored to
  the initial paths, after function execution. Ignored if `NULL`
  (default) or if the `safer_check` argument is `FALSE`: only the
  pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

A list with: `row`, row index in `pd` of the body `expr` (whose
`col1`/`line1` give the position just before the body, and
`col2`/`line2` the position of the last character of the body); `line1`,
the line of the body start; `col1`, the column (character offset) of the
body start.

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
