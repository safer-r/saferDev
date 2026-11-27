# Colons Check Message

Create the message of [`colons_check()`](colons_check.md) for one kind
of functions (basic R functions or other functions).

## Usage

``` r
.colons_check_message(
  list_fun,
  list_fun_pos,
  list_fun_internal,
  line_nb,
  ini,
  arg_user_setting2,
  text,
  internal_fun_names,
  lib_path,
  error_text
)
```

## Arguments

- list_fun:

  List of vectors of function names, one compartment per code line (see
  `line_nb`).

- list_fun_pos:

  List of the positions, in the lines of `ini`, of the first character
  of the names of `list_fun` (same structure as `list_fun`).

- list_fun_internal:

  List of logical vectors, with the structure of `list_fun`: `TRUE` for
  the calls of internal functions (see `text`).

- line_nb:

  Vector of integers: line numbers (in `ini`) of the compartments of
  `list_fun`.

- ini:

  Vector of strings of the analyzed code of the checked function, one
  string per line.

- arg_user_setting2:

  List of the argument settings of the calling function: its `x` element
  (name of the checked function) is written in the message.

- text:

  Single character string, `"BASIC"` or `"OTHER"`: kind of functions,
  written in the message. With `"OTHER"`, the calls of internal
  functions (see `list_fun_internal`) are removed from `list_fun`, and
  `internal_fun_names` is listed in the message.

- internal_fun_names:

  Vector of strings: names of the internal functions called in the
  analyzed code. Can be `NULL`.

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

A list:

- `output.cat`: the message (string), `NULL` if nothing is reported.

- `colon_not_here`: logical vector, one value per function name of
  `list_fun` (after the removal of the internal functions when
  `text = "OTHER"`): is the name written without `::` or `:::` before
  it? `FALSE` if `list_fun` is empty.

- `col1`: integer vector of the line numbers (in `ini`) of the function
  names without `::` or `:::`, after the removal of the names between
  quotes or preceded by `$` or `@`, and of the internal functions when
  `text = "OTHER"`. Length 0 if nothing is reported.

- `col2`: character vector of the corresponding function names. Length 0
  if nothing is reported.

- `col3`: character vector of the corresponding strings before the
  function names. Length 0 if nothing is reported.

## Details

Warnings:

- requires [`saferDev::arg_check`](arg_check.md), `.noclean_functions`.

- In main safer functions, in the section
  `"######## check of the required functions from the required packages"`,
  add also these 2 functions when checking for the presence of
  `.colons_check_message`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
