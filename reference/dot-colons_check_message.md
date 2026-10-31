# Colons Check Message

Create the message for the [`colons_check()`](colons_check.md) function.

## Usage

``` r
.colons_check_message(
  list_fun,
  list_fun_pos,
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

  List of names of all the functions.

- list_fun_pos:

  List of positions of first character of names of all the functions in
  `ini`.

- line_nb:

  Vector of integers of corresponding line numbers.

- ini:

  Vector of strings of the initial function code analyzed.

- arg_user_setting2:

  List of argument user settings.

- text:

  Either `"BASIC"` or `"OTHER"`.

- internal_fun_names:

  Vector of strings of names of internal functions in the function code
  analyzed. Can be `NULL`.

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
  (default): only the pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`.

## Value

A list:

- `output.cat`: the message (string).

- `colon_not_here`: logical vector. Does `list_fun` contain function
  names without `::` or `:::` ?

- `col1`: integer vector of the line numbers (in `ini`) of the function
  names without `::` or `:::`, after removal of the functions between
  quotes or preceded by `$`, and of the internal functions when
  `text = "OTHER"`. Length 0 if no problem detected.

- `col2`: character vector of the corresponding function names. Length 0
  if no problem detected.

- `col3`: character vector of the corresponding strings before the
  function names. Length 0 if no problem detected.

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
