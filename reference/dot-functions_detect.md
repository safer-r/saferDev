# Functions Detection

Detect all the functions names inside a function.

## Usage

``` r
.functions_detect(x, skipped_base, arg_user_setting2, lib_path, error_text)
```

## Arguments

- x:

  Function name to test, written without quotes and brackets.

- skipped_base:

  Vector of strings specifying the R basic functions skipped from
  detection.

- arg_user_setting2:

  Argument user settings list.

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

A list:

- `code`: vector of strings of the code of the tested function.

- `all_basic_funs`: vector or strings of names of all the basic R
  functions.

- `fun_names`: list of names of all the functions, not considering
  `base::c("function", "if", "for", "while", "repeat")`. Compartment
  names indicate the code line number of the functions in `code`.

- `fun_names_pos`: list of position of the first character of each
  `fun_names`. Compartment names indicate the code line number of the
  functions in `code`.

- `code_line_nb`: vector of integers of the code line numbers of code
  for each non empty compartment of `fun_names` and `fun_names_pos`.

- `internal_fun_names`: vector of string of names of internal functions
  in the code of the tested function.

- `arg_user_setting`: list of argument user settings of the tested
  function.

`NULL`, with a warning, if the tested function is empty of function, or
only made of comments, or only made of the `skipped_base` functions.

## Details

Does not check if the functions inside the code exist.

Use the regex pattern `"([a-zA-Z]|\.[a-zA-Z._])[a-zA-Z0-9._]*\s*\("` to
detect a function in the code.

The result in `all_basic_funs` are all the functions in
`base::c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base")`.

Warnings:

- Requires [`saferDev::arg_check`](arg_check.md),
  `.extract_all_fun_names`, `.has_odd_number_of_quotes`.

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also these 3 functions when checking for the presence of
  `.functions_detect`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
