# Function for All Arguments Here

Get the `$MISSING_ARG_NAMES`, `$MISSING_ARGS` and `$NEW` of
[`all_args_here()`](all_args_here.md).

## Usage

``` r
.all_args_here_fill(
  arg_full,
  arg_full_names,
  tempo_split,
  three_dots_log,
  i2,
  col1_i2,
  col2_i2,
  arg_user_setting_x,
  warn,
  warn_count,
  lib_path,
  error_text
)
```

## Arguments

- arg_full:

  List of all arguments of the function with default value.

- arg_full_names:

  Vector of strings of the names of the arguments of the function. Can
  be `NULL`.

- tempo_split:

  Vector of strings of the observed argument writing of the function.

- three_dots_log:

  Vector of logical. Is `...` present among `arg_full_names`.

- i2:

  Single positive and non null integer value indicating the loop number.

- col1_i2:

  Single positive and non null integer value indicating the code line
  number of the checked function.

- col2_i2:

  Single string indicating the name of the checked sub function.

- arg_user_setting_x:

  Single string indicating the name of the checked function.

- warn:

  Single warning string. Can be `NULL`.

- warn_count:

  Single positive integer value indicating the warning count.

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

- `col6`: the `$MISSING_ARG_NAMES`.

- `col7`: the `$MISSING_ARGS`.

- `col8`: the `$STATUS`.

## Details

Warnings:

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.all_args_here_fill`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
