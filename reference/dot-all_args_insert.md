# Function for All Arguments Here

Insert the missing arguments detected by
[`all_args_here()`](all_args_here.md) into the printed code of the
submitted function, at the positions given by the R parser, so that the
line breaks, the indentation and the comments are kept, and return the
modified function.

## Usage

``` r
.all_args_insert(x, out, output, lib_path, error_text)
```

## Arguments

- x:

  Function to modify. Function object.

- out:

  List returned by [`.functions_detect()`](dot-functions_detect.md):
  `$code`, `$fun_names`, `$fun_names_pos`, `$code_line_nb`,
  `$internal_fun_names` and `$arg_user_setting`. The `$code` compartment
  is the normalized code read by [`all_args_here()`](all_args_here.md)
  and must have the same line indexing as the `$LINE_NB` column of the
  `output` data frame.

- output:

  Data frame returned by the main code of
  [`all_args_here()`](all_args_here.md): the `$LINE_NB`, `$FUN_NAME`,
  `$FUN_POS`, `$DEF_ARGS`, `$MISSING_ARGS` and `$STATUS` columns are
  used to apply the corrections (`$FUN_ARGS` is not used).

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

- `fun`: the modified function object, or `NULL` if no correction has
  been applied.

- `fun_txt`: the modified code as a single string (`NULL` if no
  correction has been applied).

- `inserted`: character vector of the applied corrections ("line \<n\>:
  \<status\>" for each).

- `left`: character vector of the corrections that could not be applied
  ("line \<n\>: \<status\>", followed by a "REASON:" line explaining why
  the correction has not been applied), for the final message of
  [`all_args_here()`](all_args_here.md).

## Details

Warnings:

- requires [`saferDev::arg_check`](arg_check.md).

- In the safer Backbone section
  `"######## check of the required functions from the required packages"`,
  add also [`saferDev::arg_check`](arg_check.md) when checking for the
  presence of `.all_args_insert`.

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han
