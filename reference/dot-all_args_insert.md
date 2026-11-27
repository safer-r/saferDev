# Insert the Missing Arguments into a Function

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

  List returned by [`.functions_detect()`](dot-functions_detect.md).
  Only its `$code` compartment is used: the normalized code read by
  [`all_args_here()`](all_args_here.md), which must have the same line
  indexing as the `$LINE_NB` column of the `output` data frame.

- output:

  Data frame built by the main code of
  [`all_args_here()`](all_args_here.md): the `$LINE_NB`, `$FUN_NAME`,
  `$FUN_POS`, `$DEF_ARGS`, `$MISSING_ARGS` and `$STATUS` columns are
  used to apply the corrections (`$FUN_ARGS` is not used). The optional
  `$NO_INSERT` column gives, for each call, the reason why its proposal
  must not be inserted (`"pipe"`, `"dots"`, `"empty_arg"`, `"nargs"`,
  `"generic_dispatch"`, `"generic_first_arg"`, `"method_args"`,
  `"method_partial"` or `"builtin_empty"`), or `""`.

- lib_path:

  Vector of character strings or `NULL`: the `lib_path` value of the
  calling exported function, only passed on to
  [`arg_check()`](arg_check.md). No default value.

- error_text:

  Single character string added to the error messages to name the
  calling functions (`embed_error_text` of the calling function). `NULL`
  is converted into `""`. No default value.

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
