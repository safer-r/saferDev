# Missing Arguments of a Function Call

Compute the `$MISSING_ARG_NAMES`, `$MISSING_ARGS` and `$STATUS` columns
of the table of [`all_args_here()`](all_args_here.md) for one function
call.

## Usage

``` r
.all_args_here_fill(
  arg_full,
  arg_full_names,
  tempo_split,
  three_dots_log,
  pipe_log,
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

  List of the arguments of the called function, with their default
  values as copied into the call (empty value for the arguments without
  default value, or whose default value cannot be copied).

- arg_full_names:

  Vector of character strings: names of the arguments of the called
  function. Can be `NULL`.

- tempo_split:

  Vector of character strings: arguments written in the call.

- three_dots_log:

  Logical vector: is each element of `arg_full_names` the `...`
  argument?

- pipe_log:

  Single logical value: is the call on the right side of a pipe (e.g.,
  `x |> base::rep(3)`)? If `TRUE`, the value of the pipe takes the first
  argument not written with its name and placed before `...` (except if
  the placeholder `_` or `.` is written as an argument), and the values
  written without name are kept without name in `$STATUS`.

- i2:

  Single positive integer value: loop number in
  [`all_args_here()`](all_args_here.md).

- col1_i2:

  Single positive integer value: code line number of the call.

- col2_i2:

  Single character string: name of the called function.

- arg_user_setting_x:

  Single character string: name of the checked function.

- warn:

  Single character string of the warnings already raised, added to the
  error messages. Can be `NULL`.

- warn_count:

  Single integer value (0 or more): number of warnings in `warn`.

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
