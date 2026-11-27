# Catch the Error, Warning or Message of an Instruction

Evaluates an instruction written as a character string, and returns its
error message, its last warning message or its standard messages
(neither errors nor warnings), as a single character string.

## Usage

``` r
get_message(
  data,
  kind = "error",
  header = TRUE,
  print_no = FALSE,
  text = NULL,
  env = NULL,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- data:

  Single character string: the instruction to evaluate.

- kind:

  Single character string: the kind of message returned, either
  `"error"` (default), `"warning"` or `"message"` (standard messages).

- header:

  Single logical value. Start the returned message with a header
  (`"ERROR MESSAGE REPORTED:"`, `"WARNING MESSAGE REPORTED:"` or
  `"STANDARD MESSAGE REPORTED:"`)? If `FALSE`, errors and warnings are
  returned as formatted by R (e.g.,
  `"simpleWarning in log(-1): NaNs produced"` instead of
  `"In log(-1): NaNs produced"`).

- print_no:

  Single logical value. If `TRUE`, a `"NO ... REPORTED"` string is
  returned instead of `NULL` when nothing is caught (see the returned
  value).

- text:

  Single character string inserted at the end of the header, before the
  colon (e.g., `"ERROR MESSAGE REPORTED IN A:"` with `text = "IN A"`),
  and added to the `"NO ... REPORTED"` strings whatever `header`. `NULL`
  (default) for no text.

- env:

  Environment in which the instruction is evaluated (e.g., `.GlobalEnv`
  or `asNamespace("stats")`). If `NULL` (default), the environment from
  which `get_message()` is called.

- safer_check:

  Single logical value. If `TRUE` (default), checks before running that
  the required packages and functions are installed and that the
  classical R operators (like `<-`) are not masked by another package or
  by a user-defined object of the global environment. These checks take
  time at each call: set `FALSE` in loops and inside other safer-r
  functions, to avoid repeating them.

- lib_path:

  Vector of character strings: paths of existing extra library folders
  containing the required packages (e.g., packages installed without
  admin rights). They are searched after the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) during the call
  (the current libraries keep priority), and
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored on
  exit. If `NULL` (default), only the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used.

- error_text:

  Single character string added to the error messages (e.g.,
  `" INSIDE pkg1::fun1()"`) to name the calling functions. `NULL` is
  converted into `""`.

## Value

A single character string: the error message (`kind = "error"`), the
last warning message (`kind = "warning"`), or all the standard messages,
separated by line breaks (`kind = "message"`).

If no such message is caught: `NULL` or, with `print_no = TRUE`,
`"NO (ERROR|WARNING|STANDARD) MESSAGE REPORTED"`. If the instruction
stops with an error while `kind` is `"warning"` or `"message"`, the
warnings and messages emitted before the error are not returned: `NULL`
or, with `print_no = TRUE`,
`"NO POTENTIAL (WARNING|STANDARD) MESSAGE BECAUSE OF ERROR MESSAGE REPORTED"`.

## Details

The instruction is evaluated once, whatever `kind`. Its warnings and
standard messages are caught and not displayed. Its printed outputs
(e.g., by [`print()`](https://rdrr.io/r/base/print.html) or `show()`)
are not caught: use
`printed <- utils::capture.output(msg <- get_message(data = "print(1) ; log(-1)", kind = "warning"))`
to get them in `printed`. Text written directly to the standard error
(e.g., by `cat(file = stderr())`) is not caught either: use
`utils::capture.output(type = "message")` to get it.

Plots are drawn on a NULL pdf device (no file created), and the graphic
devices left open by the instruction are closed. A ggplot2 object
returned by the instruction is printed, to catch the errors, warnings
and messages emitted when the plot is built.

## See also

[`try`](https://rdrr.io/r/base/try.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# See also https://safer-r.github.io/saferDev/articles/get_message.html

# Error message returned by default. Here no error to report:
saferDev::get_message(data = "base::log(-1)", kind = "error", header = TRUE, print_no = FALSE,
    text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#> NULL

# Warning message to report:
saferDev::get_message(data = "base::log(-1)", kind = "warning", header = TRUE, print_no = FALSE,
    text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#> [1] "WARNING MESSAGE REPORTED:\nIn base::log(-1): NaNs produced\n"

# No standard message to report:
saferDev::get_message(data = "base::log(-1)", kind = "message", header = TRUE, print_no = TRUE,
    text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#> [1] "NO STANDARD MESSAGE REPORTED IN A"

# No standard message returned, because wilcox.test() stops with an error:
saferDev::get_message(data = "wilcox.test()", kind = "message", header = TRUE, print_no = TRUE,
    text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#> [1] "NO POTENTIAL STANDARD MESSAGE BECAUSE OF ERROR MESSAGE REPORTED IN A"

# Error message to report, with text added:
saferDev::get_message(data = "wilcox.test()", kind = "error", header = TRUE, print_no = TRUE,
    text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#> [1] "ERROR MESSAGE REPORTED IN A:\nIn wilcox.test.default() : argument \"x\" is missing, with no default\n"

# No error message to report, with text added:
saferDev::get_message(data = "sum(1)", kind = "error", header = TRUE, print_no = TRUE,
    text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#> [1] "NO ERROR MESSAGE REPORTED IN A"

# A standard message is not reported with kind = "error":
saferDev::get_message(data = "message('ahah')", kind = "error", header = TRUE, print_no = TRUE,
    text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#> [1] "NO ERROR MESSAGE REPORTED IN A"

# Standard message to report, with text added:
saferDev::get_message(data = "message('ahah')", kind = "message", header = TRUE, print_no = TRUE,
    text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#> [1] "STANDARD MESSAGE REPORTED IN A:\nahah"

# Messages of a ggplot2 plot caught:
if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){
    saferDev::get_message(data = "ggplot2::ggplot(data = data.frame(X = 1:10,
        stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()",
        kind = "message", header = TRUE, print_no = TRUE, text = "IN INSTRUCTION 1", env = NULL,
        safer_check = TRUE, lib_path = NULL, error_text = "")
}
#> [1] "STANDARD MESSAGE REPORTED IN INSTRUCTION 1:\n\033[1m\033[22m`stat_bin()` using `bins = 30`. Pick better value `binwidth`."
```
