# Get Message

Evaluate an instruction written between `""` and return the first of the
error message, or the last of the warning or standard (non error non
warning) messages if ever exist.

Using argument `print_no = FALSE`, return `NULL` if nothing reported,
which is convenient in some cases.

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

  Single character string of an instruction to evaluate

- kind:

  Single character string. Either `"error"` to get a potential error
  message, or `"warning"` to get a potential warning message, or
  `"message"` to get a potential standard (non error and non warning)
  message.

- header:

  Single logical value. Add a header in the returned message?

- print_no:

  Single logical value. Print a message saying that nothing (`NULL`
  output) has to be reported?

- text:

  Single character string added to the header of the output message,
  even if nothing reported (`print_no` is `TRUE`). Ignored if the
  `header` argument is `FALSE`. Write `NULL` if not required.

- env:

  An object corresponding to an existing environment. Then the `data`
  argument value is evaluated only in the indicated environment. Write
  `NULL` if not required (R scope is used). Example `env = .GlobalEnv`.
  Example `env = asNamespace("stats")`.

- safer_check:

  Single logical value. Perform some "safer" checks? If `TRUE`,
  checkings are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Must be set to
  `FALSE` if this function is used inside another "safer" function to
  avoid pointless multiple checkings.

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

The function returns the error, warning, or standard message, as a
single character string, depending on what has been selected using the
`kind` argument and if such message exists.

`NULL` if no selected message returned and the `print_no` argument is
`FALSE`.

The following message if no selected message returned and the `print_no`
argument is `TRUE`: `"NO (ERROR|WARNING|STANDARD) MESSAGE REPORTED"`.

The following message if 1) the `kind` argument is `"warning"` or
`"message"` while an error message is returned and 2) the `print_no`
argument is `TRUE`:
`"NO POTENTIAL (WARNING|STANDARD) MESSAGE BECAUSE OF ERROR MESSAGE REPORTED"`.

## Details

Warnings:

- Only the first error or last warning/standard message is returned.

- Always use the `env` argument when `get_message()` is used inside
  functions.

- The function does not prevent printing, for instance if
  [`print()`](https://rdrr.io/r/base/print.html) or `show()` is used
  inside the instruction tested. To prevent that, use
  `tempo <- utils::capture.output(error <- get_message(data = "arg_check(data = 'a', class = mean, neg_values = FALSE, print = TRUE)"))`.
  The return of `get_message()` is assigned into `error` and the printed
  messages are captured by
  [`utils::capture.output()`](https://rdrr.io/r/utils/capture.output.html)
  and assigned into `tempo`. See the examples.

The `env` argument is used as value for the `envir` argument of the
[`base::eval()`](https://rdrr.io/r/base/eval.html) function.

## See also

[`try`](https://rdrr.io/r/base/try.html).

## Author

[Gael Millot](mailto:gael.millot@pasteur.fr)

Haiding Wang

Yushi Han

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferDev/articles/get_message.html

# Report error message by default. Here no error to report:
saferDev::get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)")
#> NULL

# Warning message to report:
saferDev::get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "warning")
#> NULL

# No standard message to report:
saferDev::get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "message", 
print_no = TRUE, text = "IN A")
#> [1] "NO STANDARD MESSAGE REPORTED IN A"

# Standard message cannot be caught because wilcox.test() returns an error message:
saferDev::get_message(data = "wilcox.test()", kind = "message", print_no = TRUE, text = "IN A")
#> [1] "NO POTENTIAL STANDARD MESSAGE BECAUSE OF ERROR MESSAGE REPORTED IN A"

# Text added in warning message to report:
saferDev::get_message(data = "wilcox.test()", kind = "error", print_no = TRUE, text = "IN A")
#> [1] "ERROR MESSAGE REPORTED IN A:\nIn wilcox.test.default() : argument \"x\" is missing, with no default\n"

# No error message to report with text added:
saferDev::get_message(data = "sum(1)", kind = "error", print_no = TRUE, text = "IN A")
#> [1] "NO ERROR MESSAGE REPORTED IN A"

# No error message to report with text added:
saferDev::get_message(data = "message('ahah')", kind = "error", print_no = TRUE, text = "IN A")
#> [1] "NO ERROR MESSAGE REPORTED IN A"

# Standard message reported with text added:
saferDev::get_message(data = "message('ahah')", kind = "message", print_no = TRUE, text = "IN A")
#> ahah
#> [1] "NO STANDARD MESSAGE REPORTED IN A"

# Messages from ggplot2 functions caugth:
saferDev::get_message(data = "ggplot2::ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), 
mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()", kind = "message", 
print_no = TRUE, text = "IN INSTRUCTION 1")
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
#> [1] "NO STANDARD MESSAGE REPORTED IN INSTRUCTION 1"
```
