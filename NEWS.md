# saferDev 3.0.0

## Breaking changes

* `all_args_here()` returns the corrected function (`NULL`, invisibly, if nothing is corrected), instead of the data frame of the detected calls, which is now printed as a message or, with `table_export = TRUE`, written into a file.
* `all_args_here()`: `export` is renamed `table_export`, and the new `fun_export` argument is inserted at the 3rd position, which shifts the calls using argument positions. `out_path` now defaults to `NULL` (formerly `"."`) and is required when `table_export` or `fun_export` is `TRUE`.
* `colons_check()` returns the corrected function, instead of always `NULL` (still returned, invisibly, if nothing is corrected). Its new `export` and `out_path` arguments are inserted before `safer_check`, which shifts the calls using argument positions. It also gives a warning for the calls, without package, of functions that are not basic R functions (formerly only listed in the message).
* `all_args_here()` and `colons_check()` stop with an error asking to run `lambda_check()` first if the function contains `\(x)` lambda functions.
* `colons_check()`, `env_check()`, `get_message()` and `report()` check and use `lib_path` even with `safer_check = FALSE` (formerly ignored): an incorrect `lib_path` returns an error.
* `arg_check()`: with `prop = TRUE`, values between -1 and 1 are accepted by default (add `neg_values = FALSE` for the former 0 to 1 range), and `class`, `typeof` and `mode` must be `NULL`. The `object.name` output component is renamed `object_name`.
* `get_message(kind = "message")` returns a single string with all the messages, and only message conditions (not the text written to the standard error, e.g., by `cat(file = stderr())`).
* `is_function_here()`: `"pkg::fun"` requires an object exported by the package, as in R code; write `"pkg:::fun"` for an internal function.
* `report()`: `rownames_kept = FALSE` (default) removes the row names of data frames of any size, not only of those of at most 4 rows.

## New functions

* `super_assign_check()`: returns an error listing the lines that use `<<-` or `->>`, also when called as a function (e.g., `` `<<-`(y, 1) ``).
* `lambda_check()`: replaces the `\(x)` shorthand of lambda functions by `function(x)`.
* `curly_bracket_check()`: adds the missing curly brackets `{}` around the bodies of `if`/`else`, `for`, `while`, `repeat` and `function`.
* `true_false_check()`: replaces `T` and `F` used as values (e.g., `na.rm = T`, but not `x <- T`) by `TRUE` and `FALSE`.
* `for_loop_nb_check()`: inserts a checking line before each `for` loop whose number of iterations is not fixed by the writing (like `1:length(x)`), which warns at execution if the sequence is empty, contains `NA` or infinite values, has a single value, has only negative values, or starts decreasing.

## New features

* `all_args_here()`, `colons_check()` and the new functions above, except `super_assign_check()`, print a table of the problematic lines and return the corrected function (`NULL`, invisibly, if nothing is corrected), with the environment of the submitted function, and its comments and indentation when it carries its source references. With `export = TRUE` (`fun_export = TRUE` in `all_args_here()`), the corrected function is instead written into the `<function name>_safer.R` file of `out_path`. `vignette("saferDev")` gives the recommended order of these checkers.
* `all_args_here()` completes the partial argument names (like `dig` for `digits`), and writes the added default values with the package of their calls and their arguments named (e.g., `base::log(x = 2, base = base::exp(x = 1))`), so that a second run changes nothing. An argument without default value, or whose default value cannot be copied, gets an empty value (like `envir = `), listed in the new "ARGUMENTS WITH EMPTY VALUE" message.
* The safer checks (`safer_check = TRUE`) take about 1 ms per call instead of 0.1 s.
* ggplot2 is moved from Imports to Suggests.
* All the functions follow the safer-r backbone v21.2 and pass their own seven code checks.

## Bug fixes

* All the functions work when called through `base::do.call()` with a function object, `base::lapply()`, `base::Map()` and similar functions, or a function forwarding its `...`, and restore `.libPaths()` exactly on exit. Their messages name `x` correctly when it is an expression (e.g., `l$f`) or a function object.
* `colons_check()` and `all_args_here()`:
  * They read strings, raw strings, backquoted names and comments as the R parser does: calls inside a string with an odd number of escaped quotes (e.g., `"a \" WORD (c)"`) are no longer reported by `colons_check()`, calls on the same line after a string containing the other quote (e.g., `"can't do it"`) are no longer missed, such strings and strings ending with an escaped backslash no longer stop `all_args_here()` with an internal error, and the `in` of `for` loops is no longer taken for a call.
  * A function with attributes, which R prints after its code, gets an error naming them, instead of the analysis of these printed lines as code.
  * A function without any function call gets a warning and `NULL`, instead of an error. So does a primitive function (e.g., `base::sum`), formerly analyzed as R code.
  * A call whose name is separated from its `(` by empty lines or comment lines is detected, instead of being skipped without notice.
  * They no longer advise `:::` before the names starting with a dot (`base:::.libPaths()` gives an R CMD check NOTE).
* `colons_check()` no longer reports, as calls missing their package:
  * a masked name as a basic R function (e.g., `filter()` with dplyr attached, now listed with the other functions and left unchanged), and the names after `@`;
  * the calls inside formulas, and the code quoted by `quote()`, `bquote()`, `expression()`, `alist()` and `substitute()` (also when piped into them with `|>`): with a prefix, `stats::offset(z)` is not taken as an offset by `stats::terms()`, so that another model is fitted without warning, `base::log(x)` renames the coefficients, and `base::paste()` breaks the plotmath labels. The code that R evaluates in these calls (`.()` in `bquote()`, `..()` with `splice = TRUE`, and the `where` and `env` arguments) is still checked;
  * the function called by `.Internal()` (`.Internal(base::toupper(x))` fails);
  * a name ending a line followed by a line starting with `(`, which is not a call (e.g., a variable `C`, taken for `stats::C()`);
  * the calls of the arguments of the checked function or of the functions defined in it, of its internal functions (2.0.0 skipped only those created by `name <- function(` or `name <- utils::getFromNamespace(` at the start of a line, and not named like a basic R function), and its recursive calls, for which the advised prefix can silently call another function (e.g., `base::transform(x)` for an argument named `transform`). Each name is skipped only inside the function that defines it (e.g., an argument `c` of an inner function does not hide the `c()` calls outside it).
* `all_args_here()`:
  * Flags the proposals that could change what the code does: they are not inserted into the returned function, and are listed with the reason under "CORRECTIONS NOT APPLIED". This concerns the calls of functions using `nargs()` (e.g., `base::diag()`), assignment targets (e.g., `base::rownames(x) <- v`), builtin primitives that would get an empty argument, calls on the right side of a pipe (`|>`, and the pipes of magrittr), calls forwarding `...`, and calls of S3 generics whose methods name or match the arguments differently (e.g., `stats::t.test(y ~ g, data = d)`, or `y = ` added to `base::plot(x = t)`).
  * A default value that gives another result in the checked function is no longer proposed, and the argument gets an empty value: arguments tested with `missing()` (e.g., `nrow` of `base::matrix()`), and default values using an object that the checked function does not find, or finds differently.
  * The calls inside formulas, in the code quoted by `quote()`, `bquote()`, `expression()`, `alist()` and `substitute()` (also when piped into them with `|>`), and the function called by `.Internal()` are not analyzed (`"SKIPPED"`): they stopped it with an error (e.g., "NOT ASSOCIATED TO ITS PACKAGE") or, written with their package, got proposals that would change the result (e.g., rename the model coefficients). The code that R evaluates in these calls is analyzed.
  * The calls, without package, of the arguments of the checked function or of the functions defined in it, of its internal functions (formerly only those created by `name <- function(` or `name <- utils::getFromNamespace(` at the start of a line, but in the whole checked function), and its recursive calls get `INTERNAL_FUNCTION` inside the function that defines them, instead of the error "NOT ASSOCIATED TO ITS PACKAGE".
  * Values written without name, and string default values after `...`, are written correctly (e.g., `graphics::title(main = "Main", ...)`, `base::isS4(x = x)`, `sep = " "` in `base::paste()`), and commas inside `[ ]`, `[[ ]]` or `{ }`, values compared with `==`, long default values and the calls of functions named like the end of a preceding `if`, `for`, `while`, `function` or `x$name()` (e.g., `f(x)` in `if(f(x))`, formerly read as `f(f(x))`) no longer give wrong proposals or errors.
  * No longer stops with an error on re-exported functions (e.g., `graphics::plot()`), calls with more arguments than their function, calls receiving from a pipe an argument without default value (e.g., `x |> base::sort()`), empty arguments written without space (e.g., `base::matrix(, n, 0L)`), names after `@` (e.g., `x@mean(1)`), a line ending with a name followed by a line starting with `(`, or functions whose arguments all have `NA` as default value (e.g., `base::Sys.umask()`, now not analyzed, with a warning). Its internal errors no longer write a file into the working directory.
* `arg_check()`:
  * With `options`, data of type double with attributes (e.g., a matrix) or `NA` values is compared on its values, instead of being rejected as "NOT EVEN TYPE CHARACTER OR INTEGER, OR TYPE DOUBLE WITH A 0 MODULO", and `options` of type double with attributes or `NA` no longer raise a false error about decimal values.
  * With `options`, approximate integer values are rounded to the nearest integer (e.g., `(0.1 - 0.3) * 10` was accepted by `options = c(-1, 0)`, and `(0.3 - 0.1) * 10` rejected by `options = c(1, 2)`), and integer values outside the integer range are no longer turned into `NA` and wrongly accepted (e.g., `data = 4e9` with `options = c(1, 3e9)`). Very large values (e.g., `1e20`) no longer raise an R warning.
  * Dates, date-times and time differences return a problem instead of an R error when tested with `options`, or with `class` or `typeof` `"integer"` and `double_as_integer_allowed = TRUE`; as `options`, they raise a clear error.
  * `$object_name` and `$text` are single strings when the expression passed to `data` is longer than 60 characters.
* `env_check()` called through `base::source()`, `base::eval()` or knitr no longer reports all the objects of the checked environment as also present in this same environment, and names the global environment `.GlobalEnv` (formerly `UNNAMED`). A `pos` of 0, or beyond the last environment, returns a clear error.
* `get_message()` evaluates the instruction once (instead of 3 times with `kind = "warning"` or `"message"`), no longer reports an error for an instruction returning a `try-error` object, catches the messages under knitr, testthat or `suppressMessages()`, accepts empty environments in `env`, keeps the current graphic device and its layout, and closes the devices left open by the instruction, instead of stopping with an internal error.
* `is_function_here()` accepts re-exported functions (e.g., `"graphics::plot"`), operators and other non-syntactic names (e.g., `"base::+"`, `"base::names<-"`).
* `report()` writes exactly `sep` empty lines after any object, also after a string ending with a line break (2.0.0 wrote more after many objects, e.g., 3 with `sep = 1` after `"a\n"`).
* Typos in error messages are corrected: `DOES NOT EXIST` in `all_args_here()` and `CONTROLLED` in `arg_check()`.

## Documentation

* Help pages are corrected (e.g., the `lib_path` folders are searched after the current `.libPaths()`, not before), with runnable examples instead of `\dontrun{}`.
* README, `vignette("saferDev")` and the articles are corrected and completed.

# saferDev 2.0.0

* arg_check(): new behavior for the class, typeof and mode arguments. Values are now compared exactly (same values, same order) to class(data), typeof(data) and mode(data). Thus, class = "matrix" alone always returns a problem since R 4.0.0 (use class = c("matrix", "array")), class = "factor" alone always returns a problem for ordered factors (use class = c("ordered", "factor")), and class = "vector" is no longer a shortcut for numeric, integer, character, logical, complex or expression classes. In addition, double_as_integer_allowed = TRUE now also rescues class = "integer" when the tested object is a double with zero as modulo.
* All the safer-r functions now check that the ... argument is not used, and that the reserved lib_path, safer_check and error_text arguments are present.
* R (>= 4.5.0) is now required.

# saferDev 1.0.0

* Initial CRAN submission.
