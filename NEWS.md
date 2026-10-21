
# saferDev 3.0.0

* curly_bracket_check(): new function. Verifies that all the bodies of the if, else, for, while, repeat and function are delimited by curly brackets {}. The corrected function is returned, or exported with export = TRUE.
* for_loop_nb_check(): new function. Verifies that the sequence expression of every for loop is written with hard numbers (like 1:3) or with a single seq_len() or seq_along() call. Otherwise (like 1:length(x)), a checking line is added before the loop, issuing a warning at execution if the sequence is empty, contains NA or infinite values, has a single value, or is decreasing. The corrected function is returned, or exported with export = TRUE.
* true_false_check(): new function. Verifies that TRUE and FALSE are always written in full when used as values, and never abbreviated as T or F. The corrected function (T replaced by TRUE and F by FALSE) is returned, or exported with export = TRUE.
* colons_check(): the missing package attributions of the basic R functions are now added: the corrected function is returned, or exported with the new export and out_path arguments. These two arguments are placed before safer_check, lib_path and error_text (breaking change for calls using argument positions).
* all_args_here(): the missing arguments are now added: the corrected function is returned, or exported with the new fun_export argument. The former export argument is renamed table_export (breaking change).
* curly_bracket_check(), for_loop_nb_check(), true_false_check(), colons_check() and all_args_here(): the exported file is named <function name>_safer.R and written in out_path (default ".", the R working directory). It contains <function name>_safer <- function(...) ..., so that base::source() recreates the corrected function. out_path cannot be NULL nor an empty string. An error is returned if the file already exists, or if the tested function is not designated by its name. A "MODIFIED CODE EXPORTED IN" message gives the file path, and NULL is returned (invisibly).
* arg_check(): output component object.name renamed object_name (breaking change).
* arg_check(), env_check(), get_message(), all_args_here() and the other checking functions: dotted internal and local variable names renamed with underscores (backbone v21.0).
* colons_check() and all_args_here(): the "in" symbol is now skipped during code parsing, which prevents spurious parsing problems with for(j in (n - 1L):2L) writings.
* all_args_here(): an incoherent arg_full_names argument no longer causes a raw crash, but a proper internal error.

# saferDev 2.0.0

* arg_check(): new behavior for the class, typeof and mode arguments. Values are now compared exactly (same values, same order) to class(data), typeof(data) and mode(data). Thus, class = "matrix" alone always returns a problem since R 4.0.0 (use class = c("matrix", "array")), class = "factor" alone always returns a problem for ordered factors (use class = c("ordered", "factor")), and class = "vector" is no longer a shortcut for numeric, integer, character, logical, complex or expression classes. In addition, double_as_integer_allowed = TRUE now also rescues class = "integer" when the tested object is a double with zero as modulo.
* All the safer-r functions now check that the ... argument is not used, and that the reserved lib_path, safer_check and error_text arguments are present.
* R (>= 4.5.0) is now required.

# saferDev 1.0.0

* Initial CRAN submission.
