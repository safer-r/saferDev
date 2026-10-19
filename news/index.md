# Changelog

## saferDev 3.0.0

- curl_bracket_check(): new function. Verifies that all the bodies of
  the if, else, for, while, repeat and function are delimited by curly
  brackets {}, according to the safer-r recommendation.
- for_loop_nb_check(): new function. Verifies that the sequence
  expression of every for construct of a function is written with hard
  numbers, i.e., only integer-valued numeric literals joined by : (like
  1:3, 0:0, -3:-1 or 1e3), or a seq_len(…) or seq_along(…) call
  occupying the whole sequence expression. Any other writing is
  reported, notably 1:length(x).
- true_false_check(): new function. Verifies that the TRUE and FALSE
  logical constants are always written in full when used as values
  (argument values, conditions, comparisons, indexing) and never
  abbreviated as T or F.
- arg_check(): output component object.name renamed object_name
  (breaking change).
- arg_check(), env_check(), get_message(), all_args_here() and the other
  checking functions: dotted internal and local variable names renamed
  with underscores (backbone v21.0).
- colons_check() and all_args_here(): the “in” symbol is now skipped
  during code parsing, which prevents spurious parsing problems with
  for(j in (n - 1L):2L) writings.
- all_args_here(): an incoherent arg_full_names argument no longer
  causes a raw crash, but a proper internal error (INTERNAL ERROR 9 when
  arg_full is non-empty and arg_full_names is NULL).

## saferDev 2.0.0

CRAN release: 2026-10-14

- arg_check(): new behavior for the class, typeof and mode arguments.
  Values are now compared exactly (same values, same order) to
  class(data), typeof(data) and mode(data). Thus, class = “matrix” alone
  always returns a problem since R 4.0.0 (use class = c(“matrix”,
  “array”)), class = “factor” alone always returns a problem for ordered
  factors (use class = c(“ordered”, “factor”)), and class = “vector” is
  no longer a shortcut for numeric, integer, character, logical, complex
  or expression classes. In addition, double_as_integer_allowed = TRUE
  now also rescues class = “integer” when the tested object is a double
  with zero as modulo.
- All the safer-r functions now check that the … argument is not used,
  and that the reserved lib_path, safer_check and error_text arguments
  are present.
- R (\>= 4.5.0) is now required.

## saferDev 1.0.0

CRAN release: 2026-06-08

- Initial CRAN submission.
