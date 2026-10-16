# Changelog

## saferDev 2.1.0

- curl_bracket_check(): new function. Verifies that all the bodies of
  the if, else, for, while, repeat and function (including lambda (x))
  constructs of a function are delimited by curly brackets {}, according
  to the safer-r recommendation.

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
