# Changelog

## saferDev 3.0.0

- curl_bracket_check(): new function. Verifies that all the bodies of
  the if, else, for, while, repeat and function (including lambda (x))
  constructs of a function are delimited by curly brackets {}, according
  to the safer-r recommendation.
- for_loop_nb_check(): new function. Verifies that the sequence
  expression of every for construct of a function is written with hard
  numbers, i.e., only integer-valued numeric literals joined by : (like
  1:3, 0:0, -3:-1 or 1e3), or a seq_len(…) or seq_along(…) call
  occupying the whole sequence expression. Any other writing is
  reported, notably 1:length(x).

## saferDev 2.0.0

CRAN release: 2026-10-14

- arg_check(): new behavior for the class, typeof and mode arguments.
  Values are now compared exactly (same values, same order) to
  class(data), typeof(data) and mode(data).
- All the safer-r functions now check that the … argument is not used,
  and that the reserved lib_path, safer_check and error_text arguments
  are present.
- R (\>= 4.5.0) is now required.

## saferDev 1.0.0

CRAN release: 2026-06-08

- Initial CRAN submission.
