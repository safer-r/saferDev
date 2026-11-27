## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Ubuntu 26.04 (x86_64), R 4.6.2: `R CMD check --as-cran --no-manual`, with the CRAN incoming feasibility checks and internet access: Status OK, 4,931 tests passed.
* Ubuntu 24.04 (x86_64), R 4.5.2 (R 4.5 is the oldest series allowed by `Depends: R (>= 4.5.0)`): `R CMD check --no-manual`: Status OK, 4,931 tests passed.
* TO COMPLETE BEFORE SUBMISSION: win-builder (R-devel and R-release), which also checks the PDF manual.
* TO COMPLETE BEFORE SUBMISSION: mac-builder.

## Submission shortly after saferDev 2.0.0

* This is an update (saferDev 3.0.0). saferDev 2.0.0 was published on CRAN on 2026-10-14.
* We are aware that this update comes soon after 2.0.0. We submit it now because:
  * it fixes bugs that could silently change what the checked code does, or give false results: package prefixes advised by `colons_check()` inside model formulas or plotmath labels, arguments proposed by `all_args_here()` for calls whose methods name them differently, and values of `arg_check()` wrongly rejected (e.g., `matrix(c(1, 2))` with `options = c(1, 2)`) or accepted (e.g., `4e9` with `options = c(1, 3e9)`);
  * it changes the user interface (breaking changes, listed in NEWS.md): `all_args_here()` and `colons_check()` now return the corrected function, the `export` argument of `all_args_here()` is renamed `table_export`, new arguments are inserted before `safer_check` in `colons_check()` (calls using argument positions are shifted), the `object.name` output component of `arg_check()` is renamed `object_name`, and `arg_check(prop = TRUE)` now accepts values between -1 and 1 by default. We prefer to release these changes before users start depending on the 2.0.0 interface;
  * it adds five checking functions (`curly_bracket_check()`, `for_loop_nb_check()`, `lambda_check()`, `super_assign_check()`, `true_false_check()`).

## Reverse dependencies

There are currently no reverse dependencies of saferDev on CRAN.
