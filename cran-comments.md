## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Linux (Ubuntu 22.04, x86_64), R 4.6.1: `R CMD check --as-cran --no-manual`,
  3316 tests passed (run in three batches). The only NOTE ("unable to verify current time") came from the
  absence of internet access during this check.
* Windows 11 (x86_64), R 4.6.0: `R CMD check --as-cran` (with PDF manual and CRAN
  incoming feasibility checks, internet access available): Status OK, 3316 tests passed.
* TO COMPLETE BEFORE SUBMISSION: win-builder, R-devel and R-release.
* TO COMPLETE BEFORE SUBMISSION: GitHub Actions (ubuntu, macOS, windows).

## Submission shortly after saferDev 2.0.0

* This is an update (saferDev 3.0.0). saferDev 2.0.0 was published on CRAN on 2026-10-14.
* We are aware that this update comes soon after 2.0.0. We submit it now because:
  * it changes the user interface (breaking changes): the `export` argument of
    `all_args_here()` is renamed `table_export`, the order of the arguments of
    `colons_check()` changes, and the `object.name` output component of
    `arg_check()` is renamed `object_name`. We prefer to release these changes
    before users start depending on the 2.0.0 interface;
  * it adds four checking functions (`curly_bracket_check()`, `for_loop_nb_check()`,
    `lambda_check()`, `true_false_check()`), and `colons_check()` and
    `all_args_here()` now return the corrected function;
  * it fixes a bug of `all_args_here()` (strings ending with an escaped backslash).

## Reverse dependencies

There are currently no reverse dependencies of saferDev on CRAN.
