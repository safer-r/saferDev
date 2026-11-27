# Setup file, sourced by testthat before the tests (see ?testthat::test_dir).
# The tests run with safer_check = TRUE check the INSTALLED saferDev package: saferDev:::.pack_and_function_check() searches it in the library folders of base::.libPaths(), not in the source folder loaded by pkgload::load_all() or devtools::test().
# R CMD check and covr::package_coverage() install saferDev before running the tests. With devtools::test(), install it first (e.g., devtools::install()): otherwise, many tests fail with errors unrelated to what they test.
# base::system.file() without lib.loc is not used here: under pkgload::load_all(), it returns the source folder (path of the loaded namespace), even when saferDev is not installed.

if( ! base::any(base::file.exists(base::file.path(base::.libPaths(new = , include.site = TRUE), "saferDev", "DESCRIPTION", fsep = "/")), na.rm = TRUE)){
    base::stop("saferDev must be installed (e.g., devtools::install()) before running the tests: the safer_check = TRUE tests check the installed package", call. = FALSE, domain = NULL)
}
