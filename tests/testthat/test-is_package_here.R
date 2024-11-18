testthat::test_that("is_package_here()", {
    req_pkg <- "ggplot2"
    path <- "blablabla"
    
    testthat::expect_error(object = saferDev::is_package_here(
    req_package = "wrongname", # should be the problem
    lib_path = ".", # should be the problem
    safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ), regexp = NULL)
    
    testthat::expect_error(object = saferDev::is_package_here(
        req_package = req_pkg,
        lib_path = path, # should be the problem
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ), regexp = NULL)

    testthat::expect_no_error(saferDev::is_package_here(
        req_package = "utils",
        lib_path = NULL,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))

    testthat::expect_no_error(saferDev::is_package_here(
        req_package = "graphics",
        lib_path = NULL,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))
})
