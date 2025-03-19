testthat::test_that("is_package_here()", {
    req_pkg <- "ggplot2"
    path <- "blablabla"
    
    testthat::expect_error(object = is_package_here(
    req_package = "wrongname", # should be the problem
    lib_path = ".", # should be the problem
    safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ), regexp = NULL)
    
    testthat::expect_error(object = is_package_here(
        req_package = req_pkg,
        lib_path = path, # should be the problem
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ), regexp = NULL)

    testthat::expect_no_error(is_package_here(
        req_package = "utils",
        lib_path = NULL,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))

    testthat::expect_no_error(is_package_here(
        req_package = "graphics",
        lib_path = NULL,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))
    testthat::expect_error(is_package_here(
        req_package = "stats",
        lib_path = NULL,
        safer_check = 'FALSE' # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))

    testthat::expect_error(is_package_here(
        req_package = "stats",
        lib_path = not_a_path, # should be the problem
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))

    testthat::expect_error(is_package_here(
        req_package = ggplot2,
        lib_path = NULL,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))

    testthat::expect_error(is_package_here(
        req_package = list("ggplot2"),
        lib_path = NULL,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))


    result1 <- get_message("is_package_here(req_package = 'ggplot2', lib_path = 'not_a_path', safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_package_here().\n\nREQUIRED PACKAGE:\nggplot2\n\nMUST BE INSTALLED IN:\nnot_a_path\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    result2 <- get_message("is_package_here(lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected2 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_package_here().\n\nFOLLOWING ARGUMENT HAS NO DEFAULT VALUE AND REQUIRE ONE:\nreq_package\n\n================\n\n\n"
    testthat::expect_equal(result2, expected2)

    result3 <- get_message("is_package_here(req_package = list('ggplot2'), lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected3 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR INSIDE saferDev::is_package_here().\n\nTHE req_package ARGUMENT MUST BE CLASS vector AND MODE character\n\n================\n\n\n"

    testthat::expect_equal(result3, expected3)
    
})
