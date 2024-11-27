testthat::test_that("is_function_here()", {
    f <- "ggplot2::geom_point"
    path <- "blablabla"
    f2 <- "base::sum"
    f3 <- "graphics::par"
    f4 <- "stats::mean"
    lib_path1 <- 1


  # Simple examples
    result1 <- saferDev::get_message("is_function_here(fun = base::sum)", kind = "error", print.no = TRUE, text = NULL)
    # warning LINE 13 can be LINE 22
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_function_here()\nTHE fun OBJECT MUST BE CLASS vector AND MODE character\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    result2 <- saferDev::get_message("is_function_here(x = 'mean')", kind = "error", print.no = TRUE, text = NULL)
    expected2 <- "ERROR MESSAGE REPORTED:\nIn is_function_here(x = \"mean\") : unused argument (x = \"mean\")\n"
    testthat::expect_equal(result2, expected2)

    result3 <- saferDev::get_message("is_function_here(fun = 'base::mean')", kind = "error", print.no = TRUE, text = NULL)
    expected3 <- "NO ERROR MESSAGE REPORTED"
    testthat::expect_equal(result3, expected3)

    result4 <- saferDev::get_message("is_function_here(fun = 'base::mean', lib_path = 'base')", kind = "error", print.no = TRUE, text = NULL)
    expected4 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_function_here() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\nbase\n\n================\n\n\n"

    result5 <- saferDev::get_message("is_function_here(fun = 'base::mean', lib_path = NULL, safer_check = FALSE)", kind = "message", print.no = TRUE, text = NULL)
    expected5 <- "NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED"
    testthat::expect_equal(result5, expected5)

    result6 <- saferDev::get_message("is_function_here(fun = 'base::mean', lib_path = lib_path1, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected6 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_function_here() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n1\n\n================\n\n\n"
    testthat::expect_equal(result6, expected6)

    result7 <- saferDev::get_message("is_function_here(fun = 'base::mean', lib_path = NULL, safer_check = 1)", kind = "error", print.no = TRUE, text = NULL)
    expected7 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_function_here() OF THE saferDev PACKAGE\nsafer_check ARGUMENT MUST BE EITHER TRUE OR FALSE. HER IT IS:\n1\n\n================\n\n\n"
    testthat::expect_equal(result7, expected7)

    result8 <- saferDev::get_message("is_function_here(lib_path = NULL, safer_check = TRUE)", kind = "error", print.no = TRUE, text = NULL)
    expected8 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_function_here() OF THE saferDev PACKAGE\nFOLLOWING ARGUMENT HAS NO DEFAULT VALUE AND REQUIRE ONE:\nfun\n\n================\n\n\n"
    testthat::expect_equal(result8, expected8)

    result9 <- saferDev::get_message("is_function_here(fun = 'mean', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected9 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_function_here() OF THE saferDev PACKAGE\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\" OR \":::.\":\nmean\n\n================\n\n\n"
    testthat::expect_equal(result9, expected9)
    
    # do not use safer_check = TRUE because test_that() in CI does not like the package presence checking
    testthat::expect_error(saferDev::is_function_here(fun = "a", safer_check = FALSE))
    testthat::expect_error(saferDev::is_function_here(fun = f2, lib_path = "a", safer_check = FALSE))
    testthat::expect_error(saferDev::is_function_here(fun = f2, lib_path = 2, safer_check = FALSE))
    testthat::expect_error(saferDev::is_function_here(fun = f2, safer_check = "a", safer_check = FALSE))
    testthat::expect_no_error(saferDev::is_function_here(fun = f4, safer_check = FALSE))
    testthat::expect_no_error(saferDev::is_function_here(fun = f3, safer_check = FALSE))
    testthat::expect_error(saferDev::is_function_here(fun = 1, lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(saferDev::is_function_here(fun = f2, lib_path = null, safer_check = NULL))


  # sophisticated example

    testthat::expect_error(object = saferDev::is_function_here(
    fun = "wrongFct",
    lib_path = ".", # should be the problem
    safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ), regexp = NULL)
    
    testthat::expect_error(object = saferDev::is_function_here(
        fun = f, # should be the problem
        lib_path = path, 
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ), regexp = NULL)

    testthat::expect_no_error(saferDev::is_function_here(
        fun = f2,
        lib_path = NULL,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))
})