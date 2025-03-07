testthat::test_that("is_function_here()", {
    f <- "ggplot2::geom_point"
    path <- "blablabla"
    f2 <- "base::sum"
    f3 <- "graphics::par"
    f4 <- "ggplot2::mean"
    lib_path1 <- 1


  # Simple examples
    result1 <- get_message("is_function_here(fun = base::sum)", kind = "error", print.no = TRUE, text = NULL)
    # warning LINE 13 can be LINE 22
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR INSIDE saferDev::is_function_here().\n\nTHE fun ARGUMENT MUST BE CLASS vector AND MODE character\n\n================\n\n\n"

    testthat::expect_equal(result1, expected1)

    result2 <- get_message("is_function_here(x = 'mean')", kind = "error", print.no = TRUE, text = NULL)
    expected2 <- "ERROR MESSAGE REPORTED:\nIn is_function_here(x = \"mean\") : unused argument (x = \"mean\")\n"
    testthat::expect_equal(result2, expected2)

    result3 <- get_message("is_function_here(fun = 'base::mean')", kind = "error", print.no = TRUE, text = NULL)
    expected3 <- "NO ERROR MESSAGE REPORTED"
    testthat::expect_equal(result3, expected3)

    result4 <- get_message("is_function_here(fun = 'base::mean', lib_path = 'base')", kind = "error", print.no = TRUE, text = NULL)
    expected4 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_function_here() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\nbase\n\n================\n\n\n"

    result5 <- get_message("is_function_here(fun = 'base::mean', lib_path = NULL, safer_check = TRUE)", kind = "message", print.no = TRUE, text = NULL)
    expected5 <- "NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED"
    testthat::expect_equal(result5, expected5)

    result6 <- get_message("is_function_here(fun = 'base::mean', lib_path = lib_path1, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected6 <- "ERROR MESSAGE REPORTED:\nIn list.files(lib) : invalid 'path' argument\n"
    testthat::expect_equal(result6, expected6)

    result7 <- get_message("is_function_here(fun = 'base::mean', lib_path = NULL, safer_check = 1)", kind = "error", print.no = TRUE, text = NULL)
    expected7 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_function_here().\n\nTHE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n1\n\n================\n\n\n"
    testthat::expect_equal(result7, expected7)

    result8 <- get_message("is_function_here(lib_path = NULL, safer_check = TRUE)", kind = "error", print.no = TRUE, text = NULL)
    expected8 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_function_here().\n\nFOLLOWING ARGUMENT HAS NO DEFAULT VALUE AND REQUIRE ONE:\nfun\n\n================\n\n\n"
    testthat::expect_equal(result8, expected8)

    result9 <- get_message("is_function_here(fun = 'mean', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected9 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_function_here().\n\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\" OR \":::.\":\nmean\n\n================\n\n\n"
    testthat::expect_equal(result9, expected9)

    result10 <- get_message("is_function_here(fun = 'base::mean', lib_path = NULL, safer_check = 'FALSE')", kind = "error", print.no = TRUE, text = NULL)#safer_check = 'FALSE'
    expected10 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_function_here().\n\nTHE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\nFALSE\n\n================\n\n\n"
    testthat::expect_equal(result10, expected10)

    result11 <- get_message("is_function_here(fun = base::c('base::mean', NA), lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected11 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_function_here().\n\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\" OR \":::.\":\nNA\n\n================\n\n\n"

    testthat::expect_equal(result11, expected11)

    result12 <- get_message("is_function_here(fun = '', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected12 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_function_here().\n\nTHIS ARGUMENT\nfun\nCANNOT CONTAIN EMPTY STRING \"\".\n\n================\n\n\n"
    testthat::expect_equal(result12, expected12)

    
    
    # do not use safer_check = TRUE because test_that() in CI does not like the package presence checking
    testthat::expect_error(is_function_here(fun = "a", safer_check = FALSE))
    testthat::expect_error(is_function_here(fun = f2, lib_path = "a", safer_check = FALSE))
    testthat::expect_error(is_function_here(fun = f2, lib_path = 2, safer_check = FALSE))
    testthat::expect_error(is_function_here(fun = f2, safer_check = "a", safer_check = FALSE))
    testthat::expect_no_error(is_function_here(fun = f2, safer_check = FALSE))
    testthat::expect_no_error(is_function_here(fun = f3, safer_check = FALSE))
    testthat::expect_error(is_function_here(fun = 1, lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(is_function_here(fun = f2, lib_path = null, safer_check = NULL))


  # sophisticated example

    testthat::expect_error(object = is_function_here(
    fun = "wrongFct",
    lib_path = ".", # should be the problem
    safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ), regexp = NULL)
    
    testthat::expect_error(object = is_function_here(
        fun = f, # should be the problem
        lib_path = path, 
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ), regexp = NULL)

    testthat::expect_no_error(is_function_here(
        fun = f2,
        lib_path = NULL,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))
    testthat::expect_error(is_function_here(
        fun = f3,
        lib_path = NULL,
        safer_check = 'FALSE' # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))
})