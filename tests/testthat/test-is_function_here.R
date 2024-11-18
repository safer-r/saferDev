testthat::test_that("is_function_here()", {
    f <- "ggplot2::geom_point"
    path <- "blablabla"
    f2 <- "base::sum"
    f3 <- "graphics::par"


  # Simple examples
    result1 <- saferDev::get_message("is_function_here(fun = base::sum)", kind = "error", print.no = TRUE, text = NULL)
    # warning LINE 13 can be LINE 22
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_function_here()\nTHE fun OBJECT MUST BE CLASS vector AND MODE character\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    result2 <- saferDev::get_message("is_function_here(x = 'mean')", kind = "error", print.no = TRUE, text = NULL)
    expected2 <- "ERROR MESSAGE REPORTED:\nIn is_function_here(x = \"mean\") : unused argument (x = \"mean\")\n"
    testthat::expect_equal(result2, expected2)
    # do not use safer_check = TRUE because test_that() in CI does not like the package presence checking
    testthat::expect_error(saferDev::is_function_here(fun = "a", safer_check = FALSE))
    testthat::expect_error(saferDev::is_function_here(fun = "f2", lib_path = "a", safer_check = FALSE))
    testthat::expect_error(saferDev::is_function_here(fun = "f2", safer_check = "a", safer_check = FALSE))

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