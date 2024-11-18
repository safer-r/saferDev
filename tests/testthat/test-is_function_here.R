test_that("is_function_here()", {
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

    testthat::expect_error(is_function_here(fun = "a"))
    testthat::expect_error(is_function_here(fun = "f2", lib_path = "a"))
    testthat::expect_error(is_function_here(fun = "f2", safer_check = "a"))

  # sophisticated example

    expect_error(object = is_function_here(
    fun = "wrongFct",
    lib_path = "." # should be the problem
    ), regexp = NULL)
    
    expect_error(object = is_function_here(
        fun = f, # should be the problem
        lib_path = path # should be the problem
    ), regexp = NULL)

    expect_no_error(is_function_here(
        fun = f2,
        lib_path = NULL,
        safer_check = TRUE
    ))
})