test_that("test if the function is in the package", {
    f <- "ggplot2::geom_point"
    path <- "blablabla"
    f2 <- "base::sum"
    
    expect_error(object = is_function_here(
    fun = "wrongFct",
    lib.path = "."
    ), regexp = NULL)
    
    expect_error(object = is_function_here(
        fun = f,
        lib.path = path
    ), regexp = NULL)

    expect_no_error(is_function_here(
        fun = f2,
        lib.path = ".",
        safer_check = TRUE
    ))
})
