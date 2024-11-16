test_that("is_function_here()", {
    f <- "ggplot2::geom_point"
    path <- "blablabla"
    f2 <- "base::sum"
    f3 <- "graphics::par"
    
    expect_error(object = is_function_here(
    fun = "wrongFct",
    lib_path = "."
    ), regexp = NULL)
    
    expect_error(object = is_function_here(
        fun = f,
        lib_path = path
    ), regexp = NULL)

    expect_no_error(is_function_here(
        fun = f2,
        lib_path = ".",
        safer_check = TRUE
    ))
})
