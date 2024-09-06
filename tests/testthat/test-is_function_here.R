test_that("is_function_here()", {
    f <- "ggplot2::geom_point"
    path <- "blablabla"
    f2 <- "base::sum"
    f3 <- "graphics::par"
    
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
        lib.path = NULL,
        safer_check = TRUE
    ))
})
