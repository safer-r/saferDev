test_that("test if the function is in the package", {
    f <- "ggplot2::geom_point"
    path <- "blablabla"
    expect_error(object = is_function_here(
        fun = f,
        lib.path = path
    ), regexp = NULL)
})
