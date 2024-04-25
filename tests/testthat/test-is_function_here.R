test_that("test if the function is in the package", {
    f <- "ggplot2::geom_point"
    path <- "blablabla"
    
    expect_error(object = is_function_here(
    fun = "wrongFct",
    lib.path = "."
    ), regexp = NULL)
    
    expect_error(object = is_function_here(
        fun = f,
        lib.path = path
    ), regexp = NULL)

    expect_no_error(is_package_here(
        fun = "grid::gpar",
        lib.path = ".",
        safer_check = TRUE
    ))
})
