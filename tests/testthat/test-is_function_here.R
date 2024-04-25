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
})

# Test to check if function f exists and runs without errors
test_that("Function f runs without errors", {
  f2 <- "ggplot2::geom_line"

  expect_no_error(is_function_here(fun = f2, lib.path = NULL, safer_check = TRUE))
})