test_that("is_package_here()", {
    req_pkg <- "ggplot2"
    path <- "blablabla"
    
    expect_error(object = is_package_here(
    req_package = "wrongname",
    lib_path = "."
    ), regexp = NULL)
    
    expect_error(object = is_package_here(
        req_package = req_pkg,
        lib_path = path
    ), regexp = NULL)

    expect_no_error(is_package_here(
        req_package = "utils",
        lib_path = NULL,
        safer_check = TRUE
    ))

    expect_no_error(is_package_here(
        req_package = "graphics",
        lib_path = NULL,
        safer_check = TRUE
    ))
})
