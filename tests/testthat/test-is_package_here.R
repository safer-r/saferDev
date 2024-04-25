test_that("test if the package is in the computer", {
    req.pkg <- "ggplot2"
    path <- "blablabla"
    
    expect_error(object = is_package_here(
    req.package = "wrongname",
    lib.path = "."
    ), regexp = NULL)
    
    expect_error(object = is_package_here(
        req.package = req.pkg,
        lib.path = path
    ), regexp = NULL)

    expect_no_error(is_package_here(
        req.package = "utils",
        lib.path = ".",
        safer_check = TRUE
    ))
})
