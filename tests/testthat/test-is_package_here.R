test_that("test if the package is in the computer", {
    req.pkg <- "ggplot2"
    path <- "blablabla"
    expect_error(object = is_package_here(
        req.package = req.pkg,
        lib.path = path
    ), regexp = NULL)
})
