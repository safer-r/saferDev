test_that("test if the python package is in the computer", {
    lib.path <- "."
    path <- "blablabla"
    req.pkg <- "serpentine"
    
    expect_error(object = is_python_package_here(
    req.package = req.pkg,
    lib.path = path
    ), regexp = NULL)
    
    expect_error(object = is_python_package_here(
        req.package = "serpentine", 
        python.exec.path = ".", 
        python.lib.path = ".",
        lib.path = lib.path
    ), regexp = NULL)
})
