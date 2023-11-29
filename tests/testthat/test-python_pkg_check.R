test_that("test if the python package is in the computer", {
    lib.path <- "."
    
    expect_error(object = python_pkg_check(
        req.package = "serpentine", 
        python.exec.path = ".", 
        python.lib.path = ".",
        lib.path = lib.path
    ), regexp = NULL)
})
