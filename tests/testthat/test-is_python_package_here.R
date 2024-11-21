test_that("is_python_package_here()", {
    lib_path <- "."
    path <- "blablabla"
    req_pkg <- "serpentine"
    
    testthat::expect_error(object = is_python_package_here(
    req_package = req_pkg, 
    lib_path = path
    ), regexp = NULL)
    
    testthat::expect_error(object = is_python_package_here(
        req_package = "serpentine", 
        python_exec_path = ".", 
        python_lib_path = ".", 
        lib_path = lib_path, 
        safer_check = TRUE
    ), regexp = NULL)
})
