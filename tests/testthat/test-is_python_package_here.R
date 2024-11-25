test_that("is_python_package_here()", {
    lib_path <- "."
    path <- "blablabla"
    req_pkg <- "serpentine"
    req_pkg2 <- "not_a_real_package"

    # some simple tests
    result1 <- saferDev::get_message("is_python_package_here(req_package = 'serpentine', lib_path = 'blablabla')", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_python_package_here() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\nblablabla\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    
    testthat::expect_error(object = is_python_package_here(
    req_package = req_pkg, 
    lib_path = path
    ), regexp = NULL)

    testthat::expect_error(object = is_python_package_here(
        req_package = req_pkg2,
        lib_path = path
    ), regexp = NULL)
    
    testthat::expect_error(object = is_python_package_here(
        req_package = "serpentine", 
        python_exec_path = ".", 
        python_lib_path = ".", 
        lib_path = lib_path, 
        safer_check = TRUE
    ), regexp = NULL)

    testthat::expect_error(object = is_python_package_here(
        req_package = "serpentine", 
        python_exec_path = ".", 
        python_lib_path = ".", 
        lib_path = path, 
        safer_check = TRUE
    ), regexp = NULL)

    testthat::expect_error(object = is_python_package_here(
        req_package = req_pkg,
        python_exec_path = ".",
        python_lib_path = ".",
        lib_path = path
    ), regexp = NULL)

    testthat::expect_error(object = is_python_package_here(
        req_package = req_pkg2, 
        python_exec_path = ".", 
        python_lib_path = ".", 
        lib_path = lib_path, 
        safer_check = TRUE
    ), regexp = NULL)

    testthat::expect_error(
        is_python_package_here(
        req_package = "numpy", 
        python_exec_path = "/invalid/path/to/python",
        lib_path = "some/path"
    ), regexp = NULL)


})
