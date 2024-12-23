testthat::test_that("is_python_package_here()", {
    lib_path <- "."
    path <- "blablabla"
    req_pkg <- "serpentine"
    req_pkg2 <- "not_a_real_package"

    # some simple tests
    result1 <- saferDev::get_message("is_python_package_here(req_package = 'serpentine', lib_path = 'blablabla')", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_python_package_here() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\nblablabla\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    result2 <- saferDev::get_message("is_python_package_here(req_package = '', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected2 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_python_package_here() OF THE saferDev PACKAGE\nTHIS ARGUMENT\nreq_package\nCANNOT CONTAIN \"\"\n\n================\n\n\n"
    testthat::expect_equal(result2, expected2)

    result3 <- saferDev::get_message("is_python_package_here(req_package = serpentine, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected3 <- "ERROR MESSAGE REPORTED:\nIn base::eval(base::parse(text = data), envir = if (base::is.null(env)) { : \n  object 'serpentine' not found\n"
    testthat::expect_equal(result3, expected3)

    result4 <- saferDev::get_message("is_python_package_here( lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected4 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_python_package_here() OF THE saferDev PACKAGE\nFOLLOWING ARGUMENT HAS NO DEFAULT VALUE AND REQUIRE ONE:\nreq_package\n\n================\n\n\n"
    testthat::expect_equal(result4, expected4)

    result5 <- saferDev::get_message("is_python_package_here(req_package = 'serpentine', python_exec_path = 'blablabla', safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected5 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_python_package_here() OF THE saferDev PACKAGE\nFILE PATH INDICATED IN THE python_exec_path ARGUMENT DOES NOT EXISTS:\nblablabla\n\n================\n\n\n"
    testthat::expect_equal(result5, expected5)

    result6 <- saferDev::get_message("is_python_package_here(req_package = 'serpentine', python_lib_path = 'not_exist', safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE)
    expected6 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN is_python_package_here() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE python_lib_path ARGUMENT DOES NOT EXISTS:\nnot_exist\n\n================\n\n\n"
    testthat::expect_equal(result6, expected6)

      
    testthat::expect_error(object = saferDev::is_python_package_here(
        req_package = req_pkg, 
        lib_path = path
    ), regexp = NULL)

    testthat::expect_error(object = saferDev::is_python_package_here(
        req_package = req_pkg, 
        lib_path = 1, 
        safer_check = FALSE
    ), regexp = NULL)

    testthat::expect_error(object = saferDev::is_python_package_here(
        req_package = req_pkg, 
        lib_path = NULL, 
        safer_check = 1
    ), regexp = NULL)

    testthat::expect_error(object = saferDev::is_python_package_here(
        req_package = req_pkg2,
        lib_path = path
    ), regexp = NULL)
    
    testthat::expect_error(object = saferDev::is_python_package_here(
        req_package = "serpentine", 
        python_exec_path = ".", 
        python_lib_path = ".", 
        lib_path = lib_path, 
        safer_check = TRUE
    ), regexp = NULL)

    testthat::expect_error(object = saferDev::is_python_package_here(
        req_package = "serpentine", 
        python_exec_path = ".", 
        python_lib_path = ".", 
        lib_path = path, 
        safer_check = TRUE
    ), regexp = NULL)

    testthat::expect_error(object = saferDev::is_python_package_here(
        req_package = req_pkg,
        python_exec_path = ".",
        python_lib_path = ".",
        lib_path = path
    ), regexp = NULL)

    testthat::expect_error(object = saferDev::is_python_package_here(
        req_package = req_pkg2, 
        python_exec_path = ".", 
        python_lib_path = ".", 
        lib_path = lib_path, 
        safer_check = TRUE
    ), regexp = NULL)

    testthat::expect_error(
        saferDev::is_python_package_here(
        req_package = "numpy", 
        python_exec_path = "/invalid/path/to/python",
        lib_path = "some/path"
    ), regexp = NULL)


})
