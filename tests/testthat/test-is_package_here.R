testthat::test_that("is_package_here()", {

    ## data argument values
    str1 <- "stats" # base R package: no dependency on a suggested package
    str2 <- "NOTGOOD"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(is_package_here(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::is_package_here()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(is_package_here()) 
    req_package <- str1
    testthat::expect_error(is_package_here())
    rm(
        req_package
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(is_package_here(req_package = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(is_package_here(req_package = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(is_package_here(req_package = str1, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(is_package_here(req_package = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(is_package_here(req_package = str1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # req_package
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_package_here(req_package = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end req_package
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(is_package_here(req_package = "", safer_check = TRUE, lib_path = NULL, error_text = "")) 
    testthat::expect_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(is_package_here(req_package = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) 
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    result <- saferDev::get_message('is_package_here(req_package = "ggplot::geom_point", safer_check = TRUE, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_package_here().\n\nTHE STRING IN req_package ARGUMENT MUST NOT CONTAIN \"::\" OR \":::\":\nggplot::geom_point\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    testthat::expect_error(is_package_here(req_package = str2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # missing package, with an existing lib_path folder (added after the current .libPaths(), which keep priority)
    testthat::expect_error(object = saferDev::is_package_here(req_package = str2, safer_check = FALSE, lib_path = base::tempdir(check = FALSE), error_text = ""), regexp = "REQUIRED PACKAGE:\nNOTGOOD\n\nMUST BE INSTALLED IN", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::is_package_here(req_package = base::c(str2, "NOTGOOD2"), safer_check = FALSE, lib_path = base::tempdir(check = FALSE), error_text = ""), regexp = "REQUIRED PACKAGES:\nNOTGOOD\nNOTGOOD2\n\nMUST BE INSTALLED IN", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # lib_path folder that does not exist (path built from tempdir(), not machine dependent)
    testthat::expect_error(object = saferDev::is_package_here(req_package = str2, safer_check = FALSE, lib_path = base::file.path(base::tempdir(check = FALSE), "NOTGOOD_FOLDER", fsep = "/"), error_text = ""), regexp = "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # an existing lib_path folder without packages is added to .libPaths(): the packages of the current .libPaths() are still found, and .libPaths() is restored on exit
    ini_lib_path <- base::.libPaths(new = , include.site = TRUE)
    testthat::expect_no_error(object = saferDev::is_package_here(req_package = str1, safer_check = FALSE, lib_path = base::tempdir(check = FALSE), error_text = ""), message = NULL, class = NULL)
    testthat::expect_identical(object = base::.libPaths(new = , include.site = TRUE), expected = ini_lib_path, info = NULL, label = NULL, expected.label = NULL)
    # returned value: invisible NULL
    result <- base::withVisible(x = saferDev::is_package_here(req_package = base::c("stats", "utils"), safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_null(object = result$value, info = NULL, label = NULL)
    testthat::expect_false(object = result$visible, info = NULL, label = NULL)
    # examples of the help page
    testthat::expect_error(object = saferDev::is_package_here(req_package = "nopackage", safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1"), regexp = "ERROR IN saferDev::is_package_here() INSIDE P1::F1", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::is_package_here(req_package = "stats", safer_check = TRUE, lib_path = "NOTGOOD", error_text = ""), regexp = "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:\nNOTGOOD", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # below cannot be used because difficult to determine .libPath() in CI
    # result <- saferDev::get_message('is_package_here(req_package = str2, safer_check = TRUE, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    # expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_package_here().\n\nREQUIRED PACKAGE:\nNOTGOOD\n\nMUST BE INSTALLED IN:\nC:/Program Files/R/R-4.4.2/library\n\n================\n\n\n"
    # testthat::expect_equal(result, expected)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})
