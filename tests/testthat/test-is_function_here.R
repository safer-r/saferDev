testthat::test_that("is_function_here()", {

    ## data argument values
    str1 <- "ggplot2::geom_point"
    str2 <- "NOTGOOD"
    str3 <- "ggplot2::geom_point()"
    str4 <- "BAD::NOTGOOD"
    str5 <- "ggplot2::NOTGOOD"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(is_function_here(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::is_function_here()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(is_function_here()) 
    fun <- str1
    testthat::expect_error(is_function_here()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    rm(
        fun
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(is_function_here(fun = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(is_function_here(fun = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(is_function_here(fun = str1, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(is_function_here(fun = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(is_function_here(fun = str1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = fun1, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # fun
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end fun
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(is_function_here(fun = "", safer_check = TRUE, lib_path = NULL, error_text = "")) 
    testthat::expect_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(is_function_here(fun = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) 
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
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    testthat::expect_error(is_function_here(fun = str2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(is_function_here(fun = str5, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # below cannot be used because difficult to determine .libPath() in CI
    # result <- saferDev::get_message('is_function_here(fun = str2, safer_check = TRUE, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    # expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::is_function_here().\n\nREQUIRED PACKAGE:\nNOTGOOD\n\nMUST BE INSTALLED IN:\nC:/Program Files/R/R-4.4.2/library\n\n================\n\n\n"
    # testthat::expect_equal(result, expected)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})
