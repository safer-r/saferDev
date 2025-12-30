testthat::test_that(".extract_all_fun_names()", {

    ## data argument values
    str1 <- 'This is a test string with sum()'
    pattern1 <- "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\("
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.extract_all_fun_names(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.extract_all_fun_names()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.extract_all_fun_names()) 
    text <- str1
    pattern <- pattern1
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.extract_all_fun_names()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.extract_all_fun_names(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        text,
        pattern,
        lib_path,
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.extract_all_fun_names(text = NULL, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.extract_all_fun_names(text = character(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = integer(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = double(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = logical(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = complex(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = data.frame(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = list(), pattern = pattern1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.extract_all_fun_names(text = NA, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NA, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## check of lib_path
    # already done in the main function
    ######## end check of lib_path

    ######## safer_check argument checking
    # not required because not here
    ######## end safer_check argument checking

    ######## check of the required functions from the required packages
    # not required
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    # already done in the main function
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # text
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = NULL, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = NA, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = 1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = c(TRUE, FALSE), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = mat1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = factor1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = expr1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = fun1, pattern = pattern1, lib_path = NULL, error_text = ""))
    # end text
    # pattern
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = str1, lib_path = NULL, error_text = ""))
    # end pattern
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function

    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_no_error(.extract_all_fun_names(text = "", pattern = pattern1, lib_path = NULL, error_text = "")) 
    testthat::expect_error(.extract_all_fun_names(text = str1, pattern = "", lib_path = NULL, error_text = ""))
    # testthat::expect_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = "", error_text = "")) # cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    testthat::expect_no_error(.extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = "")) 
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
    result <- .extract_all_fun_names(text = str1, pattern = pattern1, lib_path = NULL, error_text = "")
    expect <- list(string = "sum", pos = 28)
    testthat::expect_equal(result, expect)

    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})

