testthat::test_that(".in_parenthesis_replacement()", {

    ## data argument values
    str1 <- "pattern = base::paste0(pattern, \"\\\\(#\"), text = text"
    str2 <- "pattern = base::paste0(pattern  \"\\\\(#\"), text = text"
    pattern1 <- ","
    open_pos1 <- 23
    close_pos1 <- 39
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.in_parenthesis_replacement(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.in_parenthesis_replacement()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.in_parenthesis_replacement()) 
    string <- str1
    pattern <- pattern1
    no_regex_pattern <- pattern1
    replacement <- " "
    perl <- TRUE
    open_pos <- open_pos1
    close_pos <- close_pos1
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.in_parenthesis_replacement()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.in_parenthesis_replacement(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        string, 
        pattern, 
        no_regex_pattern, 
        replacement, 
        perl, 
        open_pos, 
        close_pos, 
        lib_path, 
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.in_parenthesis_replacement(string = NULL, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = NULL, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = NULL, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = NULL, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = NULL, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = NULL, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.in_parenthesis_replacement(string = character(), pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = integer(), pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = double(), pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = logical(), pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = complex(), pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = data.frame(), pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = list(), pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = character(), no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = integer(), no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = double(), no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = logical(), no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = complex(), no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = data.frame(), no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = list(), no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = character(), replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = integer(), replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = double(), replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = logical(), replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = complex(), replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = data.frame(), replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = list(), replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = character(), perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = integer(), perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = double(), perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = logical(), perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = complex(), perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = data.frame(), perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = list(), perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = character(), open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = integer(), open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = double(), open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = logical(), open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = complex(), open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = data.frame(), open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = list(), open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = character(), close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = integer(), close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = double(), close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = logical(), close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = complex(), close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = data.frame(), close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = list(), close_pos = close_pos1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.in_parenthesis_replacement(string = NA, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = NA, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = NA, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = NA, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = NA, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = NA, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NA, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = NA))
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
    # string
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = NULL, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = NA, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = 1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = c(TRUE, FALSE), pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = mat1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = factor1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = expr1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = fun1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    # end string
    # pattern
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = NULL, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = NA, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = 1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = c(TRUE, FALSE), no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = mat1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = factor1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = expr1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = fun1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    # end pattern
    # no_regex_pattern
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = NULL, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = NA, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = 1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = c(TRUE, FALSE), replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = mat1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = factor1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = expr1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = fun1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = str1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    # end no_regex_pattern
    # replacement
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = NULL, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = NA, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = 1, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = c(TRUE, FALSE), perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = mat1, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = factor1, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = expr1, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = fun1, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = str1, perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    # end replacement
    # perl
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = NULL, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = NA, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = 1, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = c(TRUE, FALSE), open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = mat1, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = factor1, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = expr1, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = fun1, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = str1, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    # end perl
    # open_pos
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = NULL, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = NA, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = 1:2, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = c(TRUE, FALSE), close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = mat1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = factor1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = expr1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = fun1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = str1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    # end open_pos
    # close_pos
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = 1:2, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = str1, lib_path = NULL, error_text = ""))
    # end close_pos
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(.in_parenthesis_replacement(string = "", pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = "")) 
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = "", no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = "", replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = "", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    # testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = "", error_text = "")) # cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    testthat::expect_no_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = "")) 
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
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = 0, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = -1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = 0, close_pos = close_pos1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.in_parenthesis_replacement().\n\nARGUMENT open_pos DOES NOT REFER TO A POSITION OF OPENING PARENTHESIS.\nopen_pos:\n0\nstring:\npattern = base::paste0(pattern, \"\\\\(#\"), text = text\nsubstr(string, open_pos, open_pos):\n\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = -1, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = 0, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.in_parenthesis_replacement().\n\nARGUMENT close_pos DOES NOT REFER TO A POSITION OF CLOSING PARENTHESIS.\nclose_pos:\n0\nstring:\npattern = base::paste0(pattern, \"\\\\(#\"), text = text\nsubstr(string, close_pos, close_pos):\n\n\n================\n\n\n"
    testthat::expect_equal(result, expected)

    testthat::expect_error(.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = "o", replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('.in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = "o", replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.in_parenthesis_replacement().\n\nARGUMENT no_regex_pattern NOT CORRECTLY DETECTED.\nno_regex_pattern: \"o\"\nREPLACED CHARACTERS IN string ARGUMENT:\n,\n\n================\n\n\n"
    testthat::expect_equal(result, expected)

    result <- .in_parenthesis_replacement(string = str1, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = "")
    expect <- list(string = "pattern = base::paste0(pattern  \"\\\\(#\"), text = text", pos = 31)
    testthat::expect_equal(result, expect)
    result <- .in_parenthesis_replacement(string = str2, pattern = pattern1, no_regex_pattern = pattern1, replacement = " ", perl = TRUE, open_pos = open_pos1, close_pos = close_pos1, lib_path = NULL, error_text = "")
    expect <- list(string = "pattern = base::paste0(pattern  \"\\\\(#\"), text = text", pos = NULL)
    testthat::expect_equal(result, expect)

    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})

