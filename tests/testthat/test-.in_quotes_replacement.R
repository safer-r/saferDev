testthat::test_that(".in_quotes_replacement()", {

    ## data argument values
    str1 <- 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B() \\)")' # \) will be also replaced
    str2 <- 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B(\") ")))' # \) will be also replaced
    str3 <- 'paste0("IAGE((", paste0(1:3, collapse = " "), "A)B() ")))' # \) will be also replaced

    pattern1 <- "\\)"
    no_regex_pattern1 <- ")"
    replacement1 <- " "
    pattern2 <- "\\("
    no_regex_pattern2 <- "("
    pattern3 <- " "
    no_regex_pattern3 <- " "
    replacement3 <- "_"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.in_quotes_replacement(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.in_quotes_replacement()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.in_quotes_replacement()) 
    string <- str1
    pattern <- pattern1
    no_regex_pattern <- no_regex_pattern1
    replacement <- replacement1
    perl <- TRUE
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.in_quotes_replacement()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.in_quotes_replacement(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        string,
        pattern,
        no_regex_pattern, 
        replacement, 
        perl,
        lib_path,
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.in_quotes_replacement(string = NULL, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = NULL, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = NULL, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = NULL, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.in_quotes_replacement(string = character(), pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = integer(), pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = double(), pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = logical(), pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = complex(), pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = data.frame(), pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = list(), pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = character(), no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = integer(), no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string =str1, pattern = double(), no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = logical(), no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = complex(), no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = data.frame(), no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = list(), no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = character(), replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = integer(), replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = double(), replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = logical(), replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = complex(), replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = data.frame(), replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = list(), replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = character(), perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = integer(), perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = double(), perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = logical(), perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = complex(), perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = data.frame(), perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = list(), perl = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.in_quotes_replacement(string = NA, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = NA, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = NA, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = NA, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = NA))
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
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = NULL, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = NA, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = 1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = c(TRUE, FALSE), pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = mat1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = factor1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = expr1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = fun1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    # end string
    # pattern
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = NULL, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = NA, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = 1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = c(TRUE, FALSE), no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = mat1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = factor1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = expr1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = fun1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = str1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    # end pattern
    # no_regex_pattern
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = NULL, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = NA, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = 1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = c(TRUE, FALSE), replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = mat1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = factor1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = expr1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = fun1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = str1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    # end no_regex_pattern
    # replacement
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = NULL, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = NA, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = 1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = c(TRUE, FALSE), perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = mat1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = factor1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = expr1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = fun1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = str1, perl = TRUE, lib_path = NULL, error_text = ""))
    # end replacement
    # perl
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = str1, lib_path = NULL, error_text = ""))
    # end perl
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function

    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(.in_quotes_replacement(string = "", pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")) 
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = "", no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = "", replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = "", perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = "", lib_path = NULL, error_text = ""))
    # testthat::expect_error(.in_quotes_replacement(string = str1, pattern = pattern1, lib_path = "", error_text = "")) # cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")) 
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
    testthat::expect_error(saferDev:::.in_quotes_replacement(string = "\\(", pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = ""), regexp = "ARGUMENTS string AND pattern CANNOT BE IDENTICAL", fixed = TRUE)
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # if(base::nchar(x = no_regex_pattern, type = "chars", allowNA = FALSE, keepNA = NA) != base::nchar(x = replacement, type = "chars", allowNA = FALSE, keepNA = NA)){
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = "dddd", no_regex_pattern = "dddd", replacement = "d", perl = TRUE, lib_path = NULL, error_text = ""))
    # end if(base::nchar(x = no_regex_pattern, type = "chars", allowNA = FALSE, keepNA = NA) != base::nchar(x = replacement, type = "chars", allowNA = FALSE, keepNA = NA)){
    # detection of the characters of string that are between quotes
    # inside quotes: escaped characters (escaped quote and escaped backslash), closing quote, and the other quote or # not read
    result <- saferDev:::.in_quotes_replacement(string = 'a("it\'s (1)", \'say "hi" (2)\', "\\" (3)", "\\\\", f(4))', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'a("it\'s  1)", \'say "hi"  2)\', "\\"  3)", "\\\\", f(4))', pos = base::c(9, 25, 35)))
    # comment, up to the end of the line (quotes inside comments not read)
    result <- saferDev:::.in_quotes_replacement(string = "x <- 1 # it's (a)\ny <- '(' # \"(\"\nz <- f(1)", pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = "x <- 1 # it's (a)\ny <- ' ' # \"(\"\nz <- f(1)", pos = 25))
    # special operators (closed by the next % of the line) and comment at the end of string
    result <- saferDev:::.in_quotes_replacement(string = 'x %in% "(" ; y %\'% z ; "%(" ; 5 %% 2 ; a %b\nc("(", "%") ; 1 %% 2 # end (', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'x %in% " " ; y %\'% z ; "% " ; 5 %% 2 ; a %b\nc(" ", "%") ; 1 %% 2 # end (', pos = base::c(9, 26, 48)))
    result <- saferDev:::.in_quotes_replacement(string = 'a %b ; "("', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "") # % not closed
    testthat::expect_equal(object = result, expected = base::list(string = 'a %b ; " "', pos = 9))
    # raw strings (only their content is replaced, not their delimiters)
    result <- saferDev:::.in_quotes_replacement(string = 'r"(it\'s (a))" ; R"--[a)"b(]--" ; r\'{x(}\' ; f(1)', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'r"(it\'s  a))" ; R"--[a)"b ]--" ; r\'{x }\' ; f(1)', pos = base::c(9, 26, 38)))
    result <- saferDev:::.in_quotes_replacement(string = 'r"(a"b)" ; f(1)', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "") # quote inside a raw string
    testthat::expect_equal(object = result, expected = base::list(string = 'r"(a"b)" ; f(1)', pos = NULL))
    result <- saferDev:::.in_quotes_replacement(string = 'ar"(a"b)" ; f(1)', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "") # r at the end of a name: not a raw string
    testthat::expect_equal(object = result, expected = base::list(string = 'ar" a"b)" ; f 1)', pos = base::c(4, 14)))
    result <- saferDev:::.in_quotes_replacement(string = 'r"abc(" ; f(1)', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "") # no opening delimiter: not a raw string
    testthat::expect_equal(object = result, expected = base::list(string = 'r"abc " ; f(1)', pos = 6))
    result <- saferDev:::.in_quotes_replacement(string = 'r"(abc ; f(1)', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "") # raw string not closed
    testthat::expect_equal(object = result, expected = base::list(string = 'r"(abc ; f 1)', pos = 11))
    # successive replacements, as in all_args_here(): the delimiters of the raw string, kept by the first replacement, still close it in the second one, so that the ( of f( is not replaced
    result <- saferDev:::.in_quotes_replacement(string = 'y <- r"(a)b(c)" ; f("(", 1)', pattern = "\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'y <- r"(a b(c)" ; f("(", 1)', pos = 10))
    result <- saferDev:::.in_quotes_replacement(string = result$string, pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'y <- r"(a b c)" ; f(" ", 1)', pos = base::c(12, 22)))
    result <- saferDev:::.in_quotes_replacement(string = 'y <- r"(a)b(c)" ; f("(", 1)', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'y <- r"(a)b c)" ; f(" ", 1)', pos = base::c(12, 22)))
    result <- saferDev:::.in_quotes_replacement(string = result$string, pattern = "\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'y <- r"(a b c)" ; f(" ", 1)', pos = 10))
    # quote at the first position, backquoted names (with escaped backquote) and quote not closed
    result <- saferDev:::.in_quotes_replacement(string = '"(" ; f(1)', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = '" " ; f(1)', pos = 2))
    result <- saferDev:::.in_quotes_replacement(string = '`a(b` <- f(1) ; `x\\`(y`', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = '`a b` <- f(1) ; `x\\` y`', pos = base::c(3, 21)))
    result <- saferDev:::.in_quotes_replacement(string = 'f("(abc', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'f(" abc', pos = 4))
    # empty quoted parts, and quote not closed at the end of string
    result <- saferDev:::.in_quotes_replacement(string = 'f("", r"()", \'(\') ; g(1, "', pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = 'f("", r"()", \' \') ; g(1, "', pos = 15))
    # the quotes themselves are never replaced
    result <- saferDev:::.in_quotes_replacement(string = '\'say "hi"\' ; "a"', pattern = "\"", no_regex_pattern = "\"", replacement = " ", perl = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = base::list(string = '\'say  hi \' ; "a"', pos = base::c(6, 9)))
    # end detection of the characters of string that are between quotes
    # replacement of the matches of pattern between quotes
    testthat::expect_equal(object = saferDev:::.in_quotes_replacement(string = "f(1)", pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, lib_path = NULL, error_text = ""), expected = base::list(string = "f(1)", pos = NULL)) # no match between quotes
    testthat::expect_equal(object = saferDev:::.in_quotes_replacement(string = " ", pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""), expected = base::list(string = " ", pos = NULL)) # no match
    # end replacement of the matches of pattern between quotes
    # if( ! base::is.null(x = pos)){
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = "\\)", no_regex_pattern = "(", replacement = ")", perl = TRUE, lib_path = NULL, error_text = ""), regexp = "INTERNAL ERROR 1", fixed = TRUE)
    # end if( ! base::is.null(x = pos)){
    #### end main code
    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    result <- .in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")
    expect <-  list(string = "paste0(\"IAGE((\", paste0(1:3, collapse = \" \"), \"A B(  \\ \")", pos = c(49, 52, 55))
    testthat::expect_equal(result, expect)
    result <- .in_quotes_replacement(string = str2, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")
    expect <-  list(string = "paste0(\"IAGE((\", paste0(1:3, collapse = \" \"), \"A B(\") \"   ", pos = c(49, 56, 57, 58))
    testthat::expect_equal(result, expect)
    result <- .in_quotes_replacement(string = str3, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")
    expect <-  list(string = "paste0(\"IAGE((\", paste0(1:3, collapse = \" \"), \"A B(  \")))", pos = c(49, 52))
    testthat::expect_equal(result, expect)

    result <- .in_quotes_replacement(string = str1, pattern = pattern2, no_regex_pattern = no_regex_pattern2, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")
    expect <-  list(string = "paste0(\"IAGE  \", paste0(1:3, collapse = \" \"), \"A)B ) \\)\")", pos = c(13, 14, 51))
    testthat::expect_equal(result, expect)

    result <- .in_quotes_replacement(string = str1, pattern = pattern3, no_regex_pattern = no_regex_pattern3, replacement = replacement3, perl = TRUE, lib_path = NULL, error_text = "")
    expect <-  list(string = "paste0(\"IAGE((\", paste0(1:3, collapse = \"_\"), \"A)B()_\\)\")", pos = c(42, 53))
    testthat::expect_equal(result, expect)

    # regression tests (quotes and brackets, U58): all_args_here() relies on .in_quotes_replacement() to ignore the brackets and commas between quotes
    # functions with apostrophes inside double quotes, double quotes inside simple quotes, escaped backslashes, brackets between quotes, a backquoted name with a bracket, raw strings followed by calls (on the same line and on the next line) and a comment with an apostrophe and a bracket: the corrected function must give the same result as the initial one
    `my(var` <- "v"
    text_quote <- 'function(x){\n    y <- base::paste0("it\'s (", x, ")") # it\'s a (comment\n    z <- base::paste0(\'say "hi" (\', y, ")", collapse = NULL, recycle0 = FALSE)\n    w <- r"(it\'s)"\n    base::paste0(z, w, "\'\\\\\'", "\'\\\'", "\\"(", `my(var`, r"(a"b)", base::sum(1, 2), "#", collapse = NULL)\n}'
    fun_quote1 <- base::eval(expr = base::parse(file = "", n = NULL, text = text_quote, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = text_quote, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # with source references (code printed as written)
    fun_quote2 <- base::eval(expr = base::parse(file = "", n = NULL, text = base::deparse(expr = fun_quote1, width.cutoff = 60L, backtick = TRUE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), prompt = "?", keep.source = FALSE, srcfile = NULL, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # without source references (code printed deparsed)
    fun_expected <- base::eval(expr = base::parse(file = "", n = NULL, text = 'function(x){\n    y <- base::paste0("it\'s (", x, ")", collapse = NULL, recycle0 = FALSE)\n    z <- base::paste0(\'say "hi" (\', y, ")", collapse = NULL, recycle0 = FALSE)\n    w <- r"(it\'s)"\n    base::paste0(z, w, "\'\\\\\'", "\'\\\'", "\\"(", `my(var`, r"(a"b)", base::sum(1, 2, na.rm = FALSE), "#", collapse = NULL, recycle0 = FALSE)\n}', prompt = "?", keep.source = FALSE, srcfile = NULL, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    for(i1 in base::seq_len(length.out = 2)){
        fun_quote <- base::list(fun_quote1, fun_quote2)[[i1]]
        result <- base::suppressMessages(expr = saferDev::all_args_here(x = fun_quote, table_export = FALSE, fun_export = FALSE, out_path = base::tempdir(check = FALSE), df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
        testthat::expect_true(object = base::is.function(x = result))
        base::environment(fun = result) <- base::environment(fun = fun_quote)
        testthat::expect_identical(object = result(1), expected = fun_quote(1))
        testthat::expect_identical(object = base::deparse(expr = base::body(fun = result), width.cutoff = 60L, backtick = TRUE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), expected = base::deparse(expr = base::body(fun = fun_expected), width.cutoff = 60L, backtick = TRUE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)) # all the calls completed, without modification of the strings
    }
    # functions of R packages that stopped all_args_here() with INTERNAL ERROR 3 of .fun_args_pos()
    result <- base::tryCatch(expr = {base::suppressWarnings(expr = base::suppressMessages(expr = saferDev::all_args_here(x = stats:::quantile.default, table_export = FALSE, fun_export = FALSE, out_path = base::tempdir(check = FALSE), df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), classes = "warning") ; "NO ERROR"}, error = function(e){base::conditionMessage(c = e)}) # error expected, because the calls of quantile.default are not preceded by their package
    testthat::expect_false(object = base::grepl(x = result, pattern = "INTERNAL ERROR", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE))
    fun_title_case <- base::suppressWarnings(expr = base::suppressMessages(expr = saferDev::colons_check(x = tools::toTitleCase, export = FALSE, out_path = base::tempdir(check = FALSE), safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), classes = "warning") # package prefixes added first, as required by all_args_here()
    testthat::expect_true(object = base::is.function(x = fun_title_case))
    testthat::expect_no_error(object = result <- base::suppressWarnings(expr = base::suppressMessages(expr = saferDev::all_args_here(x = fun_title_case, table_export = FALSE, fun_export = FALSE, out_path = base::tempdir(check = FALSE), df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), classes = "warning"), message = NULL, class = NULL)
    testthat::expect_true(object = base::is.function(x = result))
    tempo_s <- base::c("the quick brown fox", "an R package (for data) in 2026", "it's a 'quoted' title: and more")
    testthat::expect_identical(object = result(text = tempo_s), expected = tools::toTitleCase(text = tempo_s)) # the corrected function gives the same result as the initial one
    # end regression tests (quotes and brackets, U58)
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})

