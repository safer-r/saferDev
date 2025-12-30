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

    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # if(base::nchar(x = no_regex_pattern, type = "chars", allowNA = FALSE, keepNA = NA) != base::nchar(x = replacement, type = "chars", allowNA = FALSE, keepNA = NA)){
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = "dddd", no_regex_pattern = "dddd", replacement = "d", perl = TRUE, lib_path = NULL, error_text = ""))
    # end if(base::nchar(x = no_regex_pattern, type = "chars", allowNA = FALSE, keepNA = NA) != base::nchar(x = replacement, type = "chars", allowNA = FALSE, keepNA = NA)){
    # if(base::length(x = string_split) > 1){
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    # end if(base::length(x = string_split) > 1){
    # if(base::nchar(x = string, type = "chars", allowNA = FALSE, keepNA = NA) == base::nchar(x = string_out, type = "chars", allowNA = FALSE, keepNA = NA) + 1){
    testthat::expect_no_error(.in_quotes_replacement(string = str2, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")) # deal with pattern being the last character, which is the case with str1 # if(odds.quotes.log == TRUE){
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")) # deal with pattern being the last character, which is the case with str1 # }else{
    # end if(base::nchar(x = string, type = "chars", allowNA = FALSE, keepNA = NA) == base::nchar(x = string_out, type = "chars", allowNA = FALSE, keepNA = NA) + 1){
    # if(base::all(base::is.na(x = pos), na.rm = TRUE)){
    testthat::expect_no_error(.in_quotes_replacement(string = " ", pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")) # pos is NULL
    testthat::expect_no_error(.in_quotes_replacement(string = ")", pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = "")) # pos is NA

    # end if(base::all(base::is.na(x = pos), na.rm = TRUE)){
    # if( ! base::is.null(x = pos)){
    testthat::expect_no_error(.in_quotes_replacement(string = str1, pattern = pattern1, no_regex_pattern = no_regex_pattern1, replacement = replacement1, perl = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(.in_quotes_replacement(string = str1, pattern = "\\)", no_regex_pattern = "(", replacement = ")", perl = TRUE, lib_path = NULL, error_text = ""))
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

    ## end other tests
    unlink(file.path(".", "*"), recursive = TRUE, force = TRUE) # to avoid a warning that block CRAN tests
})

