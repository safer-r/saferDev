testthat::test_that(".has_odd_number_of_quotes()", {

    ## data argument values
    str1 <- 'This is a "test" string with "even" quotes'
    pattern1 <- '"'
    char3_fun <- "NOTGOOD::arg_check"
    char4_fun <- "saferDev::NOTGOOD"
    mat1 <- base::matrix(-1:3)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.has_odd_number_of_quotes(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.has_odd_number_of_quotes(input_string = str1, pattern = pattern1, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.has_odd_number_of_quotes()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.has_odd_number_of_quotes()) 
    input_string <- str1
    pattern <- pattern1
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.has_odd_number_of_quotes()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.has_odd_number_of_quotes(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        input_string,
        pattern,
        lib_path,
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.has_odd_number_of_quotes(input_string = NULL, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(input_string = str1, pattern = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.has_odd_number_of_quotes(input_string = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.has_odd_number_of_quotes(input_string = str1, pattern = pattern1, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.has_odd_number_of_quotes(fun = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(fun = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(fun = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(fun = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(fun = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(fun = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(fun = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.has_odd_number_of_quotes(fun = char1_fun, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.has_odd_number_of_quotes(fun = char1_fun, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.has_odd_number_of_quotes(fun = char1_fun, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.has_odd_number_of_quotes(fun = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(fun = char1_fun, lib_path = NA, error_text = ""))
    testthat::expect_error(.has_odd_number_of_quotes(fun = char1_fun, lib_path = NULL, error_text = NA))
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
    testthat::expect_error(.has_odd_number_of_quotes(fun = 1, lib_path = NULL, error_text = "")) 
    testthat::expect_error(.has_odd_number_of_quotes(fun = fun1, lib_path = NULL, error_text = "")) 
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(.has_odd_number_of_quotes(fun = "", lib_path = NULL, error_text = "")) 
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
    # if(base::is.null(x = lib_path)){
    testthat::expect_no_error(.has_odd_number_of_quotes(fun = char1_fun, lib_path = NULL, error_text = ""))
    # end if(base::is.null(x = lib_path)){
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # if( ! base::all(tempo.log, na.rm = TRUE)){
    testthat::expect_error(.has_odd_number_of_quotes(fun = char2_fun, lib_path = NULL, error_text = ""))
    # end if( ! base::all(tempo.log, na.rm = TRUE)){
    # if( ! base::all(pkg.log, na.rm = TRUE)){
    testthat::expect_error(.has_odd_number_of_quotes(fun = char3_fun, lib_path = NULL, error_text = ""))
    # end if( ! base::all(pkg.log, na.rm = TRUE)){
    # if( ! base::all(fun.log, na.rm = TRUE)){
    testthat::expect_error(.has_odd_number_of_quotes(fun = char4_fun, lib_path = NULL, error_text = ""))
    # end if( ! base::all(fun.log, na.rm = TRUE)){
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    result <- saferDev::get_message('.has_odd_number_of_quotes(fun = char2_fun, lib_path = NULL, error_text = "")', kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.has_odd_number_of_quotes().\n\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\" OR \":::.\":\narg_check\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    result <- saferDev::get_message('.has_odd_number_of_quotes(fun = char3_fun, lib_path = NULL, error_text = "")', kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- base::paste0("ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.has_odd_number_of_quotes().\n\nREQUIRED PACKAGE:\nNOTGOOD\nMUST BE INSTALLED IN ONE OF THESE FOLDERS:\n", paste0( .libPaths(), collapse = "\n", recycle0 = FALSE), "\n\n================\n\n\n", collapse = NULL, recycle0 = FALSE)
    testthat::expect_equal(result, expected)
    result <- saferDev::get_message('.has_odd_number_of_quotes(fun = char4_fun, lib_path = NULL, error_text = "")', kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- base::paste0("ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.has_odd_number_of_quotes().\n\nREQUIRED FUNCTION IS MISSING IN THE INSTALLED PACKAGE:\nsaferDev::NOTGOOD\n\nIN ONE OF THESE FOLDERS:\n", paste0( .libPaths(), collapse = "\n", recycle0 = FALSE), "\n\n================\n\n\n", collapse = NULL, recycle0 = FALSE)
    testthat::expect_equal(result, expected)
    ## end other tests

})

