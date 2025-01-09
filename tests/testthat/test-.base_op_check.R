testthat::test_that(".base_op_check()", {

    ## data argument values
    mat1 <- base::matrix(-1:3)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.base_op_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.base_op_check(error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.base_op_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(.base_op_check()) # all internals have no defaults values
    ########  end argument with no default values

    ######## management of NULL arguments
    testthat::expect_no_error(.base_op_check(error_text = NULL))
    ######## end management of NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.base_op_check(error_text = NA))
    ######## end management of NA arguments

    ######## lib_path argument
    ######## end lib_path argument

    ######## safer_check argument
    ######## end safer_check argument

    ######## check of the required functions from the required packages
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    ######## end critical operator checking

    ######## management of "" in arguments of mode character
    ######## end management of "" in arguments of mode character

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    assign("+", 1)
    result <- saferDev::get_message(".base_op_check(error_text = '')", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE) 
    # warning LINE 13 can be LINE 22
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN .base_op_check().\n\nCRITICAL R OBJECTS CANNOT BE PRESENT SOMEWHERE ELSE IN THE R SCOPE THAN IN \"package::base\".\nPROBLEM WITH:\n+\t.GlobalEnv package:base\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    rm("+")
    result <- saferDev::get_message(".base_op_check(error_text = '')", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "NO ERROR MESSAGE REPORTED"
    testthat::expect_equal(result, expected)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    testthat::expect_no_error(.base_op_check(error_text = mat1)) # not a correct value but converted into text
    result <- saferDev::get_message(".base_op_check(error_text = mat1)", kind = "error", print.no = TRUE, text = NULL, safer_check = FALSE) 
    # warning LINE 13 can be LINE 22
    expected <-"NO ERROR MESSAGE REPORTED"
    testthat::expect_equal(result, expected)
    ## end other tests

})

