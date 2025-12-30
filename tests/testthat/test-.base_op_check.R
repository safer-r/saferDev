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
    error_text <- ""
    testthat::expect_error(.base_op_check()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    # testthat::expect_error(.base_op_check(error_text = "")) # inactivated because no other arg with default values # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    testthat::expect_no_error(.base_op_check(error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_no_error(.base_op_check(error_text = character())) # # but error_text is converted to ""
    testthat::expect_no_error(.base_op_check(error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.base_op_check(error_text = NA))
    ######## end management of NA arguments

    ######## safer_check argument
    ######## end safer_check argument

    ######## lib_path argument
    ######## end lib_path argument

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
    assign("+", 1, envir = .GlobalEnv)
    result <- saferDev::get_message(".base_op_check(error_text = '')", kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.base_op_check().\n\nCRITICAL R OBJECTS CANNOT BE PRESENT SOMEWHERE ELSE IN THE R SCOPE THAN IN \"package::base\".\nPROBLEM WITH:\n+\t.GlobalEnv package:base\n\nSWITCH THE safer_check ARGUMENT TO FALSE IF THESE OBJECTS CANNOT BE DELETED.\n\nOF NOTE, BASIC R OBJECT PROTECTED FROM OVERWRITING IN SAFER FUNCTIONS:\n-\n!\n!=\n$\n%%\n%*%\n%/%\n%in%\n&\n&&\n(\n*\n/\n:\n::\n:::\n@\n[\n[[\n^\n{\n|\n||\n~\n+\n<\n<-\n<<-\n<=\n=\n==\n>\n>=\n\\\nif\nelse\nfunction\nfor\nwhile\nrepeat\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    rm("+", envir = .GlobalEnv)
    result <- saferDev::get_message(".base_op_check(error_text = '')", kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "NO ERROR MESSAGE REPORTED"
    testthat::expect_equal(result, expected)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    testthat::expect_no_error(.base_op_check(error_text = mat1)) # not a correct value but converted into text
    result <- saferDev::get_message(".base_op_check(error_text = mat1)", kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <-"NO ERROR MESSAGE REPORTED"
    testthat::expect_equal(result, expected)
    ## end other tests
    unlink(file.path(".", "*"), recursive = TRUE, force = TRUE) # to avoid a warning that block CRAN tests
})

