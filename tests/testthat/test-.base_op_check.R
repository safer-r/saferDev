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
    # if(base::any(tempo_log, na.rm = TRUE)){: reserved objects also present in another environment of the search path (global environment, attached packages)
    expected_start <- "\n\n================\n\nERROR IN saferDev:::.base_op_check().\n\nCRITICAL R OBJECTS CANNOT BE PRESENT SOMEWHERE ELSE IN THE R SCOPE THAN IN \"package::base\".\nPROBLEM WITH:\n"
    expected_end <- "\n\nSWITCH THE safer_check ARGUMENT TO FALSE IF THESE OBJECTS CANNOT BE DELETED.\n\nOF NOTE, BASIC R OBJECT PROTECTED FROM OVERWRITING IN SAFER FUNCTIONS:\n-\n!\n!=\n$\n%%\n%*%\n%/%\n%in%\n&\n&&\n(\n*\n/\n:\n::\n:::\n@\n[\n[[\n^\n{\n|\n||\n~\n+\n<\n<-\n<<-\n<=\n=\n==\n>\n>=\nif\nelse\nfunction\nfor\nwhile\nrepeat\n\n================\n\n"
    # object of the global environment
    base::assign(x = "+", value = 1, pos = -1, envir = base::globalenv(), inherits = FALSE, immediate = TRUE)
    base::on.exit(expr = if(base::exists(x = "+", where = -1, envir = base::globalenv(), frame = NULL, mode = "any", inherits = FALSE)){base::rm(list = "+", pos = -1, envir = base::globalenv(), inherits = FALSE)}, add = TRUE, after = FALSE) # guarantee the removal of the + object even if an expectation fails, to avoid cascade failures in the tests below
    result <- base::tryCatch(expr = .base_op_check(error_text = ""), error = function(e){base::conditionMessage(c = e)}, finally = NULL)
    testthat::expect_identical(object = result, expected = base::paste0(expected_start, "+\t.GlobalEnv package:base", expected_end, collapse = NULL, recycle0 = FALSE))
    # objects of an attached package (an attached list named like a package; objects that are not functions, so that no operator is really masked during the test)
    base::attach(what = base::list("%in%" = 1, "+" = 2), pos = 2L, name = "package:saferDevMaskingTest", warn.conflicts = FALSE)
    base::on.exit(expr = if(base::any(base::search() == "package:saferDevMaskingTest", na.rm = TRUE)){base::detach(name = "package:saferDevMaskingTest", pos = 2L, unload = FALSE, character.only = TRUE, force = FALSE)}, add = TRUE, after = FALSE) # guarantee the detachment even if an expectation fails
    result <- base::tryCatch(expr = .base_op_check(error_text = ""), error = function(e){base::conditionMessage(c = e)}, finally = NULL)
    testthat::expect_identical(object = result, expected = base::paste0(expected_start, "%in%\tpackage:saferDevMaskingTest package:base\n+\t.GlobalEnv package:saferDevMaskingTest package:base", expected_end, collapse = NULL, recycle0 = FALSE)) # all the environments containing the object, in the search path order
    base::rm(list = "+", pos = -1, envir = base::globalenv(), inherits = FALSE)
    result <- base::tryCatch(expr = .base_op_check(error_text = " INSIDE P1::F1."), error = function(e){base::conditionMessage(c = e)}, finally = NULL)
    testthat::expect_identical(object = result, expected = base::paste0(base::sub(x = expected_start, pattern = "saferDev:::.base_op_check().", replacement = "saferDev:::.base_op_check() INSIDE P1::F1.", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), "%in%\tpackage:saferDevMaskingTest package:base\n+\tpackage:saferDevMaskingTest package:base", expected_end, collapse = NULL, recycle0 = FALSE))
    base::detach(name = "package:saferDevMaskingTest", pos = 2L, unload = FALSE, character.only = TRUE, force = FALSE)
    # end if(base::any(tempo_log, na.rm = TRUE)){
    testthat::expect_null(object = .base_op_check(error_text = "")) # nothing masked anymore
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    testthat::expect_no_error(.base_op_check(error_text = mat1)) # not a correct value but converted into text
    testthat::expect_null(object = .base_op_check(error_text = mat1))
    testthat::expect_invisible(call = .base_op_check(error_text = ""))
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})

