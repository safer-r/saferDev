testthat::test_that("colons_check()", {

    ## data argument values
    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    fun2 <- function(x){
        ggplot2::ggplot()
    }
    fun3 <- function(x){fun1()}
    fun4 <- function(x){
        fun5 <- function(x){}
        fun5()
    }
    fun6 <- function(x){
        ggplot2::ggplot()
        print()
    }
    fun7 <- function(x){
        ggplot()
        print()
    }
    test <- function(
            text, 
            pattern
        ){
        # AIM
        # extract all function names
        # ARGUMENTS
        # text: vector of strings
        # pattern: regex to extract function names
        # RETURN
        # A list containing the functions names, each compartment being one of the string of the input vector
        # DEBUGGING
        # text = ini[1] ; pattern = pattern
        FUN1 <- function(){}
        #### Find all matches, including trailing '(' #
        matches <- gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text) # # test
        dt <- base::c(2:8)
        matched_strings <- regmatches(x = text, m = matches)[[1]]
        FUN1()
        # Remove trailing '(' from each match #
        tempo4 <- a$regmatches(x = text, m = matches)[[1]] ; sum(1:3) ; a$regmatches(x = 1)
        tempo5 <- a$count
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(roc1()), "), col2 (", base::length(roc2), "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")
        result <- sub("\\($##", "", matched_strings) ; range(1:3) # sub("\\($##", "", matched_strings)
        tempo.cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(roc4()), "), and roc0 (", base::length(roc5), ") SHOULL\n")
        return(baba) # base::sub("\\($##", "", matched_strings)
        base::return(bobo) # a$sub("\\($##", "", matched_strings)
    }

    ## end data argument values

    ## initialization of tests
    testthat::expect_error(colons_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::colons_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(colons_check()) # the function has default values for all args.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(colons_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(colons_check(x = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(colons_check(x = test, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(colons_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(colons_check(x = test, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = test, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end x
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = "")) 
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
    testthat::expect_no_error(colons_check(x = base::mean, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = colons_check, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})
