testthat::test_that(".functions_detect()", {

    ## data argument values
    # from source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R")
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
    arg_user_setting2_1 <- base::list(x =  as.name(x = "test"))
    arg_user_setting2_2 <- base::list(x =  as.name(x = "PACK::test"))
    arg_user_setting2_3 <- base::list(x =  c("::", "PACK", "test"))
    arg_user_setting2_4 <- base::list(x =  as.name(x = "fun4"))
    vec1 <- c("function", "if", "for", "while", "repeat", "else")
    str1 <- "blabla" # error saferDev:::.noclean_functions()
    str2 <- c("", "UseMethod(\"mean\")") # no error
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    fun2 <- function(){
        # blabla
    }
    fun3 <- function
    (){
        dt <- base::c(2:8)
    }
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.functions_detect(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.functions_detect()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.functions_detect()) 
    x <- test
    arg_user_setting2 <- arg_user_setting2_1
    skipped_base <- vec1
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.functions_detect()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.functions_detect(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        x, 
        arg_user_setting2, 
        skipped_base, 
        lib_path, 
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.functions_detect(x = NULL, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = NULL, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.functions_detect(x = character(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = integer(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = double(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = logical(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = complex(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = data.frame(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = list(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = character(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = integer(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = double(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = logical(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = complex(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = data.frame(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = list(), skipped_base = vec1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.functions_detect(x = NA, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = NA, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NA, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = NA))
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
    # x
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = NULL, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = NA, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = 1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = c(TRUE, FALSE), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = str1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = mat1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = factor1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = expr1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = fun1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    # end x
    # arg_user_setting2
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = NULL, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = NA, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = 1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = c(TRUE, FALSE), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = str1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = mat1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = factor1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = expr1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = fun1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    # end arg_user_setting2
    # skipped_base
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = str1, lib_path = NULL, error_text = "")) 
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = fun1, lib_path = NULL, error_text = ""))
    # end skipped_base
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character

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
    # modification of arg_user_setting2$x for clean messages
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_3, skipped_base = vec1, lib_path = NULL, error_text = ""))
    result <- .functions_detect(x = test, arg_user_setting2 = arg_user_setting2_3, skipped_base = vec1, lib_path = NULL, error_text = "")$arg_user_setting$x
    expected <- "PACK::test()"
    testthat::expect_equal(result, expected)
    # end modification of arg_user_setting2$x for clean messages
    # recovering the basic functions of R
    detach("package:stats", unload = FALSE)
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.functions_detect().\n\nTHE default base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\npackage:stats\nTHE FUNCTION CANNOT WORK WITH THIS NAME SPACE DETACHED. PLEASE, ATTACH IT.\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    attachNamespace("stats")
    detach("package:stats", unload = FALSE)
    detach("package:graphics", unload = FALSE)
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.functions_detect().\n\nTHE default base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\npackage:stats\npackage:graphics\nTHE FUNCTION CANNOT WORK WITH THESE NAME SPACES DETACHED. PLEASE, ATTACH THEM.\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    attachNamespace("stats")
    attachNamespace("graphics")
    # end recovering the basic functions of R

    # removal of special functions
    testthat::expect_error(.functions_detect(x = fun2, arg_user_setting2 = base::list(x =  as.name(x = "fun2")), skipped_base = vec1, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('.functions_detect(x = fun2, arg_user_setting2 = base::list(x =  as.name(x = "fun2")), skipped_base = vec1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.functions_detect().\n\nTHE TESTED FUNCTION fun2 IS EMPTY OF FUNCTION OR ONLY MADE OF COMMENTS OR ONLY MADE OF THE SKIPPED FUNCTIONS:\nfunction\nif\nfor\nwhile\nrepeat\nelse\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    # end removal of special functions

    # trick to deal with end of lines between the name of the function and "("
    testthat::expect_no_error(.functions_detect(x = fun3, arg_user_setting2 = arg_user_setting2_4, skipped_base = vec1, lib_path = NULL, error_text = ""))
    # end trick to deal with end of lines between the name of the function and "("
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    unlink(file.path(".", "*"), recursive = TRUE, force = TRUE) # to avoid a warning that block CRAN tests
})

