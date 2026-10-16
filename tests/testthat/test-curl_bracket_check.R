# This file tests curl_bracket_check()
# Same structure as test-colons_check.R

testthat::test_that("curl_bracket_check()", {

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
    # functions with and without braces
    fun3 <- function(x){ if(x > 1){ base::return(x) } }
    fun4 <- function(x){ if(x > 1) base::return(x) }
    fun5 <- function(x){ for(i in 1:3){ base::print(i) } }
    fun6 <- function(x){ for(i in 1:3) base::print(i) }
    fun7 <- function(x){ while(x > 1){ x <- x - 1 } ; repeat{ x <- x + 1 } }
    fun8 <- function(x){ while(x > 1) x <- x - 1 }
    fun9 <- function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) }
    fun10 <- function(x){ base::sapply(X = 1:3, FUN = function(y) y * 2) }
    fun11 <- function(x){ base::sapply(X = 1:3, FUN = \(y){ y * 2 }) }
    fun12 <- function(x){ base::sapply(X = 1:3, FUN = \(y) y * 2) }
    fun13 <- function(x){ if(x > 1){ base::return(x) } else { base::return(0) } }
    fun14 <- function(x){ if(x > 1) base::return(x) else base::return(0) }
    fun15 <- function(x){ if(x > 1){ base::return(1) } else if(x < 0){ base::return(2) } else { base::return(3) } }
    fun16 <- function(x){ y <- if(x > 1) 1 else 2 ; y }
    fun17 <- function(x){ x <- "if(x > 1) x" ; x }
    fun18 <- function(f = function(y) y) f(1)
    fun19 <- function(x){ if(x > 1) NULL }
    fun20 <- function(x){
        if(x > 1)
            base::return(x)
        base::return(0)
    }
    fun21 <- function(x){ if(x > 1){ # c
        base::return(x)
    } }
    fun22 <- function(x){ if(x > 1) # c
        base::return(x)
    }
    fun23 <- function(x){ repeat x <- x + 1 }
    fun24 <- function(x){ if(x > 1){
        x <- x - 1
    }else
        base::return(0)
    }
    fun25 <- base::`+`
    fun26 <- base::mean
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(curl_bracket_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::curl_bracket_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(curl_bracket_check()) # the function has default values for all args except x.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(curl_bracket_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(curl_bracket_check(x = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(curl_bracket_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v20.0: unguarded lib_path check)
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curl_bracket_check(x = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_message(curl_bracket_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # braced body
    # end x
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    # detection of missing curly brackets
    testthat::expect_message(curl_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun5, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun9, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun11, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun13, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun15, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun17, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # strings are immune
    testthat::expect_message(curl_bracket_check(x = fun21, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # comments are ignored
    testthat::expect_message(curl_bracket_check(x = fun25, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # .Primitive body skipped
    testthat::expect_message(curl_bracket_check(x = curl_bracket_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # self check
    testthat::expect_message(curl_bracket_check(x = colons_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = arg_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun8, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun10, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun12, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun14, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun16, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun18, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun19, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun20, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun22, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE, {curl_bracket_check(x = fun23, safer_check = TRUE, lib_path = NULL, error_text = "")})
    testthat::expect_message(curl_bracket_check(x = fun24, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curl_bracket_check(x = fun26, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE) # UseMethod body flagged
    # exact findings for a representative case (only the LINE and CONSTRUCT columns are compared, because the CODE column depends on the srcref presence of the checked function). Warning: utils::capture.output(type = "message") does not work inside testthat (the message connection is redirected), so testthat::capture_messages() is used
    msgs <- testthat::capture_messages(curl_bracket_check(x = fun14, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res <- base::unlist(x = base::strsplit(x = msgs, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res_fields <- base::strsplit(x = res, split = "\t", fixed = TRUE)
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[1]}, FUN.VALUE = base::character(length = 1)), y = base::c("1", "1")), label = "fun14 exact line numbers")
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1)), y = base::c("if", "else")), label = "fun14 exact construct kinds")
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})
