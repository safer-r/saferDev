# This file tests curly_bracket_check()
# Same structure as test-colons_check.R

testthat::test_that("curly_bracket_check()", {

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
    fun27 <- function(x) .Primitive('+') + 1 # body starting with .Primitive() but containing more code: must be reported
    fun28 <- function(x){ .Primitive('+') + 1 } # braced body containing .Primitive(): clean
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(curly_bracket_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::curly_bracket_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(curly_bracket_check()) # the function has default values for all args except x.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(curly_bracket_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(curly_bracket_check(x = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(curly_bracket_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.0: unguarded lib_path check)
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(curly_bracket_check(x = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_message(curly_bracket_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # braced body
    # end x
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    testthat::expect_message(curly_bracket_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun5, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun9, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun11, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun13, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun15, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun17, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # strings are immune
    testthat::expect_message(curly_bracket_check(x = fun21, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # comments are ignored
    testthat::expect_message(curly_bracket_check(x = fun25, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # .Primitive body skipped
    testthat::expect_message(curly_bracket_check(x = curly_bracket_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # self check
    testthat::expect_message(curly_bracket_check(x = colons_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = arg_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun8, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun10, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun12, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun14, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun16, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun18, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun19, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun20, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun22, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE, {curly_bracket_check(x = fun23, safer_check = TRUE, lib_path = NULL, error_text = "")})
    testthat::expect_message(curly_bracket_check(x = fun24, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE)
    testthat::expect_message(curly_bracket_check(x = fun26, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE) # UseMethod body flagged
    testthat::expect_message(curly_bracket_check(x = fun27, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", fixed = TRUE) # body starting with .Primitive() but containing more code: reported
    testthat::expect_message(curly_bracket_check(x = fun28, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # braced body containing .Primitive(): clean
    testthat::expect_message(curly_bracket_check(x = base::mean, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE base::mean(), SOME CURLY BRACKETS", fixed = TRUE) # message normalization of pkg::fun
    testthat::expect_message(curly_bracket_check(x = fun20, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun20(), SOME CURLY BRACKETS", fixed = TRUE) # message normalization of anonymous-like names
    # exact findings for a representative case (only the LINE and CONSTRUCT columns are compared, because the CODE column depends on the srcref presence of the checked function). Warning: utils::capture.output(type = "message") does not work inside testthat (the message connection is redirected), so testthat::capture_messages() is used
    msgs <- testthat::capture_messages(curly_bracket_check(x = fun14, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res <- base::unlist(x = base::strsplit(x = msgs, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res_fields <- base::strsplit(x = res, split = "\t", fixed = TRUE)
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[1]}, FUN.VALUE = base::character(length = 1)), y = base::c("1", "1")), label = "fun14 exact line numbers")
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1)), y = base::c("if", "else")), label = "fun14 exact construct kinds")
    # bodies written with = (token expr_or_assign_or_help in the parse data, and not expr): the curly brackets surround the whole body (not the condition, nor a default argument value), and the corrected function is clean and behaves like the submitted one
    txt_eq1 <- "function(x = 1){\n    if(x > 1) y = 1 else y = 2\n    if(x > 1) y = 3\n    if(x > 1) y = 4 else 5\n    for(i in 1:2) y = y + i\n    while(FALSE) y = 6\n    y\n}"
    txt_eq2 <- "function(x){\n    repeat z = break\n    f <- function(a = 1) w = a\n    g <- \\(b) w = b\n    base::sapply(X = 1:2, FUN = function(i) v = i)\n}"
    txt_eq3 <- "function(x){\n\tif(x > 1)\ty = 1 else\ty = 2\n\ty\n}" # with TABs
    txt_eq4 <- "function(x) .Primitive(\"+\") = 1" # body starting with .Primitive() but containing more code: reported
    fun_eq1 <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_eq1, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_eq1, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_eq2 <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_eq2, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_eq2, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_eq3 <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_eq3, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_eq3, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_eq4 <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_eq4, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_eq4, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_eq1_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_eq1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    fun_eq2_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_eq2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    fun_eq3_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_eq3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    fun_eq4_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_eq4, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_eq1_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[1:8], expected = base::c("function(x = 1){", "    if(x > 1) { y = 1 } else { y = 2 }", "    if(x > 1) { y = 3 }", "    if(x > 1) { y = 4 } else { 5 }", "    for(i in 1:2) { y = y + i }", "    while(FALSE) { y = 6 }", "    y", "}"), label = "bodies written with =: if, else, for and while")
    testthat::expect_identical(object = utils::capture.output(fun_eq2_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[1:6], expected = base::c("function(x){", "    repeat { z = break }", "    f <- function(a = 1) { w = a }", "    g <- \\(b) { w = b }", "    base::sapply(X = 1:2, FUN = function(i) { v = i })", "}"), label = "bodies written with =: repeat, function and lambda")
    testthat::expect_identical(object = utils::capture.output(fun_eq3_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[2], expected = "\tif(x > 1)\t{ y = 1 } else\t{ y = 2 }", label = "bodies written with = after TABs")
    testthat::expect_identical(object = utils::capture.output(fun_eq4_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[1], expected = "function(x) { .Primitive(\"+\") = 1 }", label = "body starting with .Primitive() and written with =")
    testthat::expect_identical(object = fun_eq1_safer(2), expected = fun_eq1(2))
    testthat::expect_identical(object = fun_eq1_safer(0), expected = fun_eq1(0))
    testthat::expect_identical(object = fun_eq2_safer(1), expected = fun_eq2(1))
    testthat::expect_identical(object = fun_eq3_safer(2), expected = fun_eq3(2))
    testthat::expect_identical(object = fun_eq3_safer(0), expected = fun_eq3(0))
    testthat::expect_message(object = saferDev::curly_bracket_check(x = fun_eq1_safer, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(object = saferDev::curly_bracket_check(x = fun_eq2_safer, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(object = saferDev::curly_bracket_check(x = fun_eq4_safer, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    msgs_eq <- testthat::capture_messages(code = saferDev::curly_bracket_check(x = fun_eq1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res_eq <- base::unlist(x = base::strsplit(x = msgs_eq, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    res_eq <- res_eq[base::grepl(x = res_eq, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
    testthat::expect_identical(object = res_eq, expected = base::c("2\tif\t\tif(x > 1) y = 1 else y = 2", "2\telse\t\tif(x > 1) y = 1 else y = 2", "3\tif\t\tif(x > 1) y = 3", "4\tif\t\tif(x > 1) y = 4 else 5", "4\telse\t\tif(x > 1) y = 4 else 5", "5\tfor\t\tfor(i in 1:2) y = y + i", "6\twhile\t\twhile(FALSE) y = 6"), label = "bodies written with =: table of the findings")
    #### end main code

    ######## export and out_path arguments
    # default: modified function returned, NULL when clean
    testthat::expect_null(object = curly_bracket_check(x = fun1, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")) # clean function: NULL
    testthat::expect_true(object = base::is.function(x = curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."))) # modified function returned
    # the modified function passes the check itself and behaves correctly
    fun4_safer <- curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_message(curly_bracket_check(x = fun4_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_identical(object = fun4_safer(2), expected = fun4(2))
    testthat::expect_identical(object = fun4_safer(0), expected = fun4(0))
    # modified code of a multi-line function keeps its line structure (braces inserted, nothing else moved)
    fun20_safer <- curly_bracket_check(x = fun20, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_message(curly_bracket_check(x = fun20_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    # comments, indentation and multiple spaces of the submitted function are kept in the returned and exported functions: the curly brackets are inserted in the printed code
    # warning: the short parse writing is required here, since a fully-named writing with srcfile = NULL removes the srcref of the parsed code (thus the comments)
    fun_com <- base::eval(expr = base::parse(text = "function(x){\n    # a comment with if(x)\n    if(x > 0) y   <-   1   # trailing   comment\n        else y <- 2 # odd indent\n    y\n}", keep.source = TRUE)[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_com_expected <- base::c("function(x){", "    # a comment with if(x)", "    if(x > 0) { y   <-   1 }   # trailing   comment", "        else { y <- 2 } # odd indent", "    y", "}")
    fun_com_safer <- base::suppressMessages(expr = curly_bracket_check(x = fun_com, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), classes = "message")
    fun_com_print <- utils::capture.output(fun_com_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    testthat::expect_identical(object = fun_com_print[1:6], expected = fun_com_expected, label = "comments and indentation kept in the returned function")
    testthat::expect_identical(object = fun_com_safer(1), expected = fun_com(1), label = "returned function behaves like the submitted one (if)")
    testthat::expect_identical(object = fun_com_safer(-1), expected = fun_com(-1), label = "returned function behaves like the submitted one (else)")
    tempo_dir_com <- base::tempfile(pattern = "cbc_com_")
    base::dir.create(path = tempo_dir_com, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    base::suppressMessages(expr = curly_bracket_check(x = fun_com, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir_com), classes = "message")
    fun_com_lines <- base::readLines(con = base::file.path(tempo_dir_com, "fun_com_safer.R", fsep = "/"), warn = FALSE, ok = TRUE)
    testthat::expect_identical(object = fun_com_lines[1:6], expected = base::c(base::paste0("fun_com_safer <- ", fun_com_expected[1], collapse = NULL, recycle0 = FALSE), fun_com_expected[2:6]), label = "comments and indentation kept in the exported file")
    base::unlink(x = tempo_dir_com, recursive = TRUE, force = TRUE, expand = TRUE)
    # export = TRUE: file written, NULL returned, no file when clean
    tempo_dir <- base::tempdir(check = TRUE)
    testthat::expect_null(object = curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun4_safer.R")))
    # the exported file contains an assignment and can be sourced to recreate the function
    tempo_lines <- base::readLines(con = base::file.path(tempo_dir, "fun4_safer.R"), n = 5, warn = FALSE, ok = TRUE)
    testthat::expect_true(object = base::grepl(x = tempo_lines[1], pattern = "^fun4_safer <- ", perl = TRUE), label = "exported file starts with assignment")
    tempo_env <- base::new.env(parent = base::globalenv())
    base::sys.source(file = base::file.path(tempo_dir, "fun4_safer.R"), envir = tempo_env, chdir = FALSE)
    testthat::expect_identical(object = tempo_env$fun4_safer(2), expected = fun4(2))
    # export message: the file location is announced by a message
    base::unlink(x = base::file.path(tempo_dir, "fun4_safer.R"))
    testthat::expect_message(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    # pre-existing file is never overwritten
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir), regexp = "FILE NAME ALREADY EXISTS", fixed = TRUE)
    base::unlink(x = base::file.path(tempo_dir, "fun4_safer.R"))
    testthat::expect_null(object = curly_bracket_check(x = fun1, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir))
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir, "fun1_safer.R")))
    # custom out_path
    tempo_dir2 <- base::file.path(base::tempdir(check = TRUE), "sub_curly_check")
    base::dir.create(path = tempo_dir2, showWarnings = FALSE, recursive = FALSE, mode = "0777")
    testthat::expect_message(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir2), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    base::unlink(x = base::file.path(tempo_dir2, "fun4_safer.R"))
    # trailing slash tolerated (forward slash and backslash)
    testthat::expect_no_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = base::paste0(tempo_dir2, "/")))
    base::unlink(x = base::file.path(tempo_dir2, "fun4_safer.R"))
    testthat::expect_message(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = base::paste0(tempo_dir2, "\\")), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE) # trailing backslash tolerated
    base::unlink(x = base::file.path(tempo_dir2, "fun4_safer.R"))
    # non-existing out_path
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = "PATH_NOT_GOOD"), regexp = "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXIST:\n", fixed = TRUE)
    # out_path must be a single character string
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = base::c(tempo_dir, tempo_dir)))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = 1))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = factor1))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = expr1))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = fun1))
    # out_path cannot contain an empty string, whatever the value of export
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ""), regexp = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = ""), regexp = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    # out_path ignored when export = FALSE (even a non-existing path)
    testthat::expect_no_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "PATH_NOT_GOOD"))
    # export must be a single logical value
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = NA))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = 1))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = "TRUE"))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = base::c(TRUE, FALSE)))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = NULL))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = mat1))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = factor1))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = expr1))
    testthat::expect_error(curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = fun1))
    # out_path = NULL (default): accepted when export = FALSE, error when export = TRUE
    testthat::expect_null(object = base::formals(fun = saferDev::curly_bracket_check, envir = base::parent.frame(n = 1))$out_path, label = "out_path default is NULL")
    testthat::expect_true(object = base::is.function(x = base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun4, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")), label = "export = FALSE with out_path = NULL returns the corrected function")
    testthat::expect_error(object = saferDev::curly_bracket_check(x = fun4, export = TRUE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN export IS TRUE", fixed = TRUE)
    testthat::expect_error(object = saferDev::curly_bracket_check(x = fun1, export = TRUE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN export IS TRUE", fixed = TRUE) # even if nothing has to be corrected
    # export = TRUE with out_path = ".": exports in the current working directory (restored after the test)
    tempo_wd <- base::getwd()
    base::setwd(dir = tempo_dir)
    testthat::expect_null(object = curly_bracket_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = "."))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun4_safer.R")))
    base::setwd(dir = tempo_wd)
    base::unlink(x = base::file.path(tempo_dir, "fun4_safer.R"))
    # the returned modified function keeps the closure environment of the submitted function
    fun_closure <- local({
        z <- 10
        function(x){
            if(x > 1)
                z + x
        }
    })
    fun_closure_safer <- curly_bracket_check(x = fun_closure, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_identical(object = fun_closure_safer(2), expected = fun_closure(2))
    testthat::expect_identical(object = base::environment(fun = fun_closure_safer), expected = base::environment(fun = fun_closure))
    # inline definition and component writing are refused when export = TRUE (the name of the exported file could not be determined reliably)
    testthat::expect_error(curly_bracket_check(x = function(y){ if(y > 1) base::return(y) }, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    FUN_LIST <- base::list(fun = fun4)
    testthat::expect_error(curly_bracket_check(x = FUN_LIST$fun, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    # file name determination: written as pkg::fun, the package name is not part of the file name (base::noquote contains reported constructs, so a file is written)
    testthat::expect_null(object = curly_bracket_check(x = base::noquote, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "noquote_safer.R")), label = "noquote_safer.R written for the pkg::fun form")
    base::unlink(x = base::file.path(tempo_dir, "noquote_safer.R"))
    # long function (> 21 lines of recovered code): the insertion sort must not suffer from integer overflow (the if body must still get its braces)
    fun_long <- eval(parse(text = paste0(
        "function(x){\n",
        paste0("    y <- x + ", 1:30, "\n", collapse = ""),
        "    if(x > 1)\n        base::return(y)\n    base::return(0)\n}"
    )))
    fun_long_safer <- curly_bracket_check(x = fun_long, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_message(curly_bracket_check(x = fun_long_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_identical(object = fun_long_safer(2), expected = fun_long(2))
    testthat::expect_identical(object = fun_long_safer(0), expected = fun_long(0))
    # TAB-indented functions: utils::getParseData() counts a TAB as a jump to the next multiple of 8, converted into character positions before the insertions (the TABs are kept)
    txt_tab1 <- "function(x){\n\tif(x > 1) base::return(x)\n\telse base::return(0)\n}" # TAB indentation
    txt_tab2 <- "function(x){\n\ty <- \"a\tb\" ; if(x > 1) base::return(y)\n\tx\n}" # TAB inside a string before the body
    txt_tab3 <- "function(x){\n    y <- 1 ;\tif(x > 1)\tbase::return(y)\n    x\n}" # TABs in the middle of a line
    fun_tab1 <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_tab1, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_tab1, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_tab2 <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_tab2, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_tab2, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_tab3 <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_tab3, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_tab3, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_tab1_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_tab1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    fun_tab2_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_tab2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    fun_tab3_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_tab3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_tab1_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[1:4], expected = base::c("function(x){", "\tif(x > 1) { base::return(x) }", "\telse { base::return(0) }", "}"), label = "TAB indentation: curly brackets at the right positions, TABs kept")
    testthat::expect_identical(object = utils::capture.output(fun_tab2_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[2], expected = "\ty <- \"a\tb\" ; if(x > 1) { base::return(y) }", label = "TAB inside a string before the body")
    testthat::expect_identical(object = utils::capture.output(fun_tab3_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[2], expected = "    y <- 1 ;\tif(x > 1)\t{ base::return(y) }", label = "TABs in the middle of a line")
    testthat::expect_identical(object = fun_tab1_safer(2), expected = fun_tab1(2))
    testthat::expect_identical(object = fun_tab1_safer(0), expected = fun_tab1(0))
    testthat::expect_identical(object = fun_tab2_safer(2), expected = fun_tab2(2))
    testthat::expect_identical(object = fun_tab3_safer(2), expected = fun_tab3(2))
    msgs_tab <- testthat::capture_messages(code = saferDev::curly_bracket_check(x = fun_tab1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res_tab <- base::unlist(x = base::strsplit(x = msgs_tab, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    res_tab <- res_tab[base::grepl(x = res_tab, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
    testthat::expect_identical(object = res_tab, expected = base::c("2\tif\t\tif(x > 1) base::return(x)", "3\telse\t\telse base::return(0)"), label = "TAB indentation: table of the findings")
    tempo_dir_tab <- base::tempfile(pattern = "cbc_tab_", tmpdir = base::tempdir(check = FALSE), fileext = "")
    base::dir.create(path = tempo_dir_tab, showWarnings = FALSE, recursive = FALSE, mode = "0777")
    base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_tab1, export = TRUE, out_path = tempo_dir_tab, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = base::readLines(con = base::file.path(tempo_dir_tab, "fun_tab1_safer.R", fsep = "/"), n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE), expected = base::c("fun_tab1_safer <- function(x){", "\tif(x > 1) { base::return(x) }", "\telse { base::return(0) }", "}"), label = "TAB indentation: exported file")
    base::unlink(x = tempo_dir_tab, recursive = TRUE, force = TRUE, expand = TRUE)
    ######## end export and out_path arguments

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # attributes of x, printed by R after its code: saferDev error that names them, raised before the code of x is recovered as printed (formerly, the printed attribute values were analyzed as code of x: raw error of the parser when they do not parse, e.g., a character string printed with its [1] prefix, and silent analysis when they parse, e.g., a formula or a function, with wrong results)
    fun_attr <- function(x){x}
    base::attr(x = fun_attr, which = "note") <- "hello"
    fun_attr_parse <- function(x){if(x > 1) x}
    base::attr(x = fun_attr_parse, which = "fo") <- y ~ x
    base::attr(x = fun_attr_parse, which = "fn") <- function(z){z}
    testthat::expect_error(object = saferDev::curly_bracket_check(x = fun_attr, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::curly_bracket_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nnote\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::curly_bracket_check(x = fun_attr_parse, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::curly_bracket_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nfo\nfn\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # attribute that is a call: not run (formerly, the last expression of the printed code, here the value of the attribute, was evaluated as the modified function)
    fun_attr_call <- function(x){if(x > 1) x}
    base::attr(x = fun_attr_call, which = "cl") <- base::quote(expr = base::stop("THE CODE OF THE ATTRIBUTE IS RUN", call. = FALSE, domain = NULL))
    testthat::expect_error(object = saferDev::curly_bracket_check(x = fun_attr_call, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::curly_bracket_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\ncl\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # export = TRUE: error, and no file written (formerly, the printed attribute lines were written in the exported file, which could then not be sourced)
    tempo_dir_attr <- base::tempfile(pattern = "curly_bracket_attr_", tmpdir = base::tempdir(check = FALSE), fileext = "")
    base::dir.create(path = tempo_dir_attr, showWarnings = FALSE, recursive = FALSE, mode = "0777")
    testthat::expect_error(object = saferDev::curly_bracket_check(x = fun_attr_parse, export = TRUE, out_path = tempo_dir_attr, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::curly_bracket_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nfo\nfn\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_attr, "fun_attr_parse_safer.R", fsep = "/")), info = NULL, label = NULL)
    base::unlink(x = tempo_dir_attr, recursive = TRUE, force = TRUE, expand = TRUE)
    # remedy given by the error message: once the attribute is removed, the function is checked. The attributes that print() does not show (source references and comment) are accepted
    base::attr(x = fun_attr, which = "note") <- NULL
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_attr, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = NULL)
    base::comment(x = fun_attr) <- "comment not printed by print()"
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_attr, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = NULL)
    # object inserted in the body of x that is not printed as R code: saferDev error with the message of the R parser (only its position is compared, because the parser messages are translated), instead of the raw error of the parser
    fun_body_env <- function(x){x}
    base::body(fun = fun_body_env, envir = base::environment(fun = fun_body_env)) <- base::call(name = "identity", base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L))
    testthat::expect_error(object = saferDev::curly_bracket_check(x = fun_body_env, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::curly_bracket_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS CODE, AS PRINTED BY R, DOES NOT PARSE (E.G., AN OBJECT INSERTED IN ITS BODY THAT IS NOT PRINTED AS R CODE, LIKE AN ENVIRONMENT, PRINTED <environment>).\n\nERROR MESSAGE OF THE R PARSER:\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::curly_bracket_check(x = fun_body_env, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "\n\nERROR MESSAGE OF THE R PARSER:\n<text>:[0-9]+:[0-9]+: ", class = NULL, fixed = FALSE, inherit = TRUE, info = NULL, label = NULL)
    # option keep.parse.data set to FALSE (e.g., by base::sys.source() during the loading of a file): the parse data are kept anyway (same result as with the default TRUE), and the option is restored on exit
    fun_kpd <- function(x){if(x > 1) base::return(x)}
    res_kpd_ref <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_kpd, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    oop_kpd <- base::options(keep.parse.data = FALSE)
    res_kpd <- base::tryCatch(expr = base::list(fun = base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_kpd, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), option = base::getOption(x = "keep.parse.data", default = NULL)), error = function(e){base::list(fun = e, option = NULL)}, finally = base::options(oop_kpd))
    testthat::expect_true(object = base::is.function(x = res_kpd$fun), info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(res_kpd$fun, file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(res_kpd_ref, file = NULL, append = FALSE, type = NULL, split = FALSE), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_false(object = res_kpd$option, info = NULL, label = NULL)
    # quoted code: the if, else, for, while, repeat and function constructs inside quote(), bquote(), expression(), alist() and substitute(), code that R does not evaluate as written, are neither reported nor modified, whatever the writing of the call (base::, base:::, backquotes, string, native pipe), with or without source references (formerly, quote(if(a) b) became quote(if(a) { b })). Warning: each function is built from its text with source references (source code checked), and without them (keep.source = FALSE and srcfile = NULL: deparsed code checked)
    tempo_txt_quote <- base::c(
        "function(){base::quote(expr = if(a) b)}",
        "function(){quote(function(x) x)}",
        "function(b){base::bquote(if(a) .(b) else c)}", # evaluated .() part of bquote()
        "function(){base::expression(while(TRUE) break, repeat break, \\(x) x)}",
        "function(){base::alist(a = , b = for(i in x) i)}",
        "function(){base::substitute(expr = if(a) b, env = base::list(a = 1))}", # expr argument of substitute()
        "function(){quote(base:::bquote(function(x){ if(x) y }))}", # nested quoting calls
        "function(){`quote`(if(a) b) ; \"expression\"(if(a) b) ; (if(a) b) |> quote()}", # backquoted and string function names, native pipe
        "function(a){base::bquote(if(a) 1 else 2)}", # expr argument of bquote()
        "function(a){y ~ if(a) 1}", # formula
        "function(){base::bquote(f(..(base::lapply(X = 1:2, FUN = function(i) i))))}", # ..() without splice = TRUE
        "function(a){base::substitute(if(a) 1, NULL, if(a) 2) ; base::bquote(x, w = if(a) 3, wh = NULL)}", # arguments of calls whose argument matching fails (error of R)
        "function(a){(if(a) 1 else 2) |> base::bquote(where = base::environment())}" # expr argument of bquote() written with the native pipe
    )
    for(i1 in base::seq_along(along.with = tempo_txt_quote)){
        for(tempo_keep in base::c(TRUE, FALSE)){
            fun_quote <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt_quote[i1], prompt = "?", keep.source = tempo_keep, srcfile = if(tempo_keep == TRUE){base::srcfilecopy(filename = "<text>", lines = tempo_txt_quote[i1], timestamp = base::Sys.time(), isFile = FALSE)}else{NULL}, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            tempo_label <- base::paste0(tempo_txt_quote[i1], " (source references: ", tempo_keep, ")", collapse = NULL, recycle0 = FALSE)
            testthat::expect_message(object = saferDev::curly_bracket_check(x = fun_quote, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = tempo_label)
            testthat::expect_null(object = base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_quote, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = tempo_label)
        }
    }
    # construct outside the quoting call, on the lines of a multi-line quoting call indented with TABs: still reported and corrected, while the quoted construct is kept, so that the corrected function returns the same quoted code
    tempo_txt_quote2 <- "function(x){\n\te <- base::quote(expr = if(x)\n\t\t1) ; if(x) 2\n\te\n}"
    fun_quote2 <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt_quote2, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt_quote2, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    msgs_quote2 <- testthat::capture_messages(code = saferDev::curly_bracket_check(x = fun_quote2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res_quote2 <- base::unlist(x = base::strsplit(x = msgs_quote2, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    res_quote2 <- res_quote2[base::grepl(x = res_quote2, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
    testthat::expect_identical(object = base::substr(x = res_quote2, start = 1, stop = 4), expected = "3\tif", info = NULL, label = NULL, expected.label = NULL)
    fun_quote2_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_quote2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_quote2_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[1:5], expected = base::c("function(x){", "\te <- base::quote(expr = if(x)", "\t\t1) ; if(x) { 2 }", "\te", "}"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_quote2_safer(x = TRUE), expected = fun_quote2(x = TRUE), info = NULL, label = NULL, expected.label = NULL)
    # construct whose unbraced body is a quoting call: the construct is corrected, its quoted code is kept
    fun_quote3 <- function(a){ if(a) quote(if(b) c) else bquote(while(d) e) }
    msgs_quote3 <- testthat::capture_messages(code = saferDev::curly_bracket_check(x = fun_quote3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res_quote3 <- base::unlist(x = base::strsplit(x = msgs_quote3, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    res_quote3 <- res_quote3[base::grepl(x = res_quote3, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
    testthat::expect_identical(object = base::vapply(X = base::strsplit(x = res_quote3, split = "\t", fixed = TRUE, perl = FALSE, useBytes = FALSE), FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1), USE.NAMES = FALSE), expected = base::c("if", "else"), info = NULL, label = NULL, expected.label = NULL)
    fun_quote3_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_quote3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = fun_quote3_safer(a = TRUE), expected = fun_quote3(a = TRUE), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_quote3_safer(a = FALSE), expected = fun_quote3(a = FALSE), info = NULL, label = NULL, expected.label = NULL)
    # string "quote" as the last token of the code (not a function name): no error, the function body is reported
    fun_quote4 <- function(x) "quote"
    testthat::expect_message(object = saferDev::curly_bracket_check(x = fun_quote4, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "SOME CURLY BRACKETS {} ARE MISSING AFTER", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    # code evaluated by R inside bquote() and substitute(): the first argument of the .() calls of bquote() (and of its ..() calls if splice = TRUE or T is written), the where and splice arguments of bquote() and the env argument of substitute(), whatever their writing (partial, quoted or backquoted argument names, positions, native pipe with or without the placeholder _), are evaluated: their constructs are reported and corrected, and the corrected function gives the same results (formerly, the whole argument list of these calls was skipped). Each function is built from its text with source references and without them
    tempo_txt_eval <- base::c(
        "function(a){base::bquote(.(if(a) 1 else 2) + x)}", # .() of bquote()
        "function(a){base::bquote(x + .(z), where = base::as.environment(x = base::list(z = if(a) 1 else 2)))}", # where argument of bquote()
        "function(a){base::substitute(expr = u, env = base::list(u = if(a) 1 else 2))}", # env argument of substitute()
        "function(a){base::bquote(f(..(base::lapply(X = 1:2, FUN = function(i) i))), splice = TRUE)}", # ..() with splice = TRUE
        "function(a){base::list(u = if(a) 1) |> base::substitute(expr = u, env = _)}", # env argument written with the placeholder
        "function(a){base::bquote(`expr` = .(if(a) 1), \"wh\" = base::environment())}", # argument names between backquotes and quotes, partial name
        "function(a){(if(a) 1 else 2) |> base::substitute(base::list(b = if(a) 3))}" # expr argument written with the native pipe (not checked), env argument written by position
    )
    tempo_kinds_eval <- base::list(base::c("if", "else"), base::c("if", "else"), base::c("if", "else"), "function", "if", "if", "if") # constructs reported in each function
    for(i1 in base::seq_along(along.with = tempo_txt_eval)){
        for(tempo_keep in base::c(TRUE, FALSE)){
            fun_eval <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt_eval[i1], prompt = "?", keep.source = tempo_keep, srcfile = if(tempo_keep == TRUE){base::srcfilecopy(filename = "<text>", lines = tempo_txt_eval[i1], timestamp = base::Sys.time(), isFile = FALSE)}else{NULL}, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            tempo_label <- base::paste0(tempo_txt_eval[i1], " (source references: ", tempo_keep, ")", collapse = NULL, recycle0 = FALSE)
            msgs_eval <- testthat::capture_messages(code = saferDev::curly_bracket_check(x = fun_eval, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
            res_eval <- base::unlist(x = base::strsplit(x = msgs_eval, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
            res_eval <- res_eval[base::grepl(x = res_eval, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
            testthat::expect_identical(object = base::vapply(X = base::strsplit(x = res_eval, split = "\t", fixed = TRUE, perl = FALSE, useBytes = FALSE), FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1), USE.NAMES = FALSE), expected = tempo_kinds_eval[[i1]], info = NULL, label = tempo_label, expected.label = NULL)
            fun_eval_safer <- base::suppressMessages(expr = saferDev::curly_bracket_check(x = fun_eval, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
            testthat::expect_message(object = saferDev::curly_bracket_check(x = fun_eval_safer, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = tempo_label)
            testthat::expect_identical(object = fun_eval_safer(a = TRUE), expected = fun_eval(a = TRUE), info = NULL, label = tempo_label, expected.label = NULL)
            testthat::expect_identical(object = fun_eval_safer(a = FALSE), expected = fun_eval(a = FALSE), info = NULL, label = tempo_label, expected.label = NULL)
        }
    }
    ## end other tests

})
