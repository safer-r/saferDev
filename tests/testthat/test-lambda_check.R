# This file tests lambda_check()
# Same structure as test-curly_bracket_check.R
# Warning: the lambda writings \(x) are only kept in the code of a function carrying srcref (see the lambda_check() @details). The functions containing lambda writings are therefore built with base::parse(text = ..., keep.source = TRUE) followed by base::eval(), to be sure that the srcref attribute is present, whatever the value of base::getOption("keep.source")

testthat::test_that("lambda_check()", {

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
    # functions without lambda writings (clean cases)
    fun3 <- function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) }
    fun4 <- function(x){ base::sapply(X = 1:3, FUN = function(y) y * 2) }
    fun5 <- function(x){ if(x > 1){ x <- "the \\() writing is just a string" } ; x } # strings are immune
    fun6 <- function(x){ if(x > 1){ y <- 2 # a \(x) writing in a comment is ignored
    } ; x } # comments are ignored
    fun7 <- eval(parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\(y){ y * 2 }) }", keep.source = TRUE)) # lambda with braced body
    fun8 <- eval(parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\(y) y * 2) }", keep.source = TRUE)) # lambda with single-expression body
    fun9 <- eval(parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\() x * 2) }", keep.source = TRUE)) # lambda without formals
    fun10 <- eval(parse(text = "function(x){ fun_return <- \\(a, b){ a + b } ; fun_return(x, 1) }", keep.source = TRUE)) # lambda on the right side of an assignment
    fun11 <- eval(parse(text = "function(x){ base::`\\\\`(x) }", keep.source = TRUE)) # backquoted `\\` call: not a lambda writing, immune
    fun12 <- eval(parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\(y){\n        y * 2\n    }) }", keep.source = TRUE)) # multi-line lambda body
    fun13 <- eval(parse(text = "function(x){ base::lapply(X = x, FUN = \\(y){ y + 1 }) ; base::sapply(X = x, FUN = \\(z){ z - 1 }) }", keep.source = TRUE)) # two lambda writings
    fun14 <- eval(parse(text = "function(x){ x }", keep.source = TRUE)) # clean function with srcref
    fun15 <- base::mean # compiled function: no srcref, message normalization test
    fun16 <- function(x) x # function without braces: clean for lambda_check()
    fun19 <- eval(parse(text = "function(x){ y <- \"the text \\\\(x) is inside a string\" ; z <- r\"(raw \\(x))\" ; # a \\(x) in a comment\n    x }", keep.source = TRUE)) # escaped bracket writings in a string, a raw string and a comment: immune
    fun20 <- eval(parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\(y) y) ; z <- \"\\\\(\" ; x }", keep.source = TRUE)) # real lambda on the same line as a string containing \\(: only the lambda is reported and replaced
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(lambda_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(lambda_check(x = fun14, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::lambda_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(lambda_check()) # the function has default values for all args except x.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(lambda_check(x = NULL, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = NULL, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(lambda_check(x = character(), export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = integer(), export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = double(), export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = logical(), export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = complex(), export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = data.frame(), export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = list(), export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(lambda_check(x = fun14, export = character(), out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = integer(), out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = double(), out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = logical(), out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = complex(), out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = data.frame(), out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = list(), out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(lambda_check(x = NA, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = NA, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.0: unguarded lib_path check)
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = NULL, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = NA, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = 1, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = -1, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = 1:2, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = c(TRUE, FALSE), export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = mat1, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = factor1, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = expr1, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_message(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # clean function with srcref
    # end x
    # export
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = TRUE, out_path = base::tempdir(check = TRUE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = 1, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = "TRUE", out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = c(TRUE, FALSE), out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = mat1, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = factor1, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = expr1, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = fun1, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end export
    # out_path
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end out_path
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(lambda_check(x = fun14, export = FALSE, out_path = "", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(lambda_check(x = fun14, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    # detection of lambda writings
    testthat::expect_message(lambda_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # function() writing
    testthat::expect_message(lambda_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # function() single-expression writing
    testthat::expect_message(lambda_check(x = fun5, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # strings are immune
    testthat::expect_message(lambda_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # comments are ignored
    testthat::expect_message(lambda_check(x = fun16, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # function without braces
    testthat::expect_message(lambda_check(x = curly_bracket_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # self check
    testthat::expect_message(lambda_check(x = colons_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(lambda_check(x = arg_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(lambda_check(x = base::mean, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE base::mean(), EVERYTHING SEEMS CLEAN", fixed = TRUE) # message normalization of pkg::fun
    testthat::expect_message(lambda_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function()", fixed = TRUE)
    testthat::expect_message(lambda_check(x = fun8, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function()", fixed = TRUE)
    testthat::expect_message(lambda_check(x = fun9, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function()", fixed = TRUE) # lambda without formals
    testthat::expect_message(lambda_check(x = fun10, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function()", fixed = TRUE) # right side of an assignment
    testthat::expect_message(lambda_check(x = fun11, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # backquoted `\\` immune
    testthat::expect_message(lambda_check(x = fun12, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function()", fixed = TRUE) # multi-line lambda body
    testthat::expect_message(lambda_check(x = fun13, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function()", fixed = TRUE) # two lambda writings
    testthat::expect_message(lambda_check(x = fun19, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # escaped bracket writings in a string, a raw string and a comment: immune
    testthat::expect_message(lambda_check(x = fun20, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function()", fixed = TRUE) # real lambda reported on a line also containing a string with \\(
    # exact findings for a representative case (only the LINE and SYMBOL columns are compared, because the CODE column depends on the indentation of the checked function)
    msgs <- testthat::capture_messages(lambda_check(x = fun13, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res <- base::unlist(x = base::strsplit(x = msgs, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res_fields <- base::strsplit(x = res, split = "\t", fixed = TRUE)
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[1]}, FUN.VALUE = base::character(length = 1)), y = base::c("1", "1")), label = "fun13 exact line numbers")
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1)), y = base::c("\\(y)", "\\(z)")), label = "fun13 exact symbols")
    #### end main code

    ######## export and out_path arguments
    # default: modified function returned, NULL when clean
    testthat::expect_null(object = lambda_check(x = fun14, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")) # clean function: NULL
    testthat::expect_true(object = base::is.function(x = lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."))) # modified function returned
    # the modified function passes the check itself and behaves correctly
    fun7_safer <- lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_message(lambda_check(x = fun7_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_identical(object = fun7_safer(2), expected = fun7(2))
    # the multi-line lambda is also corrected and behaves identically
    fun12_safer <- lambda_check(x = fun12, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_message(lambda_check(x = fun12_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_identical(object = fun12_safer(2), expected = fun12(2))
    # export = TRUE: file written, NULL returned, no file when clean
    tempo_dir <- base::tempdir(check = TRUE)
    testthat::expect_message(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun7_safer.R")))
    # the exported file contains an assignment and can be sourced to recreate the function
    tempo_lines <- base::readLines(con = base::file.path(tempo_dir, "fun7_safer.R"), n = 5, warn = FALSE, ok = TRUE)
    testthat::expect_true(object = base::grepl(x = tempo_lines[1], pattern = "^fun7_safer <- ", perl = TRUE), label = "exported file starts with assignment")
    tempo_env <- base::new.env(parent = base::globalenv())
    base::sys.source(file = base::file.path(tempo_dir, "fun7_safer.R"), envir = tempo_env, chdir = FALSE)
    testthat::expect_identical(object = tempo_env$fun7_safer(2), expected = fun7(2))
    # pre-existing file is never overwritten
    testthat::expect_error(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir), regexp = "FILE NAME ALREADY EXISTS", fixed = TRUE)
    base::unlink(x = base::file.path(tempo_dir, "fun7_safer.R"))
    testthat::expect_null(object = lambda_check(x = fun14, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir))
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir, "fun14_safer.R")))
    # custom out_path
    tempo_dir2 <- base::file.path(base::tempdir(check = TRUE), "sub_lambda_check")
    base::dir.create(path = tempo_dir2, showWarnings = FALSE, recursive = FALSE, mode = "0777")
    testthat::expect_message(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir2), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    base::unlink(x = base::file.path(tempo_dir2, "fun7_safer.R"))
    # trailing slash tolerated (forward slash and backslash)
    testthat::expect_message(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = base::paste0(tempo_dir2, "/")), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    base::unlink(x = base::file.path(tempo_dir2, "fun7_safer.R"))
    testthat::expect_message(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = base::paste0(tempo_dir2, "\\")), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE) # trailing backslash tolerated
    base::unlink(x = base::file.path(tempo_dir2, "fun7_safer.R"))
    # non-existing out_path
    testthat::expect_error(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = "PATH_NOT_GOOD"), regexp = "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXISTS", fixed = TRUE)
    # out_path ignored when export = FALSE (even a non-existing path)
    testthat::expect_no_error(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "PATH_NOT_GOOD"))
    # out_path = NULL is invalid, whatever the value of export
    testthat::expect_error(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = NULL), regexp = "CANNOT BE NULL", fixed = TRUE)
    testthat::expect_error(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = NULL), regexp = "CANNOT BE NULL", fixed = TRUE)
    # export = TRUE with out_path = ".": exports in the current working directory (restored after the test, whatever the test result)
    tempo_wd <- base::getwd()
    base::eval(expr = base::bquote(expr = on.exit(expr = base::setwd(dir = .(tempo_wd)), add = TRUE, after = FALSE), where = base::environment(fun = NULL)))
    base::setwd(dir = tempo_dir)
    testthat::expect_message(lambda_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = "."), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun7_safer.R")))
    base::setwd(dir = tempo_wd)
    base::unlink(x = base::file.path(tempo_dir, "fun7_safer.R"))
    # the returned modified function keeps the closure environment of the submitted function
    fun_closure <- eval(parse(text = "function(x){ z <- 10 ; if(x > 1){ base::sapply(X = 1:3, FUN = \\(y){ z + y })[1] } }", keep.source = TRUE))
    fun_closure_safer <- lambda_check(x = fun_closure, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_identical(object = fun_closure_safer(2), expected = fun_closure(2))
    testthat::expect_identical(object = base::environment(fun = fun_closure_safer), expected = base::environment(fun = fun_closure))
    # inline definition and component writing are refused when export = TRUE (the name of the exported file could not be determined reliably)
    testthat::expect_error(lambda_check(x = function(y){ y + 1 }, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    FUN_LIST <- base::list(fun = fun7)
    testthat::expect_error(lambda_check(x = FUN_LIST$fun, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    # file name determination: written as pkg::fun, the package name is not part of the file name (base::noquote has no lambda writing, so no file is written)
    testthat::expect_null(object = lambda_check(x = base::noquote, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = tempo_dir))
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir, "noquote_safer.R")), label = "no file written for a clean pkg::fun form")
    ######## end export and out_path arguments

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # the corrected function of a lambda with a modified code that still parses (paranoid internal check is not triggered)
    fun13_safer <- lambda_check(x = fun13, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_message(lambda_check(x = fun13_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_identical(object = fun13_safer(1:3), expected = fun13(1:3))
    # lambda whose formals span several lines: the SYMBOL column falls back to the \() writing (see the code comment of the symbol recovery), and the whole writing is still replaced correctly
    fun17 <- eval(parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\(a,\n    b){ a })[1] }", keep.source = TRUE))
    testthat::expect_message(lambda_check(x = fun17, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function()", fixed = TRUE)
    msgs17 <- testthat::capture_messages(lambda_check(x = fun17, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res17 <- base::unlist(x = base::strsplit(x = msgs17, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res17 <- res17[base::grepl(x = res17, pattern = "^[0-9]+\\t", perl = TRUE)]
    testthat::expect_true(object = base::grepl(x = res17[1], pattern = "^1\\t\\\\\\(\\)", perl = TRUE), label = "fun17 multi-line formals fallback symbol")
    fun17_safer <- lambda_check(x = fun17, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_message(lambda_check(x = fun17_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_identical(object = fun17_safer(1:3), expected = fun17(1:3))
    # lambda written on a line other than the first one of the recovered code: the reported line number and the replacement columns are those of the lambda line
    fun18 <- eval(parse(text = "function(x){\n    if(x > 1){\n        base::sapply(X = 1:3, FUN = \\(y){ y * x })\n    } ; NULL\n}", keep.source = TRUE))
    msgs18 <- testthat::capture_messages(lambda_check(x = fun18, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res18 <- base::unlist(x = base::strsplit(x = msgs18, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res18 <- res18[base::grepl(x = res18, pattern = "^[0-9]+\\t", perl = TRUE)]
    testthat::expect_true(object = base::grepl(x = res18[1], pattern = "^3\\t", perl = TRUE), label = "fun18 lambda on line 3 of the recovered code")
    fun18_safer <- lambda_check(x = fun18, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_message(lambda_check(x = fun18_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_identical(object = fun18_safer(2), expected = fun18(2))
    # escaped bracket writings: exactly one lambda reported for fun20 (the string "\\(" on the same line is not reported) and the string is kept verbatim in the corrected function
    msgs20 <- testthat::capture_messages(lambda_check(x = fun20, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res20 <- base::unlist(x = base::strsplit(x = msgs20, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res20 <- res20[base::grepl(x = res20, pattern = "^[0-9]+\\t", perl = TRUE)]
    testthat::expect_true(object = base::length(x = res20) == 1, label = "fun20 exactly one reported lambda (the string \\( is not reported)")
    testthat::expect_true(object = base::grepl(x = res20[1], pattern = "1\\t\\\\\\(y\\)", perl = TRUE), label = "fun20 reported symbol is the lambda formals, not the string")
    fun20_safer <- lambda_check(x = fun20, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
    testthat::expect_true(object = base::any(base::grepl(x = base::deparse(expr = base::body(fun = fun20_safer), width.cutoff = 500L, nlines = -1L), pattern = 'z <- "\\\\("', fixed = TRUE)), label = "fun20 string \\( kept verbatim in the corrected function")
    testthat::expect_message(lambda_check(x = fun20_safer, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_identical(object = fun20_safer(1), expected = fun20(1))
    # fun19 (escaped brackets in string, raw string and comment): clean and behavior unchanged
    testthat::expect_null(object = lambda_check(x = fun19, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = "."))
    testthat::expect_identical(object = fun19(1), expected = fun19(1))
    ## end other tests
    # restore the working directory (on.exit() of the test block is evaluated after the rm() above)
    base::setwd(dir = tempo_wd)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests

})
