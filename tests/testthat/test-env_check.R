testthat::test_that("env_check()", {

    ## data argument values
    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(env_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_no_error(saferDev:::env_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_no_error(env_check()) # the function has default values for all args.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(env_check(pos = NULL, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(env_check(pos = int1, name = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(env_check(pos = character(), name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = integer(), name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = double(), name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = logical(), name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = complex(), name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = data.frame(), name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = list(), name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(env_check(pos = int1, name = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(env_check(pos = NA, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, in the saferDev::arg_check() calls, so the lib_path check is always executed)
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # pos
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = NULL, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = NA, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(env_check(pos = 1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = -1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = 1:2, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = c(TRUE, FALSE), name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = mat1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = factor1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = expr1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = fun1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end pos
    # name
    testthat::expect_no_error(env_check(pos = int1, name = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(env_check(pos = int1, name = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end name
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(env_check(pos = int1, name = "", safer_check = TRUE, lib_path = NULL, error_text = "")) 
    testthat::expect_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(env_check(pos = int1, name = str1, safer_check = TRUE, lib_path = NULL, error_text = "")) 
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
    # pos between 1 and the number of environments above env_check(): frames of the call stack, then search path (formerly, pos = 0 checked the frame of env_check() itself, and a pos beyond the last environment of the search path gave the R error "subscript out of bounds")
    testthat::expect_error(object = saferDev::env_check(pos = 0, name = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE pos ARGUMENT MUST BE BETWEEN 1 AND ", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    fun_pos_last <- function(){
        saferDev::env_check(pos = base::sys.nframe() + base::length(x = base::search()), name = NULL, safer_check = FALSE, lib_path = NULL, error_text = "") # package:base, the last environment of the search path
    }
    testthat::expect_null(object = fun_pos_last(), info = NULL, label = NULL)
    fun_pos_beyond <- function(){
        saferDev::env_check(pos = base::sys.nframe() + base::length(x = base::search()) + 1, name = NULL, safer_check = FALSE, lib_path = NULL, error_text = "")
    }
    testthat::expect_error(object = fun_pos_beyond(), regexp = "THE pos ARGUMENT MUST BE BETWEEN 1 AND ", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    rm(
        int1,
        str1,
        mat1,
        factor1,
        expr1,
        fun1
    )
    testthat::expect_no_error(env_check(pos = 1, name = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(env_check(pos = 2, name = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    mean <- 1
    t.test <- 1
    testthat::expect_no_error(env_check(pos = 1, name = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # pos = 2 to be in the .GlobalEnv
    # cannot test that below -> problem of env in CI
    # expected <- "SOME VARIABLES OF THE CHECKED ENVIRONMENT ARE ALSO PRESENT IN :\npackage:stats: t.test\npackage:base: mean\n"
    # testthat::expect_equal(result, expected)
    rm(
        mean,
        t.test
    )
    result <- env_check(pos = 1, name = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    expected <- NULL
    testthat::expect_equal(result, expected)
    # the environments above are those of the call stack (calling functions), not those of the lexical scope
    fun_caller <- function(){
        zz_name_test_env_check <- 1
        fun_called()
    }
    fun_called <- function(){
        zz_name_test_env_check <- 2
        saferDev::env_check(pos = 1, name = "fun_called", safer_check = FALSE, lib_path = NULL, error_text = "")
    }
    result <- fun_caller()
    testthat::expect_true(object = base::grepl(x = result, pattern = "SOME VARIABLES OF THE CHECKED ENVIRONMENT fun_called ARE ALSO PRESENT IN :\nfun_caller(): zz_name_test_env_check\n", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    testthat::expect_false(object = base::exists(x = "zz_name_test_env_check", where = -1, envir = base::environment(fun = fun_called), frame = NULL, mode = "any", inherits = TRUE), info = NULL, label = NULL) # not in the lexical scope of fun_called()
    # pos = 2: the frame one step further up (here, the frame of fun_caller_2())
    fun_caller_2 <- function(){
        mean <- 1
        fun_called_2()
    }
    fun_called_2 <- function(){
        saferDev::env_check(pos = 2, name = NULL, safer_check = FALSE, lib_path = NULL, error_text = "")
    }
    result <- fun_caller_2()
    testthat::expect_true(object = base::grepl(x = result, pattern = "SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :\npackage:base: mean\n", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    # checked environment reached through base::eval() and also present above (e.g., the global environment at the top level of code run by base::source() or knitr): not compared with itself, and named as in the search path (formerly UNNAMED, with all its objects reported as also present in itself)
    fun_eval <- function(){
        zz_name_test_env_check_eval <- 1
        base::eval(expr = base::quote(expr = saferDev::env_check(pos = 1, name = NULL, safer_check = FALSE, lib_path = NULL, error_text = "")), envir = base::environment(fun = NULL), enclos = base::baseenv())
    }
    testthat::expect_null(object = fun_eval(), info = NULL, label = NULL)
    zz_name_test_env_check_global <- 1 # also in the frame of these tests, located above the checked global environment
    base::assign(x = "zz_name_test_env_check_global", value = 2, pos = -1, envir = base::globalenv(), inherits = FALSE, immediate = TRUE)
    base::on.exit(expr = if(base::exists(x = "zz_name_test_env_check_global", where = -1, envir = base::globalenv(), frame = NULL, mode = "any", inherits = FALSE)){base::rm(list = "zz_name_test_env_check_global", pos = -1, envir = base::globalenv(), inherits = FALSE)}, add = TRUE, after = FALSE) # guarantee the removal of the object of the global environment, even if the call below fails
    result <- base::eval(expr = base::quote(expr = saferDev::env_check(pos = 1, name = NULL, safer_check = FALSE, lib_path = NULL, error_text = "")), envir = base::globalenv(), enclos = base::baseenv())
    base::rm(list = "zz_name_test_env_check_global", pos = -1, envir = base::globalenv(), inherits = FALSE)
    testthat::expect_true(object = base::grepl(x = result, pattern = "SOME VARIABLES OF THE CHECKED ENVIRONMENT .GlobalEnv ARE ALSO PRESENT IN :\n", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    testthat::expect_false(object = base::grepl(x = result, pattern = "\n.GlobalEnv: ", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL) # not compared with itself
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
