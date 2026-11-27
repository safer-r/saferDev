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
    fun8 <- function(x){
        return(NULL)
    }
    fun9 <- function(x){
        b <- c("a", "b")
    }
    # a function calling only itself: the recursive name is filtered out, leaving no other function
    fun_rec <- function(x){
        if(x > 0){
            fun_rec(x = x - 1)
        }
    }
    # a function with an internal function whose name is also a basic R function name: the basic occurrence must be skipped during the modification
    fun_skip <- function(x){
        sum <- function(y){y}
        sum(1)
    }

    # A function calling only non-base functions (assuming dplyr is available)
    fun_non_base <- function(data){
        ggplot2::ggplot(mtcars, aes(x = mpg, y = wt)) + ggplot2::geom_point()  # 'filter' is not a base function
    }

    # OR a function calling only user-defined functions (not in base)
    my_helper <- function(x) {x * 2}
    fun_custom <- function(){
        my_helper(5)  # 'my_helper' is not a base R function
    }
    # a list containing a function, to test the multi-component written name fallback of the exported file name
    FUN_LIST <- base::list(fun = fun9)
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
        tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS OF col1 (", base::length(roc1()), "), col2 (", base::length(roc2), "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")
        result <- sub("\\($##", "", matched_strings) ; range(1:3) # sub("\\($##", "", matched_strings)
        tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(roc4()), "), and roc0 (", base::length(roc5), ") SHOULL\n")
        return(baba) # base::sub("\\($##", "", matched_strings)
        base::return(bobo) # a$sub("\\($##", "", matched_strings)
    }

    ## end data argument values

    ## initialization of tests
    testthat::expect_error(colons_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)) # to test that this example works
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
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = NULL), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    # out_path can be NULL (default value)
    testthat::expect_identical(object = base::formals(fun = saferDev::colons_check, envir = base::parent.frame(n = 1))$out_path, expected = NULL)
    testthat::expect_no_error(object = base::suppressMessages(expr = saferDev::colons_check(x = fun9, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), classes = "message"))
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
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = character()), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)) # but error_text is converted to ""
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1"), regexp = "FROM saferDev::colons_check() INSIDE P1::F1\n\n(1) INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)) # documented non-empty error_text usage (added to error and warning messages)
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
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
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, in the saferDev::arg_check() calls, so the lib_path check is always executed)
    testthat::expect_error(colons_check(x = test, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    testthat::expect_error(colons_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_warning(colons_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "IS EMPTY OF FUNCTION", fixed = TRUE) # empty function: warning, not error
    testthat::expect_null(object = base::suppressWarnings(colons_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""), classes = "warning"))
    # end x
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(colons_check(x = test, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE test(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
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
    # lambda pre-check: a function carrying srcref and containing a \(y) lambda writing is refused
    fun_lambda <- base::eval(expr = base::parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\(y){ y * 2 }) ; base::return(x) }", keep.source = TRUE))
    testthat::expect_error(colons_check(x = fun_lambda, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "RUN saferDev::lambda_check", fixed = TRUE)
    # a function without srcref (compiled-style writing) prints function(y), which is not detected by lambda_check(): the pre-check silently passes
    fun_lambda_compiled <- base::eval(expr = base::parse(text = "function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) ; base::return(x) }", keep.source = FALSE))
    testthat::expect_no_error(colons_check(x = fun_lambda_compiled, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # a function containing only the "\\(y)" string literal (STR_CONST token, not a lambda writing) passes the pre-check
    fun_lambda_string <- base::eval(expr = base::parse(text = "function(x){ base::sapply(X = 1:3, FUN = function(y){ base::paste0(\"\\\\(y)\") }) ; base::return(x) }", keep.source = TRUE))
    testthat::expect_no_error(colons_check(x = fun_lambda_string, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = base::mean, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = colons_check, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_warning(colons_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "IS EMPTY OF FUNCTION", fixed = TRUE)
    testthat::expect_no_error(colons_check(x = fun2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun3(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    testthat::expect_no_error(colons_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun7(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))

    testthat::expect_no_error(colons_check(x = fun8, safer_check = FALSE, lib_path = NULL, error_text = ""))


    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = fun_non_base), regexp = "INSIDE fun_non_base(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))
    testthat::expect_no_error(testthat::expect_warning(object = colons_check(x = fun_custom), regexp = "INSIDE fun_custom(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL))

    # function calling only itself (recursive): the recursive call is a call of an internal function, neither modified nor reported, so NULL is returned without warning
    testthat::expect_message(object = fun_rec_safer <- saferDev::colons_check(x = fun_rec, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun_rec(), EVERYTHING SEEMS CLEAN.", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_null(object = fun_rec_safer, info = NULL, label = NULL)
    testthat::expect_no_warning(object = base::suppressMessages(expr = saferDev::colons_check(x = fun_rec, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), message = NULL, class = NULL)
    # function containing an internal function whose name is also a basic R function name: the basic occurrence is skipped (no modification of the internal one), and NULL is returned
    testthat::expect_null(object = colons_check(x = fun_skip, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # function named like a basic R function and calling itself: the recursive call must NOT be modified (the base:: prefix would redirect the recursion to the basic function), while the other basic functions are modified
    sum_test2 <- function(x){
        sum_test2(x)
        sum(1:2)
    }
    testthat::expect_no_warning(object = sum_test2_safer <- base::suppressMessages(expr = saferDev::colons_check(x = sum_test2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), message = NULL, class = NULL) # the recursive call is a call of an internal function: not reported
    sum_test2_body <- base::deparse(expr = base::body(fun = sum_test2_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)
    testthat::expect_false(object = base::any(base::grepl(x = sum_test2_body, pattern = "base::sum_test2\\(", perl = TRUE), na.rm = TRUE), label = "recursive call not modified")
    testthat::expect_true(object = base::any(base::grepl(x = sum_test2_body, pattern = "base::sum\\(", perl = TRUE), na.rm = TRUE), label = "basic sum call modified")

    # comments, indentation and multiple spaces of the submitted function are kept in the returned and exported functions: the package attributions are inserted in the printed code, not in the analyzed code (comments removed and multiple spaces collapsed)
    # warning: the short parse writing is required here, since a fully-named writing with srcfile = NULL removes the srcref of the parsed code (thus the comments)
    fun_com <- base::eval(expr = base::parse(text = "function(x){\n    # a comment with mean(\n    y   <-   mean(x = x)   # trailing   sum(1)\n        z <- \"a  b\" ; paste0(y, z)\n}", keep.source = TRUE)[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_com_expected <- base::c("function(x){", "    # a comment with mean(", "    y   <-   base::mean(x = x)   # trailing   sum(1)", "        z <- \"a  b\" ; base::paste0(y, z)", "}")
    fun_com_safer <- colons_check(x = fun_com, safer_check = FALSE, lib_path = NULL, error_text = "")
    fun_com_print <- utils::capture.output(fun_com_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    testthat::expect_identical(object = fun_com_print[1:5], expected = fun_com_expected, label = "comments and indentation kept in the returned function")
    testthat::expect_identical(object = fun_com_safer(1:3), expected = fun_com(1:3), label = "returned function behaves like the submitted one")
    tempo_dir_com <- base::tempfile(pattern = "cc_com_")
    base::dir.create(path = tempo_dir_com, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    base::suppressMessages(expr = colons_check(x = fun_com, export = TRUE, out_path = tempo_dir_com, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    fun_com_lines <- base::readLines(con = base::file.path(tempo_dir_com, "fun_com_safer.R", fsep = "/"), warn = FALSE, ok = TRUE)
    testthat::expect_identical(object = fun_com_lines[1:5], expected = base::c(base::paste0("fun_com_safer <- ", fun_com_expected[1], collapse = NULL, recycle0 = FALSE), fun_com_expected[2:5]), label = "comments and indentation kept in the exported file")
    base::unlink(x = tempo_dir_com, recursive = TRUE, force = TRUE, expand = TRUE)

    ######## export and out_path arguments
    # out_path cannot contain an empty string, whatever the value of export
    testthat::expect_error(colons_check(x = fun9, out_path = "", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    # out_path = NULL (default): no error when export = FALSE, error when export = TRUE (no default path for the written file)
    testthat::expect_no_error(object = base::suppressMessages(expr = saferDev::colons_check(x = fun9, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    testthat::expect_error(object = saferDev::colons_check(x = fun9, export = TRUE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN export IS TRUE.", fixed = TRUE)
    # export argument values
    testthat::expect_error(colons_check(x = fun9, export = NA, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = "TRUE", safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = base::c(TRUE, FALSE), safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = mat1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = factor1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = expr1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = fun1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun9, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # out_path values (checked when not NULL, whatever the value of export)
    testthat::expect_error(object = saferDev::colons_check(x = fun9, export = FALSE, out_path = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = factor1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = expr1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = fun1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = base::c("a", "b"), safer_check = FALSE, lib_path = NULL, error_text = ""))
    # out_path not used when export = FALSE (even a non-existing path)
    testthat::expect_no_error(colons_check(x = fun9, export = FALSE, out_path = "PATH_NOT_GOOD", safer_check = FALSE, lib_path = NULL, error_text = ""))
    # export = TRUE with a function to modify: file written in a fresh temporary directory, NULL returned (invisible), and the file recreates the function when sourced
    tempo_dir_colons <- base::tempfile(pattern = "cc_export_")
    base::dir.create(path = tempo_dir_colons, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    testthat::expect_null(object = colons_check(x = fun9, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""), label = "export TRUE returns NULL")
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_colons, "fun9_safer.R", fsep = "/")), label = "fun9_safer.R written")
    exported_lines <- base::readLines(con = base::file.path(tempo_dir_colons, "fun9_safer.R", fsep = "/"), warn = FALSE, ok = TRUE)
    testthat::expect_true(object = base::grepl(x = exported_lines[1], pattern = "^fun9_safer <- ", perl = TRUE), label = "exported file starts with the function writing")
    testthat::expect_true(object = base::any(base::grepl(x = exported_lines, pattern = "base::c\\(", perl = TRUE), na.rm = TRUE), label = "exported code contains base::c")
    tempo_env_colons <- base::new.env(parent = base::globalenv())
    base::sys.source(file = base::file.path(tempo_dir_colons, "fun9_safer.R", fsep = "/"), envir = tempo_env_colons, chdir = FALSE)
    testthat::expect_identical(object = tempo_env_colons$fun9_safer(1), expected = fun9(1), label = "exported function behaves like the submitted one")
    # pre-existing file is never overwritten
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "FILE NAME ALREADY EXISTS", fixed = TRUE)
    base::unlink(x = base::file.path(tempo_dir_colons, "fun9_safer.R", fsep = "/"))
    # clean function (only non-basic functions, all with colons): no file written, NULL returned
    testthat::expect_null(object = colons_check(x = fun2, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_colons, "fun2_safer.R", fsep = "/")))
    # file name determination: written as pkg::fun, the package name is not part of the file name (mean is not clean: its code contains basic functions, so a file is written)
    testthat::expect_null(object = colons_check(x = base::mean, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_colons, "mean_safer.R", fsep = "/")), label = "mean_safer.R written for pkg::fun form")
    base::unlink(x = base::file.path(tempo_dir_colons, "mean_safer.R", fsep = "/"))
    # file name determination: multi-component written name like FUN_LIST$fun is refused when export = TRUE (the written form is not reliable for the file name)
    testthat::expect_error(colons_check(x = FUN_LIST$fun, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_colons, "fun_safer.R", fsep = "/")), label = "no fun_safer.R written for FUN_LIST$fun")
    # trailing / and \\ tolerated in out_path
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::colons_check(x = fun_rec, export = TRUE, out_path = base::paste0(tempo_dir_colons, "/", collapse = NULL, recycle0 = FALSE), safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = NULL) # clean function (recursive call only, call of an internal function): no file written
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_colons, "fun_rec_safer.R", fsep = "/")))
    testthat::expect_null(object = colons_check(x = fun9, export = TRUE, out_path = base::paste0(tempo_dir_colons, "\\"), safer_check = FALSE, lib_path = NULL, error_text = "")) # trailing backslash tolerated, file written
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_colons, "fun9_safer.R", fsep = "/")))
    # non-existing out_path
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = "PATH_NOT_GOOD", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXIST:\n", fixed = TRUE)
    # out_path with a trailing /: the trailing / is removed first, then the path checked (the error message shows the path without the trailing /)
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = "PATH_NOT_GOOD/", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "DOES NOT EXIST:\nPATH_NOT_GOOD\n", fixed = TRUE)
    # export message: the file location is announced by a message, and the returned value is invisible
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "FILE NAME ALREADY EXISTS", fixed = TRUE) # the file fun9_safer.R was rewritten just above, so a new error is expected
    base::unlink(x = base::file.path(tempo_dir_colons, "fun9_safer.R", fsep = "/"))
    testthat::expect_message(colons_check(x = fun9, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    # inline definition and component writing are refused when export = TRUE (the name of the exported file could not be determined reliably)
    testthat::expect_error(colons_check(x = function(y){ base::c(y) }, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    testthat::expect_error(colons_check(x = FUN_LIST$fun, export = TRUE, out_path = tempo_dir_colons, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    # cleanup
    base::unlink(x = tempo_dir_colons, recursive = TRUE, force = TRUE, expand = TRUE)
    ######## end export and out_path arguments

    ######## resolution of the basic function names as R does when running x (masking)
    # masking by an attached package (like dplyr::filter masking stats::filter): the masked name is not modified but reported in the OTHER section of the message and in the warning, while the other basic functions are modified
    fun_mask <- function(df){
        y <- filter(df)
        z <- mean(1:2)
        y
    }
    fake_pack <- base::list(filter = function(.data, ...){"fake filter"})
    tempo_env_cond <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L) # caught message and warning (assign() into an environment, no <<-)
    base::attach(what = fake_pack, pos = 2L, name = "package:saferDev_test_fake_pack", warn.conflicts = FALSE)
    base::tryCatch(
        expr = {
            fun_mask_safer <- base::withCallingHandlers(
                expr = saferDev::colons_check(x = fun_mask, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""),
                message = function(m){
                    base::assign(x = "message", value = base::conditionMessage(c = m), pos = -1, envir = tempo_env_cond, inherits = FALSE, immediate = TRUE)
                    base::invokeRestart(r = "muffleMessage")
                },
                warning = function(w){
                    base::assign(x = "warning", value = base::conditionMessage(c = w), pos = -1, envir = tempo_env_cond, inherits = FALSE, immediate = TRUE)
                    base::invokeRestart(r = "muffleWarning")
                }
            )
            testthat::expect_identical(object = fun_mask_safer(1), expected = "fake filter", label = "the returned function still calls the masking function")
        },
        finally = base::detach(name = "package:saferDev_test_fake_pack", pos = 2L, unload = FALSE, character.only = TRUE, force = FALSE)
    )
    testthat::expect_false(object = "package:saferDev_test_fake_pack" %in% base::search(), label = "fake package detached")
    fun_mask_body <- base::deparse(expr = base::body(fun = fun_mask_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)
    testthat::expect_true(object = base::any(base::grepl(x = fun_mask_body, pattern = "y <- filter(df)", fixed = TRUE), na.rm = TRUE), label = "masked filter() not modified")
    testthat::expect_true(object = base::any(base::grepl(x = fun_mask_body, pattern = "z <- base::mean(1:2)", fixed = TRUE), na.rm = TRUE), label = "basic mean() modified")
    testthat::expect_true(object = base::grepl(x = tempo_env_cond$message, pattern = "BEFORE BASIC FUNCTIONS:\n\nLINE\tFUN\t\tSTRING_BEFORE\n[0-9]+\tmean\t\tz <- \n\nINSIDE fun_mask\\(\\), SOME :: OR ::: ARE MISSING BEFORE OTHER FUNCTIONS:\n\nLINE\tFUN\t\tSTRING_BEFORE\n[0-9]+\tfilter\t\ty <- \n", fixed = FALSE, perl = TRUE), label = "filter in the OTHER section of the message, not in the BASIC section")
    testthat::expect_true(object = base::grepl(x = tempo_env_cond$warning, pattern = "AMONG THEM, THESE NAMES OF R BASIC FUNCTIONS ARE MASKED: THE FUNCTION CALLED WHEN RUNNING fun_mask() IS ANOTHER FUNCTION (ATTACHED PACKAGE, IMPORT OR USER DEFINITION):\nfilter\n", fixed = TRUE), label = "masked filter in the warning")
    # masking by a function of the environment of x (like a function imported in a package namespace)
    fun_env_mask <- function(df){
        y <- filter(df)
        z <- mean(1:2)
        y
    }
    env_mask <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
    base::assign(x = "filter", value = function(.data, ...){"imported filter"}, pos = -1, envir = env_mask, inherits = FALSE, immediate = TRUE)
    base::environment(fun = fun_env_mask) <- env_mask
    testthat::expect_warning(object = fun_env_mask_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_env_mask, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "ARE MASKED: THE FUNCTION CALLED WHEN RUNNING fun_env_mask() IS ANOTHER FUNCTION (ATTACHED PACKAGE, IMPORT OR USER DEFINITION):\nfilter\n", fixed = TRUE)
    fun_env_mask_body <- base::deparse(expr = base::body(fun = fun_env_mask_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)
    testthat::expect_true(object = base::any(base::grepl(x = fun_env_mask_body, pattern = "y <- filter(df)", fixed = TRUE), na.rm = TRUE), label = "filter() of the environment of x not modified")
    testthat::expect_true(object = base::any(base::grepl(x = fun_env_mask_body, pattern = "z <- base::mean(1:2)", fixed = TRUE), na.rm = TRUE), label = "basic mean() modified")
    testthat::expect_identical(object = base::environment(fun = fun_env_mask_safer), expected = env_mask, label = "environment of x kept")
    testthat::expect_identical(object = fun_env_mask_safer(1), expected = "imported filter", label = "the returned function still calls the function of its environment")
    # a name of the environment of x bound to the basic function itself is not masked: the prefix is added
    fun_env_copy <- function(x){
        mean(x)
    }
    env_copy <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
    base::assign(x = "mean", value = base::mean, pos = -1, envir = env_copy, inherits = FALSE, immediate = TRUE)
    base::environment(fun = fun_env_copy) <- env_copy
    testthat::expect_no_warning(object = fun_env_copy_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_env_copy, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    testthat::expect_true(object = base::any(base::grepl(x = base::deparse(expr = base::body(fun = fun_env_copy_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), pattern = "base::mean(x)", fixed = TRUE), na.rm = TRUE), label = "mean bound to base::mean prefixed")
    # the package of the prefix is the one of the function found when running x: kronecker is in the methods and base packages (different functions)
    fun_kro <- function(x){
        kronecker(x, x)
    }
    base::environment(fun = fun_kro) <- base::globalenv() # search path: package:methods before package:base
    fun_kro_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_kro, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::any(base::grepl(x = base::deparse(expr = base::body(fun = fun_kro_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), pattern = "methods::kronecker(x, x)", fixed = TRUE), na.rm = TRUE), label = "methods::kronecker from the global environment")
    base::environment(fun = fun_kro) <- base::baseenv() # only the base package
    fun_kro_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_kro, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::any(base::grepl(x = base::deparse(expr = base::body(fun = fun_kro_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), pattern = "base::kronecker(x, x)", fixed = TRUE), na.rm = TRUE), label = "base::kronecker from the base environment")
    # a name whose object cannot be evaluated from the environment of x (promise of an enclosing function returning an error) is processed like the names of the other functions: not modified, and reported
    make_fun_err <- function(mean){
        function(x){
            mean(x)
        }
    }
    fun_err <- make_fun_err(mean = base::stop("PROMISE ERROR", call. = FALSE, domain = NULL))
    testthat::expect_warning(object = fun_err_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_err, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:\n\nLINE\tFUN\t\tSTRING_BEFORE\n[0-9]+\tmean\t\t\n", fixed = FALSE, perl = TRUE)
    testthat::expect_null(object = fun_err_safer)
    ######## end resolution of the basic function names as R does when running x (masking)

    ######## names starting by a dot, primitive functions
    # the added prefix is always :: (all the basic functions are exported: R CMD check reports ::: before an exported function)
    fun_dot <- function(x){
        a <- .libPaths()
        b <- .rowSums(x, 2, 2)
        base::return(a)
    }
    fun_dot_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_dot, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    fun_dot_body <- base::deparse(expr = base::body(fun = fun_dot_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)
    testthat::expect_true(object = base::any(base::grepl(x = fun_dot_body, pattern = "a <- base::.libPaths()", fixed = TRUE), na.rm = TRUE), label = "base::.libPaths()")
    testthat::expect_true(object = base::any(base::grepl(x = fun_dot_body, pattern = "b <- base::.rowSums(x, 2, 2)", fixed = TRUE), na.rm = TRUE), label = "base::.rowSums()")
    testthat::expect_false(object = base::any(base::grepl(x = fun_dot_body, pattern = ":::", fixed = TRUE), na.rm = TRUE), label = "no ::: added")
    # primitive function (e.g., base::sum, base::c, or an alias like sum2 <- base::sum): no R code to check, a warning and NULL, and no file written with export = TRUE (formerly, its printed code, function (..., na.rm = FALSE)  .Primitive("sum"), was rebuilt with base::.Primitive("sum"), so that the returned or exported function returned base::sum instead of the sum)
    testthat::expect_warning(object = sum_safer <- base::suppressMessages(expr = saferDev::colons_check(x = base::sum, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "THE TESTED FUNCTION base::sum() IS A PRIMITIVE FUNCTION, WHICH HAS NO R CODE.\n\nNOTHING TO CHECK.", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_null(object = sum_safer, info = NULL, label = NULL)
    tempo_dir_prim <- base::tempfile(pattern = "cc_prim_", tmpdir = base::tempdir(check = FALSE), fileext = "")
    base::dir.create(path = tempo_dir_prim, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    testthat::expect_warning(object = c_safer <- base::suppressMessages(expr = saferDev::colons_check(x = base::c, export = TRUE, out_path = tempo_dir_prim, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "THE TESTED FUNCTION base::c() IS A PRIMITIVE FUNCTION, WHICH HAS NO R CODE.", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_null(object = c_safer, info = NULL, label = NULL)
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_prim, "c_safer.R", fsep = "/")), info = NULL, label = "no c_safer.R written")
    sum2 <- base::sum
    testthat::expect_warning(object = sum2_safer <- base::suppressMessages(expr = saferDev::colons_check(x = sum2, export = TRUE, out_path = tempo_dir_prim, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "THE TESTED FUNCTION sum2() IS A PRIMITIVE FUNCTION, WHICH HAS NO R CODE.", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_null(object = sum2_safer, info = NULL, label = NULL)
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_prim, "sum2_safer.R", fsep = "/")), info = NULL, label = "no sum2_safer.R written")
    base::unlink(x = tempo_dir_prim, recursive = TRUE, force = TRUE, expand = TRUE)
    ######## end names starting by a dot, primitive functions

    ######## names after $ or @
    # names after $ or @ (components or slots) are not reported nor modified (x@base::mean(1) does not parse)
    fun_slot <- function(x){
        y <- x@mean(1)
        z <- x[[1]]$sum(2)
        base::return(y)
    }
    testthat::expect_message(object = fun_slot_safer <- saferDev::colons_check(x = fun_slot, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun_slot(), EVERYTHING SEEMS CLEAN.", fixed = TRUE)
    testthat::expect_null(object = fun_slot_safer)
    # idem with a tab or a form feed between $ or @ and the name (spaces for R): these names are not modified, and the other calls of the function are still modified
    code_slot3 <- "function(x){\n    y <- x$\tmean(1)\n    z <- x@\tsum(2)\n    w <- x$\frev(3)\n    v <- round(1.5)\n    base::return(y)\n}"
    fun_slot3 <- base::eval(expr = base::parse(file = "", n = NULL, text = code_slot3, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_slot3, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the tab and the form feed in the printed code
    testthat::expect_message(object = fun_slot3_safer <- saferDev::colons_check(x = fun_slot3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "LINE\tFUN\t\tSTRING_BEFORE\n5\tround\t\tv <- \n", fixed = TRUE)
    testthat::expect_identical(object = utils::capture.output(fun_slot3_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[2:5], expected = base::c("    y <- x$\tmean(1)", "    z <- x@\tsum(2)", "    w <- x$\frev(3)", "    v <- base::round(1.5)"), label = "names after $ or @ and a tab or a form feed not modified")
    testthat::expect_no_warning(object = base::suppressMessages(expr = saferDev::colons_check(x = fun_slot3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    # modified code that does not parse (function name at the beginning of a line, after a @ ending the previous line): the submitted function is returned unchanged with a warning, or an error is returned when export = TRUE
    code_slot2 <- "function(x){\n    y <- x@\n        mean(1)\n    base::return(y)\n}"
    fun_slot2 <- base::eval(expr = base::parse(file = "", n = NULL, text = code_slot2, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_slot2, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the line break after @
    testthat::expect_warning(object = fun_slot2_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_slot2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "(1) THE CODE OF fun_slot2() MODIFIED BY THE ADDITION OF THE PACKAGE ATTRIBUTIONS DOES NOT PARSE: THE SUBMITTED FUNCTION IS RETURNED UNCHANGED.", fixed = TRUE)
    testthat::expect_identical(object = fun_slot2_safer, expected = fun_slot2, label = "submitted function returned unchanged")
    tempo_dir_slot <- base::tempfile(pattern = "cc_slot_", tmpdir = base::tempdir(check = FALSE), fileext = "")
    base::dir.create(path = tempo_dir_slot, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    testthat::expect_error(object = base::suppressMessages(expr = saferDev::colons_check(x = fun_slot2, export = TRUE, out_path = tempo_dir_slot, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "THE MODIFIED CODE DOES NOT PARSE. THE MODIFIED FUNCTION CANNOT BE EXPORTED NOR RETURNED.", fixed = TRUE)
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_slot, "fun_slot2_safer.R", fsep = "/")), label = "no file written when the modified code does not parse")
    base::unlink(x = tempo_dir_slot, recursive = TRUE, force = TRUE, expand = TRUE)
    ######## end names after $ or @

    ######## calls inside formulas, function called by .Internal(), calls of internal functions
    # calls inside a formula (one-sided or two-sided, over two lines, at any depth) are neither modified nor reported: stats::offset(z) is not recognized by stats::terms(), so that another model would be fitted
    fun_formula <- function(d){
        g <- ~ mean(.x)
        h <- y ~
            poly(x, 2)
        b <- stats::lm(formula = y ~ I(x^2), data = d)
        a <- stats::coef(object = stats::lm(formula = y ~ log(x) + offset(z), data = d))
        base::c(a, sum(1))
    }
    d_formula <- base::data.frame(y = base::c(1.2, 2.3, 2.9, 4.4, 5.1, 5.8), x = base::c(1, 2, 3, 4, 5, 6), z = base::c(0.1, 0.5, 0.2, 0.8, 0.3, 0.6), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    tempo_env_formula <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L) # caught message
    testthat::expect_no_warning(object = fun_formula_safer <- base::withCallingHandlers(
        expr = saferDev::colons_check(x = fun_formula, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""),
        message = function(m){
            base::assign(x = "message", value = base::conditionMessage(c = m), pos = -1, envir = tempo_env_formula, inherits = FALSE, immediate = TRUE)
            base::invokeRestart(r = "muffleMessage")
        }
    ), message = NULL, class = NULL)
    testthat::expect_true(object = base::grepl(x = tempo_env_formula$message, pattern = "LINE\tFUN\t\tSTRING_BEFORE\n[0-9]+\tsum\t\tbase::c\\(a, \n\n$", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), info = NULL, label = "only sum() reported")
    fun_formula_body <- base::paste0(base::deparse(expr = base::body(fun = fun_formula_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = fun_formula_body, pattern = "y ~ log(x) + offset(z)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "offset() and log() not modified")
    testthat::expect_false(object = base::grepl(x = fun_formula_body, pattern = "(base|stats)::(mean|poly|I|log|offset)\\(", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), info = NULL, label = "no prefix inside the formulas")
    testthat::expect_true(object = base::grepl(x = fun_formula_body, pattern = "base::c(a, base::sum(1))", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "sum() modified")
    testthat::expect_identical(object = fun_formula_safer(d = d_formula), expected = fun_formula(d = d_formula), info = NULL, label = "same model", expected.label = NULL)
    # function called by .Internal(): not modified, because .Internal(base::toupper(x)) fails (invalid .Internal() argument), while .Internal() gets base::
    fun_internal <- function(x){
        .Internal(toupper(x))
    }
    fun_internal_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_internal, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::any(base::grepl(x = base::deparse(expr = base::body(fun = fun_internal_safer), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), pattern = "base::.Internal(toupper(x))", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE), info = NULL, label = "toupper() not modified")
    testthat::expect_identical(object = fun_internal_safer(x = "a"), expected = "A", info = NULL, label = NULL, expected.label = NULL)
    # calls inside quote(), bquote(), expression(), alist() and substitute() (code used as data: plotmath labels, symbolic derivatives, deparsed code, formal arguments): neither modified nor reported, except the code that R evaluates (e.g., round() in the .() of bquote(), see below), while the quoting functions get base:: (formerly, base::paste() was inserted into the plotmath label, base::sin() into the expression derived by stats::D(), which then failed, base::sqrt(2) was deparsed, and base::sqrt(2) became the default value of the formal argument)
    fun_plotmath <- function(){
        graphics::plot(x = 1, main = expression(paste("Temp (", degree, "C)")))
    }
    tempo_env_plotmath <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L) # caught message
    fun_plotmath_safer <- base::withCallingHandlers(
        expr = saferDev::colons_check(x = fun_plotmath, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""),
        message = function(m){
            base::assign(x = "message", value = base::conditionMessage(c = m), pos = -1, envir = tempo_env_plotmath, inherits = FALSE, immediate = TRUE)
            base::invokeRestart(r = "muffleMessage")
        }
    )
    testthat::expect_true(object = base::grepl(x = tempo_env_plotmath$message, pattern = "LINE\tFUN\t\tSTRING_BEFORE\n2\texpression\t\tgraphics::plot\\(x = 1, main = \n\n$", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), info = NULL, label = "only expression() reported")
    testthat::expect_identical(object = utils::capture.output(fun_plotmath_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        graphics::plot(x = 1, main = base::expression(paste(\"Temp (\", degree, \"C)\")))", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = base::eval(expr = base::body(fun = fun_plotmath_safer)[[2]]$main, envir = base::baseenv(), enclos = base::baseenv()), expected = base::eval(expr = base::body(fun = fun_plotmath)[[2]]$main, envir = base::baseenv(), enclos = base::baseenv()), info = NULL, label = "same plotmath label", expected.label = NULL)
    fun_deriv <- function(){
        stats::D(expr = expression(sin(x)), name = "x")
    }
    fun_deriv_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_deriv, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = fun_deriv_safer(), expected = fun_deriv(), info = NULL, label = "same derivative", expected.label = NULL)
    fun_deparse <- function(){
        e <- quote(sqrt(2)) ; base::deparse(expr = e)
    }
    fun_deparse_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_deparse, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = fun_deparse_safer(), expected = fun_deparse(), info = NULL, label = "same deparsed code", expected.label = NULL)
    fun_bquote <- function(r2, f){
        base::formals(fun = f) <- alist(a = , b = sqrt(2))
        base::list(bquote(R^2 == .(round(r2, 2))), base::formals(fun = f))
    }
    fun_bquote_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_bquote, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_bquote_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[2:3], expected = base::c("        base::formals(fun = f) <- base::alist(a = , b = sqrt(2))", "        base::list(base::bquote(R^2 == .(base::round(r2, 2))), base::formals(fun = f))"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_bquote_safer(r2 = 0.12345, f = function(){NULL}), expected = fun_bquote(r2 = 0.12345, f = function(){NULL}), info = NULL, label = NULL, expected.label = NULL)
    # name ending a line, followed by a line starting with an expression between brackets: a variable, not a function call, thus not modified, while a function name whose ( is on the next line gets its prefix (formerly, y <- C became y <- stats::C, so that y was the function stats::C instead of the list)
    fun_newline <- function(z){
        C <- base::list(1, 2)
        y <- C
        (z)
        base::c(y, paste0
        (z))
    }
    fun_newline_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_newline, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_newline_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[3:6], expected = base::c("        y <- C", "        (z)", "        base::c(y, base::paste0", "        (z))"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_newline_safer(z = 3), expected = fun_newline(z = 3), info = NULL, label = NULL, expected.label = NULL)
    # name of a called function separated from its ( by a line of comment: prefixed too (formerly, neither prefixed nor reported)
    fun_newline2 <- function(z){
        base::nchar(paste0 # comment
            # line of comment
            (z))
    }
    fun_newline2_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_newline2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_newline2_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[2:4], expected = base::c("        base::nchar(base::paste0 # comment", "            # line of comment", "            (z))"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_newline2_safer(z = 3), expected = fun_newline2(z = 3), info = NULL, label = NULL, expected.label = NULL)
    # name of a called function separated from its ( by an empty line: prefixed too (formerly, neither prefixed nor reported)
    fun_newline3 <- function(z){
        base::nchar(paste0

            (z))
    }
    fun_newline3_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_newline3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_newline3_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[2:4], expected = base::c("        base::nchar(base::paste0", "", "            (z))"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_newline3_safer(z = 3), expected = fun_newline3(z = 3), info = NULL, label = NULL, expected.label = NULL)
    # value piped into a quoting call by the native pipe (paste0("a", x) |> quote() is quote(paste0("a", x))): neither modified nor reported, as the calls inside the brackets of quote() (formerly, paste0() and mean() got base::, so that the quoted code changed)
    fun_pipe_quote <- function(x){
        a <- paste0("a", x) |> quote()
        base::list(a, mean(x) |> quote(), x |> nchar() |> quote() |> base::eval())
    }
    fun_pipe_quote_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_pipe_quote, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_pipe_quote_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[2:3], expected = base::c("        a <- paste0(\"a\", x) |> base::quote()", "        base::list(a, mean(x) |> base::quote(), x |> nchar() |> base::quote() |> base::eval())"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_pipe_quote_safer(x = "b"), expected = fun_pipe_quote(x = "b"), info = NULL, label = NULL, expected.label = NULL)
    # code that R evaluates inside bquote() and substitute(): .() of bquote() (and ..() with splice = TRUE), where argument of bquote() and env argument of substitute(), also piped. Prefixed and reported, as saferDev 2.0.0 reported them, while the rest of the quoted code is unchanged (formerly, neither prefixed nor reported)
    fun_q01 <- function(y){base::bquote(x + .(mean(y)))}
    fun_q03 <- function(y){base::substitute(u + v, list(u = sum(y)))}
    fun_q04 <- function(y){base::bquote(f(..(lapply(y, as.name))), splice = TRUE)}
    fun_q09 <- function(y){list(u = sum(y)) |> base::substitute(expr = u + v, env = _)}
    fun_q12 <- function(y){base::bquote(.(as.name(y)) ~ x)}
    tempo_env_q01 <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L) # caught message
    fun_q01_safer <- base::withCallingHandlers(
        expr = saferDev::colons_check(x = fun_q01, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""),
        message = function(m){
            base::assign(x = "message", value = base::conditionMessage(c = m), pos = -1, envir = tempo_env_q01, inherits = FALSE, immediate = TRUE)
            base::invokeRestart(r = "muffleMessage")
        }
    )
    testthat::expect_true(object = base::grepl(x = tempo_env_q01$message, pattern = "INSIDE fun_q01(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS:\n\nLINE\tFUN\t\tSTRING_BEFORE\n1\tmean\t\tfunction(y){base::bquote(x + .(\n\n", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "mean() reported")
    testthat::expect_identical(object = utils::capture.output(fun_q01_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(x + .(base::mean(y)))}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_q01_safer(y = 1:3), expected = fun_q01(y = 1:3), info = NULL, label = NULL, expected.label = NULL)
    fun_q03_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_q03, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_q03_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::substitute(u + v, base::list(u = base::sum(y)))}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_q03_safer(y = 1:3), expected = fun_q03(y = 1:3), info = NULL, label = NULL, expected.label = NULL)
    fun_q04_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_q04, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_q04_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(f(..(base::lapply(y, as.name))), splice = TRUE)}", info = NULL, label = "f() and ..() unchanged", expected.label = NULL)
    testthat::expect_identical(object = fun_q04_safer(y = base::c("a", "b")), expected = fun_q04(y = base::c("a", "b")), info = NULL, label = NULL, expected.label = NULL)
    fun_q09_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_q09, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_q09_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::list(u = base::sum(y)) |> base::substitute(expr = u + v, env = _)}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_q09_safer(y = 1:3), expected = fun_q09(y = 1:3), info = NULL, label = NULL, expected.label = NULL)
    fun_q12_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_q12, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_q12_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(.(base::as.name(y)) ~ x)}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_q12_safer(y = "a"), expected = fun_q12(y = "a"), info = NULL, label = NULL, expected.label = NULL)
    # arguments of a bquote() or substitute() call whose argument matching fails (error of R, which then evaluates none of them): neither prefixed nor reported (formerly, the arguments matched to where or env were prefixed)
    fun_qm1 <- function(y){base::bquote(x, w = mean(y), wh = 1)}
    fun_qm2 <- function(y){base::substitute(en = list(a = 1), e = sum(y))}
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::colons_check(x = fun_qm1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = "where matched by w and wh")
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::colons_check(x = fun_qm2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = "e matching expr and env, en already matching env")
    # scope of the internal functions: an argument of a function defined inside x, or a name assigned a function inside a function defined inside x, is an internal function only inside this function, so that the calls of the same name outside this function are checked (formerly, c(x, 1) and t(x) were neither prefixed nor reported, while saferDev 2.0.0 reported them, and k(2) was not reported)
    fun_sc1 <- function(x){
        g <- function(y, c){
            c(y)
        }
        a <- c(x, 1)
        g(a, base::sum)
    }
    testthat::expect_message(object = fun_sc1_safer <- saferDev::colons_check(x = fun_sc1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "LINE\tFUN\t\tSTRING_BEFORE\n5\tc\t\ta <- \n\nINSIDE fun_sc1(), ONLY INTERNAL FUNCTIONS DETECTED:\nc\ng\n", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(fun_sc1_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[3:5], expected = base::c("            c(y)", "        }", "        a <- base::c(x, 1)"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_sc1_safer(x = 1:3), expected = fun_sc1(x = 1:3), info = NULL, label = NULL, expected.label = NULL)
    fun_sc2 <- function(x){
        lapply(X = x, FUN = function(t){t + 1})
        t(x)
    }
    testthat::expect_message(object = fun_sc2_safer <- saferDev::colons_check(x = fun_sc2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "LINE\tFUN\t\tSTRING_BEFORE\n2\tlapply\t\t\n3\tt\t\t\n\n", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(fun_sc2_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[2:3], expected = base::c("        base::lapply(X = x, FUN = function(t){t + 1})", "        base::t(x)"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_sc2_safer(x = base::matrix(data = 1:6, nrow = 2, ncol = 3, byrow = FALSE, dimnames = NULL)), expected = fun_sc2(x = base::matrix(data = 1:6, nrow = 2, ncol = 3, byrow = FALSE, dimnames = NULL)), info = NULL, label = NULL, expected.label = NULL)
    fun_sc4 <- function(x){
        h <- function(){
            k <- function(z){z}
            k(1)
        }
        k(2)
    }
    testthat::expect_warning(object = base::suppressMessages(expr = saferDev::colons_check(x = fun_sc4, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:\n\nLINE\tFUN\t\tSTRING_BEFORE\n6\tk\t\t\n\n", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    # function definitions quoted by bquote(): they create no function when x runs, so that their formal arguments, and the names assigned a function inside them, are not internal functions (c(), mean() and sd() of the .() evaluated by bquote() with the basic functions are prefixed and reported, as saferDev 2.0.0 reported them), and that a name assigned in an evaluated .() inside them is an internal function of the function that calls bquote() (formerly, c, mean and sd were internal functions, neither prefixed nor reported, and rev(y) became base::rev(y), which changed the result)
    fun_qdef1 <- function(y){base::bquote(function(c) .(c(1, y)))}
    testthat::expect_message(object = fun_qdef1_safer <- saferDev::colons_check(x = fun_qdef1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "LINE\tFUN\t\tSTRING_BEFORE\n1\tc\t\tfunction(y){base::bquote(function(c) .(\n\n", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(fun_qdef1_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(function(c) .(base::c(1, y)))}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_qdef1_safer(y = 2)[1:3], expected = fun_qdef1(y = 2)[1:3], info = NULL, label = NULL, expected.label = NULL) # the 4th element of the returned function definition is its source reference
    fun_qdef2 <- function(v){
        base::eval(base::bquote(function(x, mean = .(mean(v)), sd = .(sd(v))){
            stats::dnorm(x = x, mean = mean, sd = sd, log = FALSE)
        }))
    }
    fun_qdef2_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fun_qdef2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_qdef2_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        base::eval(base::bquote(function(x, mean = .(base::mean(v)), sd = .(stats::sd(v))){", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_qdef2_safer(v = base::c(1, 2, 3))(x = 0), expected = fun_qdef2(v = base::c(1, 2, 3))(x = 0), info = NULL, label = NULL, expected.label = NULL)
    fun_qdef3 <- function(y){
        f <- base::bquote(function(a) .(rev <- function(...){0}))
        rev(y)
    }
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::colons_check(x = fun_qdef3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = "rev(y) not modified")
    fun_qdef4 <- function(y){base::bquote(function(a){c <- function(z){z} ; .(c(1, y))})}
    testthat::expect_message(object = fun_qdef4_safer <- saferDev::colons_check(x = fun_qdef4, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "LINE\tFUN\t\tSTRING_BEFORE\n1\tc\t\tfunction(y){base::bquote(function(a){c <- function(z){z} ; .(\n\n", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(fun_qdef4_safer, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(function(a){c <- function(z){z} ; .(base::c(1, y))})}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_qdef4_safer(y = 2)[[3]][[3]], expected = fun_qdef4(y = 2)[[3]][[3]], info = NULL, label = NULL, expected.label = NULL) # value of .(c(1, y)) in the returned function definition
    # calls of the formal arguments of x and of x itself (recursive call): neither modified nor reported (base::transform(x) would silently call another function)
    fun_transform <- function(x, transform){
        transform(x)
    }
    testthat::expect_message(object = fun_transform_safer <- saferDev::colons_check(x = fun_transform, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun_transform(), EVERYTHING SEEMS CLEAN.", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_null(object = fun_transform_safer, info = NULL, label = NULL)
    fact <- function(n){if(n <= 1){1}else{n * fact(n - 1)}}
    testthat::expect_no_warning(object = fact_safer <- base::suppressMessages(expr = saferDev::colons_check(x = fact, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), message = NULL, class = NULL)
    testthat::expect_null(object = fact_safer, info = NULL, label = NULL)
    # x given as a function object (e.g., through base::do.call()): the "x" placeholder written in the messages is not the name of x, so that the call of a function named x is not a recursive call, and is reported (formerly, it was neither modified nor reported)
    x <- function(a){a + 1}
    fun_obj <- function(v){
        x(v)
    }
    testthat::expect_warning(object = base::suppressMessages(expr = base::do.call(what = saferDev::colons_check, args = base::list(x = fun_obj, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), quote = FALSE, envir = base::environment(fun = NULL)), classes = "message"), regexp = "INSIDE x(), SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:\n\nLINE\tFUN\t\tSTRING_BEFORE\n2\tx\t\t\n", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    base::rm(list = "x", pos = -1, envir = base::environment(fun = NULL), inherits = FALSE)
    # functions created inside x with =, after ; or with a line break after <-: their calls are neither modified nor reported, and are listed as internal functions. A name assigned another value (c <- 3) is not an internal function: R calls base::c()
    code_local <- "function(x){\n    mean = function(a){a}\n    y <- 1 ; rev <- function(a){a}\n    sd <-\n        function(a){a}\n    c <- 3\n    base::list(mean(x), rev(x), sd(x), c(x))\n}"
    fun_local <- base::eval(expr = base::parse(file = "", n = NULL, text = code_local, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_local, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep rev <- function(a){a} after ;
    testthat::expect_message(object = fun_local_safer <- saferDev::colons_check(x = fun_local, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "LINE\tFUN\t\tSTRING_BEFORE\n7\tc\t\tbase::list(mean(x), rev(x), sd(x), \n\nINSIDE fun_local(), ONLY INTERNAL FUNCTIONS DETECTED:\nmean\nrev\nsd\n", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(fun_local_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)[7], expected = "    base::list(mean(x), rev(x), sd(x), base::c(x))", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = fun_local_safer(x = 1:3), expected = fun_local(x = 1:3), info = NULL, label = NULL, expected.label = NULL)
    # only the calls written without :: or ::: can be calls of internal functions: base::sum() is not a call of the sum argument
    fun_prefixed <- function(x, sum){
        base::sum(x) + mean(x)
    }
    tempo_env_prefixed <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L) # caught message
    fun_prefixed_safer <- base::withCallingHandlers(
        expr = saferDev::colons_check(x = fun_prefixed, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""),
        message = function(m){
            base::assign(x = "message", value = base::conditionMessage(c = m), pos = -1, envir = tempo_env_prefixed, inherits = FALSE, immediate = TRUE)
            base::invokeRestart(r = "muffleMessage")
        }
    )
    testthat::expect_false(object = base::grepl(x = tempo_env_prefixed$message, pattern = "INTERNAL FUNCTION", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "sum not listed as internal function")
    testthat::expect_identical(object = fun_prefixed_safer(x = 1:3, sum = NULL), expected = 8, info = NULL, label = NULL, expected.label = NULL)
    ######## end calls inside formulas, function called by .Internal(), calls of internal functions

    #### end main code

    #### warning output
    # the warning is issued before the restoration of the warning.length option (long warnings not truncated), and the option is restored after
    ini_warning_length_test <- base::getOption(x = "warning.length", default = NULL)
    tempo_env_wl <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L)
    base::withCallingHandlers(
        expr = base::suppressMessages(expr = saferDev::colons_check(x = fun7, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"),
        warning = function(w){
            base::assign(x = "warning_length", value = base::getOption(x = "warning.length", default = NULL), pos = -1, envir = tempo_env_wl, inherits = FALSE, immediate = TRUE)
            base::invokeRestart(r = "muffleWarning")
        }
    )
    testthat::expect_identical(object = tempo_env_wl$warning_length, expected = 8170, label = "warning.length when the warning is issued")
    testthat::expect_identical(object = base::getOption(x = "warning.length", default = NULL), expected = ini_warning_length_test, label = "warning.length restored")
    #### end warning output

    #### output
    # the modified function is returned visibly, NULL invisibly (nothing to modify, no function call, or export = TRUE)
    testthat::expect_visible(call = base::suppressMessages(expr = saferDev::colons_check(x = fun9, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    testthat::expect_invisible(call = base::suppressMessages(expr = saferDev::colons_check(x = fun2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    testthat::expect_invisible(call = base::suppressWarnings(expr = saferDev::colons_check(x = fun1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "warning"))
    testthat::expect_no_message(object = base::suppressWarnings(expr = saferDev::colons_check(x = fun1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "warning")) # no function call: no report printed, only the warning
    tempo_dir_out <-base::tempfile(pattern = "cc_out_", tmpdir = base::tempdir(check = FALSE), fileext = "")
    base::dir.create(path = tempo_dir_out, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    testthat::expect_invisible(call = base::suppressMessages(expr = saferDev::colons_check(x = fun9, export = TRUE, out_path = tempo_dir_out, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_out, "fun9_safer.R", fsep = "/")), label = "fun9_safer.R written")
    base::unlink(x = tempo_dir_out, recursive = TRUE, force = TRUE, expand = TRUE)
    #### end output

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # a word followed by ( inside a string containing escaped quotes is not reported (the escaped quotes are not string delimiters)
    FUN_ESC <- function(x){
        y <- base::paste0("a \"b\" WORD (c) ", x, collapse = NULL, recycle0 = FALSE)
        y
    }
    testthat::expect_message(object = colons_check(x = FUN_ESC, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_no_warning(object = base::suppressMessages(expr = colons_check(x = FUN_ESC, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    # attributes of x, printed by R after its code: error of lambda_check(), run first by colons_check(), which says that it happened inside colons_check() (formerly, the raw error of the parser for a character attribute, and for a formula attribute, the analysis of the printed attribute as code: x returned unchanged, with a misleading warning)
    fun_attr <- function(x){x}
    base::attr(x = fun_attr, which = "note") <- "hello"
    fun_attr_fo <- function(x){paste0(x)}
    base::attr(x = fun_attr_fo, which = "fo") <- y ~ x
    testthat::expect_error(object = saferDev::colons_check(x = fun_attr, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::lambda_check() INSIDE saferDev::colons_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nnote\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::colons_check(x = fun_attr_fo, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::lambda_check() INSIDE saferDev::colons_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nfo\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # option keep.parse.data set to FALSE (e.g., by base::sys.source() during the loading of a file): same result as with the default TRUE (here, the call inside the formula is not modified; formerly, the parse data were not available, so that offset(z) became stats::offset(z))
    fun_kpd <- function(d){stats::lm(formula = y ~ offset(z), data = d)}
    res_kpd_ref <- base::suppressMessages(expr = saferDev::colons_check(x = fun_kpd, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    oop_kpd <- base::options(keep.parse.data = FALSE)
    res_kpd <- base::tryCatch(expr = base::suppressMessages(expr = saferDev::colons_check(x = fun_kpd, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), error = function(e){e}, finally = base::options(oop_kpd))
    testthat::expect_identical(object = utils::capture.output(res_kpd, file = NULL, append = FALSE, type = "output", split = FALSE), expected = utils::capture.output(res_kpd_ref, file = NULL, append = FALSE, type = "output", split = FALSE), info = NULL, label = NULL, expected.label = NULL)
    ## end other tests

    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
