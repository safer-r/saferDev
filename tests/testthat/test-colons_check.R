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
    testthat::expect_no_error(colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
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
    testthat::expect_warning(colons_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "IS EMPTY OF FUNCTION", fixed = TRUE) # empty function: warning, not error
    testthat::expect_null(object = base::suppressWarnings(colons_check(x = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""), classes = "warning"))
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
    testthat::expect_no_error(colons_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_no_error(colons_check(x = fun8, safer_check = FALSE, lib_path = NULL, error_text = ""))


    testthat::expect_no_error(colons_check(x = fun_non_base))  
    testthat::expect_no_error(colons_check(x = fun_custom))    

    # function calling only itself (recursive): the recursive name is removed from the other functions, leaving no other function, and NULL is returned (no modification)
    testthat::expect_null(object = colons_check(x = fun_rec, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # function containing an internal function whose name is also a basic R function name: the basic occurrence is skipped (no modification of the internal one), and NULL is returned
    testthat::expect_null(object = colons_check(x = fun_skip, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # function named like a basic R function and calling itself: the recursive call must NOT be modified (the base:: prefix would redirect the recursion to the basic function), while the other basic functions are modified
    sum_test2 <- function(x){
        sum_test2(x)
        sum(1:2)
    }
    sum_test2_safer <- colons_check(x = sum_test2, safer_check = FALSE, lib_path = NULL, error_text = "")
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
    testthat::expect_error(colons_check(x = fun9, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CANNOT BE NULL", fixed = TRUE)
    testthat::expect_error(colons_check(x = fun9, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CANNOT BE NULL", fixed = TRUE)
    # export argument values
    testthat::expect_error(colons_check(x = fun9, export = NA, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = "TRUE", safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = base::c(TRUE, FALSE), safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = mat1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = factor1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = expr1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = fun1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(colons_check(x = fun9, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""))
    # out_path values
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = factor1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = expr1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = fun1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = base::c("a", "b"), safer_check = FALSE, lib_path = NULL, error_text = ""))
    # out_path ignored when export = FALSE (even a non-existing path)
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
    testthat::expect_no_error(colons_check(x = fun_rec, export = TRUE, out_path = base::paste0(tempo_dir_colons, "/"), safer_check = FALSE, lib_path = NULL, error_text = "")) # clean function for basic functions (recursive call only): no file written
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_colons, "fun_rec_safer.R", fsep = "/")))
    testthat::expect_null(object = colons_check(x = fun9, export = TRUE, out_path = base::paste0(tempo_dir_colons, "\\"), safer_check = FALSE, lib_path = NULL, error_text = "")) # trailing backslash tolerated, file written
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_colons, "fun9_safer.R", fsep = "/")))
    # non-existing out_path
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = "PATH_NOT_GOOD", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXISTS", fixed = TRUE)
    # out_path with a trailing /: the trailing / is removed first, then the path checked (the error message shows the path without the trailing /)
    testthat::expect_error(colons_check(x = fun9, export = TRUE, out_path = "PATH_NOT_GOOD/", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "DOES NOT EXISTS:\nPATH_NOT_GOOD\n", fixed = TRUE)
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

    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})
