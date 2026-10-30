testthat::test_that("all_args_here()", {

    ## data argument values
    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    fun2 <- function(x){
        dt <- base::c(2:8)
        NOT_CONSIDERED = 1
        }
    fun3 <- function(x){a$count(x)}
    fun4 <- function(){dt <- base::c(2:8)}
    fun5 <- function(...){dt <- base::c(2:8)}
    fun6 <- function(x){
        a <- function(x){x + 1}
        a(x = x)
    }
    fun7 <- function(x){
        a$count(x)
        NOT_CONSIDERED = 1
    }
    fun8 <- function(x){
        a <- function(x){x + 1}
        a(x = x)
        c(2:8)
    }
    fun9 <- function(x){
        graphics::c(2:8)
    }
    fun10 <- function(x){
        saferDev:::.colons_check_message(jfgkdgfksdgf = 2:8)
    }
    fun11 <- function(x){
        base::paste0(c(2:8))
    }
    fun12 <- function(x){
        base::gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text)
        repeat{ (x)}
    }
    fun13 <- function(x){
        base::as.environment(x)
    }
    fun14 <- function(a) {
        if(a > 0) {base::return(TRUE)}  # 'if' is a special primitive with no formals
        base::return(FALSE)
    }
    fun15 <- function(x) {
        base::length(x)  # 'if' is a special primitive with no formals
    }
    fun16 <- function(x) {
        base::getwd()  # 'if' is a special primitive with no formals
    }
    fun17 <- function(x) {
        base::gc(x)  # 'if' is a special primitive with no formals
    }
    fun18 <- function(x) {
        base::quote(x)  # 'if' is a special primitive with no formals
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
        #### Find all matches, including trailing '(' #
        matches <- base::gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text) # # test
        dt <- base::c(2:8)
        matched_strings <- base::regmatches(x = text, m = matches)[[1]]
        
        # Remove trailing '(' from each match #
        tempo4 <- a$regmatches(x = text, m = matches)[[1]] ; base::sum(1:3) ; a$regmatches(x = 1)
        tempo5 <- a$count
        tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS OF col1 (", base::length(roc2), "), col2 (", base::length(roc2), "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")
        result <- base::sub("\\($##", "", matched_strings) ; base::range(1:3) # sub("\\($##", "", matched_strings)
        tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (",  stats::ks.test(roc3), "), and roc0 (", base::length(roc5), ") SHOULL\n")
        base::return(baba) # base::sub("\\($##", "", matched_strings)
        base::return(bobo) # a$sub("\\($##", "", matched_strings)
    }
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(all_args_here(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::all_args_here()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(all_args_here()) # the function has default values for all args.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(all_args_here(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = NULL, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = NULL, overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(all_args_here(x = character(), table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = integer(), table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = double(), table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = logical(), table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = complex(), table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = data.frame(), table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = list(), table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, table_export = character(), out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = integer(), out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = double(), out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = logical(), out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = complex(), out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = data.frame(), out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = list(), out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = character(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = integer(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = double(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = logical(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = complex(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = data.frame(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = list(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = character(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = integer(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = double(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = logical(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = complex(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = data.frame(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = list(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(all_args_here(x = NA, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = NA, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = NA, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = NA, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = NULL, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = NA, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = 1, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = -1, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = 1:2, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = c(TRUE, FALSE), table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = mat1, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = factor1, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = expr1, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_warning(all_args_here(x = fun1, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "IS EMPTY OF FUNCTION", fixed = TRUE) # empty function: warning, not error
    testthat::expect_null(object = base::suppressWarnings(all_args_here(x = fun1, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), classes = "warning"))
    # end x
    # table_export
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = NULL, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = NA, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = 1, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = -1, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = 1:2, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = c(TRUE, FALSE), out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = mat1, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = factor1, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = expr1, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = fun1, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end table_export
    # out_path
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = NA, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = 1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = -1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = 1:2, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = c(TRUE, FALSE), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = mat1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = factor1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = expr1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = fun1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end out_path
    # df_name
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = NULL, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = NA, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = 1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = -1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = 1:2, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = c(TRUE, FALSE), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = mat1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = factor1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = expr1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = fun1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end df_name
    # overwrite
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = fun1, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end overwrite
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = "", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 
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
    testthat::expect_no_error(all_args_here(x = test, table_export = TRUE, out_path = "./", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.remove("./res.tsv")
    testthat::expect_no_error(all_args_here(x = test, table_export = TRUE, out_path = ".\\", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.remove("./res.tsv")
    testthat::expect_error(all_args_here(x = test, table_export = TRUE, out_path = "caca", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.create("./caca", showWarnings = FALSE)
    testthat::expect_error(all_args_here(x = test, table_export = TRUE, out_path = ".", df_name = "caca", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.remove("./caca")
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # lambda pre-check: a function carrying srcref and containing a \(y) lambda writing is refused
    fun_lambda <- base::eval(expr = base::parse(text = "function(x){ base::sapply(X = 1:3, FUN = \\(y){ y * 2 }) ; base::return(x) }", keep.source = TRUE))
    testthat::expect_error(all_args_here(x = fun_lambda, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "RUN saferDev::lambda_check", fixed = TRUE)
    # a function without srcref (compiled-style writing) prints function(y), which is not detected by lambda_check(): the pre-check silently passes
    fun_lambda_compiled <- base::eval(expr = base::parse(text = "function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) ; base::return(x) }", keep.source = FALSE))
    testthat::expect_no_error(all_args_here(x = fun_lambda_compiled, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # a function containing only the "\\(y)" string literal (STR_CONST token, not a lambda writing) passes the pre-check
    fun_lambda_string <- base::eval(expr = base::parse(text = "function(x){ base::sapply(X = 1:3, FUN = function(y){ base::paste0(\"\\\\(y)\") }) ; base::return(x) }", keep.source = TRUE))
    testthat::expect_no_error(all_args_here(x = fun_lambda_string, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # if(base::grepl(x = fun_1_line, pattern = reserved_words, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
    testthat::expect_no_error(all_args_here(x = fun2, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 

    # if( (base::length(x = col1) == 0)){
    testthat::expect_no_error(all_args_here(x = fun3, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 

    # if( ! base::is.null(x = warn)){ 
    testthat::expect_no_error(all_args_here(x = fun7, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = fun8, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = fun9, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = fun10, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))


    # if(tempo_log){
    testthat::expect_no_error(all_args_here(x = fun6, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 

    # if(tempo_string != "::"){
    testthat::expect_error(all_args_here(x = fun11, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 

    # if(base::length(x = tempo_all_args) == 0){
    testthat::expect_no_error(all_args_here(x = fun12, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 
    testthat::expect_no_error(all_args_here(x = fun13, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 




    # three_dots_log <- if(base::length(x = arg_full) > 0){
    testthat::expect_no_error(all_args_here(x = fun4, table_export = TRUE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.remove("./res.tsv")
    testthat::expect_no_error(all_args_here(x = fun5, table_export = TRUE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.remove("./res.tsv")

    # if(table_export == TRUE){
    testthat::expect_no_error(all_args_here(x = fun2, table_export = TRUE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # a <- file.remove("./res.tsv") # but inactivated to test overwrite = TRUE below
    testthat::expect_no_error(all_args_here(x = fun2, table_export = TRUE, out_path = ".", df_name = "res.tsv", overwrite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.remove("./res.tsv")

    testthat::expect_no_error(all_args_here(x = fun14))
    testthat::expect_no_error(all_args_here(x = fun15))
    testthat::expect_no_error(all_args_here(x = fun16))
    testthat::expect_no_error(all_args_here(x = fun17))
    testthat::expect_no_error(all_args_here(x = fun18))

    # fun_export argument: new feature (returns the modified function with the missing arguments added, or NULL if nothing was modified)
    # fun1: function with missing arguments (base::sum(1:2, na.rm) and base::paste0("a", "b") from the test-all_args_here.R fixture set)
    FUN1 <- function(x, y){
        missing_arg1 <- base::sum(1:2, na.rm)
        missing_arg2 <- base::paste0("a", "b")
    }
    # clean call: no modification -> NULL returned
    FUN2 <- function(x, y){
        base::sum(x, y, na.rm = FALSE)
    }
    testthat::expect_null(object = all_args_here(x = FUN2, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # modification returned as a function object
    tempo_fun <- all_args_here(x = FUN1, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = tempo_fun))
    testthat::expect_true(object = base::all(base::names(x = base::formals(fun = tempo_fun)) == base::c("x", "y"), na.rm = TRUE))
    # fun_export = TRUE: the modified function is exported as a .R file (in tempdir to avoid working directory pollution)
    tempo_dir <- base::tempdir()
    testthat::expect_no_error(all_args_here(x = FUN1, table_export = FALSE, fun_export = TRUE, out_path = tempo_dir, df_name = "res.tsv", overwrite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "FUN1_safer.R")))
    # the exported file contains an assignment and can be sourced to recreate the function
    tempo_lines <- base::readLines(con = base::file.path(tempo_dir, "FUN1_safer.R"), n = 5, warn = FALSE, ok = TRUE)
    testthat::expect_true(object = base::grepl(x = tempo_lines[1], pattern = "^FUN1_safer <- ", perl = TRUE), label = "exported file starts with assignment")
    tempo_env <- base::new.env(parent = base::globalenv())
    base::sys.source(file = base::file.path(tempo_dir, "FUN1_safer.R"), envir = tempo_env, chdir = FALSE)
    testthat::expect_true(object = base::is.function(x = tempo_env$FUN1_safer), label = "sourced exported file defines FUN1_safer")
    # export message: the file location is announced by a message
    testthat::expect_message(all_args_here(x = FUN1, table_export = FALSE, fun_export = TRUE, out_path = tempo_dir, df_name = "res.tsv", overwrite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    # the exported file parses and gives a function
    tempo_parsed <- base::parse(file = base::file.path(tempo_dir, "FUN1_safer.R"), n = NULL, text = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown")
    tempo_fun2 <- base::eval(expr = tempo_parsed[[1]], envir = base::environment(fun = NULL))
    testthat::expect_true(object = base::is.function(x = tempo_fun2))
    testthat::expect_error(all_args_here(x = FUN1, table_export = FALSE, fun_export = TRUE, out_path = tempo_dir, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # overwrite = FALSE and file already existing
    testthat::expect_no_error(all_args_here(x = FUN1, table_export = FALSE, fun_export = TRUE, out_path = tempo_dir, df_name = "res.tsv", overwrite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.remove(base::file.path(tempo_dir, "FUN1_safer.R"))
    # fun_export = TRUE with a clean function: no file written, no error even if a file with the same name already exists (the overwrite check is only performed when a file is actually written)
    testthat::expect_null(object = all_args_here(x = FUN2, table_export = FALSE, fun_export = TRUE, out_path = tempo_dir, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # x = pkg::fun writing form: the file-name determination works (the last :: -split component is kept), and for a clean pkg::fun nothing is written
    testthat::expect_null(object = all_args_here(x = saferDev::all_args_here, table_export = FALSE, fun_export = TRUE, out_path = tempo_dir, df_name = "res.tsv", overwrite = TRUE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    # x = obj$fun writing form and inline definitions are refused when fun_export = TRUE (the written form is not reliable for the file name)
    FUN_LIST <- base::list(fun = FUN2)
    testthat::expect_error(all_args_here(x = FUN_LIST$fun, table_export = FALSE, fun_export = TRUE, out_path = tempo_dir, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE) # clean function: nothing written anyway
    testthat::expect_error(all_args_here(x = function(y){ base::sum(y) }, table_export = FALSE, fun_export = TRUE, out_path = tempo_dir, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    # nested calls on the same line: both corrections applied (the missing arguments are inserted, not the STATUS proposals, which embed the original nested calls)
    FUN_NESTED <- function(v){
        res <- base::paste0(base::mean(1:2))
    }
    tempo_msg <- testthat::capture_messages(code = {tempo_fun_nested <- all_args_here(x = FUN_NESTED, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_true(object = base::is.function(x = tempo_fun_nested))
    testthat::expect_false(object = base::any(base::grepl(x = tempo_msg, pattern = "CORRECTIONS NOT APPLIED", fixed = TRUE, ignore.case = FALSE, perl = FALSE), na.rm = TRUE))
    tempo_txt <- base::paste0(utils::capture.output(tempo_fun_nested, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "base::paste0(base::mean(x = 1:2), collapse = NULL, recycle0 = FALSE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # call written on several lines, with comments and several statements inside {} (as in stats::p.adjust()): the proposal of STATUS is built from the joined lines and does not parse, but the missing arguments are inserted into the original lines, keeping the line breaks, the indentation, the comments and the strings
    txt_multi <- base::c(
        "function(v){",
        "    # first comment",
        "    res <- base::switch(v, a = {",
        "        i <- 1 # comment inside the call",
        "        base::paste0(\"a  b\", i)",
        "    }, # comment after a",
        "    b = 2) # end comment",
        "}"
    )
    FUN_MULTI <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_multi, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_multi, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL))
    tempo_msg_multi <- testthat::capture_messages(code = {tempo_fun_multi <- all_args_here(x = FUN_MULTI, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_false(object = base::any(base::grepl(x = tempo_msg_multi, pattern = "CORRECTIONS NOT APPLIED", fixed = TRUE, ignore.case = FALSE, perl = FALSE), na.rm = TRUE))
    tempo_txt <- utils::capture.output(tempo_fun_multi, file = NULL, append = FALSE, type = "output", split = FALSE)
    testthat::expect_equal(object = tempo_txt[base::seq_along(along.with = txt_multi)], expected = base::c(
        "function(v){",
        "    # first comment",
        "    res <- base::switch(EXPR = v, a = {",
        "        i <- 1 # comment inside the call",
        "        base::paste0(\"a  b\", i, collapse = NULL, recycle0 = FALSE)",
        "    }, # comment after a",
        "    b = 2) # end comment",
        "}"
    ))
    # positional missing argument (bare symbol written positionally and proposed as named): routed to CORRECTIONS NOT APPLIED, the returned function keeps the original call
    FUN_POSARG <- function(v){
        res <- base::sum(v, na.rm)
    }
    tempo_msg2 <- testthat::capture_messages(code = {tempo_fun_posarg <- all_args_here(x = FUN_POSARG, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_true(object = base::any(base::grepl(x = tempo_msg2, pattern = "CORRECTIONS NOT APPLIED", fixed = TRUE, ignore.case = FALSE, perl = FALSE), na.rm = TRUE))
    # default values that cannot be copied (depending on another argument or on the frame of evaluation): proposed with an empty value, and args with empty value recapitulated in a message
    FUN_EMPTY <- function(){
        a <- utils::getFromNamespace(x = "arg_check", ns = "saferDev")
        b <- base::do.call(what = base::sum, args = base::list(1))
    }
    tempo_msg3 <- testthat::capture_messages(code = {tempo_fun_empty <- all_args_here(x = FUN_EMPTY, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    tempo_txt <- base::paste0(utils::capture.output(tempo_fun_empty, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "utils::getFromNamespace(x = \"arg_check\", ns = \"saferDev\", pos = -1, envir = )", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "quote = FALSE, envir = )", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    tempo_msg3 <- base::paste0(tempo_msg3, collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg3, pattern = "ARGUMENTS WITH EMPTY VALUE", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_msg3, pattern = "line 2: utils::getFromNamespace(): envir (DEFAULT: as.environment(pos))", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_msg3, pattern = "line 3: base::do.call(): envir (DEFAULT: parent.frame())", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # args with no default value and not written: no error, proposed with an empty value, and recapitulated only when proposed (an empty value already written in the code is accepted and not reported)
    FUN_NO_DEF <- function(){
        a <- base::.libPaths()
        b <- base::.libPaths(new = , include.site = TRUE)
        d <- utils::getFromNamespace(x = "arg_check", ns = "saferDev", pos = , envir = )
    }
    testthat::expect_no_error(tempo_msg4 <- testthat::capture_messages(code = {tempo_fun_no_def <- all_args_here(x = FUN_NO_DEF, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}))
    tempo_txt <- base::paste0(utils::capture.output(tempo_fun_no_def, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "a <- base::.libPaths(new = , include.site = TRUE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    tempo_msg4 <- base::paste0(tempo_msg4, collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg4, pattern = "line 2: base::.libPaths(): new (NO DEFAULT VALUE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_false(object = base::grepl(x = tempo_msg4, pattern = "line 3:", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_false(object = base::grepl(x = tempo_msg4, pattern = "line 4:", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # no empty value: no recap message
    FUN_NO_EMPTY <- function(){
        a <- base::sum(1, na.rm = FALSE)
    }
    tempo_msg5 <- testthat::capture_messages(code = {all_args_here(x = FUN_NO_EMPTY, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_false(object = base::any(base::grepl(x = tempo_msg5, pattern = "ARGUMENTS WITH EMPTY VALUE", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = TRUE))
    # table returned as a message when table_export = FALSE, exported (no table message) when table_export = TRUE. The modified function is returned when fun_export = FALSE, whatever table_export
    FUN_TABLE <- function(v){
        res <- base::mean(1:2)
    }
    tempo_msg3 <- testthat::capture_messages(code = {tempo_fun_table <- all_args_here(x = FUN_TABLE, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_true(object = base::any(base::grepl(x = tempo_msg3, pattern = "MISSING_ARG_NAMES", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = TRUE))
    testthat::expect_true(object = base::is.function(x = tempo_fun_table))
    tempo_msg4 <- testthat::capture_messages(code = {tempo_fun_table2 <- all_args_here(x = FUN_TABLE, table_export = TRUE, fun_export = FALSE, out_path = tempo_dir, df_name = "res_table.tsv", overwrite = TRUE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_false(object = base::any(base::grepl(x = tempo_msg4, pattern = "MISSING_ARG_NAMES", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = TRUE))
    testthat::expect_true(object = base::file.exists(base::paste0(tempo_dir, "/res_table.tsv", collapse = NULL, recycle0 = FALSE)))
    testthat::expect_true(object = base::is.function(x = tempo_fun_table2))
    # two independent calls on the same line: both corrections applied
    FUN_SAME_LINE_BAD <- function(v){
        res <- base::mean(1:2) + base::mean(3:4)
    }
    tempo_fun_same_line <- all_args_here(x = FUN_SAME_LINE_BAD, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = tempo_fun_same_line))
    tempo_body <- base::deparse(expr = base::body(fun = tempo_fun_same_line), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)
    testthat::expect_true(object = base::any(base::grepl(x = tempo_body, pattern = "base::mean\\(x = 1:2\\)", ignore.case = FALSE, perl = TRUE, fixed = FALSE), na.rm = TRUE))
    testthat::expect_true(object = base::any(base::grepl(x = tempo_body, pattern = "base::mean\\(x = 3:4\\)", ignore.case = FALSE, perl = TRUE, fixed = FALSE), na.rm = TRUE))
    # default value that is a symbol (object = nm in stats::setNames(object = nm, nm)): the argument is not taken as an argument without default value, thus the positional values are named (formerly, DEF_ARGS was "objectnm, nm" and the correction was routed to CORRECTIONS NOT APPLIED)
    FUN_SYMBOL_DEFAULT <- function(p){
        nm <- base::letters[1:2]
        stats::setNames(p, nm)
    }
    tempo_msg6 <- testthat::capture_messages(code = {tempo_fun_symbol_default <- all_args_here(x = FUN_SYMBOL_DEFAULT, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_true(object = base::any(base::grepl(x = tempo_msg6, pattern = "object = nm, nm", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = TRUE))
    testthat::expect_false(object = base::any(base::grepl(x = tempo_msg6, pattern = "CORRECTIONS NOT APPLIED", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = TRUE))
    tempo_body <- base::deparse(expr = base::body(fun = tempo_fun_symbol_default), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)
    testthat::expect_true(object = base::any(base::grepl(x = tempo_body, pattern = "stats::setNames(object = p, nm = nm)", ignore.case = FALSE, perl = FALSE, fixed = TRUE), na.rm = TRUE))
    # table_export = TRUE: the modified function is not returned
    testthat::expect_no_error(all_args_here(x = FUN1, table_export = TRUE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- file.remove("./res.tsv")
    #### end main code

    # corrections not applied (here, a positional bare symbol proposed as named argument): both the "ARGUMENTS ARE MISSING" and the "CORRECTIONS NOT APPLIED" messages are printed (formerly, the second message replaced the first one, because both on.exit() used the same tempo_cat object)
    FUN_STR <- function(x){
        res <- base::sum(x, na.rm)
    }
    tempo_msg <- base::character(length = 0L)
    base::withCallingHandlers(
        expr = all_args_here(x = FUN_STR, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""),
        message = function(m){
            tempo_msg <<- base::c(tempo_msg, base::conditionMessage(c = m))
            base::invokeRestart(r = "muffleMessage")
        }
    )
    testthat::expect_true(object = base::any(base::grepl(x = tempo_msg, pattern = "ARGUMENTS ARE MISSING", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = FALSE))
    testthat::expect_true(object = base::sum(base::grepl(x = tempo_msg, pattern = "CORRECTIONS NOT APPLIED", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = FALSE) == 1)
    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})
