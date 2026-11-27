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
    out_dir1 <- base::tempfile(pattern = "all_args_here", tmpdir = base::tempdir(), fileext = "") # folder of the files written by the tests
    a <- base::dir.create(path = out_dir1, showWarnings = TRUE, recursive = FALSE, mode = "0777")
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
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, in the saferDev::arg_check() calls, so the lib_path check is always executed)
    testthat::expect_error(all_args_here(x = test, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
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
    testthat::expect_no_error(saferDev::all_args_here(x = test, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # out_path can be NULL (default) when no file is written
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
    # out_path NULL (default) when a file must be written: error
    testthat::expect_error(saferDev::all_args_here(x = test, table_export = TRUE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN table_export OR fun_export IS TRUE.", fixed = TRUE)
    testthat::expect_error(saferDev::all_args_here(x = test, table_export = FALSE, fun_export = TRUE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN table_export OR fun_export IS TRUE.", fixed = TRUE)
    # trailing / or \\ of out_path removed
    testthat::expect_no_error(saferDev::all_args_here(x = test, table_export = TRUE, fun_export = FALSE, out_path = base::paste0(out_dir1, "/", collapse = NULL, recycle0 = FALSE), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- base::file.remove(base::file.path(out_dir1, "res.tsv", fsep = "/"))
    testthat::expect_no_error(saferDev::all_args_here(x = test, table_export = TRUE, fun_export = FALSE, out_path = base::paste0(out_dir1, "\\", collapse = NULL, recycle0 = FALSE), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- base::file.remove(base::file.path(out_dir1, "res.tsv", fsep = "/"))
    testthat::expect_error(saferDev::all_args_here(x = test, table_export = TRUE, fun_export = FALSE, out_path = base::file.path(out_dir1, "caca", fsep = "/"), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXIST:\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL) # folder that does not exist (formerly, DOES NOT EXISTS)
    a <- base::file.create(base::file.path(out_dir1, "caca", fsep = "/"), showWarnings = FALSE)
    testthat::expect_error(saferDev::all_args_here(x = test, table_export = TRUE, fun_export = FALSE, out_path = out_dir1, df_name = "caca", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # file already existing
    a <- base::file.remove(base::file.path(out_dir1, "caca", fsep = "/"))
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
    testthat::expect_warning(object = saferDev::all_args_here(x = fun2, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "THE RESERVED WORD \"NOT_CONSIDERED\" HAS BEEN DETECTED IN THE CODE OF THE INPUT FUNCTION", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL) # reserved word in the code of fun2: warning, not error

    # if( (base::length(x = col1) == 0)){
    testthat::expect_no_error(all_args_here(x = fun3, table_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 

    # if( ! base::is.null(x = warn)){ 
    testthat::expect_warning(object = saferDev::all_args_here(x = fun7, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "THE RESERVED WORD \"NOT_CONSIDERED\" HAS BEEN DETECTED IN THE CODE OF THE INPUT FUNCTION", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
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
    testthat::expect_no_error(all_args_here(x = fun4, table_export = TRUE, out_path = out_dir1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- base::file.remove(base::file.path(out_dir1, "res.tsv", fsep = "/"))
    testthat::expect_no_error(all_args_here(x = fun5, table_export = TRUE, out_path = out_dir1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    a <- base::file.remove(base::file.path(out_dir1, "res.tsv", fsep = "/"))

    # if(base::length(x = tempo_split) > base::length(x = arg_full_names) & ! base::any(three_dots_log, na.rm = TRUE)){: call written with more arguments than its function has (base::rev() has a single argument), which fails in R: not analysed, with a warning (formerly, INTERNAL ERROR 8)
    FUN_TOO_MANY <- function(x, y){
        base::rev(x, y)
    }
    testthat::expect_warning(object = tempo_fun_too_many <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_TOO_MANY, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "(1) IN LINE 2, THE CALL\nbase::rev(x, y)\nHAS MORE ARGUMENTS THAN ITS FUNCTION, WHOSE ARGUMENTS ARE: x.\nTHIS CALL IS NOT ANALYZED.", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_null(object = tempo_fun_too_many)
    # same for the target of an assignment calling a replacement function that has more arguments (body<- has an envir argument, contrary to body()), which is valid: not analysed, with a warning, and the other calls are corrected (formerly, INTERNAL ERROR 8)
    FUN_BODY_ASSIGN <- function(f, e){
        base::body(f, envir = e) <- base::quote(expr = 1)
        base::sum(1)
    }
    testthat::expect_warning(object = tempo_fun_body_assign <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_BODY_ASSIGN, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "(1) IN LINE 2, THE CALL\nbase::body(f, envir = e)\nHAS MORE ARGUMENTS THAN ITS FUNCTION, WHOSE ARGUMENTS ARE: fun.\nTHIS CALL IS NOT ANALYZED.", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_equal(object = utils::capture.output(tempo_fun_body_assign, file = NULL, append = FALSE, type = "output", split = FALSE)[2:3], expected = base::c(
        "        base::body(f, envir = e) <- base::quote(expr = 1)",
        "        base::sum(1, na.rm = FALSE)"
    ))

    # if(table_export == TRUE){
    testthat::expect_warning(object = saferDev::all_args_here(x = fun2, table_export = TRUE, fun_export = FALSE, out_path = out_dir1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "THE RESERVED WORD \"NOT_CONSIDERED\" HAS BEEN DETECTED IN THE CODE OF THE INPUT FUNCTION", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # file not removed, to test overwrite = TRUE below
    testthat::expect_warning(object = saferDev::all_args_here(x = fun2, table_export = TRUE, fun_export = FALSE, out_path = out_dir1, df_name = "res.tsv", overwrite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "THE RESERVED WORD \"NOT_CONSIDERED\" HAS BEEN DETECTED IN THE CODE OF THE INPUT FUNCTION", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    a <- base::file.remove(base::file.path(out_dir1, "res.tsv", fsep = "/"))

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
    # fun_export = TRUE with a clean function: no file written, but the file checks still apply (error if the file already exists and overwrite is FALSE)
    testthat::expect_null(object = all_args_here(x = FUN2, table_export = FALSE, fun_export = TRUE, out_path = out_dir1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_false(object = base::file.exists(base::file.path(out_dir1, "FUN2_safer.R", fsep = "/")))
    a <- base::file.create(base::file.path(out_dir1, "FUN2_safer.R", fsep = "/"), showWarnings = TRUE)
    testthat::expect_error(all_args_here(x = FUN2, table_export = FALSE, fun_export = TRUE, out_path = out_dir1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "FILE NAME ALREADY EXISTS AT THE INDICATED PATH", fixed = TRUE)
    a <- base::file.remove(base::file.path(out_dir1, "FUN2_safer.R", fsep = "/"))
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
    # arg written with an empty value while its default value can be copied: the default value is added. The args with no default value or with a default value that cannot be copied remain empty
    FUN_FILL <- function(){
        a <- base::paste0("a", collapse = )
        b <- utils::getFromNamespace(x = "arg_check", ns = "saferDev", pos = , envir = )
    }
    testthat::capture_messages(code = {tempo_fun_fill <- all_args_here(x = FUN_FILL, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    tempo_txt <- base::paste0(utils::capture.output(tempo_fun_fill, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "base::paste0(\"a\", collapse = NULL, recycle0 = FALSE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "pos = -1", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "envir = )", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # arg written with a partial name: the full name is written (only for the args before ..., like in R), and the proposal is in the STATUS column
    FUN_PARTIAL <- function(){
        a <- base::signif(1.5, dig = 1)
        b <- base::paste0("a", coll = ",")
    }
    tempo_msg6 <- testthat::capture_messages(code = {tempo_fun_partial <- all_args_here(x = FUN_PARTIAL, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    tempo_txt <- base::paste0(utils::capture.output(tempo_fun_partial, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "base::signif(x = 1.5, digits = 1)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "base::paste0(\"a\", coll = \",\", collapse = NULL, recycle0 = FALSE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE)) # coll goes into ..., not a partial collapse
    testthat::expect_false(object = base::any(base::grepl(x = base::paste0(tempo_msg6, collapse = "", recycle0 = FALSE), pattern = "FULLY WRITTEN", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = TRUE))
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
    # table_export = TRUE and fun_export = FALSE: the table is written and the modified function is returned
    testthat::expect_true(object = base::is.function(x = base::suppressMessages(expr = saferDev::all_args_here(x = FUN1, table_export = TRUE, fun_export = FALSE, out_path = out_dir1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), classes = "message")))
    testthat::expect_true(object = base::file.exists(base::file.path(out_dir1, "res.tsv", fsep = "/")))
    a <- base::file.remove(base::file.path(out_dir1, "res.tsv", fsep = "/"))
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
    # the returned modified function keeps the environment of the submitted function, and not the internal environment of saferDev:::.all_args_insert(), where an object named out exists
    env_aah <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
    base::assign(x = "out", value = "ENV_VALUE", pos = -1, envir = env_aah, inherits = FALSE, immediate = TRUE)
    FUN_ENV <- base::eval(expr = base::parse(file = "", n = NULL, text = "function(){ base::paste0(out) }", prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = "function(){ base::paste0(out) }", timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = env_aah, enclos = base::baseenv())
    FUN_ENV_safer <- base::suppressMessages(expr = all_args_here(x = FUN_ENV, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = base::environment(fun = FUN_ENV_safer), expected = env_aah)
    testthat::expect_identical(object = FUN_ENV_safer(), expected = "ENV_VALUE")
    # argument written with an empty value and without space after the = sign (collapse=): its default value is inserted after a space
    FUN_EQ <- base::eval(expr = base::parse(file = "", n = NULL, text = "function(){ base::paste0(\"a\", collapse=) }", prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = "function(){ base::paste0(\"a\", collapse=) }", timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::globalenv(), enclos = base::baseenv())
    FUN_EQ_safer <- base::suppressMessages(expr = all_args_here(x = FUN_EQ, table_export = FALSE, fun_export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::any(base::grepl(x = utils::capture.output(FUN_EQ_safer, file = NULL, append = FALSE, type = "output", split = FALSE), pattern = "collapse= NULL", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = FALSE))
    testthat::expect_identical(object = FUN_EQ_safer(), expected = "a")
    # string default values of the arguments placed after ...: copied with their quotes (formerly, base::paste(x, "ab") got sep = , and base::interaction(x, x) got sep = ., which is an object name). The proposal for base::system.file(), which uses nargs(), is not inserted
    FUN_DOTS_STR <- function(x){
        a <- base::paste(x, "ab")
        b <- base::interaction(x, x)
        d <- base::system.file(x)
        base::list(a, b, d)
    }
    FUN_CAT <- function(x){
        base::cat("a", x, "\n")
    }
    tempo_msg_dots <- testthat::capture_messages(code = {tempo_fun_dots <- saferDev::all_args_here(x = FUN_DOTS_STR, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    tempo_txt <- base::paste0(utils::capture.output(tempo_fun_dots, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "base::paste(x, \"ab\", sep = \" \", collapse = NULL, recycle0 = FALSE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "base::interaction(x, x, drop = FALSE, sep = \".\", lex.order = FALSE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "d <- base::system.file(x)\n", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    tempo_msg_dots <- base::paste0(tempo_msg_dots, collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_dots, pattern = "line 4: system.file(x, package = \"base\", lib.loc = NULL, mustWork = FALSE)\n    REASON: the called function counts its arguments with nargs()", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_false(object = base::grepl(x = tempo_msg_dots, pattern = "ARGUMENTS WITH EMPTY VALUE", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_identical(object = tempo_fun_dots(x = "DESCRIPTION"), expected = FUN_DOTS_STR(x = "DESCRIPTION"))
    tempo_fun_cat <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_CAT, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_txt <- base::paste0(utils::capture.output(tempo_fun_cat, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "base::cat(\"a\", x, \"\\n\", file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_identical(object = utils::capture.output(tempo_fun_cat(x = "b"), file = NULL, append = FALSE, type = "output", split = FALSE), expected = utils::capture.output(FUN_CAT(x = "b"), file = NULL, append = FALSE, type = "output", split = FALSE))
    # default value of an argument tested with missing() by the called function: written with an empty value, which keeps the result (formerly, base::matrix(x, ncol = 2) got nrow = 1 and returned a 1 x 2 matrix)
    FUN_MISSING <- function(x){
        base::matrix(x, ncol = 2)
    }
    tempo_msg_missing <- testthat::capture_messages(code = {tempo_fun_missing <- saferDev::all_args_here(x = FUN_MISSING, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    tempo_txt <- base::paste0(utils::capture.output(tempo_fun_missing, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_txt, pattern = "base::matrix(data = x, ncol = 2, nrow = , byrow = FALSE, dimnames = NULL)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_identical(object = tempo_fun_missing(x = 1:6), expected = FUN_MISSING(x = 1:6))
    testthat::expect_true(object = base::grepl(x = base::paste0(tempo_msg_missing, collapse = "", recycle0 = FALSE), pattern = "line 2: base::matrix(): nrow (DEFAULT: 1, TESTED BY missing())", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::all_args_here(x = tempo_fun_missing, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")) # the empty value is accepted: nothing more to correct
    # same for the methods of an S3 generic defined in its package: as.POSIXct.POSIXlt() tests missing(tz)
    FUN_GENERIC <- function(x){
        base::as.POSIXct(x)
    }
    tempo_fun_generic <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_GENERIC, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::grepl(x = base::paste0(utils::capture.output(tempo_fun_generic, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE), pattern = "base::as.POSIXct(x = x, tz = )", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    tempo_lt <- base::as.POSIXlt(x = "2020-01-01 10:00:00", tz = "UTC", format = "%Y-%m-%d %H:%M:%S", optional = FALSE)
    testthat::expect_identical(object = tempo_fun_generic(x = tempo_lt), expected = FUN_GENERIC(x = tempo_lt))
    # function that counts its arguments with nargs(): nothing inserted (formerly, base::diag(3) became base::diag(x = 3, nrow = , ncol = , names = TRUE), a 1 x 1 matrix)
    FUN_NARGS <- function(x){
        base::diag(x)
    }
    tempo_msg_nargs <- testthat::capture_messages(code = {tempo_fun_nargs <- saferDev::all_args_here(x = FUN_NARGS, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_nargs)
    tempo_msg_nargs <- base::paste0(tempo_msg_nargs, collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_nargs, pattern = "CORRECTIONS NOT APPLIED:\nline 2: diag(x = x, nrow = , ncol = , names = TRUE)\n    REASON: the called function counts its arguments with nargs()", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_false(object = base::grepl(x = tempo_msg_nargs, pattern = "ARGUMENTS WITH EMPTY VALUE", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE)) # proposal not inserted: its empty values are not listed
    # own functions testing missing() or using nargs(), put in place of saferDev::report() during the call (mocking)
    own_missing <- function(a, b = 2){
        base::missing(b)
    }
    own_nargs <- function(a, b = 2){
        base::nargs()
    }
    FUN_OWN <- function(){
        saferDev::report(1)
    }
    tempo_fun_own <- testthat::with_mocked_bindings(code = base::suppressMessages(expr = saferDev::all_args_here(x = FUN_OWN, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), report = own_missing, .package = "saferDev", .env = base::environment(fun = NULL))
    testthat::expect_true(object = base::grepl(x = base::paste0(utils::capture.output(tempo_fun_own, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE), pattern = "saferDev::report(a = 1, b = )", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_identical(object = testthat::with_mocked_bindings(code = tempo_fun_own(), report = own_missing, .package = "saferDev", .env = base::environment(fun = NULL)), expected = TRUE)
    tempo_fun_own <- testthat::with_mocked_bindings(code = base::suppressMessages(expr = saferDev::all_args_here(x = FUN_OWN, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), report = own_nargs, .package = "saferDev", .env = base::environment(fun = NULL))
    testthat::expect_null(object = tempo_fun_own)
    # calls that are assignment targets (left side of <- or =, right side of ->), and their first argument when it is a call: left unchanged, because R then calls the replacement function (e.g., rownames<-), whose arguments differ (formerly, base::rownames(x) <- v became base::rownames(x = x, do.NULL = TRUE, prefix = "row") <- v, which fails). The other calls are corrected (here base::paste0())
    FUN_ASSIGN <- function(x, v){
        base::rownames(x) <- v
        base::c("P", "Q") -> base::colnames(x)
        base::attr(x, "a") <- base::paste0("b", "c")
        base::names(base::attr(x, base::paste0("a", ""))) = "e"
        x
    }
    tempo_msg_assign <- testthat::capture_messages(code = {tempo_fun_assign <- saferDev::all_args_here(x = FUN_ASSIGN, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    tempo_txt <- utils::capture.output(tempo_fun_assign, file = NULL, append = FALSE, type = "output", split = FALSE)
    testthat::expect_equal(object = tempo_txt[2:5], expected = base::c(
        "        base::rownames(x) <- v",
        "        base::c(\"P\", \"Q\") -> base::colnames(x)",
        "        base::attr(x, \"a\") <- base::paste0(\"b\", \"c\", collapse = NULL, recycle0 = FALSE)",
        "        base::names(base::attr(x, base::paste0(\"a\", \"\", collapse = NULL, recycle0 = FALSE))) = \"e\""
    ))
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "REASON: the call is the target of an assignment", text = base::paste0(tempo_msg_assign, collapse = "", recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 5) # rownames, colnames, attr (twice) and names
    tempo_df <- base::data.frame(p = 1, q = 2, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    testthat::expect_identical(object = tempo_fun_assign(x = tempo_df, v = "r1"), expected = FUN_ASSIGN(x = tempo_df, v = "r1"))
    # empty value proposed in a call of a builtin primitive function: nothing inserted (formerly, base::.Call() got PACKAGE = and failed with "argument 6 is empty"). Empty values remain proposed for special primitive functions, which accept them (e.g., base::substitute(expr = x, env = ))
    FUN_BUILTIN <- function(x){
        base::.Call(my_symbol, x)
    }
    tempo_msg_builtin <- testthat::capture_messages(code = {tempo_fun_builtin <- saferDev::all_args_here(x = FUN_BUILTIN, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_builtin)
    testthat::expect_true(object = base::grepl(x = base::paste0(tempo_msg_builtin, collapse = "", recycle0 = FALSE), pattern = "REASON: an argument is proposed with an empty value, but the called function is a builtin primitive function", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    FUN_SPECIAL <- function(x){
        base::substitute(x)
    }
    tempo_fun_special <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_SPECIAL, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::grepl(x = base::paste0(utils::capture.output(tempo_fun_special, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE), pattern = "base::substitute(expr = x, env = )", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_identical(object = tempo_fun_special(x = a + b), expected = FUN_SPECIAL(x = a + b))
    # strings written without argument name and containing spaces (formerly, these corrections were not applied, and NULL was returned)
    FUN_SPACE <- function(m, x){
        a <- base::sprintf("a %s", m)
        b <- base::strsplit(x, " ")
        d <- base::grepl("a  b", x)
        e <- base::gsub(" ", "", x)
        base::list(a, b, d, e)
    }
    tempo_msg_space <- testthat::capture_messages(code = {tempo_fun_space <- saferDev::all_args_here(x = FUN_SPACE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_false(object = base::grepl(x = base::paste0(tempo_msg_space, collapse = "", recycle0 = FALSE), pattern = "CORRECTIONS NOT APPLIED", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    tempo_txt <- utils::capture.output(tempo_fun_space, file = NULL, append = FALSE, type = "output", split = FALSE)
    testthat::expect_equal(object = tempo_txt[2:5], expected = base::c(
        "        a <- base::sprintf(fmt = \"a %s\", m)",
        "        b <- base::strsplit(x = x, split = \" \", fixed = FALSE, perl = FALSE, useBytes = FALSE)",
        "        d <- base::grepl(pattern = \"a  b\", x = x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)",
        "        e <- base::gsub(pattern = \" \", replacement = \"\", x = x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)"
    ))
    testthat::expect_identical(object = tempo_fun_space(m = "z", x = "a  b"), expected = FUN_SPACE(m = "z", x = "a  b"))
    # bare symbol written without argument name and named like an argument written with its name (here the variable collapse): not a reason to leave the call unchanged
    FUN_SYMBOL <- function(collapse){
        base::paste0(collapse, collapse = ",")
    }
    tempo_fun_symbol <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_SYMBOL, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = tempo_fun_symbol(collapse = base::c("a", "b")), expected = "a,b")
    # first argument of an S3 generic not written in the call (formula interface): not added, because the method is chosen from the first argument written (formerly, stats::t.test(formula = y ~ g, data = d) got x = and failed). It is added when UseMethod() has an object argument (graphics::Axis())
    FUN_DISPATCH <- function(d){
        stats::t.test(formula = y ~ g, data = d)
    }
    tempo_msg_dispatch <- testthat::capture_messages(code = {tempo_fun_dispatch <- saferDev::all_args_here(x = FUN_DISPATCH, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_dispatch)
    testthat::expect_true(object = base::grepl(x = base::paste0(tempo_msg_dispatch, collapse = "", recycle0 = FALSE), pattern = "REASON: the first argument of the called function (S3 generic) is not written in the call", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    FUN_AXIS <- function(){
        graphics::Axis(side = 1)
    }
    tempo_fun_axis <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_AXIS, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::grepl(x = base::paste0(utils::capture.output(tempo_fun_axis, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE), pattern = "graphics::Axis(side = 1, x = NULL, ", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # argument added that a method without ... does not have: not inserted (formerly, base::round(t) became base::round(x = t, digits = 0), which fails for date-times, round.POSIXt() having no digits argument). The functions named like methods that are not methods (all.names() for base::all(), qr.coef() for base::qr()) are not considered
    FUN_METHOD <- function(t){
        a <- base::round(t)
        b <- base::round(t, "mins")
        base::list(a, b)
    }
    tempo_msg_method <- testthat::capture_messages(code = {tempo_fun_method <- saferDev::all_args_here(x = FUN_METHOD, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_method)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "REASON: a method of the called function (S3 generic) has neither ... nor an argument added by the proposal", text = base::paste0(tempo_msg_method, collapse = "", recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 2)
    FUN_NOT_METHOD <- function(v, m){
        a <- base::all(v)
        b <- base::qr(m)
        base::list(a, b)
    }
    tempo_fun_not_method <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_NOT_METHOD, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_txt <- utils::capture.output(tempo_fun_not_method, file = NULL, append = FALSE, type = "output", split = FALSE)
    testthat::expect_equal(object = tempo_txt[2:3], expected = base::c(
        "        a <- base::all(v, na.rm = FALSE)",
        "        b <- base::qr(x = m)"
    ))
    tempo_mat <- base::matrix(data = base::c(2, 1, 1, 3), nrow = 2, ncol = 2, byrow = FALSE, dimnames = NULL)
    testthat::expect_identical(object = tempo_fun_not_method(v = base::c(TRUE, FALSE), m = tempo_mat), expected = FUN_NOT_METHOD(v = base::c(TRUE, FALSE), m = tempo_mat))
    # MISSING_ARGS that cannot be parsed (function written without argument name, whose body of several lines is joined into one line): not inserted
    FUN_BLOCK <- function(v){
        base::sapply(v, function(x){
            y <- x
            y
        })
    }
    tempo_msg_block <- testthat::capture_messages(code = {tempo_fun_block <- saferDev::all_args_here(x = FUN_BLOCK, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_block)
    testthat::expect_true(object = base::grepl(x = base::paste0(tempo_msg_block, collapse = "", recycle0 = FALSE), pattern = "REASON: the MISSING_ARGS or DEF_ARGS column of the call cannot be read", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # value written without argument name that is a comparison with == (formerly read as an argument written with its name: base::matrix(nrow == 2) became base::matrix(nrow == 2, data = NA, ...), which returned another matrix, and base::lapply(a == b, v) was not corrected)
    FUN_EQUAL <- function(a, b, v, nrow){
        d <- base::lapply(a == b, v)
        e <- base::matrix(nrow == 2)
        base::list(d, e)
    }
    tempo_msg_equal <- testthat::capture_messages(code = {tempo_fun_equal <- saferDev::all_args_here(x = FUN_EQUAL, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_false(object = base::grepl(x = base::paste0(tempo_msg_equal, collapse = "", recycle0 = FALSE), pattern = "CORRECTIONS NOT APPLIED", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_equal(object = utils::capture.output(tempo_fun_equal, file = NULL, append = FALSE, type = "output", split = FALSE)[2:3], expected = base::c(
        "        d <- base::lapply(X = a == b, FUN = v)",
        "        e <- base::matrix(data = nrow == 2, nrow = , ncol = , byrow = FALSE, dimnames = NULL)"
    ))
    testthat::expect_identical(object = tempo_fun_equal(a = 1:2, b = 1, v = base::identity, nrow = 2), expected = FUN_EQUAL(a = 1:2, b = 1, v = base::identity, nrow = 2))
    # call whose function is the result of a call without argument: corrected
    FUN_CALL_CALL <- function(){
        base::get("emptyenv")()
    }
    tempo_fun_call_call <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_CALL_CALL, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::grepl(x = base::paste0(utils::capture.output(tempo_fun_call_call, file = NULL, append = FALSE, type = "output", split = FALSE), collapse = "\n", recycle0 = FALSE), pattern = "base::get(x = \"emptyenv\", pos = -1L, envir = , mode = \"any\", inherits = TRUE)()", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_identical(object = tempo_fun_call_call(), expected = FUN_CALL_CALL())
    # empty values of the proposals not inserted: not listed in the ARGUMENTS WITH EMPTY VALUE message (formerly, envir of base::formals(a) <- v was listed, although this assignment target is left unchanged)
    FUN_RECAP <- function(a){
        base::formals(a) <- base::alist(z = )
        b <- utils::getFromNamespace(x = "arg_check", ns = "saferDev")
        a
    }
    tempo_msg_recap <- base::paste0(testthat::capture_messages(code = {saferDev::all_args_here(x = FUN_RECAP, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_recap, pattern = "line 2: formals(fun = a, envir = )\n    REASON: the call is the target of an assignment", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_false(object = base::grepl(x = tempo_msg_recap, pattern = "line 2: base::formals()", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_msg_recap, pattern = "line 3: utils::getFromNamespace(): envir (DEFAULT: as.environment(pos))", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # string written without argument name on several lines (formerly not corrected, with the false reason that the argument is also proposed with its name), and value written without argument name containing a comment
    txt_multi_str <- base::c(
        "function(x){",
        "    a <- base::grepl(\"a",
        "   b\", x)",
        "    d <- base::grepl(base::paste0(\"a\", # comment",
        "        \"b\"), x)",
        "    base::list(a, d)",
        "}"
    )
    FUN_MULTI_STR <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_multi_str, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_multi_str, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL))
    tempo_msg_multi_str <- testthat::capture_messages(code = {tempo_fun_multi_str <- saferDev::all_args_here(x = FUN_MULTI_STR, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_false(object = base::grepl(x = base::paste0(tempo_msg_multi_str, collapse = "", recycle0 = FALSE), pattern = "CORRECTIONS NOT APPLIED", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_equal(object = utils::capture.output(tempo_fun_multi_str, file = NULL, append = FALSE, type = "output", split = FALSE)[base::seq_along(along.with = txt_multi_str)], expected = base::c(
        "function(x){",
        "    a <- base::grepl(pattern = \"a",
        "   b\", x = x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)",
        "    d <- base::grepl(pattern = base::paste0(\"a\", # comment",
        "        \"b\", collapse = NULL, recycle0 = FALSE), x = x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)",
        "    base::list(a, d)",
        "}"
    ))
    tempo_str <- base::c("a b", "a\n   b", "ab")
    testthat::expect_identical(object = tempo_fun_multi_str(x = tempo_str), expected = FUN_MULTI_STR(x = tempo_str))
    # default value whose deparsed code is longer than a line (conf.args of stats::qqplot()): written on one line (formerly, the DEF_ARGS, MISSING_ARGS and STATUS columns were garbled, and the call was not corrected), with the package of its function (base::list())
    FUN_LONG_DEF <- function(a, b){
        stats::qqplot(a, b, plot.it = FALSE)
    }
    tempo_fun_long_def <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_LONG_DEF, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::grepl(x = utils::capture.output(tempo_fun_long_def, file = NULL, append = FALSE, type = "output", split = FALSE)[2], pattern = "^        stats::qqplot\\(x = a, y = b, plot.it = FALSE, .*conf.args = base::list\\(exact = NULL, .*, border = NULL\\)\\)$", fixed = FALSE, ignore.case = FALSE, perl = TRUE, useBytes = FALSE))
    testthat::expect_identical(object = tempo_fun_long_def(a = base::c(1, 3, 2, 5), b = base::c(2, 1, 4, 3)), expected = FUN_LONG_DEF(a = base::c(1, 3, 2, 5), b = base::c(2, 1, 4, 3)))
    # the final warning is emitted before the restoration of the warning.length option, so that it is not truncated (formerly, warning.length was restored first)
    tempo_store <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L)
    tempo_ini_options <- base::options(warning.length = 100)
    base::withCallingHandlers(
        expr = base::suppressMessages(expr = saferDev::all_args_here(x = fun2, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"),
        warning = function(w){
            base::assign(x = "warning_length", value = base::getOption(x = "warning.length", default = NULL), pos = -1, envir = tempo_store, inherits = FALSE, immediate = TRUE)
            base::invokeRestart(r = "muffleWarning")
        }
    )
    tempo_warning_length_after <- base::getOption(x = "warning.length", default = NULL)
    base::options(tempo_ini_options)
    testthat::expect_equal(object = tempo_store$warning_length, expected = 8170)
    testthat::expect_equal(object = tempo_warning_length_after, expected = 100)
    # commas inside [], [[]] and {} of an argument are not argument separators, also after a raw string with [ delimiters (formerly, base::rev(m[, 1]) and base::rep_len(x = ... + m[1, 2], length.out = 2) stopped with INTERNAL ERROR 8, base::round(m[1, 2], digits = 1) got the wrong proposal round(x = m[1, digits = 1, 2]) and base::rep(m[1, ], times = 2) was not corrected)
    txt_bracket <- base::c(
        "function(x){",
        "    m <- base::matrix(data = 1:4, nrow = 2, ncol = 2, byrow = FALSE, dimnames = NULL)",
        "    y <- base::rep_len(x = base::nchar(r\"[a\"b]\", type = \"chars\", allowNA = FALSE, keepNA = NA) + m[1, 2], length.out = 2)",
        "    z <- base::sapply(X = 1:2, FUN = function(i){m[i, 2]})",
        "    base::c(y, z, base::rev(m[, 1]), base::rep(m[1, ], times = 2), base::round(m[1, 2], digits = 1), base::rev(m[[2, 1]]))",
        "}"
    )
    FUN_BRACKET <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_bracket, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_bracket, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo_msg_bracket <- testthat::capture_messages(code = {tempo_fun_bracket <- saferDev::all_args_here(x = FUN_BRACKET, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_false(object = base::grepl(x = base::paste0(tempo_msg_bracket, collapse = "", recycle0 = FALSE), pattern = "CORRECTIONS NOT APPLIED", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_equal(object = utils::capture.output(tempo_fun_bracket, file = NULL, append = FALSE, type = "output", split = FALSE)[base::seq_along(along.with = txt_bracket)], expected = base::c(
        "function(x){",
        "    m <- base::matrix(data = 1:4, nrow = 2, ncol = 2, byrow = FALSE, dimnames = NULL)",
        "    y <- base::rep_len(x = base::nchar(x = r\"[a\"b]\", type = \"chars\", allowNA = FALSE, keepNA = NA) + m[1, 2], length.out = 2)",
        "    z <- base::sapply(X = 1:2, FUN = function(i){m[i, 2]}, simplify = TRUE, USE.NAMES = TRUE)",
        "    base::c(y, z, base::rev(x = m[, 1]), base::rep(x = m[1, ], times = 2), base::round(x = m[1, 2], digits = 1), base::rev(x = m[[2, 1]]))",
        "}"
    ))
    testthat::expect_identical(object = tempo_fun_bracket(x = 1), expected = FUN_BRACKET(x = 1))
    FUN_DROP <- function(m, j){
        base::rowSums(x = m[ , j, drop = FALSE], na.rm = TRUE, dims = 1L)
    }
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DROP, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")) # all the arguments are written: nothing to correct (formerly, INTERNAL ERROR 8)
    # names preceded by @ (slots) are not analysed, like those preceded by $ (formerly, x@mean(1) stopped with an error asking to run colons_check() first)
    FUN_SLOT <- function(x){
        y <- x@mean(1)
        base::sum(1)
    }
    tempo_msg_slot <- testthat::capture_messages(code = {tempo_fun_slot <- saferDev::all_args_here(x = FUN_SLOT, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_equal(object = utils::capture.output(tempo_fun_slot, file = NULL, append = FALSE, type = "output", split = FALSE)[1:4], expected = base::c(
        "function(x){",
        "        y <- x@mean(1)",
        "        base::sum(1, na.rm = FALSE)",
        "    }"
    ))
    testthat::expect_false(object = base::grepl(x = base::paste0(tempo_msg_slot, collapse = "", recycle0 = FALSE), pattern = "mean", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE)) # x@mean() not in the table
    # same with a TAB between $ and the name (formerly, x$<TAB>mean(1) stopped with an error asking to run colons_check() first). Function written with base::parse() to keep the TAB
    txt_tab <- base::c(
        "function(x){",
        "    y <- x$\tmean(1)",
        "    base::sum(1)",
        "}"
    )
    FUN_TAB <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_tab, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_tab, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo_fun_tab <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_TAB, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(tempo_fun_tab, file = NULL, append = FALSE, type = "output", split = FALSE)[base::seq_along(along.with = txt_tab)], expected = base::c(
        "function(x){",
        "    y <- x$\tmean(1)",
        "    base::sum(1, na.rm = FALSE)",
        "}"
    ))
    testthat::expect_identical(object = tempo_fun_tab(x = base::list(mean = base::mean)), expected = FUN_TAB(x = base::list(mean = base::mean)))
    # call of a function with the same name as a name preceded by $ or @: analysed with its own arguments (formerly, base::sum(y, 3) got the arguments of x$sum(1), and was not corrected)
    FUN_SAME_NAME <- function(x){
        y <- x$sum(1)
        base::sum(y, 3)
    }
    tempo_msg_same_name <- testthat::capture_messages(code = {tempo_fun_same_name <- saferDev::all_args_here(x = FUN_SAME_NAME, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_true(object = base::grepl(x = base::paste0(tempo_msg_same_name, collapse = "", recycle0 = FALSE), pattern = "sum(y, 3, na.rm = FALSE)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE)) # STATUS column
    testthat::expect_equal(object = utils::capture.output(tempo_fun_same_name, file = NULL, append = FALSE, type = "output", split = FALSE)[2:3], expected = base::c(
        "        y <- x$sum(1)",
        "        base::sum(y, 3, na.rm = FALSE)"
    ))
    testthat::expect_identical(object = tempo_fun_same_name(x = base::list(sum = base::sum)), expected = FUN_SAME_NAME(x = base::list(sum = base::sum)))
    # function exported by a package but defined in another one (graphics::plot, defined in base, written by colons_check() for plot()): its arguments are read in the exports of the package (formerly, INTERNAL ERROR 4). Here the first argument is written with its name (see below for the first argument written without its name). The proposal is not inserted, because y = matches ylim and ylab of plot.table(), registered by graphics for base::plot() (partial matching), so that graphics::plot(x = a, y = ) fails for tables and histograms (formerly, inserted)
    FUN_REEXPORT <- function(a){
        graphics::plot(x = a)
    }
    tempo_msg_reexport <- testthat::capture_messages(code = {tempo_fun_reexport <- saferDev::all_args_here(x = FUN_REEXPORT, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_reexport)
    testthat::expect_true(object = base::grepl(x = base::paste0(tempo_msg_reexport, collapse = "", recycle0 = FALSE), pattern = "line 2: plot(x = a, y = )\n    REASON: a method of the called function (S3 generic) lacks an argument added by the proposal, but has arguments whose names start with its name, which R matches partially", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # re-exported function whose proposal is inserted (stats4::show, defined in methods): the corrected function gives the same output
    FUN_REEXPORT_S4 <- function(a){
        stats4::show(a)
    }
    tempo_fun_reexport_s4 <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_REEXPORT_S4, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(tempo_fun_reexport_s4, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        stats4::show(object = a)")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_reexport_s4(a = 1:3), file = NULL, append = FALSE, type = "output", split = FALSE), expected = utils::capture.output(FUN_REEXPORT_S4(a = 1:3), file = NULL, append = FALSE, type = "output", split = FALSE))
    # same through the colons_check() then all_args_here() chain: the re-export is read, but the proposal is not inserted, either because plot.formula(), registered by graphics for base::plot(), names the first argument formula (first argument written without its name), or because y = matches ylim and ylab of plot.table() (first argument written with its name)
    FUN_PLOT <- function(x){
        plot(x)
    }
    FUN_PLOT_NAMED <- function(x){
        plot(x = x)
    }
    tempo_fun_plot <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_PLOT, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(tempo_fun_plot, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        graphics::plot(x)")
    tempo_msg_plot <- testthat::capture_messages(code = {tempo_fun_plot2 <- saferDev::all_args_here(x = tempo_fun_plot, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_plot2)
    testthat::expect_true(object = base::grepl(x = base::paste0(tempo_msg_plot, collapse = "", recycle0 = FALSE), pattern = "line 2: plot(x = x, y = )\n    REASON: the first argument is written without its name, and a method of this S3 generic names it differently", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    tempo_fun_plot_named <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_PLOT_NAMED, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(tempo_fun_plot_named, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        graphics::plot(x = x)")
    tempo_msg_plot_named <- testthat::capture_messages(code = {tempo_fun_plot_named2 <- saferDev::all_args_here(x = tempo_fun_plot_named, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_plot_named2)
    testthat::expect_true(object = base::grepl(x = base::paste0(tempo_msg_plot_named, collapse = "", recycle0 = FALSE), pattern = "line 2: plot(x = x, y = )\n    REASON: a method of the called function (S3 generic) lacks an argument added by the proposal, but has arguments whose names start with its name, which R matches partially", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # call on the right side of a pipe (|> on one line or split over two lines, %>%, %T>%, %<>% and %!>%): not inserted, with its reason, and the values written without name are not named in the proposal, because the value of the pipe takes the first argument (formerly, n |> base::rep(3) became n |> base::rep(x = 3), which computes base::rep(3, n), and m |> base::matrix(2) became m |> base::matrix(data = 2, nrow = , ncol = , byrow = FALSE, dimnames = NULL)). Function written with base::parse() to keep its source references, where |> is written
    txt_pipe <- base::c(
        "function(n, m){",
        "    a <- n |> base::rep(3)",
        "    b <- m |>",
        "        base::matrix(2)",
        "    d <- m %>% base::matrix(2)",
        "    base::list(a, b, d)",
        "}"
    )
    FUN_PIPE <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_pipe, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_pipe, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo_msg_pipe <- testthat::capture_messages(code = {tempo_fun_pipe <- saferDev::all_args_here(x = FUN_PIPE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_pipe)
    tempo_msg_pipe <- base::paste0(tempo_msg_pipe, collapse = "", recycle0 = FALSE)
    testthat::expect_false(object = base::grepl(x = tempo_msg_pipe, pattern = "rep(x = 3)", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE)) # n takes x and 3 goes into ...: nothing to add
    testthat::expect_true(object = base::grepl(x = tempo_msg_pipe, pattern = "line 4: matrix(2, ncol = , byrow = FALSE, dimnames = NULL)\n    REASON: the call receives a value from a pipe", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_msg_pipe, pattern = "line 5: matrix(2, ncol = , byrow = FALSE, dimnames = NULL)\n    REASON: the call receives a value from a pipe", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # same without source references: the magrittr pipes stay in the deparsed code (%!>% is the eager pipe of magrittr, formerly not detected), while %$%, which passes no argument, is corrected
    FUN_PIPE_DEPARSED <- base::eval(expr = base::parse(file = "", n = NULL, text = "function(m){\n    a <- m %T>% base::matrix(2)\n    m %<>% base::matrix(2)\n    d <- m %!>% base::matrix(2)\n    b <- m %$% base::matrix(2)\n    base::list(a, b, d)\n}", prompt = "?", keep.source = FALSE, srcfile = NULL, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo_msg_pipe2 <- testthat::capture_messages(code = {tempo_fun_pipe2 <- saferDev::all_args_here(x = FUN_PIPE_DEPARSED, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_equal(object = utils::capture.output(tempo_fun_pipe2, file = NULL, append = FALSE, type = "output", split = FALSE)[3:6], expected = base::c(
        "    a <- m %T>% base::matrix(2)",
        "    m %<>% base::matrix(2)",
        "    d <- m %!>% base::matrix(2)",
        "    b <- m %$% base::matrix(data = 2, nrow = , ncol = , byrow = FALSE, dimnames = NULL)"
    ))
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "REASON: the call receives a value from a pipe", text = base::paste0(tempo_msg_pipe2, collapse = "", recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 3)
    # call that forwards ...: not inserted, with its reason, and ... is not taken as the value of an argument (formerly, utils::read.csv(text = txt, ...) became utils::read.csv(text = txt, file = ..., header = TRUE, ...), which fails when ... contains header, and base::paste(..., sep = "-") got collapse = NULL, which fails when ... contains collapse)
    FUN_DOTS_FORWARD <- function(txt, ...){
        a <- utils::read.csv(text = txt, ...)
        b <- base::paste(..., sep = "-")
        base::list(a, b)
    }
    tempo_msg_dots_forward <- testthat::capture_messages(code = {tempo_fun_dots_forward <- saferDev::all_args_here(x = FUN_DOTS_FORWARD, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_null(object = tempo_fun_dots_forward)
    tempo_msg_dots_forward <- base::paste0(tempo_msg_dots_forward, collapse = "", recycle0 = FALSE)
    testthat::expect_false(object = base::grepl(x = tempo_msg_dots_forward, pattern = "file = ...", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_msg_dots_forward, pattern = "line 3: paste(..., sep = \"-\", collapse = NULL, recycle0 = FALSE)\n    REASON: the call forwards ..., whose content can match the added arguments", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "REASON: the call forwards ...", text = tempo_msg_dots_forward, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 2)
    # first argument of an S3 generic written without its name, while a method registered for this generic names it differently (formula in t.test.formula(), update.formula() names it old, and plot.formula() is registered by graphics for base::plot()): not inserted, with its reason (formerly, stats::t.test(y ~ g, data = d) became stats::t.test(x = y ~ g, data = d), which fails). base::sort(), whose methods all name it x, is corrected
    testthat::expect_true(object = base::requireNamespace(package = "graphics", quietly = TRUE)) # graphics registers plot.formula() when loaded
    FUN_FIRST_ARG <- function(d, fo){
        a <- stats::t.test(y ~ g, data = d)
        b <- stats::update(fo, . ~ . + z)
        base::plot(y ~ x, data = d)
        base::sort(d$y)
    }
    tempo_msg_first_arg <- testthat::capture_messages(code = {tempo_fun_first_arg <- saferDev::all_args_here(x = FUN_FIRST_ARG, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")})
    testthat::expect_equal(object = utils::capture.output(tempo_fun_first_arg, file = NULL, append = FALSE, type = "output", split = FALSE)[2:5], expected = base::c(
        "        a <- stats::t.test(y ~ g, data = d)",
        "        b <- stats::update(fo, . ~ . + z)",
        "        base::plot(y ~ x, data = d)",
        "        base::sort(x = d$y, decreasing = FALSE)"
    ))
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "REASON: the first argument is written without its name, and a method of this S3 generic names it differently", text = base::paste0(tempo_msg_first_arg, collapse = "", recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 3)
    # base::isS4(): written with x =, which runs (formerly, base::isS4(object = x), which fails, base::args() naming the argument object)
    FUN_ISS4 <- function(x){
        base::isS4(x)
    }
    tempo_fun_iss4 <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_ISS4, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(tempo_fun_iss4, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        base::isS4(x = x)")
    testthat::expect_false(object = tempo_fun_iss4(x = 1))
    # empty arguments written without space: proposal shown and not inserted, with its reason (formerly, an argument check error of .all_args_here_fill(), or the reason "cannot be located" for base::sum(x,))
    FUN_EMPTY_NO_SPACE <- function(n, x, nm){
        a <- base::matrix(, n, 0L)
        b <- base::sum(x,,1)
        d <- base::matrix(, nrow = 2, ncol = 2)
        e <- stats::setNames(, nm)
        f <- base::sum(x,)
        base::list(a, b, d, e, f)
    }
    testthat::expect_no_error(object = tempo_msg_empty_no_space <- testthat::capture_messages(code = {tempo_fun_empty_no_space <- saferDev::all_args_here(x = FUN_EMPTY_NO_SPACE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), message = NULL, class = NULL)
    testthat::expect_null(object = tempo_fun_empty_no_space)
    tempo_msg_empty_no_space <- base::paste0(tempo_msg_empty_no_space, collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_empty_no_space, pattern = "line 2: matrix(data =  , nrow =  n, ncol =  0L, byrow = FALSE, dimnames = NULL)\n    REASON: the call has an empty argument", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    testthat::expect_true(object = base::grepl(x = tempo_msg_empty_no_space, pattern = "line 6: sum(x, , na.rm = FALSE)\n    REASON: the call has an empty argument", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE)) # last empty argument
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "REASON: the call has an empty argument", text = tempo_msg_empty_no_space, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 5)
    FUN_NOT_EMPTY <- function(n){
        base::rep_len(1, n)
    }
    tempo_fun_not_empty <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_NOT_EMPTY, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(tempo_fun_not_empty, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        base::rep_len(x = 1, length.out = n)") # last value n, made of a letter of the escape sequences \n, \t and \r: not an empty argument
    # function whose arguments all have NA as default value: not analysed, with a warning (formerly, an argument check error of .all_args_here_fill())
    FUN_NA_DEFAULT <- function(){
        base::Sys.umask()
    }
    testthat::expect_warning(object = tempo_fun_na_default <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_NA_DEFAULT, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), regexp = "(1) IN LINE 2, THE CALL\nbase::Sys.umask()\nIS NOT ANALYZED, BECAUSE ALL THE ARGUMENTS OF ITS FUNCTION HAVE NA AS DEFAULT VALUE: mode.", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_null(object = tempo_fun_na_default)
    # call written with its package and named like a function created inside x: not taken as this function (formerly, base::c(v, c(v)) was INTERNAL_FUNCTION, thus not checked)
    FUN_INTERNAL_PREFIX <- function(v){
        c <- function(...){1}
        base::c(v, c(v))
    }
    tempo_msg_internal_prefix <- base::paste0(testthat::capture_messages(code = {saferDev::all_args_here(x = FUN_INTERNAL_PREFIX, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "INTERNAL_FUNCTION", text = tempo_msg_internal_prefix, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 1) # only c(v)
    # call written without package: saferDev::colons_check() advised only for the basic R functions
    FUN_NOT_BASIC <- function(x){
        my_fun(x)
    }
    testthat::expect_error(object = saferDev::all_args_here(x = FUN_NOT_BASIC, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "PLEASE, WRITE THE PACKAGE OF THIS FUNCTION BEFORE ITS NAME (E.G., pkg::my_fun()) IN FUN_NOT_BASIC,\nAND RERUN saferDev::all_args_here(FUN_NOT_BASIC).\nOF NOTE, saferDev::colons_check() ONLY ADDS THE PACKAGE OF THE BASIC R FUNCTIONS.", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    FUN_BASIC <- function(x){
        paste0(x)
    }
    testthat::expect_error(object = saferDev::all_args_here(x = FUN_BASIC, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "PLEASE, RUN saferDev::colons_check(FUN_BASIC) FIRST,\nAND RERUN saferDev::all_args_here(FUN_BASIC).", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # values written without name and arguments placed before ..., all with a default value (graphics::title(main = NULL, sub = NULL, ...), base::ordered(x = character(), ...)): the values get the names of these arguments, as in R (formerly, base::ordered(v) became base::ordered(v, x = character()), which returns an empty factor, v going into ...)
    FUN_BEFORE_DOTS <- function(v){
        base::ordered(v)
    }
    tempo_fun_before_dots <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_BEFORE_DOTS, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(tempo_fun_before_dots, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        base::ordered(x = v)")
    testthat::expect_identical(object = tempo_fun_before_dots(v = base::c("b", "a", "b")), expected = FUN_BEFORE_DOTS(v = base::c("b", "a", "b")))
    FUN_TITLE <- function(){
        graphics::title("Main")
    }
    tempo_fun_title <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_TITLE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(tempo_fun_title, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        graphics::title(main = \"Main\", sub = NULL, xlab = NULL, ylab = NULL, line = NA, outer = FALSE)")
    # calls inside formulas and function called by .Internal(): not analyzed (SKIPPED), because the modeling functions use the text of the formulas, and .Internal() the bare name of its function (formerly, log(x), offset(z), mean(.x) and toupper(s) stopped all_args_here() with NOT ASSOCIATED TO ITS PACKAGE, and base::log(x) written inside a formula was completed, which renames the coefficient)
    FUN_FORMULA <- function(d, s){
        a <- stats::coef(object = stats::lm(formula = y ~ log(x) + offset(z), data = d))
        b <- ~ mean(.x)
        base::list(a, base::all.names(expr = b, functions = TRUE, max.names = -1L, unique = TRUE), base::.Internal(toupper(s)))
    }
    tempo_d_formula <- base::data.frame(y = base::c(1.2, 2.3, 2.9, 4.4, 5.1, 5.8), x = base::c(1, 2, 3, 4, 5, 6), z = base::c(0.1, 0.5, 0.2, 0.8, 0.3, 0.6), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    tempo_msg_formula <- base::paste0(testthat::capture_messages(code = {tempo_fun_formula <- saferDev::all_args_here(x = FUN_FORMULA, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "SKIPPED", text = tempo_msg_formula, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 4, info = NULL, label = NULL) # log(x), offset(z), mean(.x) and toupper(s)
    tempo_fun_formula_code <- utils::capture.output(tempo_fun_formula, file = NULL, append = FALSE, type = "output", split = FALSE)
    testthat::expect_true(object = base::startsWith(x = tempo_fun_formula_code[2], prefix = "        a <- stats::coef(object = stats::lm(formula = y ~ log(x) + offset(z), data = d, subset = , "), info = NULL, label = NULL) # stats::lm() completed, formula unchanged
    testthat::expect_identical(object = tempo_fun_formula_code[3:4], expected = base::c("        b <- ~ mean(.x)", "        base::list(a, base::all.names(expr = b, functions = TRUE, max.names = -1L, unique = TRUE), base::.Internal(call = toupper(s)))"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_formula(d = tempo_d_formula, s = "a"), expected = FUN_FORMULA(d = tempo_d_formula, s = "a"), info = NULL, label = NULL, expected.label = NULL) # same coefficients, with the same names
    FUN_FORMULA_PREFIX <- function(d){
        stats::coef(object = stats::lm(formula = y ~ base::log(x), data = d))
    }
    tempo_fun_formula_prefix <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_FORMULA_PREFIX, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::startsWith(x = utils::capture.output(tempo_fun_formula_prefix, file = NULL, append = FALSE, type = "output", split = FALSE)[2], prefix = "        stats::coef(object = stats::lm(formula = y ~ base::log(x), data = d, subset = , "), info = NULL, label = NULL) # base::log(x) inside the formula not completed
    testthat::expect_identical(object = tempo_fun_formula_prefix(d = tempo_d_formula), expected = FUN_FORMULA_PREFIX(d = tempo_d_formula), info = NULL, label = NULL, expected.label = NULL) # same coefficient names (formerly, base::log(x = x, base = exp(1)) instead of base::log(x))
    # calls inside quote(), bquote(), expression(), alist() and substitute() (code used as data, e.g., plotmath labels): not analyzed (SKIPPED) nor completed, even written without package, while the quoting functions are analyzed (formerly, base::paste() was completed inside the plotmath label, which then showed its sep, collapse and recycle0 arguments, base::quote(expr = base::sqrt(2)) became base::quote(expr = base::sqrt(x = 2)), and sqrt(2) written without package stopped all_args_here() with NOT ASSOCIATED TO ITS PACKAGE)
    FUN_QUOTE <- function(t){
        a <- base::expression(base::paste("Temp (", degree, "C)"))
        b <- base::alist(x = , y = sqrt(2))
        base::list(a, b, base::quote(expr = base::sqrt(2)), base::paste0(t))
    }
    tempo_msg_quote <- base::paste0(testthat::capture_messages(code = {tempo_fun_quote <- saferDev::all_args_here(x = FUN_QUOTE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "SKIPPED", text = tempo_msg_quote, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 3, info = NULL, label = NULL) # base::paste(), sqrt() and base::sqrt()
    testthat::expect_identical(object = utils::capture.output(tempo_fun_quote, file = NULL, append = FALSE, type = "output", split = FALSE)[2:4], expected = base::c("        a <- base::expression(base::paste(\"Temp (\", degree, \"C)\"))", "        b <- base::alist(x = , y = sqrt(2))", "        base::list(a, b, base::quote(expr = base::sqrt(2)), base::paste0(t, collapse = NULL, recycle0 = FALSE))"), info = NULL, label = NULL, expected.label = NULL) # base::paste0() completed, the quoted code unchanged
    testthat::expect_identical(object = tempo_fun_quote(t = 1), expected = FUN_QUOTE(t = 1), info = NULL, label = NULL, expected.label = NULL) # same plotmath label, formal arguments and quoted call
    # bquote() splicing with ..(): the calls of base::c() and .. inside bquote() are SKIPPED, and base::bquote() is completed (formerly, all_args_here() stopped with INTERNAL ERROR 3, because the name .. was searched as a regular expression, whose dots matched the two spaces before the bracket of a call already processed)
    FUN_SPLICE <- function(l){
        base::bquote(expr = base::c(..(l)), splice = TRUE)
    }
    tempo_msg_splice <- base::paste0(testthat::capture_messages(code = {tempo_fun_splice <- saferDev::all_args_here(x = FUN_SPLICE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "SKIPPED", text = tempo_msg_splice, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 2, info = NULL, label = NULL) # base::c() and ..
    testthat::expect_identical(object = utils::capture.output(tempo_fun_splice, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        base::bquote(expr = base::c(..(l)), splice = TRUE, where = )", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_splice(l = base::list(1, 2)), expected = FUN_SPLICE(l = base::list(1, 2)), info = NULL, label = NULL, expected.label = NULL)
    # name ending a line, followed by a line starting with an expression between brackets: a variable, not a function call (formerly, C was taken for a call of a function whose closing bracket was missing, and all_args_here() stopped with an internal error)
    FUN_NEWLINE <- function(z){
        C <- base::list(1, 2)
        y <- C
        (z)
        base::c(y, base::paste0(z))
    }
    tempo_fun_newline <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_NEWLINE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_newline, file = NULL, append = FALSE, type = "output", split = FALSE)[3:5], expected = base::c("        y <- C", "        (z)", "        base::c(y, base::paste0(z, collapse = NULL, recycle0 = FALSE))"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_newline(z = 3), expected = FUN_NEWLINE(z = 3), info = NULL, label = NULL, expected.label = NULL)
    # name of a called function that is also the end of a word followed by a bracket (here f, the end of if): the arguments of the call are taken from the call itself (formerly, from the brackets of if(), so that the FUN_ARGS column showed f(f(x)), and a function named or called in a for() loop was analyzed with the arguments of the loop, i in x)
    FUN_SUFFIX <- function(x){
        f <- function(y){y}
        if(f(x)){
            base::paste0(x)
        }
    }
    tempo_msg_suffix <- base::paste0(testthat::capture_messages(code = {tempo_fun_suffix <- saferDev::all_args_here(x = FUN_SUFFIX, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_suffix, pattern = "\\sf\\(x\\)\\s", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), info = NULL, label = NULL) # FUN_ARGS of the call of f
    testthat::expect_false(object = base::grepl(x = tempo_msg_suffix, pattern = "f(f(x))", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    # name not called (statement ending with the name, followed by a line starting with an expression between brackets), written before a call of the same function: the arguments of the call are taken from the call itself (formerly, from the brackets of the line after the name, so that FUN_ARGS showed paste0 (x) and the correction was not applied)
    FUN_SAME_NAME <- function(x){
        y <- base::paste0
        (x)
        base::paste0(x, "a")
    }
    tempo_fun_same_name <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_SAME_NAME, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_same_name, file = NULL, append = FALSE, type = "output", split = FALSE)[4], expected = "        base::paste0(x, \"a\", collapse = NULL, recycle0 = FALSE)", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_same_name(x = "b"), expected = FUN_SAME_NAME(x = "b"), info = NULL, label = NULL, expected.label = NULL)
    # call whose ( is on the line after its name, inside the brackets of another call: analyzed with its own arguments, the line break being shown as a space in FUN_ARGS (formerly, INTERNAL ERROR 3 of .fun_args_pos(), or FUN_ARGS paste0( (x)), because a ( was added at the end of the line of the name in the code read by all_args_here(), whose brackets were then unbalanced), and the regular error for such a call written without package
    FUN_SPLIT <- function(x){
        base::nchar(base::paste0
        (x))
    }
    tempo_msg_split <- base::paste0(testthat::capture_messages(code = {tempo_fun_split <- saferDev::all_args_here(x = FUN_SPLIT, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_split, pattern = " nchar(base::paste0 (x)) ", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL) # FUN_ARGS of the call of nchar
    testthat::expect_false(object = base::grepl(x = tempo_msg_split, pattern = "paste0( (x)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    testthat::expect_identical(object = tempo_fun_split(x = "b"), expected = FUN_SPLIT(x = "b"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = utils::capture.output(tempo_fun_split, file = NULL, append = FALSE, type = "output", split = FALSE)[2:3], expected = base::c("        base::nchar(x = base::paste0", "        (x, collapse = NULL, recycle0 = FALSE), type = \"chars\", allowNA = FALSE, keepNA = NA)"), info = NULL, label = NULL, expected.label = NULL) # both calls corrected, the line break kept
    FUN_SPLIT_NO_PKG <- function(z){
        base::c(paste0
        (z))
    }
    testthat::expect_error(object = saferDev::all_args_here(x = FUN_SPLIT_NO_PKG, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE 2", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # same call, its name separated from its ( by an empty line: the regular error too (formerly, paste0() was not analyzed, and the message said that everything seemed clean)
    FUN_SPLIT_BLANK_NO_PKG <- function(z){
        base::c(paste0

        (z))
    }
    testthat::expect_error(object = saferDev::all_args_here(x = FUN_SPLIT_BLANK_NO_PKG, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE 2", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # value piped into a quoting call by the native pipe (base::paste0("a", x) |> base::quote() is base::quote(base::paste0("a", x))): SKIPPED and not completed, as the calls inside the brackets of quote() (formerly, completed, so that the quoted call was changed)
    FUN_PIPE_QUOTE <- function(x){
        a <- base::paste0("a", x) |> base::quote()
        base::list(a, base::eval(expr = a, envir = base::list(x = 1), enclos = base::baseenv()))
    }
    tempo_msg_pipe_quote <- base::paste0(testthat::capture_messages(code = {tempo_fun_pipe_quote <- saferDev::all_args_here(x = FUN_PIPE_QUOTE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_pipe_quote, pattern = "SKIPPED", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    testthat::expect_null(object = tempo_fun_pipe_quote, info = NULL, label = NULL)
    # calls written without package of the internal functions: formal arguments of x or of the functions defined in x, functions created inside x (with = or <-, at any position, including after ; or with a line break after <-) and x itself (recursive call): INTERNAL_FUNCTION (formerly, NOT ASSOCIATED TO ITS PACKAGE error, except for name <- function( at the beginning of a line)
    FUN_INTERNAL_ARG <- function(x, FUN){
        inner = function(a){a} ; inner2 <- function(b, FUN2){FUN2(b)}
        inner3 <-
            function(a){a}
        base::c(FUN(x), inner(x), inner2(x, base::sqrt), inner3(x))
    }
    tempo_msg_internal_arg <- base::paste0(testthat::capture_messages(code = {saferDev::all_args_here(x = FUN_INTERNAL_ARG, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "INTERNAL_FUNCTION", text = tempo_msg_internal_arg, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 5, info = NULL, label = NULL) # FUN2(b), FUN(x), inner(x), inner2(x, base::sqrt) and inner3(x)
    FUN_RECURSIVE <- function(n){
        if(n <= 1){
            1
        }else{
            n * FUN_RECURSIVE(n = n - 1)
        }
    }
    tempo_msg_recursive <- base::paste0(testthat::capture_messages(code = {saferDev::all_args_here(x = FUN_RECURSIVE, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_recursive, pattern = "FUN_RECURSIVE\\(n = n - 1\\)\\s+[0-9]+ INTERNAL_FUNCTION", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), info = NULL, label = NULL)
    # x given as a function object (e.g., through base::do.call()): the "x" placeholder is not the name of x, so that the call of a function named x is not a recursive call and, as the calls of the other functions, must be written with its package
    x <- function(a){a + 1}
    FUN_OBJ <- function(v){
        x(v)
    }
    testthat::expect_error(object = base::do.call(what = saferDev::all_args_here, args = base::list(x = FUN_OBJ, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), quote = FALSE, envir = base::environment(fun = NULL)), regexp = "CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE 2", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    base::rm(list = "x", pos = -1, envir = base::environment(fun = NULL), inherits = FALSE)
    # call written with its package and named like a formal argument of x (length, c): analyzed, not taken as an internal function
    FUN_ARG_PREFIX <- function(x, length){
        base::length(x)
    }
    tempo_fun_arg_prefix <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_ARG_PREFIX, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_arg_prefix, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        base::length(x = x)", info = NULL, label = NULL, expected.label = NULL)
    FUN_ARG_C <- function(v, c){
        base::c(v, 1)
    }
    tempo_msg_arg_c <- base::paste0(testthat::capture_messages(code = {saferDev::all_args_here(x = FUN_ARG_C, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_false(object = base::grepl(x = tempo_msg_arg_c, pattern = "INTERNAL_FUNCTION", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    # code that R evaluates inside bquote() and substitute() (.() of bquote(), ..() with splice = TRUE, where argument of bquote(), env argument of substitute(), also piped), after colons_check(): analyzed and completed, as saferDev 2.0.0 proposed, while the rest of the quoted code is SKIPPED and unchanged (formerly, SKIPPED)
    FUN_Q01 <- function(y){base::bquote(x + .(mean(y)))}
    tempo_fun_q01 <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_Q01, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_q01 <- base::paste0(testthat::capture_messages(code = {tempo_fun_q01_2 <- saferDev::all_args_here(x = tempo_fun_q01, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_q01, pattern = "mean(x = y)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "Q01: STATUS of the evaluated call")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_q01_2, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(expr = x + .(base::mean(x = y)), where = , splice = FALSE)}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_q01_2(y = 1:3), expected = FUN_Q01(y = 1:3), info = NULL, label = NULL, expected.label = NULL)
    FUN_Q03 <- function(y){base::substitute(u + v, list(u = sum(y)))}
    tempo_fun_q03 <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_Q03, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_q03 <- base::paste0(testthat::capture_messages(code = {tempo_fun_q03_2 <- saferDev::all_args_here(x = tempo_fun_q03, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_q03, pattern = "sum(y, na.rm = FALSE)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "Q03: STATUS of the evaluated call")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_q03_2, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::substitute(expr = u + v, env = base::list(u = base::sum(y, na.rm = FALSE)))}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_q03_2(y = 1:3), expected = FUN_Q03(y = 1:3), info = NULL, label = NULL, expected.label = NULL)
    FUN_Q04 <- function(y){base::bquote(f(..(lapply(y, as.name))), splice = TRUE)}
    tempo_fun_q04 <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_Q04, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_q04 <- base::paste0(testthat::capture_messages(code = {tempo_fun_q04_2 <- saferDev::all_args_here(x = tempo_fun_q04, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_q04, pattern = "lapply(X = y, FUN =  as.name)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "Q04: STATUS of the evaluated call")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_q04_2, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(expr = f(..(base::lapply(X = y, FUN = as.name))), splice = TRUE, where = )}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_q04_2(y = base::c("a", "b")), expected = FUN_Q04(y = base::c("a", "b")), info = NULL, label = NULL, expected.label = NULL)
    FUN_Q09 <- function(y){list(u = sum(y)) |> base::substitute(expr = u + v, env = _)}
    tempo_fun_q09 <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_Q09, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_q09 <- base::paste0(testthat::capture_messages(code = {tempo_fun_q09_2 <- saferDev::all_args_here(x = tempo_fun_q09, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_q09, pattern = "sum(y, na.rm = FALSE)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "Q09: STATUS of the evaluated call")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_q09_2, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::list(u = base::sum(y, na.rm = FALSE)) |> base::substitute(expr = u + v, env = _)}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_q09_2(y = 1:3), expected = FUN_Q09(y = 1:3), info = NULL, label = NULL, expected.label = NULL)
    FUN_Q12 <- function(y){base::bquote(.(as.name(y)) ~ x)}
    tempo_fun_q12 <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_Q12, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_q12 <- base::paste0(testthat::capture_messages(code = {tempo_fun_q12_2 <- saferDev::all_args_here(x = tempo_fun_q12, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_msg_q12, pattern = "as.name(x = y)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "Q12: STATUS of the evaluated call")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_q12_2, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(expr = .(base::as.name(x = y)) ~ x, where = , splice = FALSE)}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_q12_2(y = "a"), expected = FUN_Q12(y = "a"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "SKIPPED", text = tempo_msg_q04, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 2, info = NULL, label = NULL) # f() and ..() of Q04
    # scope of the internal functions, after colons_check(): an argument c or t of a function defined inside x does not prevent the analysis of the c() and t() calls outside this function (formerly, INTERNAL_FUNCTION)
    FUN_SC1 <- function(x){
        g <- function(y, c){
            c(y)
        }
        a <- c(x, 1)
        g(a, base::sum)
    }
    tempo_fun_sc1 <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_SC1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_sc1 <- base::paste0(testthat::capture_messages(code = {saferDev::all_args_here(x = tempo_fun_sc1, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "INTERNAL_FUNCTION", text = tempo_msg_sc1, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 2, info = NULL, label = NULL) # c(y) and g(a, base::sum)
    testthat::expect_true(object = base::grepl(x = tempo_msg_sc1, pattern = "c\\(x, 1\\)\\s+[0-9]+\\s+\\.\\.\\.", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), info = NULL, label = "c(x, 1) analyzed") # DEF_ARGS ... (base::c() has no other argument)
    FUN_SC2 <- function(x){
        lapply(X = x, FUN = function(t){t + 1})
        t(x)
    }
    tempo_fun_sc2 <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_SC2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_sc2 <- base::paste0(testthat::capture_messages(code = {tempo_fun_sc2_2 <- saferDev::all_args_here(x = tempo_fun_sc2, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_false(object = base::grepl(x = tempo_msg_sc2, pattern = "INTERNAL_FUNCTION", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(tempo_fun_sc2_2, file = NULL, append = FALSE, type = "output", split = FALSE)[3], expected = "        base::t(x = x)", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_sc2_2(x = base::matrix(data = 1:6, nrow = 2, ncol = 3, byrow = FALSE, dimnames = NULL)), expected = FUN_SC2(x = base::matrix(data = 1:6, nrow = 2, ncol = 3, byrow = FALSE, dimnames = NULL)), info = NULL, label = NULL, expected.label = NULL)
    # formal arguments of a function definition quoted by bquote(), after colons_check(): not internal functions, so that the mean() and sd() calls of the .() evaluated by bquote() are analyzed and completed (formerly, INTERNAL_FUNCTION)
    FUN_QDEF <- function(v){
        base::eval(base::bquote(function(x, mean = .(mean(v)), sd = .(sd(v))){
            stats::dnorm(x = x, mean = mean, sd = sd, log = FALSE)
        }))
    }
    tempo_fun_qdef <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_QDEF, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_qdef <- base::paste0(testthat::capture_messages(code = {tempo_fun_qdef_2 <- saferDev::all_args_here(x = tempo_fun_qdef, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_false(object = base::grepl(x = tempo_msg_qdef, pattern = "INTERNAL_FUNCTION", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(tempo_fun_qdef_2, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        base::eval(expr = base::bquote(expr = function(x, mean = .(base::mean(x = v)), sd = .(stats::sd(x = v, na.rm = FALSE))){", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_qdef_2(v = base::c(1, 2, 3))(x = 0), expected = FUN_QDEF(v = base::c(1, 2, 3))(x = 0), info = NULL, label = NULL, expected.label = NULL)
    # name assigned a function inside a function definition quoted by bquote(): not an internal function, so that the rev() call of the .() evaluated by bquote() must be written with its package, and is analyzed and completed after colons_check() (formerly, INTERNAL_FUNCTION)
    FUN_QDEF4 <- function(y){base::bquote(function(a){rev <- function(z){z} ; .(rev(y))})}
    testthat::expect_error(object = saferDev::all_args_here(x = FUN_QDEF4, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE 1", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    tempo_fun_qdef4 <- base::suppressMessages(expr = saferDev::colons_check(x = FUN_QDEF4, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_msg_qdef4 <- base::paste0(testthat::capture_messages(code = {tempo_fun_qdef4_2 <- saferDev::all_args_here(x = tempo_fun_qdef4, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_false(object = base::grepl(x = tempo_msg_qdef4, pattern = "INTERNAL_FUNCTION", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(tempo_fun_qdef4_2, file = NULL, append = FALSE, type = "output", split = FALSE)[1], expected = "function(y){base::bquote(expr = function(a){rev <- function(z){z} ; .(base::rev(x = y))}, where = , splice = FALSE)}", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_qdef4_2(y = base::c(1, 2))[[3]][[3]], expected = FUN_QDEF4(y = base::c(1, 2))[[3]][[3]], info = NULL, label = NULL, expected.label = NULL) # value of .(rev(y)) in the returned function definition
    # arguments of a bquote() or substitute() call whose argument matching fails (error of R, which then evaluates none of them): SKIPPED (formerly, the arguments matched to where or env were analyzed)
    FUN_QM <- function(y){base::bquote(x, w = base::mean(y), wh = 1)}
    tempo_msg_qm <- base::paste0(testthat::capture_messages(code = {saferDev::all_args_here(x = FUN_QM, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::sum(base::gregexpr(pattern = "SKIPPED", text = tempo_msg_qm, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE) == 1, info = NULL, label = NULL) # base::mean(y)
    # primitive function: no R code to check, a warning and NULL, and no file written (formerly, an error asked to run colons_check() first, which returned a function returning base::sum instead of the sum)
    tempo_dir_prim <- base::tempfile(pattern = "aah_prim_", tmpdir = base::tempdir(check = FALSE), fileext = "")
    base::dir.create(path = tempo_dir_prim, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    testthat::expect_warning(object = tempo_prim <- saferDev::all_args_here(x = base::sum, table_export = TRUE, fun_export = TRUE, out_path = tempo_dir_prim, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE TESTED FUNCTION base::sum IS A PRIMITIVE FUNCTION, WHICH HAS NO R CODE.\n\nNOTHING TO CHECK.", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_null(object = tempo_prim, info = NULL, label = NULL)
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_prim, "res.tsv", fsep = "/")) | base::file.exists(base::file.path(tempo_dir_prim, "sum_safer.R", fsep = "/")), info = NULL, label = "no file written")
    testthat::expect_warning(object = tempo_prim <- saferDev::all_args_here(x = base::c, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE TESTED FUNCTION base::c IS A PRIMITIVE FUNCTION, WHICH HAS NO R CODE.", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_null(object = tempo_prim, info = NULL, label = NULL)
    base::unlink(x = tempo_dir_prim, recursive = TRUE, force = TRUE, expand = TRUE)
    # default values copied into the calls: their function calls written with their package and their arguments named, so that all_args_here() run on the modified function finds nothing to complete (formerly, base = exp(1) was copied, and the second run stopped with NOT ASSOCIATED TO ITS PACKAGE)
    FUN_DEF_LOG <- function(){
        base::log(x = 2)
    }
    tempo_fun_def_log <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_LOG, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_def_log, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        base::log(x = 2, base = base::exp(x = 1))", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_def_log(), expected = FUN_DEF_LOG(), info = NULL, label = NULL, expected.label = NULL)
    tempo_msg_def_log <- base::paste0(testthat::capture_messages(code = {tempo_fun_def_log2 <- saferDev::all_args_here(x = tempo_fun_def_log, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE) # second run, on the modified function
    testthat::expect_null(object = tempo_fun_def_log2, info = NULL, label = NULL)
    testthat::expect_true(object = base::grepl(x = tempo_msg_def_log, pattern = "INSIDE tempo_fun_def_log(), EVERYTHING SEEMS CLEAN.", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    # default value using a function that the checked function does not find (here, its environment only sees base, as if grDevices was not attached): not copied, empty value (formerly, which = dev.cur() was copied, which fails in this function)
    FUN_DEF_DEV <- function(){
        grDevices::dev.off()
    }
    tempo_fun_def_dev <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_DEV, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_def_dev, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        grDevices::dev.off(which = grDevices::dev.cur())", info = NULL, label = NULL, expected.label = NULL)
    base::environment(fun = FUN_DEF_DEV) <- base::new.env(hash = TRUE, parent = base::baseenv(), size = 29L)
    tempo_msg_def_dev <- base::paste0(testthat::capture_messages(code = {tempo_fun_def_dev <- saferDev::all_args_here(x = FUN_DEF_DEV, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = "")}), collapse = "", recycle0 = FALSE)
    testthat::expect_identical(object = utils::capture.output(tempo_fun_def_dev, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        grDevices::dev.off(which = )", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_true(object = base::grepl(x = tempo_msg_def_dev, pattern = "ARGUMENTS WITH EMPTY VALUE (DEFAULT VALUE THAT CANNOT BE COPIED, OR NO DEFAULT VALUE):\nline 2: grDevices::dev.off(): which (DEFAULT: dev.cur())", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    # default value using an internal function of a package (.externalCallerEnv() of methods), which cannot be written pkg::fun: not copied (formerly copied, which fails because the function is not found)
    FUN_DEF_UNEXP <- function(){
        methods::getClasses()
    }
    tempo_fun_def_unexp <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_UNEXP, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_def_unexp, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        methods::getClasses(where = , inherits = )", info = NULL, label = NULL, expected.label = NULL)
    # variable of a default value masked by an argument of the checked function, or by another object of the environment of the checked function: not copied, so that the result is unchanged (formerly, method = p.adjust.methods was copied, which then used this argument or this object)
    FUN_DEF_MASK <- function(p, p.adjust.methods){
        stats::p.adjust(p = p)
    }
    tempo_fun_def_mask <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_MASK, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_def_mask, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        stats::p.adjust(p = p, method = , n = )", info = NULL, label = NULL, expected.label = NULL)
    tempo_p <- base::c(0.01, 0.02, 0.03)
    testthat::expect_identical(object = tempo_fun_def_mask(p = tempo_p, p.adjust.methods = "none"), expected = FUN_DEF_MASK(p = tempo_p, p.adjust.methods = "none"), info = NULL, label = NULL, expected.label = NULL)
    FUN_DEF_ENV <- function(p){
        stats::p.adjust(p = p)
    }
    tempo_env_def <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
    base::assign(x = "p.adjust.methods", value = "none", pos = -1, envir = tempo_env_def, inherits = FALSE, immediate = TRUE)
    base::environment(fun = FUN_DEF_ENV) <- tempo_env_def
    tempo_fun_def_env <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_ENV, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_def_env, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        stats::p.adjust(p = p, method = , n = )", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = tempo_fun_def_env(p = tempo_p), expected = FUN_DEF_ENV(p = tempo_p), info = NULL, label = NULL, expected.label = NULL)
    # same if the object of the environment of the checked function cannot be evaluated (promise that returns an error)
    base::rm(list = "p.adjust.methods", pos = -1, envir = tempo_env_def, inherits = FALSE)
    base::delayedAssign(x = "p.adjust.methods", value = base::stop("promise error", call. = FALSE, domain = NULL), eval.env = base::baseenv(), assign.env = tempo_env_def)
    tempo_fun_def_env <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_ENV, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_def_env, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        stats::p.adjust(p = p, method = , n = )", info = NULL, label = NULL, expected.label = NULL)
    # default value using a variable created in the body of the called function (file in utils::file.edit()) and named like an object found from the checked function (base::file()): not copied (formerly, title = file was copied, which then gave base::file()). editor = getOption("editor") is not copied either, because getOption() would be written without its default argument
    FUN_DEF_LOCAL <- function(f){
        utils::file.edit(f)
    }
    tempo_fun_def_local <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_LOCAL, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(tempo_fun_def_local, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "        utils::file.edit(f, title = , editor = , fileEncoding = \"\")", info = NULL, label = NULL, expected.label = NULL)
    # right operand of $ and operands of :: (names of an element, of a package and of an object, not objects): copied (formerly, rel.tol = .Machine$double.eps^0.25 and optim = stats::optim were not copied, double.eps and stats being taken as objects)
    FUN_DEF_DOLLAR <- function(f){
        stats::integrate(f = f, lower = 0, upper = 1)
    }
    tempo_fun_def_dollar <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_DOLLAR, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::grepl(x = utils::capture.output(tempo_fun_def_dollar, file = NULL, append = FALSE, type = "output", split = FALSE)[2], pattern = ", rel.tol = .Machine$double.eps^0.25, abs.tol = , ", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    testthat::expect_identical(object = tempo_fun_def_dollar(f = base::sqrt)$value, expected = FUN_DEF_DOLLAR(f = base::sqrt)$value, info = NULL, label = NULL, expected.label = NULL)
    FUN_DEF_COLONS <- function(f){
        stats4::mle(minuslogl = f)
    }
    tempo_fun_def_colons <- base::suppressMessages(expr = saferDev::all_args_here(x = FUN_DEF_COLONS, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::grepl(x = utils::capture.output(tempo_fun_def_colons, file = NULL, append = FALSE, type = "output", split = FALSE)[2], pattern = ", optim = stats::optim, ", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = NULL)
    # option keep.parse.data set to FALSE (e.g., by base::sys.source() during the loading of a file): the corrections are applied as with the default TRUE (formerly, the parse data were not available, so that NULL was returned, with the false reason that the MISSING_ARGS or DEF_ARGS column of the call could not be read)
    fun_kpd2 <- function(x){base::paste0(x)}
    res_kpd2_ref <- base::suppressMessages(expr = saferDev::all_args_here(x = fun_kpd2, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    oop_kpd <- base::options(keep.parse.data = FALSE)
    res_kpd2 <- base::tryCatch(expr = base::suppressMessages(expr = saferDev::all_args_here(x = fun_kpd2, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), error = function(e){e}, finally = base::options(oop_kpd))
    testthat::expect_identical(object = utils::capture.output(res_kpd2, file = NULL, append = FALSE, type = "output", split = FALSE), expected = utils::capture.output(res_kpd2_ref, file = NULL, append = FALSE, type = "output", split = FALSE), info = NULL, label = NULL, expected.label = NULL)
    ## end other tests
    base::unlink(x = out_dir1, recursive = TRUE, force = FALSE, expand = TRUE)
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
