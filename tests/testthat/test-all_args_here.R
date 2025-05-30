testthat::test_that("all_args_here()", {

    ## data argument values
    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}

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
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(roc2), "), col2 (", base::length(roc2), "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")
        result <- base::sub("\\($##", "", matched_strings) ; base::range(1:3) # sub("\\($##", "", matched_strings)
        tempo.cat <- base::paste0("IAGE\nLENGTHS OF roc00() (",  stats::ks.test(roc3), "), and roc0 (", base::length(roc5), ") SHOULL\n")
        base::return(baba) # base::sub("\\($##", "", matched_strings)
        base::return(bobo) # a$sub("\\($##", "", matched_strings)
    }
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(all_args_here(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
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
    testthat::expect_error(all_args_here(x = test, export = NULL, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = NULL, overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(all_args_here(x = character(), export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = integer(), export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = double(), export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = logical(), export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = complex(), export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = data.frame(), export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = list(), export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, export = character(), path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = integer(), path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = double(), path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = logical(), path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = complex(), path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = data.frame(), path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = list(), path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = character(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = integer(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = double(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = logical(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = complex(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = data.frame(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = list(), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = character(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = integer(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = double(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = logical(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = complex(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = data.frame(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = list(), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(all_args_here(x = NA, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = NA, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = NA, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = NA, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = NULL, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = NA, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = 1, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = -1, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = 1:2, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = c(TRUE, FALSE), export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = mat1, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = factor1, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = expr1, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = fun1, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end x
    # export
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = NULL, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = NA, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = 1, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = -1, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = 1:2, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = c(TRUE, FALSE), path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = mat1, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = factor1, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = expr1, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = fun1, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end export
    # path_out
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = NA, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = 1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = -1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = 1:2, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = c(TRUE, FALSE), df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = mat1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = factor1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = expr1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = fun1, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end path_out
    # df_name
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = NULL, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = NA, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = 1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = -1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = 1:2, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = c(TRUE, FALSE), overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = mat1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = factor1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = expr1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = fun1, overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end df_name
    # overwrite
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = fun1, safer_check = TRUE, lib_path = NULL, error_text = "")) # empty function
    # end overwrite
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = "", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(all_args_here(x = test, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) 
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

    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})
