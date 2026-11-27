testthat::test_that("get_message()", {

    testthat::local_reproducible_output(width = 80, crayon = FALSE, unicode = FALSE, rstudio = FALSE, hyperlinks = FALSE, lang = "C", .env = base::environment(fun = NULL)) # same outputs in every session (no fancy quotes, English messages), as in R CMD check, even under testthat edition 2

    ## data argument values
    str1 <- "wilcox.test(letters, c(1, 2, 4))" # error
    str2 <- "message('ahah')" # message
    str3 <- "message('ahah'); warning('ohoh')" # message and warning
    str4 <- "sum(1, 2, 3)" # nothing
    str5 <- "ggplot2::ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()" # ggplot and message
    str6 <- "1+factor(1)" # warning
    str7 <- "ggplot2::ggplot(data = data.frame(X = c(1:10, NA), stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()" # ggplot and warning
    str8 <-  "1+factor(1)" # warning message
    str9 <- "ggplot2::ggplot(data = data.frame(X = 'a', stringsAsFactors = FALSE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()" # ggplot and error
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    # str5, str7 and str9 need ggplot2 (Suggests): their expectations are inside if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(get_message(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::get_message()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(get_message()) # the function has default values for all args.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(get_message(data = NULL, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = NULL, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = NULL, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = NULL, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_no_error(get_message(data = character(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = integer(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = double(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = logical(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = complex(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = data.frame(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = list(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(get_message(data = str1, kind = character(), header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = integer(), header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = double(), header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = logical(), header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = complex(), header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = data.frame(), header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = list(), header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(get_message(data = str1, kind = "error", header = character(), print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = integer(), print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = double(), print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = logical(), print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = complex(), print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = data.frame(), print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = list(), print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = character(), text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = integer(), text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = double(), text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = logical(), text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = complex(), text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = data.frame(), text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = list(), text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = character(), env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = integer(), env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = double(), env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = logical(), env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = complex(), env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = data.frame(), env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = list(), env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = NULL, env = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = NULL, env = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = NULL, env = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = NULL, env = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = NULL, env = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = NULL, env = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE,  print_no = FALSE, text = NULL, env = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    # an environment without object is not an empty argument (base::length() of an environment is its number of objects)
    testthat::expect_null(object = saferDev::get_message(data = "base::sum(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = base::new.env(hash = TRUE, parent = base::baseenv(), size = 29L), safer_check = TRUE, lib_path = NULL, error_text = ""), info = NULL, label = NULL)
    fun_empty_frame <- function(){
        saferDev::get_message(data = "base::sum(1)", kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = base::environment(fun = NULL), safer_check = TRUE, lib_path = NULL, error_text = "") # env of a function frame without object
    }
    testthat::expect_identical(object = fun_empty_frame(), expected = "NO ERROR MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_null(object = saferDev::get_message(data = "base::sum(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = .GlobalEnv, safer_check = TRUE, lib_path = NULL, error_text = ""), info = NULL, label = NULL) # documented example, even if .GlobalEnv is empty


    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(get_message(data = NA, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = NA, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = NA, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = NA, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NA, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, in the saferDev::arg_check() calls, so the lib_path check is always executed)
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # data
    testthat::expect_error(get_message(data = NULL, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = NA, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = 1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = c(TRUE, FALSE), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = mat1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = factor1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = expr1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = fun1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end data
    # kind
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = NULL, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = NA, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = 1, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = c(TRUE, FALSE), header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = mat1, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = factor1, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = expr1, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = fun1, header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end kind
    # header
    testthat::expect_error(get_message(data = str1, kind = "error", header = NULL, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = NA, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = 1, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = c(TRUE, FALSE), print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = mat1, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = factor1, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = expr1, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = fun1, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end header
    # print_no
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = NULL, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = NA, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = 1, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = c(TRUE, FALSE), text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = mat1, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = factor1, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = expr1, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = fun1, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end print_no
    # text
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = str1, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NA, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = 1, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = c(TRUE, FALSE), env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = mat1, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = factor1, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = expr1, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = fun1, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end text
    # env
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = baseenv(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(object = saferDev::get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = .GlobalEnv, safer_check = TRUE, lib_path = NULL, error_text = ""), message = NULL, class = NULL) # formerly an error when .GlobalEnv was empty
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end env
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(get_message(data = str1, kind = "", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) 
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) 
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
    dev_list_test <- grDevices::dev.list() # devices open before these tests
    testthat::expect_error(get_message(data = plot(1), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # plot(1) drawn on the NULL pdf device of get_message(): no Rplots.pdf file created
    grDevices::pdf(file = NULL, width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = , timestamp = , producer = , author = ) # device open before running get_message(), on a NULL file (no Rplots.pdf file created)
    dev_user <- grDevices::dev.cur()
    graphics::plot(x = 1, y = NULL)
    testthat::expect_error(get_message(data = plot(1), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = "plot(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_identical(object = grDevices::dev.cur(), expected = dev_user, info = NULL, label = NULL, expected.label = NULL) # current device unchanged
    base::invisible(x = grDevices::dev.off(which = dev_user))
    testthat::expect_error(get_message(data = ggplot(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = "plot(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = "ggplot()", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_identical(object = grDevices::dev.list(), expected = dev_list_test, info = NULL, label = NULL, expected.label = NULL) # NULL pdf device of get_message() closed
    # devices left open by the instruction are closed, and the result is returned (formerly an INTERNAL ERROR)
    result <- saferDev::get_message(data = "grDevices::pdf(file = NULL)", kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result, expected = "NO ERROR MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = grDevices::dev.list(), expected = dev_list_test, info = NULL, label = NULL, expected.label = NULL)
    result <- saferDev::get_message(data = "{grDevices::pdf(file = NULL) ; grDevices::pdf(file = NULL) ; base::warning(\"two devices left open\")}", kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result, expected = "WARNING MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL, : two devices left open\n", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = grDevices::dev.list(), expected = dev_list_test, info = NULL, label = NULL, expected.label = NULL)
    # with several devices open, the current device and its multi-panel layout are kept (formerly, another device became current and graphics::par() was reset)
    grDevices::pdf(file = NULL, width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = , timestamp = , producer = , author = )
    dev_user_a <- grDevices::dev.cur()
    grDevices::pdf(file = NULL, width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = , timestamp = , producer = , author = )
    dev_user_b <- grDevices::dev.cur() # current device, opened last
    graphics::par(mfrow = base::c(2, 2), no.readonly = FALSE)
    graphics::plot(x = 1, y = NULL) # first of the 4 panels
    mfg_ini <- graphics::par("mfg", no.readonly = FALSE)
    result <- saferDev::get_message(data = "graphics::plot(x = 1, y = NULL)", kind = "message", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result, expected = "NO STANDARD MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = grDevices::dev.cur(), expected = dev_user_b, info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = graphics::par("mfg", no.readonly = FALSE), expected = mfg_ini, info = NULL, label = NULL, expected.label = NULL) # the next plot goes into the second panel
    base::invisible(x = grDevices::dev.off(which = dev_user_b))
    base::invisible(x = grDevices::dev.off(which = dev_user_a))
    testthat::expect_identical(object = grDevices::dev.list(), expected = dev_list_test, info = NULL, label = NULL, expected.label = NULL)
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # if(base::all(tempo.error == TRUE, na.rm = TRUE)){
    testthat::expect_error(get_message(data = NULL, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end if(base::all(tempo.error == TRUE, na.rm = TRUE)){
    # }else{
        testthat::expect_no_error(get_message(data = "NULL", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        result <- get_message(data = "NULL", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        expected <- NULL
        testthat::expect_equal(result, expected)
        # single evaluation of the instruction, whatever kind (formerly 3 evaluations with kind = "warning" or "message")
        env_count <- base::new.env(hash = TRUE, parent = base::baseenv(), size = 29L)
        base::assign(x = "counter", value = 0, pos = -1L, envir = env_count, inherits = FALSE, immediate = TRUE)
        data_count <- "{counter <- counter + 1 ; base::message(\"m\") ; base::warning(\"w\") ; counter}"
        result <- saferDev::get_message(data = data_count, kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = env_count, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_identical(object = result, expected = "NO ERROR MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
        testthat::expect_identical(object = base::get(x = "counter", pos = -1L, envir = env_count, mode = "any", inherits = FALSE), expected = 1, info = NULL, label = NULL, expected.label = NULL)
        result <- saferDev::get_message(data = data_count, kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = env_count, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_identical(object = result, expected = "WARNING MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL, : w\n", info = NULL, label = NULL, expected.label = NULL)
        testthat::expect_identical(object = base::get(x = "counter", pos = -1L, envir = env_count, mode = "any", inherits = FALSE), expected = 2, info = NULL, label = NULL, expected.label = NULL)
        result <- saferDev::get_message(data = data_count, kind = "message", header = TRUE, print_no = TRUE, text = NULL, env = env_count, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_identical(object = result, expected = "STANDARD MESSAGE REPORTED:\nm", info = NULL, label = NULL, expected.label = NULL)
        testthat::expect_identical(object = base::get(x = "counter", pos = -1L, envir = env_count, mode = "any", inherits = FALSE), expected = 3, info = NULL, label = NULL, expected.label = NULL)
        # printed outputs are not caught, and printed once: capture.output() snippet of the documentation
        printed <- utils::capture.output(msg <- saferDev::get_message(data = "print(1) ; log(-1)", kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), file = NULL, append = FALSE, type = "output", split = FALSE)
        testthat::expect_identical(object = printed, expected = "[1] 1", info = NULL, label = NULL, expected.label = NULL)
        testthat::expect_identical(object = msg, expected = "WARNING MESSAGE REPORTED:\nIn log(-1): NaNs produced\n", info = NULL, label = NULL, expected.label = NULL)
        # if(base::any(base::class(x = tempo_value) %in% base::c("gg", "ggplot"), na.rm = TRUE)){
        if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){
            testthat::expect_no_error(get_message(data = str5, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            result <- get_message(data = str5, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
            expected <- NULL
            testthat::expect_equal(result, expected)
            testthat::expect_no_error(get_message(data = str9, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            result <- saferDev::get_message(data = str9, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
            testthat::expect_match(object = result, regexp = "ERROR MESSAGE REPORTED:\nIn ggplot2::geom_histogram() : ", perl = FALSE, fixed = TRUE, all = TRUE, info = NULL, label = NULL) # error raised when the plot is built (printed)
            # ggplot2 plot built (printed) once, whatever kind (formerly 3 times with kind = "warning" or "message"): the summary function of the plot counts its calls, the same number for each kind
            env_count <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
            base::assign(x = "counter", value = 0, pos = -1L, envir = env_count, inherits = FALSE, immediate = TRUE)
            data_gg <- "ggplot2::ggplot(data = base::data.frame(X = 1, Y = 1), mapping = ggplot2::aes(x = X, y = Y)) + ggplot2::stat_summary(fun = function(y){base::assign(x = \"counter\", value = counter + 1, pos = -1L, envir = base::parent.env(env = base::environment(fun = NULL)), inherits = FALSE, immediate = TRUE) ; base::mean(x = y)}, geom = \"point\")"
            result <- saferDev::get_message(data = data_gg, kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = env_count, safer_check = TRUE, lib_path = NULL, error_text = "")
            testthat::expect_identical(object = result, expected = "NO ERROR MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
            count_one_build <- base::get(x = "counter", pos = -1L, envir = env_count, mode = "any", inherits = FALSE) # calls for one build of the plot
            testthat::expect_true(object = count_one_build > 0, info = NULL, label = NULL)
            result <- saferDev::get_message(data = data_gg, kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = env_count, safer_check = TRUE, lib_path = NULL, error_text = "")
            testthat::expect_identical(object = result, expected = "NO WARNING MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
            testthat::expect_identical(object = base::get(x = "counter", pos = -1L, envir = env_count, mode = "any", inherits = FALSE), expected = 2 * count_one_build, info = NULL, label = NULL, expected.label = NULL)
            result <- saferDev::get_message(data = data_gg, kind = "message", header = TRUE, print_no = TRUE, text = NULL, env = env_count, safer_check = TRUE, lib_path = NULL, error_text = "")
            testthat::expect_identical(object = result, expected = "NO STANDARD MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
            testthat::expect_identical(object = base::get(x = "counter", pos = -1L, envir = env_count, mode = "any", inherits = FALSE), expected = 3 * count_one_build, info = NULL, label = NULL, expected.label = NULL)
        }
        # end if(base::any(base::class(x = tempo_value) %in% base::c("gg", "ggplot"), na.rm = TRUE)){
        # if( ! base::all(base::class(x = tempo.error) == "try-error", na.rm = TRUE)){
        # utils::capture.output() below: show(1) prints [1] 1, which must not be printed during the tests
        utils::capture.output(testthat::expect_no_error(get_message(data = "show(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")), file = NULL, append = FALSE, type = "output", split = FALSE) # S4 object
        utils::capture.output(result <- get_message(data = "show(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), file = NULL, append = FALSE, type = "output", split = FALSE) # S4 object
        expected <- NULL
        testthat::expect_equal(result, expected)
        utils::capture.output(testthat::expect_error(get_message(data = show(1), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")), file = NULL, append = FALSE, type = "output", split = FALSE) # S4 object
        # an instruction that completes is not an error, even if its value is a try-error object (formerly reported as an error, which also hid the warnings and standard messages)
        env_try <- base::new.env(hash = TRUE, parent = base::baseenv(), size = 29L)
        data_try <- "x_try <- base::try(expr = base::log(x = \"a\", base = base::exp(x = 1)), silent = TRUE, outFile = base::getOption(x = \"try.outFile\", default = base::stderr()))"
        result <- saferDev::get_message(data = data_try, kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = env_try, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_identical(object = result, expected = "NO ERROR MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
        testthat::expect_true(object = base::inherits(x = base::get(x = "x_try", pos = -1L, envir = env_try, mode = "any", inherits = FALSE), what = "try-error", which = FALSE), info = NULL, label = NULL) # the instruction has completed, assigning a try-error object
        result <- saferDev::get_message(data = base::paste0("{base::warning(\"w\", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL) ; ", data_try, "}", collapse = NULL, recycle0 = FALSE), kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = env_try, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_identical(object = result, expected = "WARNING MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL, : w\n", info = NULL, label = NULL, expected.label = NULL)
        result <- saferDev::get_message(data = base::paste0("{base::message(\"m\", domain = NULL, appendLF = TRUE) ; ", data_try, "}", collapse = NULL, recycle0 = FALSE), kind = "message", header = TRUE, print_no = TRUE, text = NULL, env = env_try, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_identical(object = result, expected = "STANDARD MESSAGE REPORTED:\nm", info = NULL, label = NULL, expected.label = NULL)
        result <- saferDev::get_message(data = "base::try(expr = base::log(x = \"a\", base = base::exp(x = 1)), silent = TRUE, outFile = base::getOption(x = \"try.outFile\", default = base::stderr()))", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = env_try, safer_check = TRUE, lib_path = NULL, error_text = "") # try-error object as the value of the instruction
        testthat::expect_null(object = result, info = NULL, label = NULL)
        # end if( ! base::all(base::class(x = tempo.error) == "try-error", na.rm = TRUE)){
        # if(kind == "error" & ! base::is.null(x = tempo.error)){
        # if(header == TRUE){
        testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # text not NULL
        result <- get_message(data = str1, kind = "error", header = TRUE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        expected <- "ERROR MESSAGE REPORTED IN FUN1:\nIn wilcox.test.default(letters, c(1, 2, 4)) : 'x' must be numeric\n"
        testthat::expect_equal(result, expected)
        testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # text not NULL
        result <- get_message(data = str1, kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        expected <- "ERROR MESSAGE REPORTED:\nIn wilcox.test.default(letters, c(1, 2, 4)) : 'x' must be numeric\n"
        testthat::expect_equal(result, expected)
        # end if(header == TRUE){
        # }else{
        testthat::expect_no_error(get_message(data = str1, kind = "error", header = FALSE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        result <- get_message(data = str1, kind = "error", header = FALSE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        expected <- "Error in wilcox.test.default(letters, c(1, 2, 4)) : 'x' must be numeric\n"
        testthat::expect_equal(result, expected)
        # }else{
        # end if(kind == "error" & ! base::is.null(x = tempo.error)){
        # }else if(kind == "error" & base::is.null(x = tempo.error) & print_no == TRUE){
        testthat::expect_no_error(get_message(data = str4, kind = "error", header = TRUE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # text not NULL
        result <- get_message(data = str4, kind = "error", header = TRUE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "") 
        expected <- "NO ERROR MESSAGE REPORTED IN FUN1"
        testthat::expect_equal(result, expected)
        testthat::expect_no_error(get_message(data = str4, kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # text NULL
        result <- get_message(data = str4, kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "") 
        expected <- "NO ERROR MESSAGE REPORTED"
        testthat::expect_equal(result, expected)
        # end }else if(kind == "error" & base::is.null(x = tempo.error) & print_no == TRUE){
        # }else if(kind != "error" & ( ! base::is.null(x = tempo.error)) & print_no == TRUE){
        testthat::expect_no_error(get_message(data = str1, kind = "message", header = FALSE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        result <- get_message(data = str1, kind = "message", header = FALSE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        expected <- "NO POTENTIAL STANDARD MESSAGE BECAUSE OF ERROR MESSAGE REPORTED IN FUN1"
        testthat::expect_equal(result, expected)
        # the warnings and messages emitted before an error are not returned: "NO POTENTIAL ..." string with print_no = TRUE, NULL otherwise (documented in @returns)
        result <- saferDev::get_message(data = "{base::warning(\"w\", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL) ; base::stop(\"e\", call. = TRUE, domain = NULL)}", kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_identical(object = result, expected = "NO POTENTIAL WARNING MESSAGE BECAUSE OF ERROR MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
        result <- saferDev::get_message(data = "{base::warning(\"w\", call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL) ; base::stop(\"e\", call. = TRUE, domain = NULL)}", kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_null(object = result, info = NULL, label = NULL)
        result <- saferDev::get_message(data = "{base::message(\"m\", domain = NULL, appendLF = TRUE) ; base::stop(\"e\", call. = TRUE, domain = NULL)}", kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        testthat::expect_null(object = result, info = NULL, label = NULL)
        # end }else if(kind != "error" & ( ! base::is.null(x = tempo.error)) & print_no == TRUE){
        # }else if(base::is.null(x = tempo.error)){ # meaning no error
            # warning message
            testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            stats_env <- as.environment("package:stats")
            on.exit(expr = if(base::exists(x = "stats_env", inherits = FALSE)){base::rm(stats_env)}, add = TRUE, after = FALSE) # guarantee the removal of stats_env even if an expectation above fails
            testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = stats_env, safer_check = TRUE, lib_path = NULL, error_text = "")) # env
            rm(stats_env)
            if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){
                testthat::expect_no_error(get_message(data = str7, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # ggplot2
            }
            # end warning message
            # message
            testthat::expect_no_error(get_message(data = str2, kind = "message", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            stats_env <- as.environment("package:stats")
            on.exit(expr = if(base::exists(x = "stats_env", inherits = FALSE)){base::rm(stats_env)}, add = TRUE, after = FALSE) # guarantee the removal of stats_env even if an expectation above fails
            testthat::expect_no_error(get_message(data = str2, kind = "message", header = TRUE, print_no = FALSE, text = "IN FUN1", env =  stats_env, safer_check = TRUE, lib_path = NULL, error_text = ""))
            rm(stats_env)
            if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){
                testthat::expect_no_error(get_message(data = str5, kind = "message", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # ggplot2
            }
            # end message
        # if(base::any(base::class(x = tempo) %in% base::c("gg", "ggplot"), na.rm = TRUE)){
        if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){
            testthat::expect_no_error(get_message(data = str7, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            result <- saferDev::get_message(data = str7, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
            testthat::expect_match(object = result, regexp = "WARNING MESSAGE REPORTED IN FUN1:\n", perl = FALSE, fixed = TRUE, all = TRUE, info = NULL, label = NULL) # warning raised when the plot is built (printed)
            stats_env <- as.environment("package:stats")
            on.exit(expr = if(base::exists(x = "stats_env", inherits = FALSE)){base::rm(stats_env)}, add = TRUE, after = FALSE) # guarantee the removal of stats_env even if an expectation above fails
            testthat::expect_no_error(get_message(data = str7, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = stats_env, safer_check = TRUE, lib_path = NULL, error_text = ""))
            rm(stats_env)
        }
        # end }else if(base::is.null(x = tempo.error)){
        # if(kind == "warning" & base::length(x = tempo.warn) > 0){
                # if(header == TRUE){
                    # if(base::any(base::grepl(x = tempo.warn[[1]], pattern
                    testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    result <- get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
                    expected <- "WARNING MESSAGE REPORTED:\nIn Ops.factor(1, factor(1)): '+' not meaningful for factors\n"
                    testthat::expect_equal(result, expected)
                    # end if(base::any(base::grepl(x = tempo.warn[[1]], pattern
                # end if(header == TRUE){
                # }else{
                testthat::expect_no_error(get_message(data = str6, kind = "warning", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                result <- get_message(data = str6, kind = "warning", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
                expected <- "simpleWarning in Ops.factor(1, factor(1)): '+' not meaningful for factors\n"
                testthat::expect_equal(result, expected)
                # end }else{
                # several warnings: only the last one is returned
                result <- saferDev::get_message(data = "{base::warning(\"first\") ; base::warning(\"second\")}", kind = "warning", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
                testthat::expect_identical(object = result, expected = "simpleWarning in base::eval(expr = base::parse(text = data, file = \"\", n = NULL, : second\n", info = NULL, label = NULL, expected.label = NULL)
        # end if(kind == "warning" & base::length(x = tempo.warn) > 0){
        # }else if(kind == "warning" & base::length(x = tempo.warn) == 0 & print_no == TRUE){
            testthat::expect_no_error(get_message(data = str2, kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        # end }else if(kind == "warning" & base::length(x = tempo.warn) == 0 & print_no == TRUE){
        # }else if(kind == "message" & base::length(x = tempo_message) > 0){
            # if(header == TRUE){
                testthat::expect_no_error(get_message(data = str2, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                result <- get_message(data = str2, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
                expected <- "STANDARD MESSAGE REPORTED:\nahah"
                testthat::expect_identical(object = result, expected = expected, info = NULL, label = NULL, expected.label = NULL) # the message is caught by get_message() before testthat catches it
                testthat::expect_no_message(object = saferDev::get_message(data = str2, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), message = NULL, class = NULL) # the message is not displayed
                if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){
                    testthat::expect_no_error(get_message(data = str5, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    result <- saferDev::get_message(data = str5, kind = "message", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
                    testthat::expect_match(object = result, regexp = "STANDARD MESSAGE REPORTED IN FUN1:\n`stat_bin()` using `bins = 30`", perl = FALSE, fixed = TRUE, all = TRUE, info = NULL, label = NULL) # message emitted when the plot is built (printed). The end of the message depends on the ggplot2 version
                }
            # end if(header == TRUE){
            # }else{
                testthat::expect_no_error(get_message(data = str2, kind = "message", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                result <- get_message(data = str2, kind = "message", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
                expected <- "ahah"
                testthat::expect_identical(object = result, expected = expected, info = NULL, label = NULL, expected.label = NULL)
                testthat::expect_no_message(object = saferDev::get_message(data = str2, kind = "message", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
                if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){
                    testthat::expect_no_error(get_message(data = str5, kind = "message", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    result <- get_message(data = str5, kind = "message", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
                    testthat::expect_match(object = result, regexp = "`stat_bin()` using `bins = 30`", perl = FALSE, fixed = TRUE, all = TRUE, info = NULL, label = NULL)
                }
            # end }else{
            # several standard messages, or a message written on several lines: a single string, one line per message line (formerly one element per message line)
            result <- saferDev::get_message(data = "{base::message(\"first\") ; base::message(\"second\")}", kind = "message", header = TRUE, print_no = FALSE, text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
            testthat::expect_identical(object = result, expected = "STANDARD MESSAGE REPORTED IN A:\nfirst\nsecond", info = NULL, label = NULL, expected.label = NULL)
            result <- saferDev::get_message(data = "base::message(\"line1\\nline2\")", kind = "message", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
            testthat::expect_identical(object = result, expected = "line1\nline2", info = NULL, label = NULL, expected.label = NULL)
        # end if(kind == "message" & base::length(x = tempo_message) > 0){
        # }else if(kind == "message" & base::length(x = tempo_message) == 0 & print_no == TRUE){
            testthat::expect_no_error(get_message(data = str4, kind = "message", header = FALSE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            result <- get_message(data = str4, kind = "message", header = FALSE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
            expected <- "NO STANDARD MESSAGE REPORTED"
            testthat::expect_equal(result, expected)
        # end }else if(kind == "message" & base::length(x = tempo_message) == 0 & print_no == TRUE){



    # }else{
    #### end main code
    ## end tests (ordered by arg appearance and conditions in the code)


    ## other tests
    testthat::expect_no_error(get_message(data = str4, kind = "error", header = TRUE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = fun1, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # message signaled without muffle restart (base::signalCondition()): caught, without error
    result <- saferDev::get_message(data = "base::signalCondition(cond = base::simpleMessage(message = \"signaled message\\n\", call = NULL))", kind = "error", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result, expected = "NO ERROR MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
    result <- saferDev::get_message(data = "base::signalCondition(cond = base::simpleMessage(message = \"signaled message\\n\", call = NULL))", kind = "message", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result, expected = "STANDARD MESSAGE REPORTED:\nsignaled message", info = NULL, label = NULL, expected.label = NULL)
    # package startup message, without attaching any package
    result <- saferDev::get_message(data = "base::packageStartupMessage(\"startup message\", domain = NULL, appendLF = TRUE)", kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result, expected = "STANDARD MESSAGE REPORTED:\nstartup message", info = NULL, label = NULL, expected.label = NULL)
    result <- saferDev::get_message(data = "base::packageStartupMessage(\"startup message\", domain = NULL, appendLF = TRUE)", kind = "message", header = FALSE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result, expected = "startup message", info = NULL, label = NULL, expected.label = NULL)
    # text written directly to the standard error is not a message condition: not caught (formerly returned with kind = "message"), but caught by utils::capture.output(type = "message") (documented in @details)
    printed_stderr <- utils::capture.output(msg <- saferDev::get_message(data = "base::cat(\"to stderr\\n\", file = base::stderr(), sep = \" \", fill = FALSE, labels = NULL, append = FALSE)", kind = "message", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), file = NULL, append = FALSE, type = "message", split = FALSE)
    testthat::expect_identical(object = msg, expected = "NO STANDARD MESSAGE REPORTED", info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = printed_stderr, expected = "to stderr", info = NULL, label = NULL, expected.label = NULL)
    # env test
    result <- get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = baseenv(), safer_check = TRUE, lib_path = NULL, error_text = "")
    expected <-  "ERROR MESSAGE REPORTED:\nIn wilcox.test(letters, c(1, 2, 4)) : \n  could not find function \"wilcox.test\"\n" # eval only in baseenv()
    testthat::expect_equal(result, expected)
    stats_env <- as.environment("package:stats")
    on.exit(expr = if(base::exists(x = "stats_env", inherits = FALSE)){base::rm(stats_env)}, add = TRUE, after = FALSE) # guarantee the removal of stats_env even if an expectation above fails
    result <- get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = "IN FUN1", env = stats_env, safer_check = TRUE, lib_path = NULL, error_text = "")
    expected <-  "ERROR MESSAGE REPORTED IN FUN1:\nIn wilcox.test.default(letters, c(1, 2, 4)) : 'x' must be numeric\n" # eval only in stat(), str1 not found
    testthat::expect_equal(result, expected)
    rm(stats_env)
    # eval only in an environment whose enclosure is the base environment: get_message() is not found there, whatever the order of the search path (an environment enclosed in package:stats depends on the position of package:saferDev in the search path)
    env_base <- base::new.env(hash = TRUE, parent = base::baseenv(), size = 29L)
    result <- saferDev::get_message(data = 'get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = stats_env, safer_check = TRUE, lib_path = NULL, error_text = "")', kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = env_base, safer_check = TRUE, lib_path = NULL, error_text = "")
    expected <- "ERROR MESSAGE REPORTED:\nIn get_message(data = str6, kind = \"warning\", header = TRUE, print_no = FALSE,  : \n  could not find function \"get_message\"\n"
    testthat::expect_equal(result, expected)
    # end env test

    # additional env tests: documented usage examples
    # env = asNamespace("stats") (documented example): a function of the stats namespace is found
    result <- get_message(data = 'base::suppressWarnings(expr = stats::median(x = 1:3))', kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = base::asNamespace(ns = "stats", base.OK = TRUE), safer_check = TRUE, lib_path = NULL, error_text = "")
    expected <- NULL # no error, no warning, no message reported
    testthat::expect_equal(result, expected)
    # env = a local environment: a variable defined only in that env is found there
    env1 <- base::new.env(parent = base::baseenv())
    base::assign(x = "local_var", value = 7, pos = -1L, envir = env1, inherits = FALSE)
    env2 <- base::new.env(parent = base::baseenv())
    base::assign(x = "local_var", value = "text", pos = -1L, envir = env2, inherits = FALSE)
    # same expression, 3 different envs: 3 different errors, proving that env really controls the evaluation
    result_env1 <- get_message(data = "base::stop(local_var)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = env1, safer_check = TRUE, lib_path = NULL, error_text = "")
    expected_env1 <- "ERROR MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  7\n"
    testthat::expect_equal(result_env1, expected_env1)
    result_env2 <- get_message(data = "base::stop(local_var)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = env2, safer_check = TRUE, lib_path = NULL, error_text = "")
    expected_env2 <- "ERROR MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  text\n"
    testthat::expect_equal(result_env2, expected_env2)
    result_stats_ns <- get_message(data = "base::stop(local_var)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = base::asNamespace(ns = "stats", base.OK = TRUE), safer_check = TRUE, lib_path = NULL, error_text = "")
    expected_stats_ns <- "ERROR MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  object 'local_var' not found\n"
    testthat::expect_equal(result_stats_ns, expected_stats_ns)
    base::rm(env1, env2)
    # env = NULL inside a function: the instruction is evaluated in the frame of that function
    fun_local <- function(){
        local_var2 <- "value of the function frame"
        saferDev::get_message(data = "base::stop(local_var2)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    }
    testthat::expect_identical(object = fun_local(), expected = "ERROR MESSAGE REPORTED:\nIn base::eval(expr = base::parse(text = data, file = \"\", n = NULL,  : \n  value of the function frame\n", info = NULL, label = NULL, expected.label = NULL)

    # default arguments
    result <- get_message(data = "1+factor(1)", kind = "warning")
    expected <-  "WARNING MESSAGE REPORTED:\nIn Ops.factor(1, factor(1)): '+' not meaningful for factors\n"
    testthat::expect_equal(result, expected)
    result <- get_message(data = "1 + 1", kind = "warning")  # no warning
    expected <-  NULL
    testthat::expect_equal(result, expected)

    # standard messages caught whatever the outer handlers (formerly NULL inside testthat, knitr or base::suppressMessages())
    result <- get_message(data = "message('Hello world')", kind = "message", header = TRUE)
    expected <- "STANDARD MESSAGE REPORTED:\nHello world"
    testthat::expect_equal(result, expected)
    result <- get_message(data = "message('Hello world')", kind = "message", header = FALSE)
    expected <- "Hello world"
    testthat::expect_equal(result, expected)
    result <- base::suppressMessages(expr = saferDev::get_message(data = "base::message(\"Hello world\")", kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = result, expected = "STANDARD MESSAGE REPORTED:\nHello world", info = NULL, label = NULL, expected.label = NULL)
    muffle_handler <- function(m){
        base::invokeRestart(r = "muffleMessage") # outer handler muffling the messages, as knitr does
    }
    result <- base::withCallingHandlers(expr = saferDev::get_message(data = "base::message(\"Hello world\")", kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), message = muffle_handler)
    testthat::expect_identical(object = result, expected = "STANDARD MESSAGE REPORTED:\nHello world", info = NULL, label = NULL, expected.label = NULL)

    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})
