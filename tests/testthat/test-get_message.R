testthat::test_that("get_message()", {

    ## data argument values
    str1 <- "wilcox.test(letters, c(1, 2, 4))" # error
    str2 <- "message('ahah')" # message
    str3 <- "message('ahah'); warning('ohoh')" # message and warning
    str4 <- "sum(1, 2, 3)" # nothing
    str5 <- "ggplot2::ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()" # ggplot and message
    str6 <- "wilcox.test(c(1, 1, 3), c(1, 2, 4), paired = TRUE)" # warning
    str7 <- "ggplot2::ggplot(data = data.frame(X = c(1:10, NA), stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()" # ggplot and warning
    str8 <-  "1+factor(1)" # warning message
    str9 <- "ggplot2::ggplot(data = data.frame(X = 'a', stringsAsFactors = FALSE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()" # ggplot and error
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
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


    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
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
    testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = baseenv(), safer_check = TRUE, lib_path = NULL, error_text = "")) # weird. Return an error with .GlobalEnv
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
    testthat::expect_error(get_message(data = plot(1), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    plot(1)
    testthat::expect_error(get_message(data = plot(1), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    graphics.off()
    testthat::expect_error(get_message(data = ggplot(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = "plot(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    plot(1)
    testthat::expect_no_error(get_message(data = "plot(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    graphics.off()
    testthat::expect_no_error(get_message(data = "ggplot()", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
        # if(base::any(base::class(x = tempo.error) %in% base::c("gg", "ggplot"), na.rm = TRUE)){
        testthat::expect_no_error(get_message(data = str5, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        result <- get_message(data = str5, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        expected <- NULL
        testthat::expect_equal(result, expected)
        testthat::expect_no_error(get_message(data = str9, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        result <- get_message(data = str5, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
        expected <- NULL
        testthat::expect_equal(result, expected)
        # end if(base::any(base::class(x = tempo.error) %in% base::c("gg", "ggplot"), na.rm = TRUE)){
        # if( ! base::all(base::class(x = tempo.error) == "try-error", na.rm = TRUE)){
        testthat::expect_no_error(get_message(data = "show(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # S4 object
        result <- get_message(data = "show(1)", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "") # S4 object
        expected <- NULL
        testthat::expect_equal(result, expected)
        testthat::expect_error(get_message(data = show(1), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # S4 object
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
        expected <- "NO POTENTIAL STANDARD (NON ERROR AND NON WARNING) MESSAGE BECAUSE OF ERROR MESSAGE REPORTED IN FUN1"
        testthat::expect_equal(result, expected)
        # end }else if(kind != "error" & ( ! base::is.null(x = tempo.error)) & print_no == TRUE){
        # }else if(base::is.null(x = tempo.error)){ # meaning no error
            # warning message
            testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            stats_env <- as.environment("package:stats")
            testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = stats_env, safer_check = TRUE, lib_path = NULL, error_text = "")) # env
            rm(stats_env)
            testthat::expect_no_error(get_message(data = str7, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # ggplot2
            # end warning message
            # message
            testthat::expect_no_error(get_message(data = str2, kind = "message", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            stats_env <- as.environment("package:stats")
            testthat::expect_no_error(get_message(data = str2, kind = "message", header = TRUE, print_no = FALSE, text = "IN FUN1", env =  stats_env, safer_check = TRUE, lib_path = NULL, error_text = ""))
            rm(stats_env)
            testthat::expect_no_error(get_message(data = str5, kind = "message", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # ggplot2
            # end message
        # if(base::any(base::class(x = tempo) %in% base::c("gg", "ggplot"), na.rm = TRUE)){
        testthat::expect_no_error(get_message(data = str7, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        stats_env <- as.environment("package:stats")
        testthat::expect_no_error(get_message(data = str7, kind = "warning", header = TRUE, print_no = FALSE, text = "IN FUN1", env = stats_env, safer_check = TRUE, lib_path = NULL, error_text = ""))
        rm(stats_env)
        # end }else if(base::is.null(x = tempo.error)){
        # if(kind == "warning" & base::length(x = tempo.warn) > 0){
            testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                # if(header == TRUE){
                    # if(base::any(base::grepl(x = tempo.warn[[1]], pattern
                    testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(base::any(base::grepl(x = tempo.warn[[1]], pattern
                # end if(header == TRUE){
                # }else{
                testthat::expect_no_error(get_message(data = str6, kind = "warning", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                # end }else{
        # end if(kind == "warning" & base::length(x = tempo.warn) > 0){
        # }else if(kind == "warning" & base::length(x = tempo.warn) == 0 & print_no == TRUE){
            testthat::expect_no_error(get_message(data = str2, kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        # end }else if(kind == "warning" & base::length(x = tempo.warn) == 0 & print_no == TRUE){
        # }else if(kind == "message" & base::length(x = tempo_message) > 0){
            # if(header == TRUE){
                testthat::expect_no_error(get_message(data = str2, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                testthat::expect_no_error(get_message(data = str5, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            # end if(header == TRUE){
            # }else{
                testthat::expect_no_error(get_message(data = str2, kind = "message", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
                testthat::expect_no_error(get_message(data = str5, kind = "message", header = FALSE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
            # end }else{
        # end if(kind == "message" & base::length(x = tempo_message) > 0){
        # }else if(kind == "message" & base::length(x = tempo_message) == 0 & print_no == TRUE){
            testthat::expect_no_error(get_message(data = str2, kind = "message", header = FALSE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
        # end }else if(kind == "message" & base::length(x = tempo_message) == 0 & print_no == TRUE){




    testthat::expect_no_error(get_message(data = str4, kind = "error", header = TRUE, print_no = TRUE, text = "IN FUN1", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str6, kind = "warning", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = str1, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(get_message(data = fun1, kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = "library(dplyr)", kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = "library(dplyr)", kind = "message", header = TRUE, print_no = TRUE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # }else{
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})
