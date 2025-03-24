testthat::test_that("get_message()", {

    ## data argument values
    str1 <- "wilcox.test(letters, c(1, 2, 4))"
    str2 <- "message('ahah')"
    str3 <- "message('ahah'); warning('ohoh')"
    str4 <- "sum(1, 2, 3)"
    str5 <- "ggplot2::ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()"
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
    testthat::expect_no_error(saferDev:::get_message()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_no_error(get_message()) # the function has default values for all args.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_no_error(get_message(data = NULL, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    testthat::expect_no_error(get_message(data = integer(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = double(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = logical(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = complex(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = data.frame(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = list(), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))

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
    testthat::expect_no_error(get_message(data = NA, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    testthat::expect_no_error(get_message(data = NULL, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = NA, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = 1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = c(TRUE, FALSE), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = mat1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = factor1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = expr1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(get_message(data = fun1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    # testthat::expect_no_error(get_message(data = str1, kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = .GlobalEnv, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
