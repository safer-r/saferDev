testthat::test_that("arg_test()", {

    ## data argument values
    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- base::expression(1)
    fun1 <- function(x){base::return(x + 1)}
    fun2 <- function(){}


    ## end data argument values

    ## initialization of tests
    testthat::expect_error(arg_test(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::arg_test()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(arg_test()) # the function has default values for all args.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(arg_test(fun = NULL, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = NULL, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = NULL, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = NULL, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = NULL, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = NULL, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = NULL, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(arg_test(fun = character(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = integer(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = double(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = logical(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = complex(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = data.frame(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = list(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # fun: function → replaced by each type
    testthat::expect_error(arg_test(fun = character(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = integer(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = double(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = logical(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = complex(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = data.frame(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = list(), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # arg: character -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = character(), val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = integer(), val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = double(), val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = logical(), val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = complex(), val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = data.frame(), val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = list(), val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # val: list -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = character(), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = integer(), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = double(), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = logical(), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = complex(), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = data.frame(), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # expect_error: NULL -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = character(), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = integer(), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = double(), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = logical(), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = complex(), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = data.frame(), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = list(), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # parall: logical -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = character(), thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = integer(), thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = double(), thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = logical(), thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = complex(), thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = data.frame(), thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = list(), thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # thread_nb: NULL -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = character(), print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = integer(), print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = double(), print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = logical(), print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = complex(), print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = data.frame(), print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = list(), print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # print_count: numeric -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = character(), plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = integer(), plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = double(), plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = logical(), plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = complex(), plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = data.frame(), plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = list(), plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # plot_fun: logical -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = character(), export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = integer(), export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = double(), export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = logical(), export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = complex(), export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = data.frame(), export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = list(), export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # export: logical -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = character(), res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = integer(), res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = double(), res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = logical(), res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = complex(), res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = data.frame(), res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = list(), res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # res_path: character -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    # safer_check: logical -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = list(), lib_path = NULL, error_text = ""))

    # lib_path: NULL -> replaced by each type
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = integer(), error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = double(), error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = logical(), error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = complex(), error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = data.frame(), error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = list(), error_text = ""))

    # error_text:
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(arg_test(fun = NA, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = NA, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = NA, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NA, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = NA, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NA, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = NA, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = NA, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = NA, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    # safer_check: logical -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = -1, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = 1:2, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = fun2, lib_path = NULL, error_text = ""))

    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    # lib_path: NULL -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = -1, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = 1:2, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = fun2, error_text = ""))

    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # fun: function -> replaced by each value
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = NULL, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = NA, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = 1, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = -1, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = 1:2, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = c(TRUE, FALSE), arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = mat1, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = factor1, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = expr1, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = fun2, arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # arg: character -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = fun1, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = NULL, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = NA, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = 1, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = -1, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = 1:2, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = c(TRUE, FALSE), val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = mat1, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = factor1, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = expr1, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = fun2, val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # val: list -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = fun1, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = NULL, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = NA, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = 1, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = -1, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = 1:2, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = c(TRUE, FALSE), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = mat1, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = factor1, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = expr1, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = fun2, expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # expect_error: NULL -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = fun1, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NA, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = 1, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = -1, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = 1:2, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = c(TRUE, FALSE), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = mat1, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = factor1, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = expr1, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = fun2, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # parall: logical -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = fun1, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = NULL, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = NA, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = 1, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = -1, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = 1:2, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = c(TRUE, FALSE), thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = mat1, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = factor1, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = expr1, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = fun2, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # thread_nb: NULL -> replaced by each value. Parallel must be TRUE
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = fun1, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = NA, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = 1, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = -1, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = 1:2, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = c(TRUE, FALSE), print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = mat1, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = factor1, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = expr1, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = TRUE, thread_nb = fun2, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # print_count: numeric -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = fun1, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = NULL, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = NA, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 1, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = -1, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 1:2, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = c(TRUE, FALSE), plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = mat1, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = factor1, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = expr1, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = fun2, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # plot_fun: logical -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = "fun1", export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = NULL, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = NA, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = 1, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = -1, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = 1:2, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = c(TRUE, FALSE), export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = mat1, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = factor1, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = expr1, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = fun2, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # export: logical -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = fun1, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = NULL, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = NA, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = 1, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = -1, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = 1:2, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = c(TRUE, FALSE), res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = mat1, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = factor1, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = expr1, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = fun2, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))

    # res_path: character -> replaced by each value
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = fun2, safer_check = TRUE, lib_path = NULL, error_text = ""))

    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(arg_test(fun = "fun1", arg = "", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = "", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(arg_test(fun = "fun1", arg = "x", val = list(x = 1), expect_error = NULL, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
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
