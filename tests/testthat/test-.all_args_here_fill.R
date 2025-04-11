testthat::test_that(".all_args_here_fill()", {

    ## data argument values
    list_fun1 <- list(x = pairlist(x = quote(expr = )))
    list_fun2 <- list(... = quote(expr = ), na.rm = FALSE) # args of sum()
    str1 <- "x"
    str2 <- "length"
    arg_full_names_2 <- c("...", "na.rm") # args of sum()
    arg_full_names_3 <- c("...", "na.rm", "na.rm_test")
    tempo_split_2 <- "1:2" # args of sum()
    tempo_split_3 <- c("1:2", "na.rm = FALSE") # args of sum()
    tempo_split_4 <- c("1:2", "na.rm = FALSE", "na.rm = FALSE") 
    col2_i2_1 <- "pairlist"
    col2_i2_2 <- "sum"
    arg_user_setting_x_1 <- "\"FUN1\""
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.all_args_here_fill(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.all_args_here_fill()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.all_args_here_fill()) 
    arg_full <- list_fun1
    arg_full_names <- str1
    tempo_split <- str1
    three_dots_log <- FALSE
    i2 <- 1
    col1_i2 <- 1
    col2_i2 <- str2
    arg_user_setting_x <- arg_user_setting_x_1
    warn <- NULL
    warn_count <- 0
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.all_args_here_fill()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.all_args_here_fill(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        arg_full,
        arg_full_names, 
        tempo_split, 
        three_dots_log, 
        i2, 
        col1_i2, 
        col2_i2,
        arg_user_setting_x, 
        warn,
        warn_count,
        lib_path, 
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.all_args_here_fill(arg_full = NULL, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = NULL, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = NULL, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = NULL, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = NULL, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = NULL, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = NULL, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = NULL, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.all_args_here_fill(arg_full = character(), arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = integer(), arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = double(), arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = logical(), arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = complex(), arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = data.frame(), arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list(), arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = character(), tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = integer(), tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = double(), tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = logical(), tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = complex(), tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = data.frame(), tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = list(), tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = character(), three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = integer(), three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = double(), three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = logical(), three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = complex(), three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = data.frame(), three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = list(), three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = character(), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = integer(), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = double(), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = logical(), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = complex(), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = data.frame(), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = list(), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = character(), col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = integer(), col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = double(), col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = logical(), col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = complex(), col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = data.frame(), col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = list(), col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = character(), col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = integer(), col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = double(), col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = logical(), col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = complex(), col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = data.frame(), col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = list(), col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = character(), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = integer(), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = double(), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = logical(), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = complex(), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = data.frame(), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = list(), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = character(), warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = integer(), warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = double(), warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = logical(), warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = complex(), warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = data.frame(), warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = list(), warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = character(), warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = integer(), warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = double(), warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = logical(), warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = complex(), warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = data.frame(), warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = list(), warn_count = 0, lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(.all_args_here_fill(arg_full = NA, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = NA, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = NA, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = NA, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = NA, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = NA, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = NA, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = NA, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NA, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NA, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = NA))
    # all the arguments must be present

    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## check of lib_path
    # already done in the main function
    ######## end check of lib_path

    ######## safer_check argument checking
    # not required because not here
    ######## end safer_check argument checking

    ######## check of the required functions from the required packages
    # not required
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    # already done in the main function
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # arg_full
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = NULL, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = NA, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = 1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = c(TRUE, FALSE), arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = str1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = mat1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = factor1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = expr1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    # end arg_full
    # arg_full_names
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = NULL, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = NA, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = 1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = c(TRUE, FALSE), tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = mat1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = factor1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = expr1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = fun1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    # end arg_full_names
    # tempo_split
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = NULL, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = NA, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = 1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = c(TRUE, FALSE), three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = mat1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = factor1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = expr1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = fun1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    # end tempo_split
    # three_dots_log
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = NULL, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = NA, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = 1, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = c(TRUE, FALSE), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = str1, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = mat1, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = factor1, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = expr1, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = fun1, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    # end three_dots_log
    # i2
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = NULL, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = NA, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = -1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = c(TRUE, FALSE), col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = str1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = mat1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = factor1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = expr1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = fun1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    # end i2
    # col1_i2
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = NULL, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = NA, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = -1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = c(TRUE, FALSE), col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = str1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = mat1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = factor1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = expr1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = fun1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    # end col1_i2
    # col2_i2
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = NULL, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = NA, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = 1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = c(TRUE, FALSE), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = str1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = c(str1, str1), arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = mat1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = factor1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = expr1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = fun1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    # end col2_i2
    # arg_user_setting_x
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = NULL, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = NA, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = 1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = c(TRUE, FALSE), warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = str1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = mat1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = factor1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = expr1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = fun1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    # end arg_user_setting_x
    # warn
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NA, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = 1, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = c(TRUE, FALSE), warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = str1, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = mat1, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = factor1, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = expr1, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = fun1, warn_count = 0, lib_path = NULL, error_text = ""))
    # end warn
    # warn_count
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = NA, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = -1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = str1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = fun1, lib_path = NULL, error_text = ""))
    # end warn_count
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = "", tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = "", three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = "", arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = "", warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun1, arg_full_names = str1, tempo_split = str1, three_dots_log = FALSE, i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_1, arg_user_setting_x = arg_user_setting_x_1, warn = "", warn_count = 0, lib_path = NULL, error_text = ""))
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
    # if(base::any(three_dots_log, na.rm = TRUE)){
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun2, arg_full_names = arg_full_names_2, tempo_split = tempo_split_2, three_dots_log = c(TRUE, FALSE), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_2, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    result <- .all_args_here_fill(arg_full = list_fun2, arg_full_names = arg_full_names_2, tempo_split = tempo_split_2, three_dots_log = c(TRUE, FALSE), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_2, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = "")
    expected <- list(col6 = "na.rm", col7 = "na.rm = FALSE", col8 = "sum(1:2, na.rm = FALSE)")
    testthat::expect_equal(result, expected)
    # end if(base::any(three_dots_log, na.rm = TRUE)){

    # if(base::length(x = tempo_split) == 0 & base::length(x = arg_full_names) > 0){
    testthat::expect_error(.all_args_here_fill(arg_full = list_fun2, arg_full_names = arg_full_names_2, tempo_split = tempo_split_4, three_dots_log = c(TRUE, FALSE), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_2, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    result <- get_message(data = '.all_args_here_fill(arg_full = list_fun2, arg_full_names = arg_full_names_2, tempo_split = tempo_split_4, three_dots_log = c(TRUE, FALSE), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_2, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = "")', kind = "error")
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.all_args_here_fill().\n\npattern3 DETECTED SEVERAL TIMES IN ARGUMENTS:\n\npattern3:\n^[\\s\\r\\n]*na.rm[\\s]*=\n\ntempo_split:\nna.rm = FALSE\nna.rm = FALSE\n\nCHECK IF THE ARGUMENT IS PRESENT SEVERAL TIMES IN LINE 1, INSIDE sum\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    # end if(base::length(x = tempo_split) == 0 & base::length(x = arg_full_names) > 0){

    # if(base::sum(tempo_log, na.rm = TRUE) > 1){
    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun2, arg_full_names = arg_full_names_3, tempo_split = tempo_split_3, three_dots_log = c(TRUE, FALSE, FALSE), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_2, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))
    result <- .all_args_here_fill(arg_full = list_fun2, arg_full_names = arg_full_names_3, tempo_split = tempo_split_3, three_dots_log = c(TRUE, FALSE, FALSE), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_2, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = "")
    expected <- list(col6 = "na.rm_test", col7 = "na.rm_test = NULL", col8 = "sum(1:2,na.rm = FALSE, na.rm_test = NULL)")
    testthat::expect_equal(result, expected)
    # end if(base::sum(tempo_log, na.rm = TRUE) > 1){




    testthat::expect_no_error(.all_args_here_fill(arg_full = list_fun2, arg_full_names = str3, tempo_split = str4, three_dots_log = c(TRUE, FALSE), i2 = 1, col1_i2 = 1, col2_i2 = col2_i2_2, arg_user_setting_x = arg_user_setting_x_1, warn = NULL, warn_count = 0, lib_path = NULL, error_text = ""))

    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})

