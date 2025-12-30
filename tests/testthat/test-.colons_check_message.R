testthat::test_that(".colons_check_message()", {

    ## data argument values
    list_fun1 <- list(c2 = "UseMethod")
    list_fun2 <- list(c2 = c("UseMethod", "mean", "rep"))
    list_fun3 <- list(c1 = "function", c2 = c("paste0", "mean"))
    list_fun_pos1 <- list(c2 = 1)
    list_fun_pos2 <- list(c2 = 1:3)
    list_fun_pos3 <- list(c2 = 1, c3 = c(1, 8))
    line_nb1 <- 2
    line_nb2 <- 1:2
    ini1 <- c("function (x, ...) ", "UseMethod(\"mean\")")
    ini2 <- c("function (x, ...) ", "paste0$mean(1:3)")
    arg_user_setting2_1 <- list(x = "mean")
    arg_user_setting2_2 <- list(x = c("::", "PACK", "test"))
    text1 <- "BASIC"
    str1 <- "blabla" # error saferDev:::.noclean_functions()
    str2 <- c("", "UseMethod(\"mean\")") # no error

    mat1 <- matrix(-1:3)
    factor1 <- as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.colons_check_message(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.colons_check_message()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.colons_check_message()) 
    list_fun <- list_fun1
    list_fun_pos <- list_fun_pos1
    line_nb <- line_nb1
    ini <- ini1
    arg_user_setting2 <- arg_user_setting2_1
    text <- text1
    internal_fun_names <- NULL
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.colons_check_message()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.colons_check_message(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        list_fun, 
        list_fun_pos, 
        line_nb, 
        ini, 
        arg_user_setting2, 
        text,
        internal_fun_names, 
        lib_path, 
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.colons_check_message(list_fun = NULL, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = NULL, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = NULL, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = NULL, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = NULL, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = NULL, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.colons_check_message(list_fun = character(), list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = integer(), list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = double(), list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = logical(), list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = complex(), list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = data.frame(), list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list(), list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))

    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = character(), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = integer(), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = double(), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = logical(), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = complex(), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = data.frame(), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list(), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))

    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = character(), ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = integer(), ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = double(), ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = logical(), ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = complex(), ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = data.frame(), ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = list(), ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))

    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = character(), arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = integer(), arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = double(), arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = logical(), arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = complex(), arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = data.frame(), arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = list(), arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))

    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = character(), text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = integer(), text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = double(), text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = logical(), text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = complex(), text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = data.frame(), text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = list(), text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))

    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = character(), internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = integer(), internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = double(), internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = logical(), internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = complex(), internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = data.frame(), internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = list(), internal_fun_names = NULL, lib_path = NULL, error_text = ""))

    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.colons_check_message(list_fun = NA, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = NA, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = NA, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = NA, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = NA, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = NA, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NA, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = NA))
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
    # list_fun
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = NULL, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = NA, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = 1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = c(TRUE, FALSE), list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = str1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = mat1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = factor1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = expr1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end list_fun
    # list_fun_pos
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = NULL, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = NA, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = 1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = c(TRUE, FALSE), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = str1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = mat1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = factor1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = expr1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = fun1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end list_fun_pos
    # line_nb
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = NULL, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = NA, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = 1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = c(TRUE, FALSE), ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = str1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = mat1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = factor1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = expr1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = fun1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end line_nb
    # ini
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = NULL, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = NA, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = 1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = c(TRUE, FALSE), arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = str1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = str2, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = mat1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = factor1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = expr1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = fun1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end ini
    # arg_user_setting2
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = NULL, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = NA, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = 1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = c(TRUE, FALSE), text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = str1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = mat1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = factor1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = expr1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = fun1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end arg_user_setting2
    # text
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = NULL, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = NA, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = 1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = c(TRUE, FALSE), internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = str1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = mat1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = factor1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = expr1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = fun1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end text
    # internal_fun_names
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = str1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = fun1, lib_path = NULL, error_text = ""))
    # end internal_fun_names
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = "", arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = "")) # error but due to saferDev:::.noclean_functions()
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = "", internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = "", lib_path = NULL, error_text = ""))
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
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos3, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # modification of arg_user_setting2$x for clean messages
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_2, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end modification of arg_user_setting2$x for clean messages
    # check the identical structure of list_fun and list_fun_pos
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = arg_user_setting2_2, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end check the identical structure of list_fun and list_fun_pos
    # remove internal functions in other functions (list_fun and list_fun_pos)
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun2, list_fun_pos = list_fun_pos2, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = "OTHER", internal_fun_names = c("mean", "sum"), lib_path = NULL, error_text = ""))
    # end remove internal functions in other functions (list_fun and list_fun_pos)
    # if( ! (base::length(x = list_fun) == base::length(x = list_fun_pos) & base::length(x = list_fun) == base::length(x = line_nb) & base::length(x = list_fun) == base::length(x = basic_ini))){
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = 1:3, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end if( ! (base::length(x = list_fun) == base::length(x = list_fun_pos) & base::length(x = list_fun) == base::length(x = line_nb) & base::length(x = list_fun) == base::length(x = basic_ini))){
    # if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE) == base::sapply(X = res, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), na.rm = TRUE)){
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list(50), line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    # end if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE) == base::sapply(X = res, FUN = function(x){base::length(x = x)}, simplify = TRUE, USE.NAMES = TRUE), na.rm = TRUE)){
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun3, list_fun_pos = list_fun_pos3, line_nb = line_nb2, ini = ini2, arg_user_setting2 = arg_user_setting2_1, text = text1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))


    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = "BASIC", internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = "OTHER", internal_fun_names = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.colons_check_message(list_fun = list_fun1, list_fun_pos = list_fun_pos1, line_nb = line_nb1, ini = ini1, arg_user_setting2 = arg_user_setting2_1, text = str1, internal_fun_names = NULL, lib_path = NULL, error_text = ""))




    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})

