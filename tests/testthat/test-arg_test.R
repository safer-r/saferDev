testthat::test_that("arg_test()", {
    f <- "unique"
    argum <- base::c("x", "incomparables")
    value <- base::list(x = base::list(1:10, base::c(1,1,2,8), NA), incomparable = base::c(TRUE, FALSE, NA))
    error <- base::list(x = base::list(FALSE, FALSE, TRUE), incomparable = base::c(FALSE, FALSE, TRUE))
    f2 <- "plot"
    argum2 <- base::c("x", "y")
    value2 <- base::list(x = base::list(1:10, 12:13, NA, (1:10)^2), y = base::list(1:10, NA, NA))
    error2 <- base::list(x = base::list(FALSE, TRUE, TRUE, FALSE), y = base::list(FALSE, TRUE, TRUE))
    error5 <- base::list(x = base::list('a','b','c','d'), y = base::list('e','f','g'))
    f3 <- "round"
    argum3 <- base::c("data", "dec.nb", "after.lead.zero")
    value3 <- base::list(L1 = base::list(c(1, 1.0002256, 1.23568), "a", NA), L2 = base::list(2, c(1,3), NA), L3 = base::c(TRUE, FALSE, NA))
    f4 <- "ggbox"
    argum4 <- base::c("x", "y", "color")
    value4 <- base::data.frame(x = base::c(1:10, 12:13, NA), y = base::c(1:10, NA, NA, NA), color = base::c("red", "blue", "green", rep(NA, 10)))
    error4 <- base::list(x = base::list(FALSE, TRUE, TRUE), y = base::list(FALSE, TRUE, TRUE), color = base::c(FALSE, TRUE, TRUE))

    #simple test
    testthat::expect_no_error(arg_test(fun = f2, arg = argum2, val = value2, expect_error = error2, parall = FALSE, thread_nb = 4, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = TRUE))
    testthat::expect_error(arg_test(fun = f2, arg = argum, val = value2, expect_error = error2, parall = FALSE, thread_nb = 4, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = TRUE)) # wrong arg
    testthat::expect_error(arg_test(fun = f3, arg = argum3, val = value3))# some of the strings in arg are not arguments of fun 
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, expect_error = error4, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = TRUE)) # wrong val, it must be class list
    testthat::expect_error(arg_test(arg = argum4, val = value4, expect_error = error4, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE)) # wrong fun, it must have a default value
    testthat::expect_error(arg_test(fun = f4, val = value4, expect_error = error4, parall = FALSE, thread_nb = NULL, print_count = "10", plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE)) # wrong arg, it must have a default value
    testthat::expect_error(arg_test(fun = f4, arg = argum4, expect_error = error4, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE)) # wrong val, it must have a default value
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = "not_real_path", safer_check = FALSE)) # wrong lib_path, it does not exist
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, expect_error = error4, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = 1, safer_check = TRUE)) # wrong lib_path, it must be a character
    testthat::expect_error(arg_test(fun = base::list(1:2), arg = base::list(3:4), val = value, expect_error = error, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE)) # wrong fun and arg, they must be character
    testthat::expect_error(arg_test(fun = f2, arg = argum2, val = value2, expect_error = error, parall = FALSE, thread_nb = 4, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE)) # wrong ecpext.error, length of expect_error is not equal to length of val
    testthat::expect_error(arg_test(fun = f2, arg = argum2, val = value2, expect_error = error5, parall = FALSE, thread_nb = 4, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE)) # wrong ecpext.error, it must be logical values, TRUE or FALSE




    testthat::expect_error(arg_test(fun = 1, arg = argum, val = value, expect_error = error, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f2, arg = argum2, val = value2, expect_error = error2, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = 1, safer_check = FALSE))  
    testthat::expect_error(arg_test(fun = f, arg = argum, val = value, expect_error = error, parall = TRUE, thread_nb = 0.3, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f, arg = '', val = value, expect_error = error, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = 'not_function', arg = argum, val = value, expect_error = error, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f2, arg = argum, val = value2, expect_error = error2, parall = FALSE, thread_nb = 4, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = '.', lib_path = NULL, safer_check = FALSE))    
    testthat::expect_error(arg_test(fun = f2, arg = base::character(0), val = value2, expect_error = error2, parall = FALSE, thread_nb = 4, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = '.', lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f, arg = argum, val = value, expect_error = error, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = NA, lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f, arg = argum, val =  base::list(base::c(1, 2, 3), base::list(a = 1, b = 2)), expect_error = error, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f2, arg = argum2, val = base::list(base::matrix(1:4, 2, 2)), expect_error = error2, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = '.', lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f2, arg = base::c('x'), val = value2, expect_error = error2, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = '.', lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f2, arg = base::rep(argum2,22), val = rep(value2, 22), expect_error = rep(error2,22), parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = '.', lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f2, arg = argum2, val = base::list(x = base::list(1,2,3,4), y = base::list(3,5)), expect_error = error2, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = '.', lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f, arg = argum, val = value, expect_error = error, parall = TRUE, thread_nb = NULL, print_count = 10, plot_fun = FALSE, export = FALSE, res_path = ".", lib_path = NULL, safer_check = FALSE))
    testthat::expect_no_error(arg_test(fun = f2, arg = argum2, val = value2, expect_error = error2, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = TRUE, res_path = ".", lib_path = NULL, safer_check = FALSE))
    testthat::expect_error(arg_test(fun = f2, arg = argum2, val = value2, expect_error = error2, parall = FALSE, thread_nb = NULL, print_count = 10, plot_fun = TRUE, export = FALSE, res_path = ".", lib_path = 'not_real_path', safer_check = FALSE))


})
