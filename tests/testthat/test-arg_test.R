test_that("arg_test()", {
    f <- "unique"
    argum <- base::c("x", "incomparables")
    value <- base::list(x = base::list(1:10, base::c(1,1,2,8), NA), incomparable = base::c(TRUE, FALSE, NA))
    error <- base::list(x = base::list(FALSE, FALSE, TRUE), incomparable = base::c(FALSE, FALSE, TRUE))
    f2 <- "plot"
    argum2 <- base::c("x", "y")
    value2 <- base::list(x = base::list(1:10, 12:13, NA, (1:10)^2), y = base::list(1:10, NA, NA))
    error2 <- base::list(x = base::list(FALSE, TRUE, TRUE, FALSE), y = base::list(FALSE, TRUE, TRUE))
    f3 <- "round"
    argum3 <- base::c("data", "dec.nb", "after.lead.zero")
    value3 <- base::list(L1 = base::list(c(1, 1.0002256, 1.23568), "a", NA), L2 = base::list(2, c(1,3), NA), L3 = base::c(TRUE, FALSE, NA))
    f4 <- "ggbox"
    argum4 <- base::c("x", "y", "color")
    value4 <- base::data.frame(x = base::c(1:10, 12:13, NA), y = base::c(1:10, NA, NA, NA), color = base::c("red", "blue", "green", rep(NA, 10)))
    error4 <- base::list(x = base::list(FALSE, TRUE, TRUE), y = base::list(FALSE, TRUE, TRUE), color = base::c(FALSE, TRUE, TRUE))

    #simple test
    testthat::expect_no_error(arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = TRUE
))
    testthat::expect_error(arg_test(fun = f2, arg = argum, val = value2, expect.error = error2, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = TRUE
))
    testthat::expect_error(arg_test(fun = f3, arg = argum3, val = value3
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = TRUE
))
    testthat::expect_error(arg_test(arg = argum4, val = value4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = FALSE
))
    testthat::expect_error(arg_test(fun = f4, val = value4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = "10", plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = FALSE
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = FALSE
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = "not_real_path", safer_check = FALSE
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = 1, safer_check = TRUE
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, expect.error = error4, parall = FANALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = TRUE
))
    testthat::expect_error(arg_test(fun = base::list(1:2), arg = base::list(3:4), val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE
))

    result1 <- saferDev::get_message("arg_test(fun = 1, arg = argum, val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = TRUE)", kind = "error", print.no = TRUE, text = NULL)
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test()\nTHE fun OBJECT MUST BE MODE character\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    result2 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = 1, safer_check = TRUE)", kind = "error", print.no = TRUE, text = NULL)
    expected2 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n1\n\n================\n\n\n"
    testthat::expect_equal(result2, expected2)

    result3 <- saferDev::get_message("arg_test(fun = f, arg = argum, val = value, expect.error = error, parall = TRUE, thread.nb = 0.3, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected3 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nthread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: 0.3\n\n================\n\n\n"
    testthat::expect_equal(result3, expected3)

    result4 <- saferDev::get_message("arg_test(fun = f, arg = '', val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected4 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nTHIS ARGUMENT\narg\nCANNOT CONTAIN \"\"\n\n================\n\n\n"
    testthat::expect_equal(result4, expected4)

    result5 <- saferDev::get_message("arg_test(fun = 'not_function', arg = argum, val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected5 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nCHARACTER STRING IN fun ARGUMENT DOES NOT EXIST IN THE R WORKING ENVIRONMENT: not_function\n\n================\n\n\n"
    testthat::expect_equal(result5, expected5)

    result6 <- saferDev::get_message("arg_test(fun = f2, arg = argum, val = value2, expect.error = error2, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected6 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nSOME OF THE STRINGS IN arg ARE NOT ARGUMENTS OF fun\nfun ARGUMENTS: x y ...\nPROBLEMATIC STRINGS IN arg: incomparables\n\n================\n\n\n"
    testthat::expect_equal(result6, expected6)

    result7 <- saferDev::get_message("arg_test(fun = f2, arg = base::character(0), val = value2, expect.error = error2, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected7 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\narg ARGUMENT CANNOT BE LENGTH 0\n\n================\n\n\n"
    testthat::expect_equal(result7, expected7)

       
    result8 <- arg_test(
        fun = f, 
        arg = argum, 
        val = value, 
        expect.error = error, 
        parall = FALSE, 
        thread.nb = NULL, 
        print.count = 10, 
        plot.fun = FALSE, 
        export = FALSE, 
        res.path = NULL, 
        lib_path = NULL,
        safer_check = TRUE
    )
    # WARNING: trick to get the same result2$ini
    arg_test <- function(
        fun = f, 
        arg = argum, 
        val = value, 
        expect.error = error, 
        parall = FALSE, 
        thread.nb = NULL, 
        print.count = 10, 
        plot.fun = FALSE, 
        export = FALSE, 
        res.path = NULL, 
        lib_path = NULL,
        safer_check = TRUE
    ){
        base::return(base::match.call(expand.dots = FALSE))
    }
    tempo <- arg_test(
            fun = f, 
            arg = argum, 
            val = value, 
            expect.error = error, 
            parall = FALSE, 
            thread.nb = NULL, 
            print.count = 10, 
            plot.fun = FALSE, 
            export = FALSE, 
            res.path = NULL, 
            lib_path = NULL,
            safer_check = TRUE
        )
    base::rm(arg_test)
    # end WARNING: trick to get the same result2$ini
    expected8 <- base::list(
        fun = "unique",
        ini = tempo,
        data = base::data.frame(
            x = c("1 2 3 4 5 6 7 8 9 10", "1 2 3 4 5 6 7 8 9 10", "1 2 3 4 5 6 7 8 9 10", "1 1 2 8", "1 1 2 8", "1 1 2 8", "NA", "NA", "NA"),
            incomparables = base::c("TRUE", "FALSE", "NA", "TRUE", "FALSE", "NA", "TRUE", "FALSE", "NA"), 
            kind = "OK", 
            problem = FALSE, 
            expected.error = base::c(FALSE, FALSE,  TRUE, FALSE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE), 
            message = base::c("", "", "", "", "", "", "", "", ""),
            row.names = base::c("arg_test_1", "arg_test_2", "arg_test_3", "arg_test_4", "arg_test_5", "arg_test_6", "arg_test_7", "arg_test_8", "arg_test_9")
        )
    )
    testthat::expect_equal(result8[1:3], expected8) # [1:3] to do not compare system parameters

    result9 <- arg_test(
        fun = f2, 
        arg = argum2, 
        val = value2, 
        expect.error = error2, 
        parall = FALSE, 
        thread.nb = NULL, 
        print.count = 10, 
        plot.fun = TRUE, 
        export = FALSE, 
        res.path = ".", 
        lib_path = NULL,
        safer_check = TRUE
    )

    arg_test <- function(
        fun = f2, 
        arg = argum2, 
        val = value2, 
        expect.error = error2, 
        parall = FALSE, 
        thread.nb = NULL, 
        print.count = 10, 
        plot.fun = TRUE, 
        export = FALSE, 
        res.path = ".", 
        lib_path = NULL,
        safer_check = TRUE
    ){
        base::return(base::match.call(expand.dots = FALSE))
    }
    tempo2 <- arg_test(
            fun = f2, 
            arg = argum2, 
            val = value2, 
            expect.error = error2, 
            parall = FALSE, 
            thread.nb = NULL, 
            print.count = 10, 
            plot.fun = TRUE, 
            export = FALSE, 
            res.path = ".", 
            lib_path = NULL,
            safer_check = TRUE
        )
    base::rm(arg_test)
    # end WARNING: trick
    expected9 <- base::list(
        fun = "plot",
        ini = tempo2,
        data = base::data.frame(
            x = c("1 2 3 4 5 6 7 8 9 10", "1 2 3 4 5 6 7 8 9 10", "1 2 3 4 5 6 7 8 9 10", "12 13", "12 13", "12 13", "NA", "NA", "NA", "1 4 9 16 25 36 49 64 81 100", "1 4 9 16 25 36 49 64 81 100", "1 4 9 16 25 36 49 64 81 100"),
            y = c("1 2 3 4 5 6 7 8 9 10", "NA", "NA", "1 2 3 4 5 6 7 8 9 10", "NA", "NA", "1 2 3 4 5 6 7 8 9 10", "NA", "NA", "1 2 3 4 5 6 7 8 9 10", "NA", "NA"),
            kind = base::c("OK", "ERROR", "ERROR", "ERROR", "ERROR", "ERROR", "ERROR", "ERROR", "ERROR", "OK", "ERROR", "ERROR"),
            problem = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
            expected.error = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
            message = c("", 
                        "Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n", 
                        "Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n", 
                        "Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n", 
                        "Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n", 
                        "Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n", 
                        "Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n", 
                        "Error in plot.window(...) : need finite 'xlim' values\n", 
                        "Error in plot.window(...) : need finite 'xlim' values\n", 
                        "", 
                        "Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n", 
                        "Error in xy.coords(x, y, xlabel, ylabel, log) : \n  'x' and 'y' lengths differ\n"),
            row.names = c("arg_test_01", "arg_test_02", "arg_test_03", "arg_test_04", "arg_test_05", "arg_test_06", "arg_test_07", "arg_test_08", "arg_test_09", "arg_test_10", "arg_test_11", "arg_test_12")
        ))
    testthat::expect_equal(result9[1:3], expected9) # [1:3] to do not compare system parameters
})
