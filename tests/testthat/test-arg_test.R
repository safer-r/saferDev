test_that("arg_test()", {
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
    testthat::expect_no_error(arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = TRUE
))
    testthat::expect_error(arg_test(fun = f2, arg = argum, val = value2, expect.error = error2, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = TRUE # wrong arg
))
    testthat::expect_error(arg_test(fun = f3, arg = argum3, val = value3 # some of the strings in arg are not arguments of fun 
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = TRUE # wrong val, it must be class list
))
    testthat::expect_error(arg_test(arg = argum4, val = value4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = FALSE # wrong fun, it must have a default value
))
    testthat::expect_error(arg_test(fun = f4, val = value4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = "10", plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = FALSE # wrong arg, it must have a default value
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = FALSE # wrong val, it must have a default value
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = "not_real_path", safer_check = FALSE # wrong lib_path, it does not exist
))
    testthat::expect_error(arg_test(fun = f4, arg = argum4, val = value4, expect.error = error4, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = 1, safer_check = TRUE # wrong lib_path, it must be a character
))
    testthat::expect_error(arg_test(fun = base::list(1:2), arg = base::list(3:4), val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE # wrong fun and arg, they must be character
))
    testthat::expect_error(arg_test(fun = f2, arg = argum2, val = value2, expect.error = error, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = FALSE # wrong ecpext.error, length of expect.error is not equal to length of val
))
    testthat::expect_error(arg_test(fun = f2, arg = argum2, val = value2, expect.error = error5, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = ".", lib_path = NULL, safer_check = FALSE # wrong ecpext.error, it must be logical values, TRUE or FALSE
))

    # tests with get_message

    result1 <- saferDev::get_message("arg_test(fun = 1, arg = argum, val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test()\nTHE fun OBJECT MUST BE MODE character\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    result2 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = 1, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
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

    result8 <- saferDev::get_message("arg_test(fun = f, arg = argum, val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NA, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected8 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test()\nTHE res.path OBJECT MUST BE MODE character AND THE res.path OBJECT CONTAINS NA WHILE NOT AUTHORIZED\n\n================\n\n\n"
    testthat::expect_equal(result8, expected8)

    result9 <- saferDev::get_message("arg_test(fun = f, arg = argum, val =  base::list(base::c(1, 2, 3), base::list(a = 1, b = 2)), expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected9 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nLENGTH OF COMPARTMENT 2 OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF COMPARTMENT 2 OF expect.error ARGUMENT:\nHERE IT IS: 2 VERSUS 3\n\n================\n\n\n"
    testthat::expect_equal(result9, expected9)

    result10 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = base::list(base::matrix(1:4, 2, 2)), expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected10 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nCOMPARTMENT 1 OF val ARGUMENT MUST BE A VECTOR OR A LIST\n\n================\n\n\n"
    testthat::expect_equal(result10, expected10)

    result11 <- saferDev::get_message("arg_test(fun = f2, arg = base::c('x'), val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expectd11 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nLENGTH OF arg ARGUMENT MUST BE IDENTICAL TO LENGTH OF val ARGUMENT:\nHERE IT IS: 1 VERSUS 2\n\n================\n\n\n"
    testthat::expect_equal(result11, expectd11)

    result12 <- saferDev::get_message("arg_test(fun = f2, arg = base::rep(argum2,22), val = rep(value2, 22), expect.error = rep(error2,22), parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected12 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nCANNOT TEST MORE THAN 43 ARGUMENTS IF THEY ALL HAVE AT LEAST 2 VALUES EACH\nHERE THE NUMBER IS: 44\n\n================\n\n\n"
    testthat::expect_equal(result12, expected12)

    result13 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = base::list(x = base::list(1,2,3,4), y = base::list(3,5)), expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected13 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nLENGTH OF COMPARTMENT 2 OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF COMPARTMENT 2 OF expect.error ARGUMENT:\nHERE IT IS: 2 VERSUS 3\n\n================\n\n\n"
    testthat::expect_equal(result13, expected13)

    result14 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error5, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected14 <- "ERROR MESSAGE REPORTED:\nIn expect.error[[1]][[i1]] | expect.error[[2]][[i2]] : \n  operations are possible only for numeric, logical or complex types\n"
    testthat::expect_equal(result14, expected14)

    result15 <- saferDev::get_message("arg_test(fun = f, arg = argum, val = value, expect.error = error, parall = TRUE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected15 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nres.path ARGUMENT MUST BE SPECIFIED IF parall ARGUMENT IS TRUE\n\n================\n\n\n"
    testthat::expect_equal(result15, expected15)

    result16 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = TRUE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected16 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nres.path ARGUMENT MUST BE SPECIFIED IF export ARGUMENT TRUE\n\n================\n\n\n"
    testthat::expect_equal(result16, expected16)

    result17 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = 'not_real_path', safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected17 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\nnot_real_path\n\n================\n\n\n"
    testthat::expect_equal(result17, expected17)


       
    result18 <- arg_test(
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
    expected18 <- base::list(
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
    testthat::expect_equal(result18[1:3], expected18) # [1:3] to do not compare system parameters

    result19 <- arg_test(
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
    expected19 <- base::list(
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
    testthat::expect_equal(result19[1:3], expected19) # [1:3] to do not compare system parameters
})
