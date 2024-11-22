test_that("arg_test()", {
    f <- "unique"
    argum <- base::c("x", "incomparables")
    value <- base::list(x = base::list(1:10, base::c(1,1,2,8), NA), incomparable = base::c(TRUE, FALSE, NA))
    error <- base::list(x = base::list(FALSE, FALSE, TRUE), incomparable = base::c(FALSE, FALSE, TRUE))
    f2 <- "plot"
    argum2 <- base::c("x", "y")
    value2 <- base::list(x = base::list(1:10, 12:13, NA, (1:10)^2), y = base::list(1:10, NA, NA))
    error2 <- base::list(x = list(FALSE, TRUE, TRUE, FALSE), y = list(FALSE, TRUE, TRUE))
    
    result2 <- arg_test(
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
    expected2 <- base::list(
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
    testthat::expect_equal(result2[1:3], expected2) # [1:3] to do not compare system parameters

    result3 <- arg_test(
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
    expected3 <- base::list(
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
    testthat::expect_equal(result3[1:3], expected3) # [1:3] to do not compare system parameters
})
