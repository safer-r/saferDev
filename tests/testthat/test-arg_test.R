test_that("arg_test()", {
    f <- "unique"
    argum <- c("x", "incomparables")
    value <- list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA))
    error <- list(x = list(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE))
    
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
        return(base::match.call(expand.dots = FALSE))
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
    rm(arg_test)
    # end WARNING: trick to get the same result2$ini
    expected2 <- list(
        fun = "unique",
        ini = tempo,
        data = data.frame(
            x = c("1 2 3 4 5 6 7 8 9 10", "1 2 3 4 5 6 7 8 9 10", "1 2 3 4 5 6 7 8 9 10", "1 1 2 8", "1 1 2 8", "1 1 2 8", "NA", "NA", "NA"),
            incomparables = c("TRUE", "FALSE", "NA", "TRUE", "FALSE", "NA", "TRUE", "FALSE", "NA"), 
            kind = "OK", 
            problem = FALSE, 
            expected.error = c(FALSE, FALSE,  TRUE, FALSE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE), 
            message = c("", "", "", "", "", "", "", "", ""),
            row.names = c("arg_test_1", "arg_test_2", "arg_test_3", "arg_test_4", "arg_test_5", "arg_test_6", "arg_test_7", "arg_test_8", "arg_test_9")
        )
    )
    expect_equal(result2[1:3], expected2) # [1:3] to do not compare system parameters
})
