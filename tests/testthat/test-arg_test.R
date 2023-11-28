test_that("test the function unique and compare the results", {
    f <- "unique"
    argum <- c("x", "incomparables")
    value <- list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA))
    error <- list(x = list(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE))
    result <- arg_test(
        fun = f, 
        arg = argum, 
        val = value, 
        expect.error = error, 
        parall = FALSE, 
        thread.nb = NULL, 
        print.count = 10, 
        plot.fun = FALSE, 
        export = TRUE, 
        res.path = ".", 
        lib.path = NULL
    )
    ini.date <- Sys.time()
    ini.time <- as.numeric(ini.date)
    end.date <- Sys.time()
    end.time <- as.numeric(end.date)
    total.lapse <- round(lubridate::seconds_to_period(end.time - ini.time))
     
    p1 <- paste0("\ntest JOB IGNITION\n")
    p2 <- paste0("\nTHE TOTAL NUMBER OF TESTS IS: ",9, "\n")
    p3 <- paste0("\nLOOP PROCESS ENDED | ","\nPROCESS ",9," ENDED | ","LOOP ",format(0, big.mark = ",")," / ",format(9), big.mark = ",", " | TIME SPENT: ",0, "\n\n")
    p4 <- paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE discrepancy_table_from_arg_test_1-",9, ".tsv FILE)\n\n")
    p5 <- paste0("test JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", total.lapse, "\n\n\n")
    expected <- cat(paste0(p1,p2,p3,p4,p5))
    expect_equal(result, expected)
})
