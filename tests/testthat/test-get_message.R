test_that("get_message()", {
    char1 <- "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" # single character string
    char2 <- "message('ahah')"
    
    result1 <- saferDev::get_message(data = char1, header = FALSE)
    testthat::expect_null(result1)
    
    result2 <- saferDev::get_message(data = char1, print.no = TRUE)
    expected2 <- "NO ERROR MESSAGE REPORTED"
    testthat::expect_equal(result2,expected2)
    
    result3 <- saferDev::get_message(data = char2, print.no = TRUE, text = "IN A")
    expected3 <- "NO ERROR MESSAGE REPORTED IN A"
    testthat::expect_equal(result3,expected3)

    result4 <- saferDev::get_message(data = char2, kind = "error", print.no = TRUE, text = "IN A")
    expected4 <- "NO ERROR MESSAGE REPORTED IN A"
    testthat::expect_equal(result4,expected4)
    
    result5 <- saferDev::get_message(
        data = char1, 
        kind = "warning", 
        header = FALSE,
        print.no = TRUE, 
        text = "IN A",
        env = NULL,
        safer_check = TRUE
    )
    expected5 <- "simpleWarning in wilcox.test.default(c(1, 1, 3), c(1, 2, 4), paired = TRUE): cannot compute exact p-value with zeroes\n"
    testthat::expect_equal(result5, expected5)
})
