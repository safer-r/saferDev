test_that("the function prints the warning message", {
    char1 <- "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" # single character string
    char2 <- "message('ahah')"
    
    result1 <- get_message(data = char1, header = FALSE)
    expect_null(result1)
    
    result2 <- get_message(data = char1, print.no = TRUE)
    expected2 <- "NO ERROR MESSAGE REPORTED"
    expect_equal(result2,expected2)
    
    result3 <- get_message(data = char2, print.no = TRUE, text = "IN A")
    expected3 <- "NO ERROR MESSAGE REPORTED IN A"
    expect_equal(result3,expected3)
    
    result4 <- get_message(
        data = char1, 
        kind = "warning", 
        header = FALSE,
        print.no = TRUE, 
        text = "IN A",
        env = NULL
    )
    expected4 <- "simpleWarning in wilcox.test.default(c(1, 1, 3), c(1, 2, 4), paired = TRUE): cannot compute exact p-value with zeroes\n"
    expect_equal(result4, expected4)
})
