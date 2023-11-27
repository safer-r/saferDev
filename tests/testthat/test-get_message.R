test_that("the function prints the warning message", {
    char1 <- "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" # single character string
    result <- get_message(
        data = char1, 
        kind = "warning", 
        header = FALSE,
        print.no = TRUE, 
        text = "IN A",
        env = NULL
    )
    expected <- "simpleWarning in wilcox.test.default(c(1, 1, 3), c(1, 2, 4), paired = TRUE): cannot compute exact p-value with zeroes\n"
    expect_equal(result, expected)
})
