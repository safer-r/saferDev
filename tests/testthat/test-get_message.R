testthat::test_that("get_message()", {
    char1 <- "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" # single character string
    char2 <- "message('ahah')"
    char3 <- "message('ahah'); warning('ohoh')"
    char4 <- "sum(1, 2, 3)"
    char5 <- "ggplot2::ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()"

    testthat::expect_error(saferDev::get_message(data = char1, kind = "error", print.no = TRUE, text = "IN A", safer_check = 'TRUE'))
    testthat::expect_error(saferDev::get_message( kind = "warning", print.no = TRUE, text = "IN A", safer_check = FALSE))
    testthat::expect_no_error(saferDev::get_message(data = char1, kind = "message", print.no = TRUE,text = NULL, safer_check = FALSE))
    testthat::expect_error(saferDev::get_message(data = char1, kind = NULL, print.no = TRUE,text = NULL, safer_check = FALSE))
    testthat::expect_no_error(saferDev::get_message(data = "", kind = "error", print.no = TRUE,text = NULL, safer_check = FALSE)) # because data can be ""
    testthat::expect_no_error(saferDev::get_message(data = NOT_CHARACTER, kind = "error", print.no = TRUE,text = NULL, safer_check = FALSE))

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

    result6 <- saferDev::get_message(
        data = char3, 
        kind = "warning", 
        header = FALSE,
        print.no = TRUE, 
        text = "IN A",
    )
    expected6 <- "simpleWarning in base::eval(base::parse(text = data), envir = if (base::is.null(env)) {: ohoh\n"
    testthat::expect_equal(result6, expected6)

    result7 <- saferDev::get_message(data = char4, kind = "error",print.no = TRUE, text = "IN A")
    expected7 <- "NO ERROR MESSAGE REPORTED IN A"
    testthat::expect_equal(result7,expected7)

    result8 <- saferDev::get_message(data = char5, kind = "message",print.no = TRUE, text = "IN INSTRUCTION 1")
    expected8 <- "NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED IN INSTRUCTION 1"
    testthat::expect_equal(result8,expected8)

    
















})
