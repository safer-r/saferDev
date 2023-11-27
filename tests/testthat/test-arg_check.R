test_that("output of fun_check is a list contains 'problem', 'text', and 'object.name'", {
    vec1 <- -1:3 # vector of integers
    resultCheck <- arg_check(
        data = vec1, 
        class = "integer", 
        typeof = NULL, 
        mode = NULL, 
        length = NULL, 
        prop = FALSE, 
        double.as.integer.allowed = FALSE, 
        options = NULL, 
        all.options.in.data = FALSE, 
        na.contain = FALSE, 
        neg.values = TRUE, 
        inf.values = TRUE, 
        print = FALSE, 
        data.name = NULL, 
        fun.name = NULL
    )
    expected <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 OBJECT", object.name = "vec1")
    expect_equal(resultCheck, expected)
})