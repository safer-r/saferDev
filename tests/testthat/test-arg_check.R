testthat::test_that("arg_check()", {

    ## data argument values
    vec1 <- -1:3 # vector of integers
    vec3 <- base::c(1, 2, 3) # vector of double
    vec4 <- "pearson"
    vec5 <- base::c("a", "b","a", "b")
    vec6 <- base::list(1:3, 4:6)
    vec7 <- vec3 / 2 # vector of decimals
    vec8 <- c(vec3, Inf)
    mat1 <- base::matrix(vec1)
    mat2 <- base::matrix(base::c(1:3 / 3, NA))
    number <- 1
    factor1 <- base::as.factor(vec5)
    expr1 <- expression(1)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(arg_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric")) # to test that this example works
    ## end initialization of tests


})

