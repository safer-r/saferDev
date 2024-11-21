testthat::test_that("colons_check()", {
    # Example datasets
    vec1 <- 3 
    vec2 <- 4
    fun1 <- function(
        vec1,
        vec2
    ){
        output <- base::sum(vec1,vec3)
        base::return(output)
    }
    vec3 <- 3:8
    mat1 <- base::matrix(vec3, nrow = 2, ncol = 3, byrow = TRUE)
    fun2 <- function(mat1){
        nc <- base::ncol(mat1)
        means <- numeric(nc)
        for (i in 1:nc){
        means[i] <- mean(mat1[,i])
        }
        base::return(means)
    } 

  # Test cases
  # Simple examples
    testthat::expect_no_error(colons_check(x = fun1))
    testthat::expect_no_error(colons_check(x = fun2))
    testthat::expect_no_error(colons_check(x = fun2, safer_check = TRUE))


  # sophisticated example
    source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") # Warning: comments are removed by the CI and lines are changed
    result1 <- capture.output(colons_check(x = test))
    expected1 <- c(
        "", 
        "", 
        "INSIDE test(), SOME :: OR ::: ARE MISSING AT BASIC FUNCTION POSITIONS:", 
        "", 
        "LINE\tFUN\t\tSTRING_BEFORE", 
        "5\tgregexpr\t\tmatches <- " , 
        "8\tregmatches\t\tmatched_strings <- " , 
        "11\tsum\t\t", 
        "18\tsub\t\tresult <- " , 
        "19\trange\t\t", 
        "22\treturn\t\t", 
        "", 
        "INSIDE test(), INTERNAL FUNCTION DETECTED:", 
        "F", 
        "", 
        "INSIDE test(), SOME :: OR ::: ARE MISSING AT OTHER FUNCTION POSITIONS:", 
        "", 
        "LINE\tFUN\t\tSTRING_BEFORE", 
        "9\tFUN1\t\t", 
        "16\troc1\t\tbase::length(", 
        "20\troc4\t\ttempo.cat <- base::paste0(\"IAGE\\nLENGTHS OF roc00() (\", base::ks.test(", 
        ""
    )
    testthat::expect_equal(result1, expected1)

})
