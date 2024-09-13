test_that("colons_check()", {
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
    mat1 <- matrix(vec3, nrow = 2, ncol = 3, byrow = TRUE)
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
    expect_no_error(colons_check(x = fun1))
    expect_no_error(colons_check(x = fun2))
    expect_no_error(colons_check(x = fun2, safer_check = TRUE))


  # sophisticated example
    source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
    print(test)
    result1 <- capture.output(colons_check(x = test))
    expected1 <- c(
        "", 
        "", 
        "================", 
        "", 
        "INSIDE test, SOME :: OR ::: ARE MISSING AT BASIC FUNCTION POSITIONS:", 
        "", 
        "LINE\tFUN\t\tSTRING_BEFORE", 
        "15\tgregexpr\t\tmatches <- "  , 
        "17\tregmatches\t\tmatched_strings <- ", 
        "20\tsum\t\tx = text, m = matches)[[1]] ; ", 
        "23\tsub\t\tresult <- "     , 
        "23\trange\t\t\"\\\\($##\", \"\", matched_strings) ; ", 
        "25\treturn\t\t"  , 
        "", 
        "INSIDE test, SOME :: OR ::: ARE MISSING AT OTHER FUNCTION POSITIONS:", 
        "", 
        "LINE\tFUN\t\tSTRING_BEFORE", 
        "22\tcol1\t\t\", base::length(", 
        "24\tcol1\t\ttempo.cat <- base::paste0(\"IAGE\\nLENGTHS OF ", 
        "", 
        "================", 
        ""
    )
    expect_equal(result1, expected1)

})
