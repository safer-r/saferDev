test_that("two_colons_check function works correctly", {
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
    expect_no_error(two_colons_check(x = fun1))
    expect_no_error(two_colons_check(x = fun2))
    expect_no_error(two_colons_check(x = fun2, safer_check = TRUE))
})
