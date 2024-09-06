test_that("report()", {
  # Example datasets
  vec1 <- 1:6
  vec2 <- c("(", "a", ")", "(", "b", ")", "(", "c", ")")
  vec3 <- rep(c("Male", "Female"), times = 10)
  vec4 <- sample(1:40, 20, replace = TRUE)
  mat1 <- matrix(vec1, nrow = 2, ncol = 3, byrow = TRUE)
  rownames(mat1) <- c("Row 1", "Row 2")
  mat2 <- matrix(vec2, nrow = 3, ncol = 3, byrow = TRUE)
  rownames(mat2) <- c("R1", "R2", "R3")
  tab1 <- table(vec3, vec4)


  # Simple examples
    expect_no_error(report(data = vec1, path = "."))
    expect_no_error(report(data = vec2, path = "."))
    expect_no_error(report(data = mat1, path = "."))
    expect_no_error(report(data = mat2, path = "."))
    expect_no_error(report(data = tab1, path = "."))
    expect_no_error(report(data = tab1, path = "."))
    expect_no_error(report(
      data = mat2, 
      output = "test.txt", 
      path = ".", 
      overwrite = TRUE, 
      rownames.kept = TRUE, 
      vector.cat = TRUE, 
      noquote = FALSE, 
      sep = 4,
      safer_check = TRUE
  ))
})

