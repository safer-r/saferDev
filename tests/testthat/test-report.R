test_that("report function works correctly", {
  # Example datasets
  vec1 <- 1:6
  vec2 <- c("(", "a", ")", "(", "b", ")", "(", "c", ")")
  mat1 <- matrix(vec1, nrow = 2, ncol = 3, byrow = TRUE)
  rownames(mat1) <- c("Row 1", "Row 2")
  mat2 <- matrix(vec2, nrow = 3, ncol = 3, byrow = TRUE)
  rownames(mat2) <- c("R1", "R2", "R3")

  # Simple examples
  test_that("handles simple examples", {
    expect_no_error(report(data = vec1, path = "."))
    expect_no_error(report(data = vec2, path = "."))
    expect_no_error(report(data = mat1, path = "."))
    expect_no_error(report(data = mat2, path = "."))
  })
})

