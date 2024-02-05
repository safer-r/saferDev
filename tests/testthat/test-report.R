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

  # Argument output
  test_that("handles 'output' argument", {
    expect_no_error(report(data = vec1, output = "test.txt", path = "."))
    expect_true(file.exists("test.txt"))
  })

  # Argument overwrite
  test_that("handles 'overwrite' argument", {
    # Create a file with the same name
    cat("Existing content", file = "test_overwrite.txt")
    expect_no_error(report(data = vec2, overwrite = TRUE, path = "."))
    content <- readLines("test_overwrite.txt")
    expect_true(all(content == ""))
  })

  # Argument rownames.kept
  test_that("handles 'rownames.kept' argument", {
    expect_no_error(report(data = mat1, rownames.kept = TRUE, path = "."))
    content <- readLines("report.txt")
    expect_true(any(grepl("Row 1", content)) && any(grepl("Row 2", content)))
  })

  # Argument vector.cat
  test_that("handles 'vector.cat' argument", {
    expect_no_error(report(data = mat2, vector.cat = TRUE, path = "."))
    content <- readLines("report.txt")
    expect_true(all(grepl("(", content) && grepl(")", content)))
  })

  # Argument noquote
  test_that("handles 'noquote' argument", {
    expect_no_error(report(data = mat2, noquote = FALSE, path = "."))
    content <- readLines("report.txt")
    expect_true(all(grepl("\"", content)))
  })

  # Argument sep
  test_that("handles 'sep' argument", {
    expect_no_error(report(data = vec1, sep = 4, path = "."))
    content <- readLines("report.txt")
    expect_equal(length(content), length(vec1) + 4)
  })

  # All the arguments
  test_that("handles all arguments", {
    expect_no_error(report(
      data = mat2,
      output = "test.txt",
      path = ".",
      overwrite = TRUE,
      rownames.kept = TRUE,
      vector.cat = TRUE,
      noquote = FALSE,
      sep = 4
    ))
    content <- readLines("test.txt")
    expect_true(all(grepl("(", content) && grepl(")", content)))
    expect_true(any(grepl("R1", content)) && any(grepl("R2", content)) && any(grepl("R3", content)))
    expect_true(length(content) == length(vec2) + 4)
  })
})

