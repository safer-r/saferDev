# Example for "data" argument
test_that("report handles 'data' argument", {
  data <- data.frame(ID = 1:3, Value = c("A", "B", "C"))
  output <- "results_data.txt"
  path <- "."
  report(data = data, output = output, path = path, overwrite = TRUE)
  expect_true(file.exists(file.path(path, output)))
})

# Example for "output" argument
test_that("report handles 'output' argument", {
  data <- data.frame(ID = 1:3, Value = c("A", "B", "C"))
  output <- "results_output.txt"
  path <- "."
  report(data = data, output = output, path = path, overwrite = TRUE)
  expect_true(file.exists(file.path(path, output)))
})

# Example for "path" argument
test_that("report handles 'path' argument", {
  data <- data.frame(ID = 1:3, Value = c("A", "B", "C"))
  output <- "results_path.txt"
  path <- "."
  report(data = data, output = output, path = path, overwrite = TRUE)
  expect_true(file.exists(file.path(path, output)))
})

# Example for "overwrite" argument
test_that("report handles 'overwrite' argument", {
  data <- data.frame(ID = 1:3, Value = c("A", "B", "C"))
  output <- "results_overwrite.txt"
  path <- "."
  # Create a file with the same name
  cat("Existing content", file = file.path(path, output))
  report(data = data, output = output, path = path, overwrite = TRUE)
  content <- readLines(file.path(path, output))
  expect_length(content, 1)  # Expect only one line, meaning file is overwritten
  expect_true(all(content == ""))
})

# Example for "rownames.kept" argument
test_that("report handles 'rownames.kept' argument", {
  data <- data.frame(ID = 1:3, Value = c("A", "B", "C"))
  output <- "results_rownames.txt"
  path <- "."
  report(data = data, output = output, path = path, overwrite = TRUE, rownames.kept = TRUE)
  content <- readLines(file.path(path, output))
  expect_equal(content[1], "")  # Expect empty row names
})

# Example for "vector.cat" argument
test_that("report handles 'vector.cat' argument", {
  data <- c(1, 2, 3)
  output <- "results_vector_cat.txt"
  path <- "."
  report(data = data, output = output, path = path, overwrite = TRUE, vector.cat = TRUE)
  content <- readLines(file.path(path, output))
  expect_equal(content, "1 2 3")  # Expect vector elements concatenated
})

# Example for "noquote" argument
test_that("report handles 'noquote' argument", {
  data <- data.frame(ID = 1:3, Value = c("A", "B", "C"))
  output <- "results_noquote.txt"
  path <- "."
  report(data = data, output = output, path = path, overwrite = TRUE, noquote = FALSE)
  content <- readLines(file.path(path, output))
  expect_true(all(grepl("\"A\"", content)))  # Expect double quotes around values
})

# Example for "sep" argument
test_that("report handles 'sep' argument", {
  data <- data.frame(ID = 1:3, Value = c("A", "B", "C"))
  output <- "results_sep.txt"
  path <- "."
  report(data = data, output = output, path = path, overwrite = TRUE, sep = 4)
  content <- readLines(file.path(path, output))
  expect_true(all(grepl("^\\s{4}$", content)))  # Expect lines with 4 spaces as separator
})

# Example using all arguments
test_that("report handles all arguments", {
  data <- data.frame(ID = 1:3, Value = c("A", "B", "C"))
  output <- "results_all_args.txt"
  path <- "."
  report(data = data, output = output, path = path, overwrite = TRUE, rownames.kept = TRUE, vector.cat = TRUE, noquote = FALSE, sep = 4)
  expect_true(file.exists(file.path(path, output)))
  # Additional expectations if needed
})
