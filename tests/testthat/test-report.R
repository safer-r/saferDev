test_that("report()", {
    # Example datasets
    vec1 <- 1:6
    vec2 <- letters[1:6]
    mat1 <- base::matrix(vec1, nrow = 2, ncol = 3, byrow = TRUE)
    base::rownames(mat1) <- base::c("Row 1", "Row 2")
    mat2 <- base::matrix(letters[1:9], nrow = 3, ncol = 3, byrow = TRUE)
    base::rownames(mat2) <- base::c("R1", "R2", "R3")
    t1 <- base::table(mat2) # 
    vec3 <- base::as.vector(vec2)
    base::names(vec3) <- letters[1:6]
    t2 <- base::table(vec2, vec2)
    l1 <- base::list(vec1, vec2)

    testthat::expect_no_error(report(
        data = vec1, 
        path = ".",
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = vec2, 
        path = ".",
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = vec3, 
        path = ".",
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = vec3, 
        path = ".", 
        rownames.kept = TRUE,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = mat1, 
        path = ".",
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = mat1, 
        path = ".", 
        rownames.kept = TRUE,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = t1, 
        path = ".",
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = t1, 
        path = ".", 
        rownames.kept = TRUE,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = t2, 
        path = ".",
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = t2, 
        path = ".", 
        rownames.kept = TRUE,
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
        data = l1, 
        path = ".",
        safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
        ))
    testthat::expect_no_error(report(
          data = mat1, 
          output = "test.txt", 
          path = ".", 
          overwrite = TRUE, 
          rownames.kept = TRUE, 
          vector.cat = TRUE, 
          noquote = FALSE, 
          sep = 4,
          safer_check = FALSE # do not set to TRUE because test_that() in CI does not like the package presence checking
    ))
})

