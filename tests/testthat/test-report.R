test_that("report()", {
    # Example datasets
    vec1 <- 1:6
    vec2 <- letters[1:6]
    vec3 <- rep(c("Male", "Female"), times = 10)
    vec4 <- sample(1:40, 20, replace = TRUE)
    mat1 <- matrix(vec1, nrow = 2, ncol = 3, byrow = TRUE)
    rownames(mat1) <- c("Row 1", "Row 2")
    t1 <- table(mat2) # 
    vec5 <- as.vector(vec2)
    names(vec5) <- letters[1:9]
    t2 <- table(vec2, vec2)

    expect_no_error(report(data = vec1, path = "."))
    expect_no_error(report(data = vec2, path = "."))
    expect_no_error(report(data = vec3, path = "."))
    expect_no_error(report(data = vec4, path = "."))
    expect_no_error(report(data = vec5, path = "."))
    expect_no_error(report(data = vec5, path = ".", rownames.kept = TRUE))
    expect_no_error(report(data = mat1, path = "."))
    expect_no_error(report(data = mat1, path = ".", rownames.kept = TRUE))
    expect_no_error(report(data = t1, path = "."))
    expect_no_error(report(data = t1, path = ".", rownames.kept = TRUE))
    expect_no_error(report(data = t2, path = "."))
    expect_no_error(report(data = t2, path = ".", rownames.kept = TRUE))
    expect_no_error(report(
          data = mat1, 
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

