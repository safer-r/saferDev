test_that("report()", {
    # Example datasets
    vec1 <- 1:6
    vec2 <- letters[1:6]
    mat1 <- matrix(vec1, nrow = 2, ncol = 3, byrow = TRUE)
    rownames(mat1) <- c("Row 1", "Row 2")
    mat2 <- matrix(letters[1:9], nrow = 3, ncol = 3, byrow = TRUE)
    rownames(mat2) <- c("R1", "R2", "R3")
    t1 <- table(mat2) # 
    vec3 <- as.vector(vec2)
    names(vec3) <- letters[1:6]
    t2 <- table(vec2, vec2)
    l1 <- list(vec1, vec2)

    expect_no_error(report(data = vec1, path = "."))
    expect_no_error(report(data = vec2, path = "."))
    expect_no_error(report(data = vec3, path = "."))
    expect_no_error(report(data = vec3, path = ".", rownames.kept = TRUE))
    expect_no_error(report(data = mat1, path = "."))
    expect_no_error(report(data = mat1, path = ".", rownames.kept = TRUE))
    expect_no_error(report(data = t1, path = "."))
    expect_no_error(report(data = t1, path = ".", rownames.kept = TRUE))
    expect_no_error(report(data = t2, path = "."))
    expect_no_error(report(data = t2, path = ".", rownames.kept = TRUE))
    expect_no_error(report(data = l1, path = "."))
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

