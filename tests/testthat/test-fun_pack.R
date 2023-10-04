test_that("If a package R is installed in the folder", {
    tempo.cat2 <- paste0("ERROR IN ", "fun_pack()", ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", "test")
    err2 <- paste0("\n\n================\n\n", tempo.cat2, "\n\n================\n\n")
    expect_error(object = fun_pack(
        req.package = "test2", 
        load = TRUE,
        lib.path = "test"),regexp = err2,fixed = TRUE)
})


test_that("If a package R is installed in the folder", {
    # tempo.cat2 <- paste0("ERROR IN ", "fun_pack()", ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", "test")
    # err2 <- paste0("\n\n================\n\n", tempo.cat2, "\n\n================\n\n")
    expect_no_error(object = fun_pack(
        req.package = "ggplot2", 
        load = TRUE,
        lib.path = NULL))
})

