testthat::test_that(".base_op_check()", {

    testthat::expect_error(saferDev:::.base_op_check(caca = 1)) # not a correct argument
    testthat::expect_error(saferDev:::.base_op_check(error_text = 1)) # not a correct value

  # Simple examples
    result1 <- saferDev::all_args_here(x = saferDev:::.base_op_check, export = FALSE, path_out = ".", df_name = "res.tsv", overwrite = FALSE, lib_path = NULL, safer_check = FALSE, error_text = "")$STATUS
    # warning LINE 13 can be LINE 22
    expected1 <- c("GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "", "GOOD", "GOOD", "GOOD", "", "", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD", "GOOD")
    testthat::expect_equal(result1, expected1)
})

