testthat::test_that("report()", {

    ## data argument values
    data_1 <- data.frame(A = 1:8, B = letters[1:8])
    output_1 <- "results.txt"
    path_1 <- base::tempfile(pattern = "test_report_", tmpdir = base::tempdir(check = FALSE), fileext = "") # the tests write into a new folder of the temporary directory, never into the working directory
    base::dir.create(path = path_1, showWarnings = TRUE, recursive = FALSE, mode = "0777")

    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}

    data_2 <- data.frame(A = 1:3, B = letters[1:3])
    data_3 <- as.matrix(data_2)
    data_4 <- table(1:4)

    data_5 <- 1:3
    data_6 <- letters[1:3]
    data_7 <- list(letters[1:3])

    ## end data argument values

    ## initialization of tests
    testthat::expect_error(report(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::report()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(report()) # the function has default values for all args.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_no_error(report(data = NULL, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = NULL, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = NULL, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = NULL, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = NULL, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = NULL, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = NULL, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(report(data = character(), output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = integer(), output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = double(), output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = logical(), output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = complex(), output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data.frame(), output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = list(), output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = character(), path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = integer(), path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = double(), path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = logical(), path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = complex(), path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = data.frame(), path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = list(), path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = character(), append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = integer(), append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = double(), append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = logical(), append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = complex(), append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = data.frame(), append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = list(), append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = character(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = integer(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = double(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = logical(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = complex(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = data.frame(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = list(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = character(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = integer(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = double(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = logical(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = complex(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = data.frame(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = list(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = character(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = integer(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = double(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = logical(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = complex(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = data.frame(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = list(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = character(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = integer(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = double(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = logical(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = complex(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = data.frame(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = list(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(report(data = NA, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = NA, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = NA, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = NA, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = NA, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = NA, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = NA, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, in the saferDev::arg_check() calls, so the lib_path check is always executed)
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # data
    testthat::expect_no_error(report(data = NULL, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = NA, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = 1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = c(TRUE, FALSE), output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = mat1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = factor1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = expr1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = fun1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end data
    # output
    testthat::expect_error(report(data = data_1, output = NULL, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = NA, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = 1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = c(TRUE, FALSE), path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = mat1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = factor1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = expr1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = fun1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end output
    # path
    testthat::expect_error(report(data = data_1, output = output_1, path = NULL, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = NA, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = 1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = c(TRUE, FALSE), append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = mat1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = factor1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = expr1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = fun1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end path
    # append
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = NULL, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = NA, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = 1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = c(TRUE, FALSE), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = mat1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = factor1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = expr1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = fun1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end append
    # rownames_kept
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = NULL, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = NA, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = 1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = c(TRUE, FALSE), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = mat1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = factor1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = expr1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = fun1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end rownames_kept
    # vector_cat
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = NULL, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = NA, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = 1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = c(TRUE, FALSE), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = mat1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = factor1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = expr1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = fun1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end vector_cat
    # noquote
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = NULL, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = NA, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = 1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = c(TRUE, FALSE), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = mat1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = factor1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = expr1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = fun1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end noquote
    # sep
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end sep
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(report(data = data_1, output = "", path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = "", append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")) 
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking

    ######## end graphic device checking

    ######## other checkings
    testthat::expect_error(report(data = data_1, output = output_1, path = "caca", append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('report(data = data_1, output = output_1, path = "caca", append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")')
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::report().\n\npath ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY:\ncaca\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 0, safer_check = TRUE, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('report(data = data_1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 0, safer_check = TRUE, lib_path = NULL, error_text = "")')
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::report().\n\nsep ARGUMENT CANNOT BE EQUAL TO ZERO.\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    base::dir.create(path = base::file.path(path_1, "test", fsep = "/"), showWarnings = TRUE, recursive = FALSE, mode = "0777")
    testthat::expect_error(report(data = data_1, output = "test", path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(object = saferDev::report(data = data_1, output = "test", path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = base::paste0("ERROR IN saferDev::report().\n\nFILE DEFINED BY THE path AND output ARGUMENTS\n", path_1, "/test\nALREADY EXISTS AND CANNOT BE OVERWRITTEN.\nPLEASE:\nREMOVE THE FILE\nOR CHANGE THE NAME OF THE output ARGUMENT\nOR SET THE append ARGUMENT TO TRUE TO APPEND IN THE EXISTING FILE.\n", collapse = NULL, recycle0 = FALSE), class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    base::unlink(x = base::file.path(path_1, "test", fsep = "/"), recursive = TRUE, force = TRUE, expand = TRUE)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # if( ! base::is.null(x = data)){
            # if(base::all(base::class(x = data) == "data.frame", na.rm = TRUE) | base::all(base::class(x = data) == "table", na.rm = TRUE) | base::all(base::class(x = data) %in% base::c("matrix", "array"), na.rm = TRUE)){
                # if(rownames_kept == FALSE & base::all(base::class(x = data) == "data.frame", na.rm = TRUE)){ # data frames, whatever the number of rows (data_2; the matrix data_3 and the table data_4 go to the else part)
                    testthat::expect_no_error(report(data = data_2, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    testthat::expect_no_error(report(data = data_3, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    testthat::expect_no_error(report(data = data_4, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # if(noquote == TRUE){
                    testthat::expect_no_error(report(data = data_2, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    testthat::expect_no_error(report(data = data_3, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    testthat::expect_no_error(report(data = data_4, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(noquote == TRUE){
                # end if(rownames_kept == FALSE & base::all(base::class(x = data) == "data.frame", na.rm = TRUE)){
                # }else if(base::is.vector(x = data, mode = "any") & base::all(base::class(x = data) != "list", na.rm = TRUE) & (base::length(x = data) == 1L | vector_cat == TRUE)){
                    testthat::expect_no_error(report(data = data_5, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # if(noquote == TRUE){
                    testthat::expect_no_error(report(data = data_5, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(noquote == TRUE){
                    testthat::expect_no_error(report(data = 1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = TRUE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # if(noquote == TRUE){
                    testthat::expect_no_error(report(data = 1, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = TRUE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(noquote == TRUE){
                # end }else if(base::is.vector(x = data, mode = "any") & base::all(base::class(x = data) != "list", na.rm = TRUE) & (base::length(x = data) == 1L | vector_cat == TRUE)){
                #}else if(base::all(base::mode(x = data) == "character", na.rm = TRUE)){
                    testthat::expect_no_error(report(data = data_6, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # if(noquote == TRUE){
                    testthat::expect_no_error(report(data = data_6, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(noquote == TRUE){
                # end }else if(base::all(base::mode(x = data) == "character", na.rm = TRUE)){
            # end if(base::all(base::class(x = data) == "data.frame", na.rm = TRUE) | base::all(base::class(x = data) == "table", na.rm = TRUE) | base::all(base::class(x = data) %in% base::c("matrix", "array"), na.rm = TRUE)){
            # }else{ # other object (lists like data_7, factors, dates, S4 objects): written as printed in the console
            testthat::expect_no_error(report(data = data_7, output = output_1, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
            # factors, dates and lists are written as printed in the console, whatever vector_cat (documented)
            output_vec <- "results_vector_cat_test.txt"
            testthat::expect_no_error(object = saferDev::report(data = base::factor(x = base::c("a", "b"), levels = base::c("a", "b"), labels = base::c("a", "b"), exclude = NA, ordered = FALSE, nmax = NA), output = output_vec, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = TRUE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = base::as.Date(x = "2026-11-25"), output = output_vec, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = TRUE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = base::list(1), output = output_vec, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = TRUE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            out_vec <- base::readLines(con = base::file.path(path_1, output_vec, fsep = "/"), n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE)
            testthat::expect_identical(object = out_vec, expected = base::c("[1] a b", "Levels: a b", "", "[1] \"2026-11-25\"", "", "[[1]]", "[1] 1", ""), info = NULL, label = NULL, expected.label = NULL)
            base::unlink(x = base::file.path(path_1, output_vec, fsep = "/"), recursive = FALSE, force = TRUE, expand = TRUE)
            # end }else{ # other object

            # rownames_kept = TRUE (untested half of the documented parameter): row names must be kept in 2D objects
            # dedicated output file, removed before each write because report() with append = FALSE refuses to overwrite an existing file
            output_rownames <- "results_rownames_test.txt"
            base::unlink(x = base::file.path(path_1, output_rownames), recursive = TRUE, force = TRUE)
            data_rownames <- base::matrix(1:4, nrow = 2, ncol = 2, dimnames = base::list(base::c("rowA", "rowB"), base::c("colA", "colB")))
            # reference: rownames_kept = FALSE must strip the row names
            testthat::expect_no_error(report(data = data_rownames, output = output_rownames, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
            out_no_rownames <- base::readLines(con = base::file.path(path_1, output_rownames), ok = TRUE, warn = FALSE, skipNul = FALSE)
            testthat::expect_false(object = base::any(base::grepl(x = out_no_rownames, pattern = "rowA", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE))
            testthat::expect_true(object = base::any(base::grepl(x = out_no_rownames, pattern = "colA", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE)) # col names are not removed by rownames_kept (only row names are)
            # rownames_kept = TRUE must keep them
            base::unlink(x = base::file.path(path_1, output_rownames), recursive = TRUE, force = TRUE)
            testthat::expect_no_error(report(data = data_rownames, output = output_rownames, path = path_1, append = FALSE, rownames_kept = TRUE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
            out_with_rownames <- base::readLines(con = base::file.path(path_1, output_rownames), ok = TRUE, warn = FALSE, skipNul = FALSE)
            testthat::expect_true(object = base::any(base::grepl(x = out_with_rownames, pattern = "rowA", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE))
            testthat::expect_true(object = base::any(base::grepl(x = out_with_rownames, pattern = "colA", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE))
            testthat::expect_true(object = base::any(base::grepl(x = out_with_rownames, pattern = "rowB", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE))
            testthat::expect_true(object = base::any(base::grepl(x = out_with_rownames, pattern = "colB", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE))
            # same on a data.frame with row names (default FALSE: written without row names, whatever the number of rows)
            df_rownames <- base::data.frame(A = 1:2, B = base::c("x", "y"), row.names = base::c("r1", "r2"), stringsAsFactors = TRUE)
            base::unlink(x = base::file.path(path_1, output_rownames), recursive = TRUE, force = TRUE)
            testthat::expect_no_error(report(data = df_rownames, output = output_rownames, path = path_1, append = FALSE, rownames_kept = TRUE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
            out_df_true <- base::readLines(con = base::file.path(path_1, output_rownames), ok = TRUE, warn = FALSE, skipNul = FALSE)
            testthat::expect_true(object = base::any(base::grepl(x = out_df_true, pattern = "r1", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE))
            base::unlink(x = base::file.path(path_1, output_rownames), recursive = TRUE, force = TRUE)
            testthat::expect_no_error(report(data = df_rownames, output = output_rownames, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
            out_df_false <- base::readLines(con = base::file.path(path_1, output_rownames), ok = TRUE, warn = FALSE, skipNul = FALSE)
            testthat::expect_false(object = base::any(base::grepl(x = out_df_false, pattern = "r1", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE))
            base::unlink(x = base::file.path(path_1, output_rownames), recursive = TRUE, force = TRUE) # cleanup, same as the final unlink of output_1
            # rownames_kept = FALSE also removes the row names of data frames of more than 4 rows (before: kept)
            df_rownames_5 <- base::data.frame(A = 1:5, B = letters[1:5], row.names = base::paste0("row", 1:5, collapse = NULL, recycle0 = FALSE), check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
            testthat::expect_no_error(object = saferDev::report(data = df_rownames_5, output = output_rownames, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            out_df_5 <- base::readLines(con = base::file.path(path_1, output_rownames, fsep = "/"), n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE)
            testthat::expect_identical(object = out_df_5, expected = base::c(" A B", " 1 a", " 2 b", " 3 c", " 4 d", " 5 e", ""), info = NULL, label = NULL, expected.label = NULL)
            base::unlink(x = base::file.path(path_1, output_rownames, fsep = "/"), recursive = FALSE, force = TRUE, expand = TRUE)
            testthat::expect_no_error(object = saferDev::report(data = df_rownames_5, output = output_rownames, path = path_1, append = FALSE, rownames_kept = TRUE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            out_df_5 <- base::readLines(con = base::file.path(path_1, output_rownames, fsep = "/"), n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE)
            testthat::expect_identical(object = out_df_5, expected = base::c("     A B", "row1 1 a", "row2 2 b", "row3 3 c", "row4 4 d", "row5 5 e", ""), info = NULL, label = NULL, expected.label = NULL)
            base::unlink(x = base::file.path(path_1, output_rownames, fsep = "/"), recursive = FALSE, force = TRUE, expand = TRUE)
            # deal with sep: exactly sep empty lines after data, whatever data (before: sep + 1 after single values, 2 after lists with sep = 1)
            output_sep <- "results_sep_test.txt"
            testthat::expect_no_error(object = saferDev::report(data = "TITLE", output = output_sep, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = 1:3, output = output_sep, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 2, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = base::c("a", "b"), output = output_sep, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = TRUE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = base::list(a = 1), output = output_sep, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = base::list(base::list(1)), output = output_sep, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 2, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = 5, output = output_sep, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 3, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            out_sep <- base::readLines(con = base::file.path(path_1, output_sep, fsep = "/"), n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE)
            testthat::expect_identical(object = out_sep, expected = base::c("TITLE", "", "[1] 1 2 3", "", "", "a b", "", "$a", "[1] 1", "", "[[1]]", "[[1]][[1]]", "[1] 1", "", "", "5", "", "", ""), info = NULL, label = NULL, expected.label = NULL)
            # returned value: invisible NULL
            result <- base::withVisible(x = saferDev::report(data = "abc", output = output_sep, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
            testthat::expect_null(object = result$value, info = NULL, label = NULL)
            testthat::expect_false(object = result$visible, info = NULL, label = NULL)
            base::unlink(x = base::file.path(path_1, output_sep, fsep = "/"), recursive = FALSE, force = TRUE, expand = TRUE)
            # string ending with a line break, written with cat(): this line break ends the written line, then exactly sep empty lines follow (before: one more empty line)
            output_nl <- "results_newline_test.txt"
            file_nl <- base::file.path(path_1, output_nl, fsep = "/")
            testthat::expect_error(object = saferDev::report(data = "a\n", output = output_nl, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 0, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "sep ARGUMENT CANNOT BE EQUAL TO ZERO.", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
            testthat::expect_error(object = saferDev::report(data = "a\n\n", output = output_nl, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 0, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "sep ARGUMENT CANNOT BE EQUAL TO ZERO.", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
            testthat::expect_false(object = base::file.exists(file_nl), info = NULL, label = NULL) # nothing written
            testthat::expect_no_error(object = saferDev::report(data = "a\n", output = output_nl, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_identical(object = base::readLines(con = file_nl, n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE), expected = base::c("a", ""), info = NULL, label = NULL, expected.label = NULL)
            testthat::expect_no_error(object = saferDev::report(data = "a\n", output = output_nl, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 2, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_identical(object = base::readLines(con = file_nl, n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE), expected = base::c("a", "", "a", "", ""), info = NULL, label = NULL, expected.label = NULL)
            testthat::expect_no_error(object = saferDev::report(data = "a\n\n", output = output_nl, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_identical(object = base::readLines(con = file_nl, n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE), expected = base::c("a", "", "a", "", "", "a", "", ""), info = NULL, label = NULL, expected.label = NULL) # "a", the empty line of the string, then 1 empty line
            testthat::expect_no_error(object = saferDev::report(data = "a\n\n", output = output_nl, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 2, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_identical(object = base::readLines(con = file_nl, n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE), expected = base::c("a", "", "a", "", "", "a", "", "", "a", "", "", ""), info = NULL, label = NULL, expected.label = NULL)
            base::unlink(x = file_nl, recursive = FALSE, force = TRUE, expand = TRUE)
            # same with vector_cat = TRUE when the last string ends with a line break
            testthat::expect_no_error(object = saferDev::report(data = base::c("x", "y\n"), output = output_nl, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = TRUE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = base::c("x", "y\n"), output = output_nl, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = TRUE, noquote = FALSE, sep = 2, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = base::c("x\n", "y"), output = output_nl, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = TRUE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL) # line break not at the end of the written text
            str_bytes <- base::"Encoding<-"(x = base::rawToChar(x = base::as.raw(x = base::c(231, 10)), multiple = FALSE), value = "bytes") # non-ASCII string marked "bytes" and ending with a line break
            testthat::expect_no_error(object = saferDev::report(data = str_bytes, output = output_nl, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL) # cat() writes its bytes as \xe7\x0a: the written text does not end with a line break
            testthat::expect_identical(object = base::readLines(con = file_nl, n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE), expected = base::c("x y", "", "x y", "", "", "x", " y", "", "\\xe7\\x0a", ""), info = NULL, label = NULL, expected.label = NULL)
            base::unlink(x = file_nl, recursive = FALSE, force = TRUE, expand = TRUE)
            # object printed as nothing: no line of data, only the sep empty lines (before: one more empty line). Here, S3 print methods writing nothing, or only empty lines (removed as trailing empty lines of the printing)
            base::registerS3method(genname = "print", class = "report_test_silent", method = function(x, ...){base::invisible(x = x)}, envir = base::environment(fun = NULL)) # registered in the S3 methods table of the base namespace, removed below
            base::registerS3method(genname = "print", class = "report_test_empty_lines", method = function(x, ...){base::cat("\n\n", file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) ; base::invisible(x = x)}, envir = base::environment(fun = NULL))
            testthat::expect_no_error(object = saferDev::report(data = base::structure(.Data = base::list(1), class = "report_test_silent"), output = output_nl, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 2, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_identical(object = base::readLines(con = file_nl, n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE), expected = base::c("", ""), info = NULL, label = NULL, expected.label = NULL)
            base::unlink(x = file_nl, recursive = FALSE, force = TRUE, expand = TRUE)
            testthat::expect_no_error(object = saferDev::report(data = "TITLE", output = output_nl, path = path_1, append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = base::structure(.Data = base::list(1), class = "report_test_empty_lines"), output = output_nl, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_no_error(object = saferDev::report(data = "END", output = output_nl, path = path_1, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), message = NULL, class = NULL)
            testthat::expect_identical(object = base::readLines(con = file_nl, n = -1L, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE), expected = base::c("TITLE", "", "", "END", ""), info = NULL, label = NULL, expected.label = NULL) # 1 empty line after TITLE, then 1 after the object printed as empty lines
            base::rm(list = base::c("print.report_test_silent", "print.report_test_empty_lines"), pos = -1, envir = base::get(x = ".__S3MethodsTable__.", pos = -1L, envir = base::environment(fun = base::print), mode = "environment", inherits = FALSE), inherits = FALSE) # registered methods removed
            base::unlink(x = file_nl, recursive = FALSE, force = TRUE, expand = TRUE)


    #### end main code
    ## end tests (ordered by arg appearance and conditions in the code)


    ## other tests


    ## end other tests
    base::unlink(x = path_1, recursive = TRUE, force = TRUE, expand = TRUE) # removes the folder of the files written by the tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
