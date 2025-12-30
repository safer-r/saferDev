testthat::test_that("report()", {

    ## data argument values
    data_1 <- data.frame(A = 1:8, B = letters[1:8])
    output_1 <- "results.txt"
    path_1 <- "."

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
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    testthat::expect_no_error(report(data = NULL, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = NULL, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = NULL, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = NULL, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = NULL, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = NULL, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = NULL, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(report(data = character(), output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = integer(), output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = double(), output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = logical(), output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = complex(), output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data.frame(), output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = list(), output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = character(), path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = integer(), path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = double(), path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = logical(), path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = complex(), path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = data.frame(), path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = list(), path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = character(), overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = integer(), overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = double(), overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = logical(), overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = complex(), overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = data.frame(), overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = list(), overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = character(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = integer(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = double(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = logical(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = complex(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = data.frame(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = list(), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = character(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = integer(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = double(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = logical(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = complex(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = data.frame(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = list(), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = character(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = integer(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = double(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = logical(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = complex(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = data.frame(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = list(), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = character(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = integer(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = double(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = logical(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = complex(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = data.frame(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = list(), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(report(data = NA, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = NA, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = NA, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = NA, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = NA, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = NA, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = NA, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # data
    testthat::expect_no_error(report(data = NULL, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = NA, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = 1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = c(TRUE, FALSE), output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = mat1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = factor1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = expr1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(report(data = fun1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end data
    # output
    testthat::expect_error(report(data = data_1, output = NULL, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = NA, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = 1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = c(TRUE, FALSE), path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = mat1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = factor1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = expr1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = fun1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end output
    # path
    testthat::expect_error(report(data = data_1, output = output_1, path = NULL, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = NA, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = 1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = c(TRUE, FALSE), overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = mat1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = factor1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = expr1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = fun1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end path
    # overwrite
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = NULL, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = NA, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = 1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = c(TRUE, FALSE), rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = mat1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = factor1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = expr1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = fun1, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end overwrite
    # rownames_kept
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = NULL, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = NA, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = 1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = c(TRUE, FALSE), vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = mat1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = factor1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = expr1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = fun1, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end rownames_kept
    # vector_cat
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = NULL, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = NA, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = 1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = c(TRUE, FALSE), noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = mat1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = factor1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = expr1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = fun1, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end vector_cat
    # noquote
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = NULL, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = NA, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = 1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = c(TRUE, FALSE), sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = mat1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = factor1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = expr1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = fun1, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end noquote
    # sep
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end sep
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(report(data = data_1, output = "", path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = "", overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")) 
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
    testthat::expect_error(report(data = data_1, output = output_1, path = "caca", overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('report(data = data_1, output = output_1, path = "caca", overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")')
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::report().\n\npath ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY:\ncaca\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    testthat::expect_error(report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 0, safer_check = TRUE, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('report(data = data_1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 0, safer_check = TRUE, lib_path = NULL, error_text = "")')
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::report().\n\nsep ARGUMENT CANNOT BE EQUAL TO ZERO.\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    dir.create("test")
    testthat::expect_error(report(data = data_1, output = "test", path = path_1, overwrite = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('report(data = data_1, output = "test", path = path_1, overwrite = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = "")')
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::report().\n\nFILE DEFINED BY THE path AND output ARGUMENTS\n./test\nALREADY EXISTS AND CANNOT BE OVERWRITTEN.\nPLEASE:\nREMOVE THE FILE\nOR CHANGE THE NAME OF THE output ARGUMENT\nOR SET THE overwrite ARGUMENT TO FALSE TO APPEND IN THE EXISTING FILE.\n\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    unlink("test", recursive = TRUE)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # if( ! base::is.null(x = data)){
            # if(base::all(base::class(x = data) == "data.frame", na.rm = TRUE) | base::all(base::class(x = data) == "table", na.rm = TRUE) | base::all(base::class(x = data) %in% base::c("matrix", "array"), na.rm = TRUE)){
                # if(rownames_kept == FALSE & base::all(base::class(x = data) == "data.frame", na.rm = TRUE) & base::nrow(x = data) != 0 & base::nrow(x = data) <= 4){ # for data frames with nrows <= 4
                    testthat::expect_no_error(report(data = data_2, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    testthat::expect_no_error(report(data = data_3, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    testthat::expect_no_error(report(data = data_4, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # if(noquote == TRUE){
                    testthat::expect_no_error(report(data = data_2, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    testthat::expect_no_error(report(data = data_3, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    testthat::expect_no_error(report(data = data_4, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(noquote == TRUE){
                # end if(rownames_kept == FALSE & base::all(base::class(x = data) == "data.frame", na.rm = TRUE) & base::nrow(x = data) != 0 & base::nrow(x = data) <= 4){ # for data frames with nrows <= 4
                # }else if(base::is.vector(x = data, mode = "any") & base::all(base::class(x = data) != "list", na.rm = TRUE) & (base::length(x = data) == 1L | vector_cat == TRUE)){
                    testthat::expect_no_error(report(data = data_5, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # if(noquote == TRUE){
                    testthat::expect_no_error(report(data = data_5, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(noquote == TRUE){
                    testthat::expect_no_error(report(data = 1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = TRUE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # if(noquote == TRUE){
                    testthat::expect_no_error(report(data = 1, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = TRUE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(noquote == TRUE){
                # end }else if(base::is.vector(x = data, mode = "any") & base::all(base::class(x = data) != "list", na.rm = TRUE) & (base::length(x = data) == 1L | vector_cat == TRUE)){
                #}else if(base::all(base::mode(x = data) == "character", na.rm = TRUE)){
                    testthat::expect_no_error(report(data = data_6, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # if(noquote == TRUE){
                    testthat::expect_no_error(report(data = data_6, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
                    # end if(noquote == TRUE){
                # end }else if(base::all(base::mode(x = data) == "character", na.rm = TRUE)){
            # end if(base::all(base::class(x = data) == "data.frame", na.rm = TRUE) | base::all(base::class(x = data) == "table", na.rm = TRUE) | base::all(base::class(x = data) %in% base::c("matrix", "array"), na.rm = TRUE)){
            # if(base::all(base::class(x = data) == "list", na.rm = TRUE)){
            testthat::expect_no_error(report(data = data_7, output = output_1, path = path_1, overwrite = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = FALSE, sep = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
            # end if(base::all(base::class(x = data) == "list", na.rm = TRUE)){


    #### end main code
    ## end tests (ordered by arg appearance and conditions in the code)


    ## other tests


    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})
