testthat::test_that("arg_check()", {

    ## data argument values
    vec1 <- -1:3 # vector of integers
    vec3 <- base::c(1, 2, 3) # vector of double
    vec4 <- "pearson"
    vec5 <- base::c("a", "b","a", "b")
    vec6 <- base::list(1:3, 4:6)
    vec7 <- vec3 / 2 # vector of decimals
    vec8 <- c(vec3, Inf)
    mat1 <- base::matrix(vec1)
    mat2 <- base::matrix(base::c(1:3 / 3, NA))
    mat3 <- base::matrix(vec5)
    arr1 <- base::array(mat1)
    number <- 1
    factor1 <- base::as.factor(vec5)
    ord_factor1 <- ordered(factor1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(arg_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev::arg_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(arg_check())
    data <- vec1
    testthat::expect_error(arg_check()) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        data
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    testthat::expect_error(arg_check(data = NULL))
    result <- arg_check(data = NULL, mode = "numeric")
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE NULL ARGUMENT MUST BE MODE numeric", object_name = "NULL")
    testthat::expect_equal(result, expect)
    testthat::expect_no_error(arg_check(data = vec1, class = NULL,  mode = "numeric"))
    testthat::expect_no_error(arg_check(data = vec1, class = "list", mode = NULL))
    result <- arg_check(data = vec1, class = "list", mode = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS list", object_name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", typeof = NULL))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", length = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", prop = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = NULL))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", options = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", all_options_in_data = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", na_contain = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", neg_values = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", inf_values = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", print = NULL))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", data_name = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_arg = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = NULL))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = NULL))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_no_error(arg_check(data = character(), mode = "numeric"))
    testthat::expect_no_error(arg_check(data = integer(), mode = "numeric"))
    testthat::expect_no_error(arg_check(data = double(), mode = "numeric"))
    testthat::expect_no_error(arg_check(data = logical(), mode = "numeric"))
    testthat::expect_no_error(arg_check(data = complex(), mode = "numeric"))
    testthat::expect_no_error(arg_check(data = data.frame(), mode = "numeric"))
    testthat::expect_no_error(arg_check(data = list(), mode = "numeric"))

    testthat::expect_error(arg_check(data = vec1, class = character()))
    testthat::expect_error(arg_check(data = vec1, typeof = character()))
    testthat::expect_error(arg_check(data = vec1, mode = character()))
    testthat::expect_error(arg_check(data = vec1, length = integer()))
    testthat::expect_error(arg_check(data = vec1, prop = double()))
    testthat::expect_error(arg_check(data = vec1, double_as_integer_allowed = logical()))
    testthat::expect_error(arg_check(data = vec1, options = NA))
    testthat::expect_error(arg_check(data = vec1, all_options_in_data = logical()))
    testthat::expect_error(arg_check(data = vec1, na_contain = logical()))
    testthat::expect_error(arg_check(data = vec1, neg_values = logical()))
    testthat::expect_error(arg_check(data = vec1, inf_values = logical()))
    testthat::expect_error(arg_check(data = vec1, print = logical()))
    testthat::expect_error(arg_check(data = vec1, data_name = character()))
    testthat::expect_error(arg_check(data = vec1, data_arg = character()))
    testthat::expect_error(arg_check(data = vec1, safer_check = logical()))
    testthat::expect_error(arg_check(data = vec1, lib_path = character()))
    testthat::expect_error(arg_check(data = vec1, error_text = character()))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(arg_check(data = NA))
    testthat::expect_error(arg_check(data = vec1, class = NA))
    testthat::expect_error(arg_check(data = vec1, typeof = NA))
    testthat::expect_error(arg_check(data = vec1, mode = NA))
    testthat::expect_error(arg_check(data = vec1, length = NA))
    testthat::expect_error(arg_check(data = vec1, prop = NA))
    testthat::expect_error(arg_check(data = vec1, double_as_integer_allowed = NA))
    testthat::expect_error(arg_check(data = vec1, options = NA))
    testthat::expect_error(arg_check(data = vec1, all_options_in_data = NA))
    testthat::expect_error(arg_check(data = vec1, na_contain = NA))
    testthat::expect_error(arg_check(data = vec1, neg_values = NA))
    testthat::expect_error(arg_check(data = vec1, inf_values = NA))
    testthat::expect_error(arg_check(data = vec1, print = NA))
    testthat::expect_error(arg_check(data = vec1, data_name = NA))
    testthat::expect_error(arg_check(data = vec1, data_arg = NA))
    testthat::expect_error(arg_check(data = vec1, safer_check = NA))
    testthat::expect_error(arg_check(data = vec1, lib_path = NA))
    testthat::expect_error(arg_check(data = vec1, error_text = NA))
    ######## end management of NA arguments

    ######## safer_check argument
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = c(TRUE, FALSE)))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = mat1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = factor1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = expr1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = fun1))
    result <- arg_check(data = vec1, mode = "numeric", safer_check = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", safer_check = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    ######## end safer_check argument

    ######## lib_path argument
    # safer_check must be TRUE
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = c(TRUE, FALSE)))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = mat1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = factor1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = expr1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = fun1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = "PATH_NOT_GOOD"))
    result <- arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = base::.libPaths())
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # lib_path restoration assertion (the .libPaths() must be restored after execution, as documented)
    ini_lib_path <- base::.libPaths(new = , include.site = TRUE)
    tempo_lib_dir <- base::file.path(base::tempdir(), "safer_dev_test_lib_dir")
    base::dir.create(path = tempo_lib_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = tempo_lib_dir))
    testthat::expect_equal(base::.libPaths(new = , include.site = TRUE), ini_lib_path) # .libPaths() must be restored after function execution
    # same assertion after an error triggered inside the function (on.exit() must still restore)
    # note: an error triggered before the .libPaths() modification (e.g. non-existing lib_path dir) would not exercise on.exit(), so an error AFTER the modification is used (invalid mode value caught by the secondary checking): the temp dir must still exist at this point
    ini_lib_path2 <- base::.libPaths(new = , include.site = TRUE)
    testthat::expect_error(arg_check(data = vec1, mode = 123, safer_check = TRUE, lib_path = tempo_lib_dir)) # invalid mode value: must stop after the .libPaths() modification
    testthat::expect_equal(base::.libPaths(new = , include.site = TRUE), ini_lib_path2) # .libPaths() must be restored even after an internal error
    base::unlink(x = tempo_lib_dir, recursive = TRUE, force = TRUE)
    ######## end lib_path argument

    ######## check of the required functions from the required packages
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE)) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE)) # see above for the result comparison
    ######## end critical operator checking

    ######## management of "" in arguments of mode character
    testthat::expect_error(arg_check(data = vec1, class = 1))
    testthat::expect_error(arg_check(data = vec1, typeof = 1))
    testthat::expect_error(arg_check(data = vec1, mode = 1))
    testthat::expect_error(arg_check(data = vec1, data_name = 1))
    testthat::expect_error(arg_check(data = vec1, safer_check = TRUE, lib_path = 1))

    testthat::expect_no_error(arg_check(data = "", mode = "numeric"))
    testthat::expect_error(arg_check(data = vec1, class = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", typeof = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", mode = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", length = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", prop = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", options = ""))
    testthat::expect_no_error(arg_check(data = vec1, options = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", all_options_in_data = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", na_contain = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", neg_values = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", inf_values = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", print = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_name = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = ""))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = ""))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", error_text = ""))
    ######## end management of "" in arguments of mode character

    ######## other checkings
    # management of special classes
    result <- arg_check(data = vec1, class = mat3, typeof = "integer") # mat3 is a matrix of characters: now accepted as a vector of class strings (any string allowed), returning a problem because class(vec1) is "integer"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS a, b, a, b", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", typeof = mat3) # mat3 is a matrix of characters: now accepted as a vector of typeof strings (any string allowed), returning a problem because typeof(vec1) is a single string. Note that mode = "numeric" now matches exactly mode(vec1)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE TYPEOF a, b, a, b", object_name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", mode = mat3)) # R itself errors because the formal argument "mode" is matched by multiple actual arguments. Replaced by the two lines below
    result <- arg_check(data = vec1, mode = mat3) # mat3 is a matrix of characters: now accepted as a vector of mode strings (any string allowed), returning a problem because mode(vec1) is a single string
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE MODE a, b, a, b", object_name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", length = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", prop = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", options = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", all_options_in_data = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", na_contain = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", neg_values = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", inf_values = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", print = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_name = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_arg = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = TRUE, lib_path = mat3))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", safer_check = mat3))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", error_text = mat3))
    # end management of special classes
    # authorized modes for the arguments
    testthat::expect_error(arg_check(data = vec1, double_as_integer_allowed = function(){}))
    testthat::expect_error(arg_check(data = vec1, options = TRUE))
    testthat::expect_error(arg_check(data = vec1, options = 1.2))
    # end authorized modes for the arguments
    # management of the logical arguments
    # prop tested below
    # double_as_integer_allowed
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", double_as_integer_allowed = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, typeof = "integer", double_as_integer_allowed = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, typeof = "integer", double_as_integer_allowed = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE TYPEOF integer", object_name = "vec3")
    testthat::expect_equal(result, expect)
    # end double_as_integer_allowed
    # all_options_in_data
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", all_options_in_data = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", all_options_in_data = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", all_options_in_data = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", all_options_in_data = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, mode = "numeric", all_options_in_data = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", all_options_in_data = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # end all_options_in_data
    # na_contain
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", na_contain = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", na_contain = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", na_contain = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", na_contain = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, mode = "numeric", na_contain = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", na_contain = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = mat2, mode = "numeric", na_contain = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE mat2 ARGUMENT.", object_name = "mat2")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = mat2, mode = "numeric", na_contain = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat2 ARGUMENT CONTAINS NA WHILE NOT AUTHORIZED.", object_name = "mat2")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", na_contain = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", na_contain = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric", object_name = "vec5")
    testthat::expect_equal(result, expect)
    # end na_contain
    # neg_values
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", neg_values = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", neg_values = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", neg_values = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", neg_values = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, mode = "numeric", neg_values = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", neg_values = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND THE vec5 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS NOT EVEN NUMERIC.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    # end neg_values
    # inf_values
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", inf_values = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", inf_values = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", inf_values = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", inf_values = c(TRUE, FALSE)))
    result <- arg_check(data = vec8, mode = "numeric", inf_values = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec8 ARGUMENT.", object_name = "vec8")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec8, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text =  "ERROR\n\nTHE vec8 ARGUMENT MUST BE MADE OF NON INFINITE VALUES.", object_name = "vec8")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", inf_values = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND THE vec5 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN MODE NUMERIC OR TYPE DOUBLE.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    # end inf_values
    # print
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", print = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", print = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", print = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", print = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, mode = "numeric", print = TRUE) 
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", print = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", print = TRUE) # the error message is printed but it is not an arror, stopping the execution
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", print = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric", object_name = "vec5")
    testthat::expect_equal(result, expect)
    # end print
    # data_arg
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_arg = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_arg = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_arg = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_arg = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, mode = "numeric", data_arg = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", data_arg = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 OBJECT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", data_arg = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", data_arg = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 OBJECT MUST BE MODE numeric", object_name = "vec5")
    testthat::expect_equal(result, expect)
    # end data_arg
    # end management of the logical arguments
    # other checkings of the arguments by order
    # THE data_name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", data_name = c("D1", "D2")))
    # end THE data_name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT
     # AT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop).
    testthat::expect_error(arg_check(data = vec1, options = NULL, class = NULL, typeof = NULL, prop = FALSE, length = NULL))
     # end AT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop).
    # THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED.
    testthat::expect_error(arg_check(data = vec1, options = "a", class = "integer", typeof = NULL, mode = NULL, prop = FALSE)) 
    testthat::expect_error(arg_check(data = vec1, options = "a", class = NULL, typeof = "integer", mode = NULL, prop = FALSE))
    testthat::expect_error(arg_check(data = vec1, options = "a", class = NULL, typeof = NULL, mode = "numeric", prop = FALSE))
    testthat::expect_error(arg_check(data = vec1, options = "a", class = NULL, typeof = NULL, mode = NULL, prop = TRUE))
    result <- arg_check(data = vec1, options = "a", class = NULL, typeof = NULL, mode = NULL, prop = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE SOME OF THESE OPTIONS:\na\nTHE PROBLEMATIC ELEMENTS OF vec1 ARE:\n-1\n0\n1\n2\n3", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, options = NULL, class = "vector", typeof = "double", mode = "numeric", prop = TRUE) # class = "vector" is not a shorthand anymore: class(vec1) is "integer"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS vector AND TYPEOF double AND THE vec1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # end THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED.
    # THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    testthat::expect_error(arg_check(data = vec1, class = NULL, typeof = NULL, mode = NULL, neg_values = FALSE, prop = TRUE))
    # end THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    # THE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    testthat::expect_error(arg_check(data = vec1, class = NULL, typeof = NULL, mode = NULL, inf_values = FALSE, prop = TRUE))
    # end THE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    #  ! base::is.null(class) # the class argument is free now: any character strings (single or vector) tested exactly against class(data). See the main code below
    testthat::expect_error(arg_check(data = vec5, class = NULL))
    testthat::expect_error(arg_check(data = vec5, class = NA))
    testthat::expect_error(arg_check(data = vec5, class = 1))
    result <- arg_check(data = vec5, class = "NOTGOOD") # no error thrown: any string is allowed. Returns a problem because class(vec5) is "character"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE CLASS NOTGOOD", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, class = c("character", "NOTGOOD")) # no error thrown: any string is allowed. Returns a problem because class(vec5) is "character"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE CLASS character, NOTGOOD", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, class = "character")
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec5 ARGUMENT.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, class = c("character", "character")) # no error thrown: any string is allowed. Returns a problem because class(vec5) is a single string and cannot be equal to a vector of 2 values
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE CLASS character, character", object_name = "vec5")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec5, class = "vector", neg_values = FALSE)) # error because typeof and mode are NULL and class is not NULL. Note that class = "vector" is not a shorthand anymore
    testthat::expect_no_error(arg_check(data = vec5, class = "vector", neg_values = TRUE)) # neg_values is inactivated
    testthat::expect_error(arg_check(data = vec5, class = "vector", inf_values = FALSE))
    testthat::expect_error(arg_check(data = vec5, class = "integer", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec5, class = "vector", inf_values = TRUE)) # inf_values is inactivated
    # end  ! base::is.null(class)
    #  ! base::is.null(typeof) # the typeof argument is free now: any character strings (single or vector) tested exactly against typeof(data). See the main code below
    testthat::expect_error(arg_check(data = vec1, typeof = NULL))
    testthat::expect_error(arg_check(data = vec1, typeof = NA))
    testthat::expect_error(arg_check(data = vec1, typeof = 1))
    result <- arg_check(data = vec1, typeof = "NOTGOOD") # no error thrown: any string is allowed. Returns a problem because typeof(vec1) is "integer"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE TYPEOF NOTGOOD", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, typeof = c("integer", "NOTGOOD")) # no error thrown: any string is allowed. Returns a problem because typeof(vec1) is "integer"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE TYPEOF integer, NOTGOOD", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, typeof = c("integer", "integer")) # no error thrown: any string is allowed. Returns a problem because typeof(vec1) is a single string and cannot be equal to a vector of 2 values
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE TYPEOF integer, integer", object_name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, typeof = "character", neg_values = FALSE)) # error because neg_values FALSE requires a single "double" or "integer" typeof value
    testthat::expect_no_error(arg_check(data = vec1, typeof = "double", neg_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "character", neg_values = TRUE)) # neg_values is inactivated
    testthat::expect_error(arg_check(data = vec1, typeof = "character", inf_values = FALSE)) # error because inf_values FALSE requires a single "double" typeof value
    testthat::expect_error(arg_check(data = vec1, typeof = "integer", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "double", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "character", inf_values = TRUE)) # inf_values is inactivated
    # end  ! base::is.null(typeof)
    #  ! base::is.null(mode) # the mode argument is free now: any character strings (single or vector) tested exactly against mode(data). See the main code below
    testthat::expect_error(arg_check(data = vec1, mode = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = NA))
    testthat::expect_error(arg_check(data = vec1, mode = 1))
    result <- arg_check(data = vec1, mode = "NOTGOOD") # no error thrown: any string is allowed. Returns a problem because mode(vec1) is "numeric"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE MODE NOTGOOD", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = c("numeric", "NOTGOOD")) # no error thrown: any string is allowed. Returns a problem because mode(vec1) is "numeric"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE MODE numeric, NOTGOOD", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = c("numeric", "numeric")) # no error thrown: any string is allowed. Returns a problem because mode(vec1) is a single string and cannot be equal to a vector of 2 values
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE MODE numeric, numeric", object_name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, mode = "character", neg_values = FALSE)) # error because neg_values FALSE requires a single "numeric" mode value
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", neg_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "character", neg_values = TRUE)) # neg_values is inactivated
    testthat::expect_error(arg_check(data = vec1, mode = "character", inf_values = FALSE)) # error because inf_values FALSE requires a single "numeric" mode value
    testthat::expect_error(arg_check(data = vec1, mode = "integer", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "character", inf_values = TRUE)) # inf_values is inactivated
    # end  ! base::is.null(mode)
    #  ! base::is.null(length)
    testthat::expect_error(arg_check(data = vec1, length = NULL))
    testthat::expect_error(arg_check(data = vec1, length = NA))
    result <- arg_check(data = vec1, length = 1)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE LENGTH 1", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", length = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", length = "a"))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", length = 1.1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", length = 1:2))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", length = 1))
    #  end ! base::is.null(length)
    #  prop
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", prop = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", prop = NA))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", prop = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", prop = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, mode = "numeric", prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND THE vec5 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    testthat::expect_no_error(arg_check(data = vec1, class = "vector", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec1, class = "vector", prop = FALSE))
    testthat::expect_error(arg_check(data = vec5, mode = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec5, mode = "character", prop = FALSE))
    testthat::expect_error(arg_check(data = vec1, typeof = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "character", prop = FALSE))
    testthat::expect_error(arg_check(data = vec7, typeof = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec7, typeof = "character", prop = FALSE))
    testthat::expect_no_error(arg_check(data = vec7, typeof = "double", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec7, typeof = "double", prop = FALSE))
    #  end prop
    # other checkings of the arguments by order
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # if(base::is.null(data_name)){
    testthat::expect_no_error(arg_check(data = vec1, class = "vector", data_name = NULL))
    result <- arg_check(data = vec1, mode = "numeric", data_name = "my_obj") # valid non NULL data_name: used in the message and the returned object_name
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE my_obj ARGUMENT.", object_name = "my_obj")
    testthat::expect_equal(result, expect)
    # end if(base::is.null(data_name)){
    # if(( ! base::is.null(options)) & (base::all(base::typeof(data) == "character") | base::all(base::typeof(data) == "integer") | base::all(base::typeof(data) == "double"))){
    testthat::expect_no_error(arg_check(data = vec5, options = "a")) # data of typeof "character"
    testthat::expect_no_error(arg_check(data = vec1, options = "a")) # data of typeof "integer"

 # if(( ! base::is.null(x = options)) & (base::all(base::typeof(x = data) == "character", na.rm = TRUE) | base::all(base::typeof(x = data) == "integer", na.rm = TRUE) | base::all(base::typeof(x = data) == "double", na.rm = TRUE))){
    # if(base::all(base::typeof(x = tempo_data_opt) == "double", na.rm = TRUE)){
    result <- arg_check(data = vec7, options = 1:2, all_options_in_data = FALSE) # vec7 is double decimal
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER, OR TYPE DOUBLE WITH A 0 MODULO.", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, options = 1:2, all_options_in_data = TRUE) # vec7 is double decimal
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER, OR TYPE DOUBLE WITH A 0 MODULO.", object_name = "vec7")
    testthat::expect_equal(result, expect)
    # end if(base::all(base::typeof(x = tempo_data_opt) == "double", na.rm = TRUE)){
    # if( ! base::all(tempo_data_opt %in% options, na.rm = TRUE)){
    result <- arg_check(data = vec3, options = 1:2, all_options_in_data = FALSE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = c(1,2), all_options_in_data = FALSE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = c(1,2,4), all_options_in_data = FALSE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\n4\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1, all_options_in_data = FALSE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\nTHE PROBLEMATIC ELEMENTS OF vec3 ARE:\n2\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1:4, all_options_in_data = FALSE) # vec3 is double but integer like, options is integer
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = c(1.0, 2.0, 3.0, 4.0), all_options_in_data = FALSE) # options written as doubles but with a 0 modulo: accepted as documented ("Numbers of type "double" are accepted if they have a 0 modulo")
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = c(1.0, 2.0, 3.0), all_options_in_data = TRUE) # options as doubles with 0 modulo, all present in data: accepted
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec3, options = c(1.5, 2.0), all_options_in_data = FALSE)) # options containing a real decimal (non zero modulo): error (stop) expected
    result <- arg_check(data = vec4, options = 1:4, all_options_in_data = FALSE) # vec3 is double but integer like, options is integer
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec4 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\n3\n4\nTHE PROBLEMATIC ELEMENT OF vec4 IS:\npearson", object_name = "vec4")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec4, options = c("pearson", "spearman"), all_options_in_data = FALSE) # vec4 is double but integer like, options is integer
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec4 ARGUMENT.", object_name = "vec4")
    testthat::expect_equal(result, expect)
    # end if( ! base::all(tempo_data_opt %in% options, na.rm = TRUE)){
    # if(all_options_in_data == TRUE){
    result <- arg_check(data = vec3, options = 1:2, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = c(1,2), all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = c(1,2,4), all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\n4\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3 AND THE vec3 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n4\nTHE MISSING ELEMENT IN vec3 IS:\n4", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = 1, options = c(1,2,4), all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE 1 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n4\nTHE MISSING ELEMENTS IN 1 ARE:\n2\n4", object_name = "1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\nTHE PROBLEMATIC ELEMENTS OF vec3 ARE:\n2\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1:4, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n3\n4\nTHE MISSING ELEMENT IN vec3 IS:\n4", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1:4, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n3\n4\nTHE MISSING ELEMENT IN vec3 IS:\n4", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec4, options = 1:4, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec4 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\n3\n4\nTHE PROBLEMATIC ELEMENT OF vec4 IS:\npearson AND THE vec4 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n3\n4\nTHE MISSING ELEMENTS IN vec4 ARE:\n1\n2\n3\n4", object_name = "vec4")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec4, options = c("pearson", "spearman"), all_options_in_data = TRUE) # vec4 is double but integer like, options is integer
    expect <- list(problem = TRUE, text =  "ERROR\n\nTHE vec4 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\npearson\nspearman\nTHE MISSING ELEMENT IN vec4 IS:\nspearman", object_name = "vec4")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1:2, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = c(1,2), all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = 1, options = c(1,2), all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE 1 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\nTHE MISSING ELEMENT IN 1 IS:\n2", object_name = "1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = c(1,2,4), all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem =  TRUE, text =  "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\n4\nTHE PROBLEMATIC ELEMENT OF vec3 IS:\n3 AND THE vec3 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n4\nTHE MISSING ELEMENT IN vec3 IS:\n4", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1:4, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n3\n4\nTHE MISSING ELEMENT IN vec3 IS:\n4", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1:4, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n3\n4\nTHE MISSING ELEMENT IN vec3 IS:\n4", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec4, options = 1:4, all_options_in_data = TRUE) # vec3 is double but integer like, options is integer
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec4 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\n3\n4\nTHE PROBLEMATIC ELEMENT OF vec4 IS:\npearson AND THE vec4 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\n1\n2\n3\n4\nTHE MISSING ELEMENTS IN vec4 ARE:\n1\n2\n3\n4", object_name = "vec4")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec4, options = c("pearson", "spearman"), all_options_in_data = TRUE) # vec4 is double but integer like, options is integer
    expect <- list(problem = TRUE, text =  "ERROR\n\nTHE vec4 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\npearson\nspearman\nTHE MISSING ELEMENT IN vec4 IS:\nspearman", object_name = "vec4")
    testthat::expect_equal(result, expect)
    # end if(all_options_in_data == TRUE){

    result <- arg_check(data = vec1, options = 1:2, all_options_in_data = FALSE)
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENTS OF vec1 ARE:\n-1\n0\n3", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, options = 1:2, all_options_in_data = TRUE)
    expect <- list(problem =  TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENTS OF vec1 ARE:\n-1\n0\n3", object_name = "vec1")
    testthat::expect_equal(result, expect)


    result <- arg_check(data = vec1, options = -1:3, all_options_in_data = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, options = 1:2, all_options_in_data = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\n2\nTHE PROBLEMATIC ELEMENTS OF vec1 ARE:\n-1\n0\n3", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, options = -1:3, all_options_in_data = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, options = 8, all_options_in_data = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE SOME OF THESE OPTIONS:\n8\nTHE PROBLEMATIC ELEMENTS OF vec1 ARE:\n-1\n0\n1\n2\n3", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), options = 1)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE ggplot2::ggplot_build(ggplot2::ggplot()) ARGUMENT MUST BE SOME OF THESE OPTIONS:\n1\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER, OR TYPE DOUBLE WITH A 0 MODULO.", object_name = "ggplot2::ggplot_build(ggplot2::ggplot())")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), class = "ggplot_built", options = 1))
    testthat::expect_no_error(arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), class = "ggplot_built"))
    # if( ! base::is.null(x = length)){
    result <- arg_check(data = vec1, length = 5)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, length = 2)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE LENGTH 2", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, length = 2, error_text = " IN ADDED_TEXT")
    expect <- list(problem = TRUE, text = "ERROR IN ADDED_TEXT\n\nTHE vec1 ARGUMENT MUST BE LENGTH 2", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec4, options = c("pearson", "spearman"), length = 2)
    expect <- list(problem = TRUE, text =   "ERROR\n\nTHE LENGTH OF vec4 MUST BE 2 AND NOT 1", object_name = "vec4")
    testthat::expect_equal(result, expect)
    # end if( ! base::is.null(x = length)){

    # end if(( ! base::is.null(options)) & (base::all(base::typeof(data) == "character") | base::all(base::typeof(data) == "integer") | base::all(base::typeof(data) == "double"))){
    #     }else if( ! base::is.null(options)){
    result <- arg_check(data = vec6, options = "a") # data of typeof "list". Error message output expected because only integers or characters allowed for options.
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec6 ARGUMENT MUST BE SOME OF THESE OPTIONS:\na\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER, OR TYPE DOUBLE WITH A 0 MODULO.", object_name = "vec6")
    testthat::expect_equal(result, expect)
    # end     }else if( ! base::is.null(options)){
    # if( ! base::is.null(class)){
    result <- arg_check(data = mat1, class = c("matrix", "array")) # since R 4.0.0, class(mat1) is c("matrix", "array"): this is the valid form of https://github.com/safer-r/saferDev/issues/100
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE mat1 ARGUMENT.", object_name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, class = "factor") # plain (non ordered) factor: class(factor1) is "factor", so class = "factor" alone is valid
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE factor1 ARGUMENT.", object_name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = ord_factor1, class = c("ordered", "factor")) # class(ord_factor1) is c("ordered", "factor"): this is the valid form of https://github.com/safer-r/saferDev/issues/100
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE ord_factor1 ARGUMENT.", object_name = "ord_factor1")
    testthat::expect_equal(result, expect)
    # (a weak expect_no_error(arg_check(data = mat1, class = "matrix")) test was here before: it was misleading because arg_check() does not stop() on a problem, and class = "matrix" alone returns problem = TRUE since R 4.0.0. The correct assertion is made below in the exact-match section)
    # end if( ! base::is.null(class)){
    #     if(base::is.null(options)){ # work on the 4 "class", "typeof", "mode", "length" arguments
    # for class
    # data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") but finally, data is double but not made of integers # if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg_names[i2] == "class" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg_names[i2] == "typeof" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){
    result <- arg_check(data = vec7, typeof = "integer", options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, typeof = "integer", length = 1, options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer AND LENGTH 1", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, typeof = "integer", options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, class = "integer", options = NULL, double_as_integer_allowed = TRUE) # class path rescue: class(vec3) is "numeric" but vec3 is a 0 modulo double, so it is accepted as integer
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, class = "integer", options = NULL) # same without the rescue: problem expected because class(vec3) is "numeric", not "integer"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE CLASS integer", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, class = "integer", options = NULL, double_as_integer_allowed = TRUE) # vec7 is a decimal double (non zero modulo): the rescue must NOT apply, problem expected
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE CLASS integer", object_name = "vec7")
    testthat::expect_equal(result, expect)
    # end data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") but finally, data is double but not made of integers # if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg_names[i2] == "class" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg_names[i2] == "typeof" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){
    # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg_names[i2], "(data)"))) %in% base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    result <- arg_check(data = vec7, typeof = "integer", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, typeof = "integer", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer AND LENGTH 1", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, typeof = "integer", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # end test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg_names[i2], "(data)"))) %in% base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    # test of class == "vector" # }else if(arg_names[i2] == "class" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "vector") & ! (base::all(base::class(data) %in% "numeric") | base::all(base::class(data) %in% "integer") | base::all(base::class(data) %in% "character") | base::all(base::class(data) %in% "logical") | base::all(base::class(data) %in% "complex") | base::all(base::class(data) %in% "expression"))){ # test class == "vector". base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) check if user has used the argument class = "vector". If TRUE and base::length(data) > 1, the class "numeric" "integer" "character" "logical" "complex" "expression" should be returned. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg_names. Other classes "list", "name", "symbol", "function", "environment", "S4", "call" return a list if length of data > 1
    result <- arg_check(data = mat1, class = "vector", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS vector", object_name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = mat1, class = "vector", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS vector AND LENGTH 1", object_name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "vector", options = NULL) # class = "vector" is not a shorthand anymore: class(vec1) is "integer", so a problem is returned
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS vector", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "integer", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # end test of class == "vector" # }else if(arg_names[i2] == "class" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "vector") & ! (base::all(base::class(data) %in% "numeric") | base::all(base::class(data) %in% "integer") | base::all(base::class(data) %in% "character") | base::all(base::class(data) %in% "logical") | base::all(base::class(data) %in% "complex") | base::all(base::class(data) %in% "expression"))){ # test class == "vector". base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) check if user has used the argument class = "vector". If TRUE and base::length(data) > 1, the class "numeric" "integer" "character" "logical" "complex" "expression" should be returned. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg_names. Other classes "list", "name", "symbol", "function", "environment", "S4", "call" return a list if length of data > 1
    # test of class == "ggplot2" # }else if(arg_names[i2] == "class" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "ggplot2") & ! base::all(base::class(data) %in% base::c("gg", "ggplot"))){
    result <- arg_check(data = mat1, class = "ggplot2", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS ggplot2", object_name = "mat1")
    testthat::expect_equal(result, expect)
    if (utils::packageVersion("ggplot2") >= "4.0.0"){
        result <- arg_check(data = ggplot2::ggplot(), class = c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"), options = NULL) # class = "ggplot2" is not a shorthand anymore: it must match class(data) exactly
    }else{
        result <- arg_check(data = ggplot2::ggplot(), class = c("gg", "ggplot"), options = NULL) # or c("gg", "ggplot") depending on what arg_check should accept
    }
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE ggplot2::ggplot() ARGUMENT.", object_name = "ggplot2::ggplot()")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = ggplot2::ggplot(), class = "ggplot2", options = NULL) # class(data) of a ggplot object is not the single string "ggplot2"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE ggplot2::ggplot() ARGUMENT MUST BE CLASS ggplot2", object_name = "ggplot2::ggplot()")
    testthat::expect_equal(result, expect)
    r_version <- as.numeric_version(paste0(R.version$major, ".", R.version$minor))
    if (utils::packageVersion("ggplot2") >= "4.0.0"){
        result <- arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), class = c("ggplot2::ggplot_built", "ggplot_built", "ggplot2::gg", "S7_object"), options = NULL)
    }else{
        result <- arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), class = "ggplot_built", options = NULL) # before ggplot2 4.0.0, class(ggplot_build()) is the single string "ggplot_built"
    }
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE ggplot2::ggplot_build(ggplot2::ggplot()) ARGUMENT.", object_name = "ggplot2::ggplot_build(ggplot2::ggplot())")
    testthat::expect_equal(result, expect)
    if (r_version >= "4.5.0" && utils::packageVersion("ggplot2") >= "4.0.0"){
        result <- arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), class = c("ggplot2::ggplot_built", "ggplot_built", "ggplot2::gg", "S7_object"), mode = "object")
    }else{
        result <- arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), class = "ggplot_built", mode = "list")
    }
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE ggplot2::ggplot_build(ggplot2::ggplot()) ARGUMENT.", object_name = "ggplot2::ggplot_build(ggplot2::ggplot())")
    testthat::expect_equal(result, expect)
    if (r_version >= "4.5.0" && utils::packageVersion("ggplot2") >= "4.0.0"){
        result <- arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), class = c("ggplot2::ggplot_built", "ggplot_built", "ggplot2::gg", "S7_object"), typeof = "object")
    }else{
        result <- arg_check(data = ggplot2::ggplot_build(ggplot2::ggplot()), class = "ggplot_built", typeof = "list")
    }
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE ggplot2::ggplot_build(ggplot2::ggplot()) ARGUMENT.", object_name = "ggplot2::ggplot_build(ggplot2::ggplot())")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = list(), class = "ggplot_built")
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE list() ARGUMENT MUST BE CLASS ggplot_built", object_name = "list()")
    testthat::expect_equal(result, expect)

    # end test of class == "ggplot2" # end }else if(arg_names[i2] == "class" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "ggplot2") & ! base::all(base::class(data) %in% base::c("gg", "ggplot"))){
    # end for class
    # for typeof
    # data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") but finally, data is double but not made of integers # if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg_names[i2] == "class" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg_names[i2] == "typeof" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){
    result <- arg_check(data = vec7, typeof = "integer", options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, typeof = "integer", length = 1, options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer AND LENGTH 1", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, typeof = "integer", options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    # end data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") but finally, data is double but not made of integers # if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg_names[i2] == "class" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg_names[i2] == "typeof" & base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){
    # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg_names[i2], "(data)"))) %in% base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    result <- arg_check(data = vec7, typeof = "integer", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, typeof = "integer", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer AND LENGTH 1", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, typeof = "integer", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # end test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg_names[i2], "(data)"))) %in% base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    # end for typeof
    # for mode
    # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg_names[i2], "(data)"))) %in% base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    result <- arg_check(data = vec7, mode = "character", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE MODE character", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, mode = "character", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE MODE character AND LENGTH 1", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # end test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg_names[i2], "(data)"))) %in% base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    # end for mode
    # for length
    # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg_names[i2], "(data)"))) %in% base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    result <- arg_check(data = vec7, length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE LENGTH 1", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, mode = "character", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE MODE character AND LENGTH 1", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, mode = "numeric", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE LENGTH 1", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, length = 5, options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # end test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg_names[i2], "(data)"))) %in% base::get(arg_names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    # end for length
    # end     if(base::is.null(options)){ # work on the 4 "class", "typeof", "mode", "length" arguments

    # if(prop == TRUE & base::all(base::typeof(data) == "double")){
    result <- arg_check(data = vec7, prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, length = 1, prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE LENGTH 1 AND THE vec7 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object_name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, prop = TRUE, length = 1)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE LENGTH 1 AND THE vec1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    # end if(prop == TRUE & base::all(base::typeof(data) == "double")){
    # if(base::all(base::class(data) %in% "expression")){
    result <- arg_check(data = expr1, prop = TRUE, length = 2)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE expr1 ARGUMENT MUST BE LENGTH 2 AND THE expr1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object_name = "expr1")
    testthat::expect_equal(result, expect)
    # end if(base::all(base::class(data) %in% "expression")){
    # if(na_contain == FALSE & (base::mode(data) %in% base::c("logical", "numeric", "complex", "character", "list"))){
    result <- arg_check(data = mat2, mode = "numeric", na_contain = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat2 ARGUMENT CONTAINS NA WHILE NOT AUTHORIZED.", object_name = "mat2")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = mat2, mode = "numeric", length = 1, na_contain = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat2 ARGUMENT MUST BE LENGTH 1 AND THE mat2 ARGUMENT CONTAINS NA WHILE NOT AUTHORIZED.", object_name = "mat2")
    testthat::expect_equal(result, expect)
    # end if(na_contain == FALSE & (base::mode(data) %in% base::c("logical", "numeric", "complex", "character", "list"))){
    # if(neg_values == FALSE & base::all(base::mode(data) %in% "numeric") & ! base::any(base::class(data) %in% "factor")){
    result <- arg_check(data = vec1, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", length = 1, neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE LENGTH 1 AND THE vec1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES.", object_name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE factor1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR", object_name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, mode = "numeric", length = 1, neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE factor1 ARGUMENT MUST BE LENGTH 1 AND THE factor1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR", object_name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND THE vec5 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS NOT EVEN NUMERIC.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", length = 1, neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND LENGTH 1 AND THE vec5 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS NOT EVEN NUMERIC.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", class = "vector", typeof = "integer", length = 1, neg_values = FALSE) # class = "vector" is not a shorthand anymore: class(vec5) is "character"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE CLASS vector AND TYPEOF integer AND MODE numeric AND LENGTH 1 AND THE vec5 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS NOT EVEN NUMERIC.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    # end if(neg_values == FALSE & base::all(base::mode(data) %in% "numeric") & ! base::any(base::class(data) %in% "factor")){
    # if(inf_values == FALSE & base::all(base::typeof(data) %in% "double") & ! base::any(base::class(data) %in% "factor")){
    result <- arg_check(data = vec8, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec8 ARGUMENT MUST BE MADE OF NON INFINITE VALUES.", object_name = "vec8")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec8, mode = "numeric", length = 1, inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec8 ARGUMENT MUST BE LENGTH 1 AND THE vec8 ARGUMENT MUST BE MADE OF NON INFINITE VALUES.", object_name = "vec8")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object_name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE factor1 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS A FACTOR", object_name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, mode = "numeric", length = 1, inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE factor1 ARGUMENT MUST BE LENGTH 1 AND THE factor1 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS A FACTOR", object_name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND THE vec5 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN MODE NUMERIC OR TYPE DOUBLE.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", length = 1, inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND LENGTH 1 AND THE vec5 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN MODE NUMERIC OR TYPE DOUBLE.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, class = "vector", mode = "numeric", typeof = "double", length = 1, inf_values = FALSE) # class = "vector" is not a shorthand anymore: class(vec5) is "character"
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE CLASS vector AND TYPEOF double AND MODE numeric AND LENGTH 1 AND THE vec5 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN MODE NUMERIC OR TYPE DOUBLE.", object_name = "vec5")
    testthat::expect_equal(result, expect)
    # end if(inf_values == FALSE & base::all(base::typeof(data) %in% "double") & ! base::any(base::class(data) %in% "factor")){
    # if(print == TRUE & problem == TRUE){
    result <- arg_check(data = vec8, mode = "character", print = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec8 ARGUMENT MUST BE MODE character", object_name = "vec8")
    testthat::expect_equal(result, expect)
    # end if(print == TRUE & problem == TRUE){
    # if( ! base::is.null(x = base::get(x = arg_names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE))){ # exact matching of class(data) now
    testthat::expect_no_error(arg_check(data = arr1, class = "array"))
    result <- arg_check(data = mat1, class = "array", print = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS array", object_name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = mat1, class = "matrix", print = TRUE) # since R 4.0.0, class(mat1) is c("matrix", "array"), so class = "matrix" alone returns a problem. This is the fix of https://github.com/safer-r/saferDev/issues/100
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS matrix", object_name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = arr1, class = "matrix", print = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE arr1 ARGUMENT MUST BE CLASS matrix", object_name = "arr1")
    testthat::expect_equal(result, expect)

    result <- arg_check(data = ord_factor1, class = "factor", print = TRUE) # class(ord_factor1) is c("ordered", "factor"), so class = "factor" alone returns a problem
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE ord_factor1 ARGUMENT MUST BE CLASS factor", object_name = "ord_factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = ord_factor1, class = "vector", print = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE ord_factor1 ARGUMENT MUST BE CLASS vector", object_name = "ord_factor1")
    testthat::expect_equal(result, expect)

    # valid exact-match forms of issue #100: the full class vector, in order
    result <- arg_check(data = mat1, class = c("matrix", "array")) # class(mat1) is c("matrix", "array"): exact match, valid (no print = TRUE because print only acts when a problem is detected)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE mat1 ARGUMENT.", object_name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = ord_factor1, class = c("ordered", "factor")) # class(ord_factor1) is c("ordered", "factor"): exact match, valid
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE ord_factor1 ARGUMENT.", object_name = "ord_factor1")
    testthat::expect_equal(result, expect)

    # end if( ! base::is.null(x = base::get(x = arg_names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE))){
                


    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests

    #### sophiticated examples
    result17 <- saferDev::get_message("arg_check(class = 'list', safer_check = FALSE)", kind = "error", print_no = TRUE, text = NULL) 
    expect17 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nFOLLOWING ARGUMENT HAS NO DEFAULT VALUE AND REQUIRE ONE:\ndata\n\n================\n\n\n"
    testthat::expect_equal(result17, expect17)

    result18 <- saferDev::get_message("arg_check(data = vec6, class = NA, typeof = 'integer', safer_check = FALSE)", kind = "error", print_no = TRUE, text = NULL)
    expect18 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHIS ARGUMENT CANNOT BE MADE OF NA ONLY:\nclass\n\n================\n\n\n"
    testthat::expect_equal(result18, expect18)

    result19 <- saferDev::get_message("arg_check(data = vec6, class = 'list', typeof = 'integer', prop = NULL, safer_check = FALSE)", kind = "error", print_no = TRUE, text = NULL)
    expect19 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHIS ARGUMENT CANNOT BE NULL:\nprop\n\n================\n\n\n"
    testthat::expect_equal(result19, expect19)
    testthat::expect_error(arg_check(data = vec6, class = "list", typeof = "integer", prop = 'FALSE', safer_check = FALSE))
    result20 <- saferDev::get_message("arg_check(data = vec6, class = list, typeof = 'integer', prop = FALSE, double_as_integer_allowed = FALSE, safer_check = FALSE)", kind = "error", print_no = TRUE, text = NULL)
    expect20 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHIS ARGUMENT MUST BE MADE OF CHARACTER STRINGS:\nclass\n\n================\n\n\n"
    testthat::expect_equal(result20, expect20)
    result21 <- saferDev::get_message("arg_check(data = vec6, class = 'list', typeof = 'integer', prop = FALSE, data_name = base::c('first', 'second'), safer_check = FALSE)", kind = "error", print_no = TRUE, text = NULL)
    expect21 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHE data_name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT:\nfirst\nsecond\n\n================\n\n\n"
    testthat::expect_equal(result21, expect21)
    result22 <- saferDev::get_message("arg_check(data = vec6)", kind = "error", print_no = TRUE, text = NULL)
    expect22 <-  "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nAT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop).\n\n================\n\n\n"
    testthat::expect_equal(result22, expect22)
    result23 <- saferDev::get_message("arg_check(data = vec5, class = 'character', options = base::c('a', 'b', 'c'), all_options_in_data = TRUE)", kind = "error", print_no = TRUE, text = NULL)
    expect23 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED.\n\n================\n\n\n"
    testthat::expect_equal(result23, expect23)
    result24 <- saferDev::get_message("arg_check(data = base::list(x = 'a', y = '2'), length = 2, options = NULL, prop = FALSE, all_options_in_data = FALSE, neg_values = FALSE, safer_check = FALSE)", kind = "error", print_no = TRUE, text = NULL)
    expect24 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF typeof AND mode ARGUMENTS ARE NULL.\n\n================\n\n\n"
    testthat::expect_equal(result24, expect24)
    result25 <- saferDev::get_message("arg_check(data = base::list(x = 'a', y = '2'), length = 2, options = NULL, prop = FALSE, all_options_in_data = FALSE, inf_values = FALSE, safer_check = FALSE)", kind = "error", print_no = TRUE, text = NULL)
    expect25 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF typeof AND mode ARGUMENTS ARE NULL.\n\n================\n\n\n"
    testthat::expect_equal(result25, expect25)
    result26 <- arg_check(
        data = vec1, 
        class = "integer", 
        typeof = "integer", 
        mode = NULL, 
        length = NULL, 
        prop = FALSE, 
        double_as_integer_allowed = FALSE, 
        options = NULL, 
        all_options_in_data = FALSE, 
        na_contain = FALSE, 
        neg_values = TRUE, 
        inf_values = TRUE, 
        print = FALSE, 
        data_name = NULL, 
        data_arg = FALSE, 
        lib_path = NULL,
        safer_check = TRUE, 
        error_text = " IN P1::F1"
    )
    expect26 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 OBJECT IN P1::F1", object_name = "vec1")
    testthat::expect_equal(result26, expect26)

    #### end sophiticated examples

    ## end other tests

})

