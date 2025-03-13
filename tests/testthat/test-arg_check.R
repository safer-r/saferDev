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
    number <- 1
    factor1 <- base::as.factor(vec5)
    expr1 <- expression(1)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(arg_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric")) # to test that this example works
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
    result <- arg_check(data = NULL, class = "numeric")
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE NULL ARGUMENT MUST BE CLASS numeric", object.name = "NULL")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, class = NULL))
    result <- arg_check(data = vec1, class = "numeric")
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", typeof = NULL))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", mode = NULL))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", length = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", prop = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = NULL))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", options = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", all_options_in_data = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", na_contain = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", print = NULL))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", data_name = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_arg = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = NULL))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = NULL))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_no_error(arg_check(data = character(), class = "numeric"))
    testthat::expect_no_error(arg_check(data = integer(), class = "numeric"))
    testthat::expect_no_error(arg_check(data = double(), class = "numeric"))
    testthat::expect_no_error(arg_check(data = logical(), class = "numeric"))
    testthat::expect_no_error(arg_check(data = complex(), class = "numeric"))
    testthat::expect_no_error(arg_check(data = data.frame(), class = "numeric"))
    testthat::expect_no_error(arg_check(data = list(), class = "numeric"))

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
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", safer_check = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", safer_check = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    ######## end safer_check argument

    ######## lib_path argument
    # safer_check must be TRUE
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = "PATH_NOT_GOOD"))
    result <- arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = base::.libPaths())
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # ini_lib_path <- base:::.libPaths(new = , include.site = TRUE)
    # testthat::expect_no_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = ".")) # lib_path = "." with safer_check = TRUE returns an error
    # testthat::expect_equal(ini_lib_path, base:::.libPaths(new = , include.site = TRUE)) # .libPaths must not be changed by lib_path = "."
    ######## end lib_path argument

    ######## check of the required functions from the required packages
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE)) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE)) # see above for the result comparison
    ######## end critical operator checking

    ######## management of "" in arguments of mode character
    testthat::expect_error(arg_check(data = vec1, class = 1))
    testthat::expect_error(arg_check(data = vec1, typeof = 1))
    testthat::expect_error(arg_check(data = vec1, mode = 1))
    testthat::expect_error(arg_check(data = vec1, data_name = 1))
    testthat::expect_error(arg_check(data = vec1, safer_check = TRUE, lib_path = 1))

    testthat::expect_no_error(arg_check(data = "", class = "numeric"))
    testthat::expect_error(arg_check(data = vec1, class = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", typeof = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", mode = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", length = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", prop = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", options = ""))
    testthat::expect_no_error(arg_check(data = vec1, options = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", all_options_in_data = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", na_contain = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", print = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_name = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = ""))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", error_text = ""))
    ######## end management of "" in arguments of mode character

    ######## other checkings
    # management of special classes
    testthat::expect_error(arg_check(data = vec1, class = mat3, typeof = "integer"))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", typeof = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", mode = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", length = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", prop = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", options = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", all_options_in_data = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", na_contain = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", print = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_name = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_arg = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE, lib_path = mat3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = mat3))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", error_text = mat3))
    # end management of special classes
    # management of the logical arguments
    # prop tested below
    # double_as_integer_allowed
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", double_as_integer_allowed = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end double_as_integer_allowed
    # all_options_in_data
    testthat::expect_error(arg_check(data = vec1, class = "numeric", all_options_in_data = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", all_options_in_data = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", all_options_in_data = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", all_options_in_data = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", all_options_in_data = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", all_options_in_data = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end all_options_in_data
    # na_contain
    testthat::expect_error(arg_check(data = vec1, class = "numeric", na_contain = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", na_contain = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", na_contain = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", na_contain = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", na_contain = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", na_contain = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end na_contain
    # neg_values
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", neg_values = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric AND THE vec1 ARGUMENT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end neg_values
    # inf_values
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", inf_values = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric AND THE vec1 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN TYPE DOUBLE.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end inf_values
    # print
    testthat::expect_error(arg_check(data = vec1, class = "numeric", print = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", print = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", print = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", print = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", print = TRUE) # the error message is printed but it is not an arror, stopping the execution
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", print = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end print
    # data_arg
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_arg = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_arg = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_arg = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_arg = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", data_arg = TRUE) # the error message is printed but it is not an arror, stopping the execution
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", data_arg = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end data_arg
    # end management of the logical arguments
    # other checkings of the arguments by order
    # THE data_name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_name = c("D1", "D2")))
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
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE SOME OF THESE OPTIONS:\na\nTHE PROBLEMATIC ELEMENTS OF vec1 ARE:\n-1\n0\n1\n2\n3", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, options = NULL, class = "vector", typeof = "double", mode = "numeric", prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE TYPEOF double AND THE vec1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED.
    # THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    testthat::expect_error(arg_check(data = vec1, class = NULL, typeof = NULL, mode = NULL, neg_values = FALSE, prop = TRUE))
    # end THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    # THE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    testthat::expect_error(arg_check(data = vec1, class = NULL, typeof = NULL, mode = NULL, inf_values = FALSE, prop = TRUE))
    # end THE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    #  ! base::is.null(class)
    testthat::expect_error(arg_check(data = vec1, class = NULL))
    testthat::expect_error(arg_check(data = vec1, class = NA))
    testthat::expect_error(arg_check(data = vec1, class = 1))
    testthat::expect_error(arg_check(data = vec1, class = "NOTGOOD"))
    testthat::expect_error(arg_check(data = vec1, class = c("character", "list")))
    testthat::expect_error(arg_check(data = vec1, class = "character", neg_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, class = "character", neg_values = TRUE)) # neg_values is inactivated
    testthat::expect_error(arg_check(data = vec1, class = "character", inf_values = FALSE))
    testthat::expect_error(arg_check(data = vec1, class = "integer", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, class = "character", inf_values = TRUE)) # inf_values is inactivated
    # end  ! base::is.null(class)
    #  ! base::is.null(typeof)
    testthat::expect_error(arg_check(data = vec1, typeof = NULL))
    testthat::expect_error(arg_check(data = vec1, typeof = NA))
    testthat::expect_error(arg_check(data = vec1, typeof = 1))
    testthat::expect_error(arg_check(data = vec1, typeof = "NOTGOOD"))
    testthat::expect_error(arg_check(data = vec1, typeof = c("character", "list")))
    testthat::expect_error(arg_check(data = vec1, typeof = "character", neg_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "double", neg_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "character", neg_values = TRUE)) # neg_values is inactivated
    testthat::expect_error(arg_check(data = vec1, typeof = "character", inf_values = FALSE))
    testthat::expect_error(arg_check(data = vec1, typeof = "integer", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "double", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "character", inf_values = TRUE)) # inf_values is inactivated
    # end  ! base::is.null(typeof)
    #  ! base::is.null(mode)
    testthat::expect_error(arg_check(data = vec1, mode = NULL))
    testthat::expect_error(arg_check(data = vec1, mode = NA))
    testthat::expect_error(arg_check(data = vec1, mode = 1))
    testthat::expect_error(arg_check(data = vec1, mode = "NOTGOOD"))
    testthat::expect_error(arg_check(data = vec1, mode = c("character", "list")))
    testthat::expect_error(arg_check(data = vec1, mode = "character", neg_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", neg_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "character", neg_values = TRUE)) # neg_values is inactivated
    testthat::expect_error(arg_check(data = vec1, mode = "character", inf_values = FALSE))
    testthat::expect_error(arg_check(data = vec1, mode = "integer", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", inf_values = FALSE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "character", inf_values = TRUE)) # inf_values is inactivated
    # end  ! base::is.null(mode)
    #  ! base::is.null(length)
    testthat::expect_error(arg_check(data = vec1, length = NULL))
    testthat::expect_error(arg_check(data = vec1, length = NA))
    result <- arg_check(data = vec1, length = 1)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE LENGTH 1", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", length = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", length = "a"))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", length = 1.1))
    testthat::expect_error(arg_check(data = vec1, mode = "numeric", length = 1:2))
    testthat::expect_no_error(arg_check(data = vec1, mode = "numeric", length = 1))
    #  end ! base::is.null(length)
    #  prop
    testthat::expect_error(arg_check(data = vec1, class = "numeric", prop = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", prop = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", prop = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", prop = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE CLASS numeric AND THE vec1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, class = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec1, class = "character", prop = FALSE))
    testthat::expect_error(arg_check(data = vec1, mode = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "character", prop = FALSE))
    testthat::expect_error(arg_check(data = vec1, typeof = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "character", prop = FALSE))
    #  end prop
    # other checkings of the arguments by order
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # if(base::is.null(data_name)){
    testthat::expect_no_error(arg_check(data = vec1, class = "integer", data_name = NULL))
    # end if(base::is.null(data_name)){
    # if(( ! base::is.null(options)) & (base::all(base::typeof(data) == "character") | base::all(base::typeof(data) == "integer") | base::all(base::typeof(data) == "double"))){
    testthat::expect_no_error(arg_check(data = vec5, options = "a")) # data of typeof "character"
    testthat::expect_no_error(arg_check(data = vec1, options = "a")) # data of typeof "integer"
    result <- arg_check(data = vec7, options = "a") # data of typeof "double". Error message output expected because only integers or characters allowed for options.
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE SOME OF THESE OPTIONS:\na\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER.", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = "a")
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\na\nTHE PROBLEMATIC ELEMENTS OF vec3 ARE:\n1\n2\n3", object.name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = "a", all_options_in_data = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\na\nTHE PROBLEMATIC ELEMENTS OF vec3 ARE:\n1\n2\n3\nERROR\nTHE vec3 ARGUMENT MUST BE MADE OF ALL THESE OPTIONS:\na\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE:\na", object.name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = "a", length = 2)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec3 ARGUMENT MUST BE SOME OF THESE OPTIONS:\na\nTHE PROBLEMATIC ELEMENTS OF vec3 ARE:\n1\n2\n3\nERROR\nTHE LENGTH OF vec3 MUST BE 2 AND NOT 3", object.name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, options = 1:3)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object.name = "vec3")
    testthat::expect_equal(result, expect)
    # end if(( ! base::is.null(options)) & (base::all(base::typeof(data) == "character") | base::all(base::typeof(data) == "integer") | base::all(base::typeof(data) == "double"))){
    #     }else if( ! base::is.null(options)){
    result <- arg_check(data = vec6, options = "a") # data of typeof "list". Error message output expected because only integers or characters allowed for options.
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec6 ARGUMENT MUST BE SOME OF THESE OPTIONS:\na\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER.", object.name = "vec6")
    testthat::expect_equal(result, expect)
    # end     }else if( ! base::is.null(options)){
    # if( ! base::is.null(class)){
    testthat::expect_no_error(arg_check(data = mat1, class = "matrix"))
    testthat::expect_no_error(arg_check(data = factor1, class = "factor"))
    # end if( ! base::is.null(class)){
    #     if(base::is.null(options)){ # work on the 4 "class", "typeof", "mode", "length" arguments
    # for class
    # data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") but finally, data is double but not made of integers # if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg.names[i2] == "typeof" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){
    result <- arg_check(data = vec7, class = "integer", options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE CLASS integer", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, class = "integer", length = 1, options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE CLASS integer AND LENGTH 1", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, class = "integer", options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object.name = "vec3")
    testthat::expect_equal(result, expect)
    # end data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") but finally, data is double but not made of integers # if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg.names[i2] == "typeof" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){
    # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    result <- arg_check(data = vec7, class = "integer", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE CLASS integer", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, class = "integer", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE CLASS integer AND LENGTH 1", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "integer", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    # test of class == "vector" # }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "vector") & ! (base::all(base::class(data) %in% "numeric") | base::all(base::class(data) %in% "integer") | base::all(base::class(data) %in% "character") | base::all(base::class(data) %in% "logical") | base::all(base::class(data) %in% "complex") | base::all(base::class(data) %in% "expression"))){ # test class == "vector". base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) check if user has used the argument class = "vector". If TRUE and base::length(data) > 1, the class "numeric" "integer" "character" "logical" "complex" "expression" should be returned. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names. Other classes "list", "name", "symbol", "function", "environment", "S4", "call" return a list if length of data > 1
    result <- arg_check(data = mat1, class = "vector", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS vector", object.name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = mat1, class = "vector", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS vector AND LENGTH 1", object.name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "vector", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end test of class == "vector" # }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "vector") & ! (base::all(base::class(data) %in% "numeric") | base::all(base::class(data) %in% "integer") | base::all(base::class(data) %in% "character") | base::all(base::class(data) %in% "logical") | base::all(base::class(data) %in% "complex") | base::all(base::class(data) %in% "expression"))){ # test class == "vector". base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) check if user has used the argument class = "vector". If TRUE and base::length(data) > 1, the class "numeric" "integer" "character" "logical" "complex" "expression" should be returned. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names. Other classes "list", "name", "symbol", "function", "environment", "S4", "call" return a list if length of data > 1
    # test of class == "ggplot2" # }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "ggplot2") & ! base::all(base::class(data) %in% base::c("gg", "ggplot"))){
    result <- arg_check(data = mat1, class = "ggplot2", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat1 ARGUMENT MUST BE CLASS ggplot2", object.name = "mat1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = ggplot2::ggplot(), class = "ggplot2", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE ggplot2::ggplot() ARGUMENT.", object.name = "ggplot2::ggplot()")
    testthat::expect_equal(result, expect)
    # end test of class == "ggplot2" # end }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "ggplot2") & ! base::all(base::class(data) %in% base::c("gg", "ggplot"))){
    # end for class
    # for typeof
    # data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") but finally, data is double but not made of integers # if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg.names[i2] == "typeof" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){
    result <- arg_check(data = vec7, typeof = "integer", options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, typeof = "integer", length = 1, options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer AND LENGTH 1", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, typeof = "integer", options = NULL, double_as_integer_allowed = TRUE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object.name = "vec3")
    testthat::expect_equal(result, expect)
    # end data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") but finally, data is double but not made of integers # if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg.names[i2] == "typeof" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){
    # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    result <- arg_check(data = vec7, typeof = "integer", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, typeof = "integer", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE TYPEOF integer AND LENGTH 1", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, typeof = "integer", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    # end for typeof
    # for mode
    # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    result <- arg_check(data = vec7, mode = "character", options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE MODE character", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, mode = "character", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE MODE character AND LENGTH 1", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    # end for mode
    # for length
    # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    result <- arg_check(data = vec7, length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE LENGTH 1", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, mode = "character", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE MODE character AND LENGTH 1", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, mode = "numeric", length = 1, options = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE LENGTH 1", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, length = 5, options = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 ARGUMENT.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function # }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
    # end for length
    # end     if(base::is.null(options)){ # work on the 4 "class", "typeof", "mode", "length" arguments

    # if(prop == TRUE & base::all(base::typeof(data) == "double")){
    result <- arg_check(data = vec7, prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec7, length = 1, prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec7 ARGUMENT MUST BE LENGTH 1 AND THE vec7 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "vec7")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, prop = TRUE, length = 1)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE LENGTH 1 AND THE vec1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end if(prop == TRUE & base::all(base::typeof(data) == "double")){
    # if(base::all(base::class(data) %in% "expression")){
    result <- arg_check(data = expr1, prop = TRUE, length = 2)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE expr1 ARGUMENT MUST BE LENGTH 2 AND THE expr1 ARGUMENT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "expr1")
    testthat::expect_equal(result, expect)
    # end if(base::all(base::class(data) %in% "expression")){
    # if(na_contain == FALSE & (base::mode(data) %in% base::c("logical", "numeric", "complex", "character", "list"))){
    result <- arg_check(data = mat2, mode = "numeric", na_contain = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat2 ARGUMENT CONTAINS NA WHILE NOT AUTHORIZED.", object.name = "mat2")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = mat2, mode = "numeric", length = 1, na_contain = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE mat2 ARGUMENT MUST BE LENGTH 1 AND THE mat2 ARGUMENT CONTAINS NA WHILE NOT AUTHORIZED.", object.name = "mat2")
    testthat::expect_equal(result, expect)
    # end if(na_contain == FALSE & (base::mode(data) %in% base::c("logical", "numeric", "complex", "character", "list"))){
    # if(neg_values == FALSE & base::all(base::mode(data) %in% "numeric") & ! base::any(base::class(data) %in% "factor")){
    result <- arg_check(data = vec1, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", length = 1, neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 ARGUMENT MUST BE LENGTH 1 AND THE vec1 ARGUMENT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object.name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE factor1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR", object.name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, mode = "numeric", length = 1, neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE factor1 ARGUMENT MUST BE LENGTH 1 AND THE factor1 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR", object.name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND THE vec5 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS NOT EVEN MODE NUMERIC.", object.name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", length = 1, neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND LENGTH 1 AND THE vec5 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS NOT EVEN MODE NUMERIC.", object.name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", class = "integer", typeof = "integer", length = 1, neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE CLASS integer AND TYPEOF integer AND MODE numeric AND LENGTH 1 AND THE vec5 ARGUMENT MUST BE MADE OF NON NEGATIVE VALUES BUT IS NOT EVEN MODE NUMERIC.", object.name = "vec5")
    testthat::expect_equal(result, expect)
    # end if(neg_values == FALSE & base::all(base::mode(data) %in% "numeric") & ! base::any(base::class(data) %in% "factor")){
    # if(inf_values == FALSE & base::all(base::typeof(data) %in% "double") & ! base::any(base::class(data) %in% "factor")){
    result <- arg_check(data = vec8, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec8 ARGUMENT MUST BE MADE OF NON INFINITE NUMERIC VALUES.", object.name = "vec8")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec8, mode = "numeric", length = 1, inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec8 ARGUMENT MUST BE LENGTH 1 AND THE vec8 ARGUMENT MUST BE MADE OF NON INFINITE NUMERIC VALUES.", object.name = "vec8")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec3, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 ARGUMENT.", object.name = "vec3")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE factor1 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS A FACTOR", object.name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = factor1, mode = "numeric", length = 1, inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE factor1 ARGUMENT MUST BE LENGTH 1 AND THE factor1 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS A FACTOR", object.name = "factor1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND THE vec5 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN TYPE DOUBLE.", object.name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", length = 1, inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE MODE numeric AND LENGTH 1 AND THE vec5 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN TYPE DOUBLE.", object.name = "vec5")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec5, mode = "numeric", class = "numeric", typeof = "double", length = 1, inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec5 ARGUMENT MUST BE CLASS numeric AND TYPEOF double AND MODE numeric AND LENGTH 1 AND THE vec5 ARGUMENT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN TYPE DOUBLE.", object.name = "vec5")
    testthat::expect_equal(result, expect)
    # end if(inf_values == FALSE & base::all(base::typeof(data) %in% "double") & ! base::any(base::class(data) %in% "factor")){
    # if(print == TRUE & problem == TRUE){
    result <- arg_check(data = vec8, mode = "character", print = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec8 ARGUMENT MUST BE MODE character", object.name = "vec8")
    testthat::expect_equal(result, expect)
    # end if(print == TRUE & problem == TRUE){

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
    expect24 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.\n\n================\n\n\n"
    testthat::expect_equal(result24, expect24)
    result25 <- saferDev::get_message("arg_check(data = base::list(x = 'a', y = '2'), length = 2, options = NULL, prop = FALSE, all_options_in_data = FALSE, inf_values = FALSE, safer_check = FALSE)", kind = "error", print_no = TRUE, text = NULL)
    expect25 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev::arg_check().\n\nTHE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.\n\n================\n\n\n"
    testthat::expect_equal(result25, expect25)
    result26 <- arg_check(
        data = vec1, 
        class = "integer", 
        typeof = NULL, 
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
    expect26 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 OBJECT IN P1::F1", object.name = "vec1")
    testthat::expect_equal(result26, expect26)

    #### end sophiticated examples

    ## end other tests

})

