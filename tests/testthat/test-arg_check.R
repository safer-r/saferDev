testthat::test_that("arg_check()", {

    #### data argument values
    vec1 <- -1:3 # vector of integers
    vec3 <- base::c(1, 2, 3)
    vec4 <- "pearson"
    vec5 <- base::c("a", "b","a", "b")
    vec6 <- base::list(1:3, 4:6)
    mat1 <- base::matrix(vec1)
    mat2 <- base::matrix(base::c(1:3 / 3, NA))
    number <- 1
    #### end data argument values

    #### tests (ordered by arg appearance and conditions in the code) 

    ########  argument with no default values
    testthat::expect_error(arg_check())
    ########  end argument with no default values

    ######## management of NULL arguments
    testthat::expect_error(arg_check(data = NULL))
    result <- arg_check(data = NULL, class = "numeric")
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE NULL OBJECT MUST BE CLASS numeric", object.name = "NULL")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, class = NULL))
    result <- arg_check(data = vec1, class = "numeric")
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
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
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", lib_path = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = NULL))
    ######## end management of NULL arguments

    ######## management of NA arguments
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
    testthat::expect_error(arg_check(data = vec1, lib_path = NA))
    testthat::expect_error(arg_check(data = vec1, safer_check = NA))
    testthat::expect_error(arg_check(data = vec1, error_text = NA))
    ######## end management of NA arguments

    ######## lib_path argument
    testthat::expect_error(arg_check(data = vec1, class = "numeric", lib_path = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", lib_path = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", lib_path = "PATH_NOT_GOOD"))
    result <- arg_check(data = vec1, class = "numeric", lib_path = NULL)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", lib_path = base::.libPaths())
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    ######## end lib_path argument

    ######## safer_check argument
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", safer_check = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", safer_check = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    ######## end safer_check argument

    ######## check of the required functions from the required packages
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE)) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", safer_check = TRUE)) # see above for the result comparison
    ######## end critical operator checking

    ######## management of "" in arguments of mode character
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
    testthat::expect_error(arg_check(data = vec1, class = "numeric", lib_path = ""))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = ""))
    testthat::expect_no_error(arg_check(data = vec1, class = "numeric", error_text = ""))
    ######## end management of "" in arguments of mode character

    ######## other checkings
    # management of special classes
    testthat::expect_error(arg_check(data = vec1, class = mat1, typeof = "integer"))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", typeof = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", mode = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", length = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", prop = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", options = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", all_options_in_data = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", na_contain = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", print = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", data_name = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", lib_path = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = mat1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = mat1))
    # end management of special classes
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
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE SOME OF THESE OPTIONS:\na\nTHE PROBLEMATIC ELEMENTS OF vec1 ARE:\n-1\n0\n1\n2\n3", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, options = NULL, class = "vector", typeof = "double", mode = "numeric", prop = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE TYPEOF double AND THE vec1 OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED.
    # THE neg_values ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", neg_values = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", neg_values = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", neg_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric AND THE vec1 OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end THE neg_values ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).
    # THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    testthat::expect_error(arg_check(data = vec1, class = NULL, typeof = NULL, mode = NULL, neg_values = FALSE, prop = TRUE))
    # end THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.
    # THE inf_values ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", inf_values = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", inf_values = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", inf_values = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric AND THE vec1 OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN TYPE DOUBLE."
, object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end THE inf_values ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).
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
    testthat::expect_error(arg_check(data = vec1, typeof = "double", neg_values = FALSE))
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
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE LENGTH 1", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, mode = "numeric", length = NULL)
    expect <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 OBJECT.", object.name = "vec1")
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
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric AND THE vec1 OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", object.name = "vec1")
    testthat::expect_equal(result, expect)
    testthat::expect_error(arg_check(data = vec1, class = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec1, class = "character", prop = FALSE))
    testthat::expect_error(arg_check(data = vec1, mode = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec1, mode = "character", prop = FALSE))
    testthat::expect_error(arg_check(data = vec1, typeof = "character", prop = TRUE))
    testthat::expect_no_error(arg_check(data = vec1, typeof = "character", prop = FALSE))
    #  end prop
    # double_as_integer_allowed
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = 1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", double_as_integer_allowed = c(TRUE, FALSE)))
    result <- arg_check(data = vec1, class = "numeric", double_as_integer_allowed = TRUE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", double_as_integer_allowed = FALSE)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric"
, object.name = "vec1")
    testthat::expect_equal(result, expect)
    # end double_as_integer_allowed

    ######## end other checkings









    ######## error_text argument
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = NULL))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = NA))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = 1)) # ERROR IN saferDev::arg_check()1 displayed which is expected, and will be solved with error_text correction
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = c("ERROR1", "ERROR2"))) # ERROR IN saferDev::arg_check()ERROR1ERROR2 displayed which is expected, and will be solved with error_text correction
    result <- arg_check(data = vec1, class = "numeric", error_text = " IN P1::F1.")
    expect <- list(problem = TRUE, text = "ERROR IN P1::F1.\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result,expect)
    ######## end error_text argument



    #### end tests (ordered by arg appearance and conditions in the code) 


    # Test cases
    result1 <- arg_check(data = vec1, class = "numeric")
    expect1 <- base::list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result1,expect1)
    
    
    result2 <- arg_check(data = vec1, class = "vector", typeof = "integer", length = 3, neg_values = FALSE, na_contain = FALSE)
    expect2 <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE LENGTH 3 AND THE vec1 OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES", object.name = "vec1")
    testthat::expect_equal(result2,expect2)
    

    result3 <- arg_check(data = NULL, class = "integer")
    expect3 <- base::list(problem = TRUE, text = "ERROR\n\nTHE NULL OBJECT MUST BE CLASS integer", object.name = "NULL")
    testthat::expect_equal(result3,expect3)
    

    result4 <- arg_check(data = vec1, 
    typeof = "integer", 
    mode = "character", # the mode "character" exists but is inconsistant with typeof "integer". However, this aspect is not signaled by the function
)
    expect4 <- base::list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE MODE character", object.name = "vec1")
    testthat::expect_equal(result4,expect4)
    
    
    testthat::expect_no_error(object = arg_check(
        data = vec1, 
        mode = "numeric", # the mode "numeric" exists and is consistent with typeof "integer"
        ))

    testthat::expect_no_error(object = arg_check(
        data = vec1, 
        mode = "numeric", 
        typeof = "integer", # the mode "numeric" exists and is consistent with typeof "integer"
        ))

    testthat::expect_no_error(object = arg_check(
        data = vec1, 
        class = "integer",
        ))

    testthat::expect_error(object = arg_check(
        data = vec1, 
        mode = "integer", # the mode "integer" does not exist in the mode() function of R
        ), regexp = NULL)
    

    result6 <- arg_check(data = mat2, 
    prop = TRUE # Check for values between 0 and 1 only
)
    expect6 <- base::list(problem = TRUE, text = "ERROR\n\nTHE mat2 OBJECT CONTAINS NA WHILE NOT AUTHORIZED", object.name = "mat2")
    testthat::expect_equal(result6,expect6)
    

    result7 <- arg_check(data = vec3, typeof = "integer",
    double_as_integer_allowed = TRUE # with TRUE, integers stored as double are accepted
)
    expect7 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 OBJECT", object.name = "vec3")
    testthat::expect_equal(result7,expect7)
    

    result8 <- arg_check(data = vec4, 
    options = base::c("pearson", "spearman", "kendall")
)
    expect8 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec4 OBJECT", object.name = "vec4")
    testthat::expect_equal(result8,expect8)
    

    result9 <- arg_check(data = vec5,
    options = base::c("a", "b", "c"), 
    all_options_in_data = TRUE
)
    expect9 <- base::list(problem = TRUE, text = "ERROR\n\nTHE vec5 OBJECT MUST BE MADE OF ALL THESE OPTIONS: a b c\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: c", object.name = "vec5")
    testthat::expect_equal(result9,expect9)
    

    result10 <- arg_check(data = mat2, class = "matrix", prop = TRUE,
    na_contain = FALSE # with TRUE, integers stored as double are accepted
)
    expect10 <- base::list(problem = TRUE, text = "ERROR\n\nTHE mat2 OBJECT CONTAINS NA WHILE NOT AUTHORIZED", object.name = "mat2")
    testthat::expect_equal(result10,expect10)
    

    result11 <- arg_check(data = mat1, class = "matrix",
    neg_values = FALSE # with TRUE, integers stored as double are accepted
)
    expect11 <- base::list(problem = TRUE, text = "ERROR\n\nTHE mat1 OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES", object.name = "mat1")
    testthat::expect_equal(result11,expect11)
    

    result12 <- arg_check(data = mat1, class = "matrix",
    inf_values = FALSE
)
    expect12 <- base::list(problem = TRUE, text = "ERROR\n\nTHE mat1 OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN TYPE DOUBLE", object.name = "mat1")
    testthat::expect_equal(result12,expect12)

    result13 <- arg_check(data = vec6, class = "list"
    # vec6 is a list of two vectors of integers
)
    expect13 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec6 OBJECT", object.name = "vec6")
    testthat::expect_equal(result13,expect13)

    result14 <- arg_check(data = vec6, class = "list", typeof = "integer" # vec6 is a list of two vectors of integers
)
    expect14 <- base::list(problem = TRUE, text = "ERROR\n\nTHE vec6 OBJECT MUST BE TYPEOF integer", object.name = "vec6")
    testthat::expect_equal(result14,expect14)

    result15 <- arg_check(data = vec6, class = "list", neg_values = TRUE, # vec6 is a list of two vectors of integers
)
    expect15 <- base::list(problem = TRUE, text = "NO PROBLEM DETECTED FOR THE vec6 OBJECT", object.name = "vec6")


    testthat::expect_error(arg_check(data = vec6, class = "list", prop = TRUE, # vec6 is a list of two vectors of integers
), regexp = NULL)

    testthat::expect_no_error(arg_check(data = vec6, class = "list", mode = "list", prop = FALSE, neg_values = TRUE, inf_values = TRUE, # vec6 is a list of two vectors of integers, no check is performed for the presence of negative values, infinite values are allowed
))
    testthat::expect_no_error(arg_check(data = vec6, class = "list", na_contain = TRUE, # vec6 is a list of two vectors of integers, it could contain NA
))

    result16 <- arg_check(data = vec6, class = "list",  length = 2, # vec6 is a list of two vectors of integers
)
    expect16 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec6 OBJECT", object.name = "vec6")
    testthat::expect_equal(result16, expect16)

    testthat::expect_no_error(arg_check(data = number, class = "numeric",typeof = "double", mode = "numeric", length = 1, double_as_integer_allowed = TRUE, neg_values = FALSE, inf_values = FALSE
))

    testthat::expect_error(arg_check(data = vec6, class = "list", safer_check = 1
))
    result17 <- saferDev::get_message("arg_check(class = 'list', safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL 
) 
    expect17 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_check() OF THE saferDev PACKAGE\nFOLLOWING ARGUMENT HAS NO DEFAULT VALUE AND REQUIRE ONE:\ndata\n\n================\n\n\n"
    testthat::expect_equal(result17, expect17)

    result18 <- saferDev::get_message("arg_check(data = vec6, class = NA, typeof = 'integer', safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL
)
    expect18 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_check() OF THE saferDev PACKAGE\nTHIS ARGUMENT CANNOT JUST BE NA:envir\n\n================\n\n\n"
    testthat::expect_equal(result18, expect18)

    result19 <- saferDev::get_message("arg_check(data = vec6, class = 'list', typeof = 'integer', prop = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL
)
    expect19 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_check() OF THE saferDev PACKAGE\nTHIS ARGUMENT\nprop\nCANNOT BE NULL\n\n================\n\n\n"
    testthat::expect_equal(result19, expect19)

    testthat::expect_error(arg_check(data = vec6, class = "list", typeof = "integer", prop = 'FALSE', safer_check = FALSE))

    result20 <- saferDev::get_message("arg_check(data = vec6, class = list, typeof = 'integer', prop = FALSE, double_as_integer_allowed = FALSE, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL
)
    expect20 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nINTERNAL ERROR IN THE BACKBONE PART OF arg_check() OF THE saferDev PACKAGE\nTHIS ARGUMENT IS NOT MODE \"character\":\nclass\n\nPLEASE, REPORT THIS ERROR HERE: https://github.com/safer-r/saferDev/issues/new\n\n================\n\n\n"
    testthat::expect_equal(result20, expect20)

    testthat::expect_error(arg_check(data = vec6
))

    result21 <- saferDev::get_message("arg_check(data = vec6, class = 'list', typeof = 'integer', prop = FALSE, data_name = base::c('first', 'second'), safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL
)
    expect21 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_check()() OF THE saferDev PACKAGE\ndata_name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT first second\n\n================\n\n\n"
    testthat::expect_equal(result21, expect21)

    result22 <- saferDev::get_message("arg_check(data = vec6)", kind = "error", print.no = TRUE, text = NULL
)
    expect22 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_check()() OF THE saferDev PACKAGE\nAT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop)\n\n================\n\n\n"
    testthat::expect_equal(result22, expect22)

    result23 <- saferDev::get_message("arg_check(data = vec5, class = 'character', options = base::c('a', 'b', 'c'), all_options_in_data = TRUE)", kind = "error", print.no = TRUE, text = NULL
)
    expect23 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_check()() OF THE saferDev PACKAGE\nTHE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED\n\n================\n\n\n"
    testthat::expect_equal(result23, expect23)

    testthat::expect_error(arg_check(data = vec5, class = "character", options = NULL, all_options_in_data = FALSE, neg_values = 'TRUE', safer_check = FALSE))

    result24 <- saferDev::get_message("arg_check(data = base::list(x = 'a', y = '2'), length = 2, options = NULL, prop = FALSE, all_options_in_data = FALSE, neg_values = FALSE, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL
)
    expect24 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_check()() OF THE saferDev PACKAGE\nTHE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL\n\n================\n\n\n"
    testthat::expect_equal(result24, expect24)

    testthat::expect_error(arg_check(data = vec5, class = "character", options = NULL, all_options_in_data = FALSE, inf_values = 'TRUE', safer_check = FALSE))

    result25 <- saferDev::get_message("arg_check(data = base::list(x = 'a', y = '2'), length = 2, options = NULL, prop = FALSE, all_options_in_data = FALSE, inf_values = FALSE, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL
)
    expect25 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_check()() OF THE saferDev PACKAGE\nTHE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL\n\n================\n\n\n"
    testthat::expect_equal(result25, expect25)

    testthat::expect_error(arg_check(data = vec5, class = "wrong_class", options = NULL, all_options_in_data = FALSE, na_contain = 'TRUE', safer_check = FALSE))


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
        fun_name = NULL,
        pack_name = NULL, 
        safer_check = TRUE
    )
    expect26 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 OBJECT", object.name = "vec1")
    testthat::expect_equal(result26, expect26)

})

