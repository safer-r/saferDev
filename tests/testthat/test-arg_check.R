testthat::test_that("arg_check()", {
    #### values

    ######## data argument
    vec1 <- -1:3 # vector of integers
    vec3 <- base::c(1, 2, 3)
    vec4 <- "pearson"
    vec5 <- base::c("a", "b","a", "b")
    vec6 <- base::list(1:3, 4:6)
    mat1 <- base::matrix(vec1)
    mat2 <- base::matrix(base::c(1:3 / 3, NA))
    number <- 1
    ######## end data argument

    ######## error_text argument
    error_text_e1 <- NULL
    error_text_e2 <- NA
    error_text_e3 <- 1
    error_text_e4 <- c("ERROR1", "ERROR2")
    error_text_g1 <- " IN P1::F1."
    ######## end error_text argument

    ######## lib_path argument
    lib_path_e1 <- NA
    lib_path_e2 <- 1
    lib_path_e3 <- "PATH_NOT_GOOD"
    lib_path_g1 <- NULL
    lib_path_g2 <- base::.libPaths()
    ######## end lib_path argument

    ######## safer_check argument
    safer_check_e1 <- NULL
    safer_check_e2 <- NA
    safer_check_e3 <- 1
    safer_check_e4 <- c(TRUE, FALSE)
    safer_check_g1 <- TRUE
    safer_check_g2 <- FALSE
    ######## end safer_check argument

    #### end values

    #### argument test

    ######## error_text argument
    # warning: use other argument values that do not return an error
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = error_text_e1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = error_text_e2))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = error_text_e3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", error_text = error_text_e4))
    result <- arg_check(data = vec1, class = "numeric", error_text = error_text_g1)
    expect <- list(problem = TRUE, text = "ERROR IN P1::F1.\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result,expect)
    ######## end error_text argument

    ######## lib_path argument
    # warning: use other argument values that do not return an error
    testthat::expect_error(arg_check(data = vec1, class = "numeric", lib_path = lib_path_e1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", lib_path = lib_path_e2))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", lib_path = lib_path_e3))
    result <- arg_check(data = vec1, class = "numeric", lib_path = lib_path_g1)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", lib_path = lib_path_g2)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    ######## end lib_path argument

    ######## safer_check argument
    # warning: use other argument values that do not return an error
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = safer_check_e1))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = safer_check_e2))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = safer_check_e3))
    testthat::expect_error(arg_check(data = vec1, class = "numeric", safer_check = safer_check_e4))
    result <- arg_check(data = vec1, class = "numeric", safer_check = safer_check_g1)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    result <- arg_check(data = vec1, class = "numeric", safer_check = safer_check_g2)
    expect <- list(problem = TRUE, text = "ERROR\n\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result, expect)
    ######## end safer_check argument

    ########  argument with no default values
    # data
    testthat::expect_error(arg_check())
    ########  end argument with no default values

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

    ######## management of NULL arguments
    testthat::expect_error(arg_check(data = vec1, prop = NULL))
    testthat::expect_error(arg_check(data = vec1, double_as_integer_allowed = NULL))
    testthat::expect_error(arg_check(data = vec1, all_options_in_data = NULL))
    testthat::expect_error(arg_check(data = vec1, na_contain = NULL))
    testthat::expect_error(arg_check(data = vec1, neg_values = NULL))
    testthat::expect_error(arg_check(data = vec1, inf_values = NULL))
    testthat::expect_error(arg_check(data = vec1, print = NULL))
    testthat::expect_error(arg_check(data = vec1, safer_check = NULL))
    testthat::expect_error(arg_check(data = vec1, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of "" in arguments of mode character
    testthat::expect_error(arg_check(data = vec1, class = ""))
    testthat::expect_error(arg_check(data = vec1, typeof = ""))
    testthat::expect_error(arg_check(data = vec1, mode = ""))
    testthat::expect_error(arg_check(data = vec1, data_name = ""))
    testthat::expect_error(arg_check(data = vec1, lib_path = ""))
    ######## end management of "" in arguments of mode character
    






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

