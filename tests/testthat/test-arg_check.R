test_that("arg_check()", {
    vec1 <- -1:3 # vector of integers
    vec3 <- base::c(1, 2, 3)
    vec4 <- "pearson"
    vec5 <- base::c("a", "b","a", "b")
    vec6 <- base::list(1:3, 4:6)
    mat1 <- base::matrix(vec1)
    mat2 <- base::matrix(base::c(1:3 / 3, NA))

    #Test cases
    result1 <- saferDev::arg_check(data = vec1, class = "numeric")
    expected1 <- base::list(problem = TRUE, text = "ERROR\nTHE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    testthat::expect_equal(result1,expected1)
    
    
    result2 <- saferDev::arg_check(data = vec1, class = "vector", typeof = "integer", length = 3, neg_values = FALSE, na_contain = FALSE)
    expected2 <- list(problem = TRUE, text = "ERROR\nTHE vec1 OBJECT MUST BE LENGTH 3 AND THE vec1 OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES", object.name = "vec1")
    testthat::expect_equal(result2,expected2)
    

    result3 <- saferDev::arg_check(data = NULL, class = "integer")
    expected3 <- base::list(problem = TRUE, text = "ERROR\nTHE NULL OBJECT MUST BE CLASS integer", object.name = "NULL")
    testthat::expect_equal(result3,expected3)
    

    result4 <- saferDev::arg_check(data = vec1, 
    typeof = "integer", 
    mode = "character", # the mode "character" exists but is inconsistant with typeof "integer". However, this aspect is not signaled by the function
)
    expected4 <- base::list(problem = TRUE, text = "ERROR\nTHE vec1 OBJECT MUST BE MODE character", object.name = "vec1")
    testthat::expect_equal(result4,expected4)
    
    
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
    

    result6 <- saferDev::arg_check(data = mat2, 
    prop = TRUE # Check for values between 0 and 1 only
)
    expected6 <- base::list(problem = TRUE, text = "ERROR\nTHE mat2 OBJECT CONTAINS NA WHILE NOT AUTHORIZED", object.name = "mat2")
    testthat::expect_equal(result6,expected6)
    

    result7 <- saferDev::arg_check(data = vec3, typeof = "integer",
    double_as_integer_allowed = TRUE # with TRUE, integers stored as double are accepted
)
    expected7 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 OBJECT", object.name = "vec3")
    testthat::expect_equal(result7,expected7)
    

    result8 <- saferDev::arg_check(data = vec4, 
    options = base::c("pearson", "spearman", "kendall")
)
    expected8 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec4 OBJECT", object.name = "vec4")
    testthat::expect_equal(result8,expected8)
    

    result9 <- saferDev::arg_check(data = vec5,
    options = base::c("a", "b", "c"), 
    all_options_in_data = TRUE
)
    expected9 <- base::list(problem = TRUE, text = "ERROR\nTHE vec5 OBJECT MUST BE MADE OF ALL THESE OPTIONS: a b c\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: c", object.name = "vec5")
    testthat::expect_equal(result9,expected9)
    

    result10 <- saferDev::arg_check(data = mat2, class = "matrix", prop = TRUE,
    na_contain = FALSE # with TRUE, integers stored as double are accepted
)
    expected10 <- base::list(problem = TRUE, text = "ERROR\nTHE mat2 OBJECT CONTAINS NA WHILE NOT AUTHORIZED", object.name = "mat2")
    testthat::expect_equal(result10,expected10)
    

    result11 <- saferDev::arg_check(data = mat1, class = "matrix",
    neg_values = FALSE # with TRUE, integers stored as double are accepted
)
    expected11 <- base::list(problem = TRUE, text = "ERROR\nTHE mat1 OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES", object.name = "mat1")
    testthat::expect_equal(result11,expected11)
    

    result12 <- saferDev::arg_check(data = mat1, class = "matrix",
    inf_values = FALSE
)
    expected12 <- base::list(problem = TRUE, text = "ERROR\nTHE mat1 OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN TYPE DOUBLE", object.name = "mat1")
    testthat::expect_equal(result12,expected12)

    result13 <- saferDev::arg_check(data = vec6, class = "list"
    # vec6 is a list of two vectors of integers
)
    expected13 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec6 OBJECT", object.name = "vec6")
    testthat::expect_equal(result13,expected13)

    result14 <- saferDev::arg_check(data = vec6, class = "list", typeof = "integer" # vec6 is a list of two vectors of integers
)
    expected14 <- base::list(problem = TRUE, text = "ERROR\nTHE vec6 OBJECT MUST BE TYPEOF integer", object.name = "vec6")
    testthat::expect_equal(result14,expected14)

    result15 <- saferDev::arg_check(data = vec6, class = "list", neg_values = TRUE, # vec6 is a list of two vectors of integers
)
    expected15 <- base::list(problem = TRUE, text = "NO PROBLEM DETECTED FOR THE vec6 OBJECT", object.name = "vec6")


    testthat::expect_error(saferDev::arg_check(data = vec6, class = "list", prop = TRUE, # vec6 is a list of two vectors of integers
), regexp = NULL)

    testthat::expect_no_error(saferDev::arg_check(data = vec6, class = "list", mode = "list", prop = FALSE, neg_values = TRUE, inf_values = TRUE, # vec6 is a list of two vectors of integers, no check is performed for the presence of negative values, infinite values are allowed
))
    testthat::expect_no_error(saferDev::arg_check(data = vec6, class = "list", na_contain = TRUE, # vec6 is a list of two vectors of integers, it could contain NA
))

    result16 <- saferDev::arg_check(data = vec6, class = "list",  length = 2, # vec6 is a list of two vectors of integers
)
    expected16 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec6 OBJECT", object.name = "vec6")
    testthat::expect_equal(result16, expected16)

    result17 <- saferDev::arg_check(
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
    expected17 <- base::list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 OBJECT", object.name = "vec1")
    testthat::expect_equal(result17, expected17)
})

