    test_that("output of arg_check is a list contains 'problem', 'text', and 'object.name'", {
    vec1 <- -1:3 # vector of integers
    vec3 <- c(1, 2, 3)
    vec4 <- "pearson"
    vec5 <- c("a", "b","a", "b")
    mat1 <- matrix(vec1)
    mat2 <- matrix(c(1:3 / 3, NA))
    
    result1 <- arg_check(data = vec1, class = "numeric")
    expected1 <- list(problem = TRUE, text = "ERROR: THE vec1 OBJECT MUST BE CLASS numeric", object.name = "vec1")
    expect_equal(result1,expected1)
    
    result2 <- arg_check(data = vec1, class = "vector", typeof = "integer", length = 3, neg.values = FALSE, na.contain = FALSE)
    expected2 <- list(problem = TRUE, text = "ERROR: THE vec1 OBJECT MUST BE LENGTH 3 AND THE vec1 OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES", object.name = "vec1")
    expect_equal(result2,expected2)
    
    result3 <- arg_check(data = NULL, class = "integer")
    expected3 <- list(problem = TRUE, text = "ERROR: THE NULL OBJECT MUST BE CLASS integer", object.name = "NULL")
    expect_equal(result3,expected3)
    
    result4 <- arg_check(data = vec1, 
    typeof = "integer", 
    mode = "character", # the mode "character" exists but is inconsistant with typeof "integer". However, this aspect is not signaled by the function
)
    expected4 <- list(problem = TRUE, text = "ERROR: THE vec1 OBJECT MUST BE MODE character", object.name = "vec1")
    expect_equal(result4,expected4)
    
    
    expect_error(object = arg_check(data = vec1, 
    mode = "integer", # the mode "integer" does not exist in the mode() function of R
), regexp = NULL)
    
    result6 <- arg_check(data = mat2, 
    prop = TRUE # Check for values between 0 and 1 only
)
    expected6 <- list(problem = TRUE, text = "ERROR: THE mat2 OBJECT CONTAINS NA WHILE NOT AUTHORIZED", object.name = "mat2")
    expect_equal(result6,expected6)
    
    result7 <- arg_check(data = vec3, typeof = "integer",
    double.as.integer.allowed = TRUE # with TRUE, integers stored as double are accepted
)
    expected7 <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec3 OBJECT", object.name = "vec3")
    expect_equal(result7,expected7)
    
    result8 <- arg_check(data = vec4, 
    options = c("pearson", "spearman", "kendall")
)
    expected8 <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec4 OBJECT", object.name = "vec4")
    expect_equal(result8,expected8)
    
    result9 <- arg_check(data = vec5,
    options = c("a", "b", "c"), 
    all.options.in.data = TRUE
)
    expected9 <- list(problem = TRUE, text = "ERROR: THE vec5 OBJECT MUST BE MADE OF ALL THESE OPTIONS: a b c\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: c", object.name = "vec5")
    expect_equal(result9,expected9)
    
    result10 <- arg_check(data = mat2, class = "matrix", prop = TRUE,
    na.contain = FALSE # with TRUE, integers stored as double are accepted
)
    expected10 <- list(problem = TRUE, text = "ERROR: THE mat2 OBJECT CONTAINS NA WHILE NOT AUTHORIZED", object.name = "mat2")
    expect_equal(result10,expected10)
    
    result11 <- arg_check(data = mat1, class = "matrix",
    neg.values = FALSE # with TRUE, integers stored as double are accepted
)
    expected11 <- list(problem = TRUE, text = "ERROR: THE mat1 OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES", object.name = "mat1")
    expect_equal(result11,expected11)
    
    result12 <- arg_check(data = mat1, class = "matrix",
    inf.values = FALSE
)
    expected12 <- list(problem = TRUE, text = "ERROR: THE mat1 OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS NOT EVEN TYPE DOUBLE", object.name = "mat1")
    expect_equal(result12,expected12)
    
    result13 <- arg_check(
        data = vec1, 
        class = "integer", 
        typeof = NULL, 
        mode = NULL, 
        length = NULL, 
        prop = FALSE, 
        double.as.integer.allowed = FALSE, 
        options = NULL, 
        all.options.in.data = FALSE, 
        na.contain = FALSE, 
        neg.values = TRUE, 
        inf.values = TRUE, 
        print = FALSE, 
        data.name = NULL, 
        fun.name = NULL,
        safer_check = TRUE
    )
    expected13 <- list(problem = FALSE, text = "NO PROBLEM DETECTED FOR THE vec1 OBJECT", object.name = "vec1")
    expect_equal(result13, expected13)
})
