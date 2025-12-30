testthat::test_that(".noclean_functions()", {

    ## data argument values
    ini1 = c(
        'function(', 
        '        text, ', 
        '        pattern', 
        '    ){', 
        '        # AIM', 
        '        # extract all function names', 
        '        # ARGUMENTS', 
        '        # text: vector of strings', 
        '        # pattern: regex to extract function names', 
        '        # RETURN', 
        '        # A list containing the functions names, each compartment being one of the string of the input vector', 
        '        # DEBUGGING', 
        '        # text = ini[1] ; pattern = pattern', 
        '        FUN1 <- function(){}', 
        '        #### Find all matches, including trailing "(" #', 
        '        matches <- gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text) # # test', 
        '        dt <- base::c(2:8)', 
        '        matched_strings <- regmatches(x = text, m = matches)[[1]]', 
        '        FUN1()', 
        '        # Remove trailing "(" from each match #', 
        '        tempo4 <- a$regmatches(x = text, m = matches)[[1]] ; sum(1:3) ; a$regmatches(x = 1)', 
        '        tempo5 <- a$count', 
        '        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(roc1()), "), col2 (", base::length(roc2), "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")', 
        '        result <- sub("\\($##", "", matched_strings) ; range(1:3) # sub("\\($##", "", matched_strings)', 
        '        tempo.cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(roc4()), "), and roc0 (", base::length(roc5), ") SHOULL\n")', 
        '        return(baba) # base::sub("\\($##", "", matched_strings)', 
        '        base::return(bobo) # a$sub("\\($##", "", matched_strings)', 
        '    }'
    )
    int1 <-  c(15, 17)
    int2 <- c(15, 25)
    int3 <- c(15, 25000)
    str1 <- c("gregexpr", "regmatches")
    str2 <- c("matches <- ",  "matched_strings <- " )
    str3 <- c("matches <- ",  "matched_strings$" )
    str4 <- c("gregexpr", "roc00")
    str5 <- c("matches <- ",  "tempo.cat <- base::paste0('IAGE\nLENGTHS OF ")
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.noclean_functions(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.noclean_functions()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.noclean_functions()) 
    col1 <-  int1
    col2 <- str1
    col3 <- str2
    ini <- ini1
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.noclean_functions()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.noclean_functions(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        col1, 
        col2, 
        col3, 
        ini,  
        lib_path, 
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.noclean_functions(col1 =  NULL, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = NULL, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = NULL, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.noclean_functions(col1 = character(), col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = integer(), col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = double(), col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = logical(), col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = complex(), col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = data.frame(), col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = list(), col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = character(), col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = integer(), col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = double(), col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = logical(), col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = complex(), col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = data.frame(), col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = list(), col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = character(), ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = integer(), ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = double(), ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = logical(), ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = complex(), ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = data.frame(), ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = list(), ini = ini1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.noclean_functions(col1 = NA, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = NA, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = NA, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NA, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## check of lib_path
    # already done in the main function
    ######## end check of lib_path

    ######## safer_check argument checking
    # not required because not here
    ######## end safer_check argument checking

    ######## check of the required functions from the required packages
    # not required
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    # already done in the main function
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # col1
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  NULL, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = NA, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = 1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = c(TRUE, FALSE), col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = mat1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = factor1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = expr1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 = fun1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    # end col1
    # col2
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = NULL, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = NA, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = 1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = c(TRUE, FALSE), col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = mat1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = factor1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = expr1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = fun1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    # end col2
    # col3
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = NULL, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = NA, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = 1, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = c(TRUE, FALSE), ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = mat1, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = factor1, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = expr1, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = fun1, ini = ini1, lib_path = NULL, error_text = ""))
    # end col3
    # ini
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = fun1, lib_path = NULL, error_text = ""))
    # end ini
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(.noclean_functions(col1 = "", col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = "")) 
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = "", col3 = str2, ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = "", ini = ini1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = "", lib_path = NULL, error_text = ""))
    # testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = "", error_text = "")) # cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    testthat::expect_no_error(.noclean_functions(col1 =  int1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = "")) 
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
    result <- saferDev::get_message('.noclean_functions(col1 = 1, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.noclean_functions().\n\ncol1, col2 AND col3 ARGUMENTS MUST HAVE THE SAME LENGTH.\nLENGTH OF col1:\n1\nLENGTH OF col2:\n2\nLENGTH OF col3:\n2\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    result <- saferDev::get_message('.noclean_functions(col1 = int3, col2 = str1, col3 = str2, ini = ini1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL, safer_check = FALSE) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.noclean_functions().\n\ncol1 CANNOT BE OVER THE LENGTH OF ini.\ncol1:\n15\n25000\nLENGTH OF ini:\n28\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # detection of a$fun() pattern
    result <- .noclean_functions(col1 =  int1, col2 = str1, col3 = str3, ini = ini1, lib_path = NULL, error_text = "")
    expected <- c(FALSE, TRUE)
    testthat::expect_equal(result, expected)
    # end detection of a$fun() pattern

    # detection of functions between quotes
    result <- .noclean_functions(col1 =  int2, col2 = str4, col3 = str5, ini = ini1, lib_path = NULL, error_text = "")
    expected <- c(FALSE, TRUE)
    testthat::expect_equal(result, expected)
    # end detection of functions between quotes
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})

