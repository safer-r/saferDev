testthat::test_that(".fun_args_pos()", {

    ## data argument values
    str1 <- ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)'
    str2 <- 'paste0("I", "b")' # no middle brackets
    pattern1 <- paste0("paste0", "[\\s\\r\\n]*\\(")
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.fun_args_pos(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.fun_args_pos()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.fun_args_pos()) 
    text <- str1
    pattern <- pattern1
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.fun_args_pos()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.fun_args_pos(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        text,
        pattern,
        lib_path,
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.fun_args_pos(text = NULL, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.fun_args_pos(text = character(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = integer(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = double(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = logical(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = complex(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = data.frame(), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = list(), pattern = pattern1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.fun_args_pos(text = str1, pattern = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.fun_args_pos(text = NA, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NA, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = NA))
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
    # text
    testthat::expect_no_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = NULL, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = NA, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = 1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = c(TRUE, FALSE), pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = mat1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = factor1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = expr1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = fun1, pattern = pattern1, lib_path = NULL, error_text = ""))
    # end text
    # pattern
    testthat::expect_no_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.fun_args_pos(text = str1, pattern = str1, lib_path = NULL, error_text = ""))
    # end pattern
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(.fun_args_pos(text = "", pattern = pattern1, lib_path = NULL, error_text = "")) 
    testthat::expect_error(.fun_args_pos(text = str1, pattern = "", lib_path = NULL, error_text = ""))
    # testthat::expect_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = "", error_text = "")) # cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    testthat::expect_no_error(.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = "")) 
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

    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    files_before <- base::list.files(path = ".", pattern = NULL, all.files = TRUE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = TRUE, no.. = TRUE) # the internal errors below must not write any file
    # check_pos(): pattern not found (internal error 1) and no ( after the match of pattern (internal error 2)
    testthat::expect_error(object = saferDev:::.fun_args_pos(text = str1, pattern = "nofun[\\s\\r\\n]*\\(", lib_path = NULL, error_text = ""), regexp = "INTERNAL ERROR 1 IN saferDev:::.fun_args_pos().\n\nINTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF THE FUNCTION NAME (pattern NOT FOUND IN text).\nDETECTED POSITION: -1\npattern: nofun[\\s\\r\\n]*\\(\nfun_pos: -1\ntext (CHARACTERS 1 TO 72 OF 72):\n \"a\" ; paste0(", fixed = TRUE)
    testthat::expect_error(object = saferDev:::.fun_args_pos(text = "a b", pattern = "b", lib_path = NULL, error_text = ""), regexp = "INTERNAL ERROR 2 IN saferDev:::.fun_args_pos().\n\nINTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF THE OPENING BRACKET OF THE FUNCTION.\nDETECTED POSITION: NA\npattern: b\nfun_pos: 3\ntext (CHARACTERS 1 TO 3 OF 3):\na b", fixed = TRUE)
    # end check_pos()
    # while_loop(): closing bracket not found (internal error 3), with an excerpt of 200 characters of text, starting 50 characters before the opening bracket
    testthat::expect_error(object = saferDev:::.fun_args_pos(text = base::paste0(base::strrep(x = "a", times = 100), " paste0(1, sum(2) ", base::strrep(x = "b", times = 300), collapse = NULL, recycle0 = FALSE), pattern = pattern1, lib_path = NULL, error_text = " INSIDE P1::F1"), regexp = base::paste0("INTERNAL ERROR 3 IN saferDev:::.fun_args_pos() INSIDE P1::F1\n\nINTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF THE CLOSING BRACKET.\npattern: paste0[\\s\\r\\n]*\\(\nPOSITION OF THE OPENING BRACKET: 108\nBRACKETS NOT CLOSED: 1\ntext (CHARACTERS 58 TO 257 OF 418):\n", base::strrep(x = "a", times = 43), " paste0(1, sum(2) ", base::strrep(x = "b", times = 139), "\n\nPLEASE, REPORT THIS ERROR HERE", collapse = NULL, recycle0 = FALSE), fixed = TRUE)
    # end while_loop()
    testthat::expect_identical(object = base::list.files(path = ".", pattern = NULL, all.files = TRUE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = TRUE, no.. = TRUE), expected = files_before)
    # middle brackets
    testthat::expect_no_error(.fun_args_pos(text = str2, pattern = pattern1, lib_path = NULL, error_text = ""))
    testthat::expect_equal(object = saferDev:::.fun_args_pos(text = str2, pattern = pattern1, lib_path = NULL, error_text = ""), expected = base::list(begin_fun = 1L, begin = 7L, end = 16L, middle_bracket_pos = NULL))
    testthat::expect_equal(object = saferDev:::.fun_args_pos(text = str1, pattern = pattern1, lib_path = NULL, error_text = ""), expected = base::list(begin_fun = 8L, begin = 14L, end = 61L, middle_bracket_pos = base::list(base::c(26L, 51L), base::c(30L, 34L), base::c(57L, 59L))))
    # end middle brackets
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})

