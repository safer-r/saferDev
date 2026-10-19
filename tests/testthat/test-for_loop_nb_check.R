# This file tests for_loop_nb_check()
# Same structure as test-curl_bracket_check.R

testthat::test_that("for_loop_nb_check()", {

    ## data argument values
    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    fun2 <- function(x){
        ggplot2::ggplot()
    }
    # clean for() sequences (hard numbers, seq_len, seq_along, degenerate forms deliberately not reported)
    fun3 <- function(x){ for(i in 1:3){ base::print(i) } }
    fun4 <- function(x){ for(i in 1:3) base::print(i) }
    fun5 <- function(x){ for(i in 0:0){ base::print(i) } } # equal bounds: not reported by this function (target of the runtime checking line)
    fun6 <- function(x){ for(i in 1:1){ base::print(i) } } # equal bounds: not reported by this function
    fun7 <- function(x){ for(i in -3:-1){ base::print(i) } } # both negative: not reported by this function
    fun8 <- function(x){ for(i in 1:0){ base::print(i) } } # decreasing: not reported by this function
    fun9 <- function(x){ for(i in 3:1){ base::print(i) } } # decreasing literal bounds: not reported by this function
    fun10 <- function(x){ for(i in 1e3:1e3){ base::print(i) } } # 1e3 is a finite integer-valued literal (1000)
    fun11 <- function(x){ for(i in 1L:3L){ base::print(i) } } # integer literals
    fun12 <- function(x){ for(i in 0x10:0x11){ base::print(i) } } # hexadecimal literals
    fun13 <- function(x){ for(i in base::seq_along(along.with = x)){ base::print(i) } } # recommended replacement
    fun14 <- function(x){ for(i in seq_along(along.with = x)){ base::print(i) } } # unnamespaced seq_along
    fun15 <- function(x){ for(i in base::seq_len(length.out = 3)){ base::print(i) } } # recommended replacement
    fun16 <- function(x){ for(i in seq_len(3)){ base::print(i) } } # unnamespaced seq_len
    fun17 <- function(x){ for(i in 1:3){ for(i2 in 1:3){ base::print(i2) } } } # nested loops
    fun18 <- function(x){ for(i in 1:3){ # c
        base::print(i)
    } } # comments are ignored
    fun19 <- function(x){ for(i in 1:3){ base::print(x = "for(i in x) y <- i") } } # strings are immune
    # reported for() sequences
    fun20 <- function(x){ for(i in 1:length(x)){ base::print(i) } } # 1:0 when x is empty: 2 loops
    fun21 <- function(x){ x <- character() ; for(i in 1:length(x)){ y <- i } ; y } # returns y = 0
    fun22 <- function(x){ for(i in 1:base::length(x = x)){ base::print(i) } } # namespaced length
    fun23 <- function(x){ for(i in 1:n){ base::print(i) } } # plain variable
    fun24 <- function(x){ for(i in n){ base::print(i) } } # plain variable, no colon
    fun25 <- function(x){ for(i in 1:(3 - 1)){ base::print(i) } } # parenthesized constant expression: reported (purely syntactic rule)
    fun26 <- function(x){ for(i in 1:2.5){ base::print(i) } } # silently truncated toward 0
    fun27 <- function(x){ for(i in 1:1e-3){ base::print(i) } } # non integer-valued literal
    fun28 <- function(x){ for(i in 1:NA){ base::print(i) } } # NA literal
    fun29 <- function(x){ for(i in 1:Inf){ base::print(i) } } # Inf literal
    fun30 <- function(x){ for(i in 1:TRUE){ base::print(i) } } # TRUE is not a numeric literal
    fun31 <- function(x){ for(i in x){ base::print(i) } } # plain object
    fun32 <- function(x){ for(i in base::rev(x = x)){ base::print(i) } } # non seq_len/seq_along call
    fun33 <- function(x){ for(i in base::seq(x = x)){ base::print(i) } } # 2 loops when x = 0
    fun34 <- function(x){ for(i in names(x)){ base::print(i) } } # unnamespaced call
    fun35 <- function(x){ for(i in 1:nrow(df)){ base::print(i) } } # 1:0 when df has 0 rows
    fun36 <- function(x){ for(i in 1:length(x) - 1){ base::print(i) } } # 0:-1 when x is empty: 2 loops
    fun37 <- function(x){ for(i in base::seq(from = 1, to = base::length(x = x))){ base::print(i) } } # seq() with length()
    fun38 <- function(x){ x <- 0 ; for(i in 0:0){ y <- i } ; y } # from the @description example: hard numbers, so clean here
    fun39 <- function(f = function(y) for(i in y){ base::print(i) }) f(1) # for inside a formal default: reported
    fun40 <- function(x){ if(x > 1){ for(i in 1:length(x)){ base::print(i) } } } # for inside if
    fun41 <- function(x){ for(i in base::seq_along(along.with = x) - 1){ base::print(i) } } # seq_along followed by an operator: reported (gives c(0, 1) when x has 2 elements)
    fun42 <- function(x){ for(i in seq_len(3) + 1){ base::print(i) } } # seq_len followed by an operator: reported
    fun43 <- function(x){ for(i in seq_len(3)[1]){ base::print(i) } } # subscripted seq_len: reported
    fun44 <- function(x){ for(i in 1:3 - 1){ base::print(i) } } # binary minus: reported (gives c(-2, -1))
    fun45 <- function(x){ for(i in 3-1){ base::print(i) } } # binary minus without spaces: reported (1, same as 2, but the writing is not hard numbers)
    fun46 <- function(x){ for(i in 2 - 1){ base::print(i) } } # binary minus around a single number: reported
    fun47 <- function(x){ for(i in 1:3-1:2){ base::print(i) } } # chained colon with binary minus: reported
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(for_loop_nb_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::for_loop_nb_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(for_loop_nb_check()) # the function has default values for all args except x.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(for_loop_nb_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(for_loop_nb_check(x = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(for_loop_nb_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.0: unguarded lib_path check)
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_message(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # braced body
    # end x
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    # detection of for() sequences not written with hard numbers
    # clean cases
    testthat::expect_message(for_loop_nb_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(for_loop_nb_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(for_loop_nb_check(x = fun5, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # equal bounds 0:0: not reported
    testthat::expect_message(for_loop_nb_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # equal bounds 1:1: not reported
    testthat::expect_message(for_loop_nb_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # both negative: not reported
    testthat::expect_message(for_loop_nb_check(x = fun8, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # decreasing 1:0: not reported
    testthat::expect_message(for_loop_nb_check(x = fun9, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # decreasing 3:1: not reported
    testthat::expect_message(for_loop_nb_check(x = fun10, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # 1e3 is integer-valued
    testthat::expect_message(for_loop_nb_check(x = fun11, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # integer literals
    testthat::expect_message(for_loop_nb_check(x = fun12, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # hexadecimal literals
    testthat::expect_message(for_loop_nb_check(x = fun13, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # namespaced seq_along
    testthat::expect_message(for_loop_nb_check(x = fun14, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # unnamespaced seq_along
    testthat::expect_message(for_loop_nb_check(x = fun15, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # namespaced seq_len
    testthat::expect_message(for_loop_nb_check(x = fun16, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # unnamespaced seq_len
    testthat::expect_message(for_loop_nb_check(x = fun17, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # nested loops
    testthat::expect_message(for_loop_nb_check(x = fun18, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # comments are ignored
    testthat::expect_message(for_loop_nb_check(x = fun19, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # strings are immune
    testthat::expect_message(for_loop_nb_check(x = fun38, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # hard numbers from the @description example
    testthat::expect_message(for_loop_nb_check(x = mean, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # no for() at all
    testthat::expect_message(for_loop_nb_check(x = for_loop_nb_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # self check
    # reported cases
    testthat::expect_message(for_loop_nb_check(x = fun20, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE)
    testthat::expect_message(for_loop_nb_check(x = fun21, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE)
    testthat::expect_message(for_loop_nb_check(x = fun22, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # namespaced length
    testthat::expect_message(for_loop_nb_check(x = fun23, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE)
    testthat::expect_message(for_loop_nb_check(x = fun24, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE)
    testthat::expect_message(for_loop_nb_check(x = fun25, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # parenthesized constant
    testthat::expect_message(for_loop_nb_check(x = fun26, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # 2.5
    testthat::expect_message(for_loop_nb_check(x = fun27, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # 1e-3
    testthat::expect_message(for_loop_nb_check(x = fun28, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # NA
    testthat::expect_message(for_loop_nb_check(x = fun29, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # Inf
    testthat::expect_message(for_loop_nb_check(x = fun30, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # TRUE
    testthat::expect_message(for_loop_nb_check(x = fun31, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # plain object
    testthat::expect_message(for_loop_nb_check(x = fun32, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # rev()
    testthat::expect_message(for_loop_nb_check(x = fun33, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # seq()
    testthat::expect_message(for_loop_nb_check(x = fun34, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # names()
    testthat::expect_message(for_loop_nb_check(x = fun35, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # nrow()
    testthat::expect_message(for_loop_nb_check(x = fun36, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # 1:length(x) - 1
    testthat::expect_message(for_loop_nb_check(x = fun37, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # seq(from = , to = length())
    testthat::expect_message(for_loop_nb_check(x = fun39, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # for inside a formal default
    testthat::expect_message(for_loop_nb_check(x = fun40, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # for inside if
    testthat::expect_message(for_loop_nb_check(x = fun41, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # seq_along followed by an operator
    testthat::expect_message(for_loop_nb_check(x = fun42, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # seq_len followed by an operator
    testthat::expect_message(for_loop_nb_check(x = fun43, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # subscripted seq_len
    testthat::expect_message(for_loop_nb_check(x = fun44, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # binary minus 1:3 - 1
    testthat::expect_message(for_loop_nb_check(x = fun45, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # binary minus 3-1
    testthat::expect_message(for_loop_nb_check(x = fun46, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # binary minus 2 - 1
    testthat::expect_message(for_loop_nb_check(x = fun47, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE) # chained colon 1:3-1:2
    # message normalization of pkg::fun
    testthat::expect_message(for_loop_nb_check(x = base::rep, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE base::rep(), EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(for_loop_nb_check(x = fun20, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun20(), SOME for() SEQUENCES", fixed = TRUE)
    # exact findings for a representative case (only the LINE and CONSTRUCT columns are compared, because the CODE column depends on the srcref presence of the checked function). Warning: utils::capture.output(type = "message") does not work inside testthat (the message connection is redirected), so testthat::capture_messages() is used
    msgs <- testthat::capture_messages(for_loop_nb_check(x = fun40, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res <- base::unlist(x = base::strsplit(x = msgs, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res_fields <- base::strsplit(x = res, split = "\t", fixed = TRUE)
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[1]}, FUN.VALUE = base::character(length = 1)), y = "1"), label = "fun40 exact line number")
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1)), y = "for"), label = "fun40 exact construct kind")
    # several for() constructs are all reported (the function is defined on its own line: an inline assignment would make arg_user_setting$x a call object, which distorts the message concatenation)
    f_multi <- function(x){ for(i in 1:length(x)){ y <- i } ; for(i2 in 1:3){ y <- i2 } ; for(i3 in x){ y <- i3 } }
    msgs2 <- testthat::capture_messages(for_loop_nb_check(x = f_multi, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res2 <- base::unlist(x = base::strsplit(x = msgs2, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res2 <- res2[base::grepl(x = res2, pattern = "^[0-9]+\\t", perl = TRUE)]
    testthat::expect_true(object = base::length(x = res2) == 2, label = "two findings for two reported for() constructs")
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

})
