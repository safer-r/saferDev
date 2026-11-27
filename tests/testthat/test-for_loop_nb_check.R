# This file tests for_loop_nb_check()
# Same structure as test-curly_bracket_check.R

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
    fun33 <- function(x){ for(i in base::seq(x)){ base::print(i) } } # 2 loops when x = 0
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
    fun44 <- function(x){ for(i in 1:3 - 1){ base::print(i) } } # binary minus: reported (gives c(0, 1, 2))
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
    # a construct that requires the wrap (body of an if without curly brackets) and contains a comment is corrected: the comment is kept on its line
    fun48 <- function(x){
        if(x) for(i in 1:length(x)){ # comment inside the construct
            base::print(i)
        }
    }
    testthat::expect_message(for_loop_nb_check(x = fun48, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE)
    r48 <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun48, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::is.function(x = r48), label = "wrap with a comment: corrected function returned")
    testthat::expect_identical(object = utils::capture.output(base::suppressWarnings(expr = r48(x = 2), classes = "warning"), file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(fun48(x = 2), file = NULL, append = FALSE, type = NULL, split = FALSE))
    # two constructs, one in a braced block and one that requires the wrap and contains a comment: both corrected
    fun49 <- function(x){
        for(i in 1:length(x)){ base::print(i) }
        if(x) for(j in 1:length(x)){ # comment inside the construct
            base::print(j)
        }
    }
    r49 <- for_loop_nb_check(x = fun49, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r49), label = "two constructs: modified function returned")
    txt49 <- base::paste0(base::deparse(expr = r49, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = txt49, pattern = "tempo_for_seq1", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "two constructs: first construct modified")
    testthat::expect_true(object = base::grepl(x = txt49, pattern = "tempo_for_seq2", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "two constructs: second construct modified")
    # constructs sharing the end of their body (a loop without curly brackets whose body is another loop), both requiring the wrap: both corrected
    fun50 <- function(x){
        if(x) for(i in 1:nrow(df)) for(j in 1:length(x)){ base::print(j) }
    }
    msgs50 <- testthat::capture_messages(code = saferDev::for_loop_nb_check(x = fun50, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_false(object = base::any(base::grepl(x = msgs50, pattern = "CHECKING LINES NOT INSERTED", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = FALSE), label = "shared body end: no construct left unchanged")
    r50 <- for_loop_nb_check(x = fun50, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r50), label = "shared body end: modified function returned")
    txt50 <- base::paste0(base::deparse(expr = r50, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = txt50, pattern = "for (i in tempo_for_seq1) {", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "shared body end: outer construct modified")
    testthat::expect_true(object = base::grepl(x = txt50, pattern = "for (j in tempo_for_seq2) {", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "shared body end: inner construct modified")
    # same as fun50, but with the constructs written on several lines, and run
    fun51 <- function(x){
        if(base::length(x = x) > 0) for(i in 1:length(x)) for(j in 1:length(x)){
            base::print(x = i * j)
        }
    }
    msgs51 <- testthat::capture_messages(code = saferDev::for_loop_nb_check(x = fun51, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_false(object = base::any(base::grepl(x = msgs51, pattern = "CHECKING LINES NOT INSERTED", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = FALSE), label = "shared body end, multi-line: no construct left unchanged")
    r51 <- for_loop_nb_check(x = fun51, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r51), label = "shared body end, multi-line: modified function returned")
    testthat::expect_identical(object = utils::capture.output(r51(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(fun51(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE))
    # a reported construct whose tempo object name (tempo_for_seq1) is already used in the checked function code is skipped: the construct remains unmodified and the name is listed in the message
    fun54 <- function(x){
        tempo_for_seq1 <- x
        for(i in 1:length(x)){ base::print(i) }
        for(j in 1:length(x)){ base::print(j) }
    }
    testthat::expect_message(for_loop_nb_check(x = fun54, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "the object name tempo_for_seq1 is already used in the checked function code", fixed = TRUE)
    r54 <- for_loop_nb_check(x = fun54, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r54), label = "tempo name already used: modified function returned")
    txt54 <- base::paste0(base::deparse(expr = r54, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = txt54, pattern = "for \\(i in 1:length\\(x\\)\\)", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), label = "tempo name already used: first construct unmodified")
    testthat::expect_true(object = base::grepl(x = txt54, pattern = "tempo_for_seq2", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "tempo name already used: second construct modified")
    msgs54 <- testthat::capture_messages(code = saferDev::for_loop_nb_check(x = fun54, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res54 <- base::unlist(x = base::strsplit(x = msgs54, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    testthat::expect_identical(object = base::sum(base::grepl(x = res54, pattern = "^[0-9]+\\tfor\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), na.rm = FALSE), expected = 2L) # the skipped construct is also listed in the table (only the rows are counted, because the CODE column depends on the srcref presence of the checked function)
    # single reported construct skipped because its tempo object name is already used: listed in the message (not reported as clean), and NULL returned
    fun54b <- function(x){ tempo_for_seq1 <- 0 ; for(i in 1:length(x)){ y <- i } ; y }
    testthat::expect_message(object = saferDev::for_loop_nb_check(x = fun54b, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS", fixed = TRUE)
    testthat::expect_message(object = saferDev::for_loop_nb_check(x = fun54b, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "the object name tempo_for_seq1 is already used in the checked function code", fixed = TRUE)
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun54b, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    # a reported for() construct whose sequence expression contains another reported for() construct is skipped (the insertion spans would overlap): listed in the message, and only the inner construct is modified
    fun55 <- function(x){
        for(i in if(x > 0) for(j in x) j){
            base::print(i)
        }
    }
    testthat::expect_message(for_loop_nb_check(x = fun55, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "line 2: the sequence expression of the for() construct contains another reported for() construct", fixed = TRUE)
    r55 <- for_loop_nb_check(x = fun55, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r55), label = "overlapping sequence expressions: modified function returned")
    txt55 <- base::paste0(base::deparse(expr = r55, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(x = txt55, pattern = "for \\(i in if \\(x > 0\\)", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), label = "overlapping sequence expressions: outer construct unmodified")
    testthat::expect_false(object = base::grepl(x = txt55, pattern = "tempo_for_seq1", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "overlapping sequence expressions: outer construct gets no checking line")
    testthat::expect_true(object = base::grepl(x = txt55, pattern = "tempo_for_seq2", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "overlapping sequence expressions: inner construct modified")
    # a sequence expression that cannot be rewritten on a single line (comment inside the sequence span) is skipped: listed in the message, construct remains unmodified
    fun56 <- function(x){
        for(i in # c
            1:length(x)){
            base::print(i)
        }
    }
    testthat::expect_message(for_loop_nb_check(x = fun56, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "the sequence expression cannot be rewritten on a single line", fixed = TRUE)
    testthat::expect_true(object = base::is.null(x = for_loop_nb_check(x = fun56, safer_check = TRUE, lib_path = NULL, error_text = "")))
    # sequence expression written on exactly 2 lines (the regression fixed here: the middle-line extraction used a descending range)
    fun57 <- function(x){ for(i in
        1:length(x)){ base::print(i) } }
    r57 <- for_loop_nb_check(x = fun57, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r57), label = "sequence expression on 2 lines: modified function returned")
    testthat::expect_true(object = base::grepl(x = base::paste0(base::deparse(expr = r57, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE), pattern = "tempo_for_seq1 <- 1:length\\(x\\)", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), label = "sequence expression on 2 lines: checking line inserted")
    # sequence expression written on 3 lines
    fun58 <- function(x){ for(i in
        1:
        length(x)){ base::print(i) } }
    r58 <- for_loop_nb_check(x = fun58, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r58), label = "sequence expression on 3 lines: modified function returned")
    testthat::expect_true(object = base::grepl(x = base::paste0(base::deparse(expr = r58, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE), pattern = "tempo_for_seq1 <- 1:length\\(x\\)", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), label = "sequence expression on 3 lines: checking line inserted")
    # an unbraced for() construct whose body starts on the next line (wrap path, construct written on exactly 2 lines: the regression fixed here)
    fun59 <- function(x){ if(x > 1) for(i in 1:length(x))
        base::print(i) }
    r59 <- for_loop_nb_check(x = fun59, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r59), label = "2-line unbraced construct: modified function returned")
    testthat::expect_true(object = base::grepl(x = base::paste0(base::deparse(expr = r59, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE), pattern = "tempo_for_seq1", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "2-line unbraced construct: checking line inserted")
    # an unbraced for() construct whose body starts 2 lines after the for keyword (wrap path, construct written on 3 lines)
    fun60 <- function(x){ if(x > 1) for(i in
        1:length(x))
        base::print(i) }
    r60 <- for_loop_nb_check(x = fun60, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = r60), label = "3-line unbraced construct: modified function returned")
    testthat::expect_true(object = base::grepl(x = base::paste0(base::deparse(expr = r60, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE), pattern = "tempo_for_seq1", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "3-line unbraced construct: checking line inserted")
    # a reported for() construct whose sequence expression, written on several lines, contains another reported for() construct is also skipped: listed in the message, and only the inner construct is modified
    tempo_txt <- "function(x){\n    for(i in base::sapply(X = x, FUN = function(y){\n        for(j in 1:length(y)){ base::print(x = j) }\n        y\n    })){\n        base::print(x = i)\n    }\n}"
    fun55b <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    testthat::expect_message(object = saferDev::for_loop_nb_check(x = fun55b, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "line 2: the sequence expression of the for() construct contains another reported for() construct", fixed = TRUE)
    r55b <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun55b, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(base::suppressWarnings(expr = r55b(x = 1:2), classes = "warning"), file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(fun55b(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE))
    # a comment or a string written on several lines inside the sequence expression prevents the one-line writing: listed in the message, construct unmodified (the comment would otherwise be removed, and the line breaks of the string would become spaces)
    tempo_txt <- "function(x){\n    for(i in base::c(\"a\nb\", x)){\n        base::print(x = i)\n    }\n}"
    fun_mlstr1 <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    testthat::expect_message(object = saferDev::for_loop_nb_check(x = fun_mlstr1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "line 2: the sequence expression cannot be rewritten on a single line", fixed = TRUE)
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_mlstr1, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    tempo_txt <- "function(x){\n    for(i in x # comment\n    ){\n        base::print(x = i)\n    }\n}"
    fun_mlstr1b <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    testthat::expect_message(object = saferDev::for_loop_nb_check(x = fun_mlstr1b, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "line 2: the sequence expression cannot be rewritten on a single line", fixed = TRUE) # the comment would be removed
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_mlstr1b, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    # a statement of a braced block written on its own line, inside a sequence expression written on several lines: joining the lines would join the statements, so that the construct is listed and unmodified
    tempo_txt <- "function(x){\n    for(i in base::sapply(X = x, FUN = function(y){\n        z <- y\n        -1\n    })){\n        base::print(x = i)\n    }\n}"
    fun_mlstat <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    testthat::expect_message(object = saferDev::for_loop_nb_check(x = fun_mlstat, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "line 2: the sequence expression cannot be rewritten on a single line", fixed = TRUE)
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_mlstat, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    # a construct that requires the wrap and contains a string written on several lines is corrected: the string is kept
    tempo_txt <- "function(x){\n    if(base::length(x = x) > 0) for(i in 1:length(x))\n        base::cat(\"a\nb\", i)\n}"
    fun_mlstr2 <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_mlstr2_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_mlstr2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_print <- utils::capture.output(fun_mlstr2_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    tempo_print <- base::gsub(x = tempo_print, pattern = "if\\(base::length\\(x = tempo_for_seq[0-9]\\) == 0\\).*?\\} ; ", replacement = "<CHECKS> ; ", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) # the checking lines are replaced by <CHECKS>
    testthat::expect_identical(object = tempo_print[1:5], expected = base::c("function(x){", "    if(base::length(x = x) > 0) { tempo_for_seq1 <- 1:length(x) ; <CHECKS> ; for(i in tempo_for_seq1)", "        base::cat(\"a", "b\", i) }", "}"))
    testthat::expect_identical(object = utils::capture.output(fun_mlstr2_safer(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(fun_mlstr2(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE))
    # a construct that requires the wrap and is written on several lines keeps its lines: the statements of its body are not joined (the joined statements returned an internal error or, as here, silently changed the result: out <- out + i and -1 became out <- out + i -1)
    tempo_txt <- "function(x){\n    out <- 0\n    if(base::length(x = x) > 0) for(i in 1:length(x)){\n        out <- out + i\n        -1\n    }\n    out\n}"
    fun_wrap_ml <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_wrap_ml_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_wrap_ml, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_print <- utils::capture.output(fun_wrap_ml_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    tempo_print <- base::gsub(x = tempo_print, pattern = "if\\(base::length\\(x = tempo_for_seq[0-9]\\) == 0\\).*?\\} ; ", replacement = "<CHECKS> ; ", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) # the checking lines are replaced by <CHECKS>
    testthat::expect_identical(object = tempo_print[1:8], expected = base::c("function(x){", "    out <- 0", "    if(base::length(x = x) > 0) { tempo_for_seq1 <- 1:length(x) ; <CHECKS> ; for(i in tempo_for_seq1){", "        out <- out + i", "        -1", "    } }", "    out", "}"))
    testthat::expect_identical(object = fun_wrap_ml_safer(x = 1:3), expected = fun_wrap_ml(x = 1:3))
    # same, in the argument value of a function call
    tempo_txt <- "function(x){\n    tempo <- base::suppressWarnings(expr = for(i in 1:length(x)){\n        a <- i\n        base::print(x = a)\n    }, classes = \"warning\")\n}"
    fun_wrap_arg <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_wrap_arg_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_wrap_arg, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = utils::capture.output(fun_wrap_arg_safer(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(fun_wrap_arg(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE))
    # same, for a function without source references, whose deparsed code writes each statement on its own line (internal error before)
    fun_wrap_nosrc <- base::eval(expr = base::parse(file = "", n = NULL, text = "function(x){ out <- 0 ; base::invisible(x = for(i in 1:length(x)){ out <- out + i ; -1 }) ; out }", prompt = "?", keep.source = FALSE, srcfile = NULL, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_wrap_nosrc_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_wrap_nosrc, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::is.function(x = fun_wrap_nosrc_safer), label = "deparsed multi-line wrap: corrected function returned")
    testthat::expect_identical(object = fun_wrap_nosrc_safer(x = 1:3), expected = fun_wrap_nosrc(x = 1:3))
    # a comment written between the condition and the body written on the next line, in a braced block and in a construct that requires the wrap (internal error before: the comment is a child of the for expression in the parse data)
    tempo_txt <- "function(x){\n    out <- 0\n    for(i in 1:length(x)) # comment\n        out <- out + i\n    if(base::length(x = x) > 0) for(j in 1:length(x)) # comment\n        out <- out + j\n    out\n}"
    fun_com_header <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_com_header_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_com_header, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_print <- utils::capture.output(fun_com_header_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    tempo_print <- base::gsub(x = tempo_print, pattern = "if\\(base::length\\(x = tempo_for_seq[0-9]\\) == 0\\).*?\\} ; ", replacement = "<CHECKS> ; ", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) # the checking lines are replaced by <CHECKS>
    testthat::expect_identical(object = tempo_print[1:8], expected = base::c("function(x){", "    out <- 0", "    tempo_for_seq1 <- 1:length(x) ; <CHECKS> ; for(i in tempo_for_seq1) # comment", "        out <- out + i", "    if(base::length(x = x) > 0) { tempo_for_seq2 <- 1:length(x) ; <CHECKS> ; for(j in tempo_for_seq2) # comment", "        out <- out + j }", "    out", "}"))
    testthat::expect_identical(object = fun_com_header_safer(x = 1:3), expected = fun_com_header(x = 1:3))
    # the sequence expression is copied as written: non-syntactic names keep their backquotes (df$p-value would be df$p minus value), numbers keep all their digits, and the statements of a braced block written on a single line are not joined (deparse() wrote them on several lines, then joined with a space: z <- y ; -1 became z <- y -1)
    tempo_txt <- "function(x){\n    df <- base::data.frame(`p-value` = base::c(0.1, 0.2), p = base::c(5, 6), check.names = FALSE) ; value <- 1\n    for(v in df$`p-value`){ base::print(x = v) }\n    for(v in x + 1.123456789012345678){ base::print(x = v, digits = 17) }\n    for(v in base::sapply(X = x, FUN = function(y){ z <- y ; -1 })){ base::print(x = v) }\n}"
    fun_seq_txt <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_seq_txt_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_seq_txt, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_print <- utils::capture.output(fun_seq_txt_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    testthat::expect_true(object = base::grepl(x = tempo_print[3], pattern = "    tempo_for_seq1 <- df$`p-value` ; ", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "backquotes kept")
    testthat::expect_true(object = base::grepl(x = tempo_print[4], pattern = "    tempo_for_seq2 <- x + 1.123456789012345678 ; ", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "digits kept")
    testthat::expect_true(object = base::grepl(x = tempo_print[5], pattern = "    tempo_for_seq3 <- base::sapply(X = x, FUN = function(y){ z <- y ; -1 }) ; ", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "statements kept")
    testthat::expect_identical(object = utils::capture.output(base::suppressWarnings(expr = fun_seq_txt_safer(x = 1:2), classes = "warning"), file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(fun_seq_txt(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE))
    # TAB-indented function carrying its source references, with a TAB inside a string, TABs in the middle of the lines and a sequence written on 2 lines: the checking lines are inserted at the right positions and the TABs are kept (also the TAB inside the sequence expression, copied as written)
    tempo_txt <- "function(x){\n\ty <- \"a\tb\" ; for(i in 1:length(x)){ base::print(x = i) }\n\tif(base::length(x = x) > 0)\tfor(j in 1:\tlength(x))\tbase::print(x = j)\n\tfor(k in\n\t\t1:length(x)){\n\t\tbase::print(x = k)\n\t}\n}"
    fun_tab <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_tab_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_tab, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_print <- utils::capture.output(fun_tab_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    tempo_print <- base::gsub(x = tempo_print, pattern = "if\\(base::length\\(x = tempo_for_seq[0-9]\\) == 0\\).*?\\} ; ", replacement = "<CHECKS> ; ", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) # the checking lines are replaced by <CHECKS>
    testthat::expect_identical(object = tempo_print[1:8], expected = base::c("function(x){", "\ty <- \"a\tb\" ; tempo_for_seq1 <- 1:length(x) ; <CHECKS> ; for(i in tempo_for_seq1){ base::print(x = i) }", "\tif(base::length(x = x) > 0)\t{ tempo_for_seq2 <- 1:\tlength(x) ; <CHECKS> ; for(j in tempo_for_seq2)\tbase::print(x = j) }", "\ttempo_for_seq3 <- 1:length(x) ; <CHECKS> ; for(k in tempo_for_seq3", "){", "\t\tbase::print(x = k)", "\t}", "}"))
    testthat::expect_identical(object = utils::capture.output(fun_tab_safer(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(fun_tab(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE))
    # NULL returned invisibly when nothing is corrected: clean function, or reported loops all left unchanged
    testthat::expect_invisible(call = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun3, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    testthat::expect_invisible(call = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun54b, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    # the returned function keeps the environment of the submitted function
    tempo_env <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
    fun_env <- base::eval(expr = base::parse(file = "", n = NULL, text = "function(x){ for(i in 1:length(x)){ base::print(x = i) } }", prompt = "?", keep.source = FALSE, srcfile = NULL, encoding = "unknown")[[1]], envir = tempo_env, enclos = base::environment(fun = NULL))
    fun_env_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_env, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = base::environment(fun = fun_env_safer), expected = tempo_env)
    #### end main code

    ######## export and out_path arguments
    # out_path: NULL (default) accepted when export is FALSE, error when export is TRUE (whatever the function), and no empty string, whatever the value of export
    testthat::expect_error(object = saferDev::for_loop_nb_check(x = fun20, export = TRUE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN export IS TRUE", fixed = TRUE)
    testthat::expect_error(object = saferDev::for_loop_nb_check(x = fun3, export = TRUE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN export IS TRUE", fixed = TRUE)
    testthat::expect_no_error(object = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun20, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    testthat::expect_true(object = base::is.function(x = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun20), classes = "message")), label = "default call: corrected function returned") # default values of all the arguments except x
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = "", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    testthat::expect_error(for_loop_nb_check(x = fun20, export = FALSE, out_path = "", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    # export argument values
    testthat::expect_error(for_loop_nb_check(x = fun20, export = NA, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = "TRUE", safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = base::c(TRUE, FALSE), safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = mat1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = factor1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = expr1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = fun1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(for_loop_nb_check(x = fun20, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""))
    # out_path values
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = 1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = factor1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = expr1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = fun1, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = base::c("a", "b"), safer_check = FALSE, lib_path = NULL, error_text = ""))
    # out_path not used when export = FALSE: a non-existing path is accepted, but a non-NULL value must still be a single non-empty character string
    testthat::expect_no_error(for_loop_nb_check(x = fun20, export = FALSE, out_path = "PATH_NOT_GOOD", safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_error(object = saferDev::for_loop_nb_check(x = fun20, export = FALSE, out_path = 1, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "THE out_path ARGUMENT MUST BE TYPEOF character", fixed = TRUE)
    # export = TRUE with a function to modify: file written in a fresh temporary directory, NULL returned (invisible), and the file recreates the function when sourced
    tempo_dir_forloop <- base::tempfile(pattern = "fl_export_")
    base::dir.create(path = tempo_dir_forloop, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    testthat::expect_null(object = for_loop_nb_check(x = fun20, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""), label = "export TRUE returns NULL")
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_forloop, "fun20_safer.R", fsep = "/")), label = "fun20_safer.R written")
    exported_lines <- base::readLines(con = base::file.path(tempo_dir_forloop, "fun20_safer.R", fsep = "/"), warn = FALSE, ok = TRUE)
    testthat::expect_true(object = base::grepl(x = exported_lines[1], pattern = "^fun20_safer <- ", perl = TRUE), label = "exported file starts with the function writing")
    testthat::expect_true(object = base::any(base::grepl(x = exported_lines, pattern = "tempo_for_seq", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), na.rm = TRUE), label = "exported code contains the checking line")
    tempo_env_forloop <- base::new.env(parent = base::globalenv())
    base::sys.source(file = base::file.path(tempo_dir_forloop, "fun20_safer.R", fsep = "/"), envir = tempo_env_forloop, chdir = FALSE)
    testthat::expect_identical(object = utils::capture.output(tempo_env_forloop$fun20_safer(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(fun20(x = 1:2), file = NULL, append = FALSE, type = NULL, split = FALSE), label = "exported function behaves like the submitted one") # the printed output is captured and compared
    # export message: the file location is announced by a message
    base::unlink(x = base::file.path(tempo_dir_forloop, "fun20_safer.R", fsep = "/"))
    testthat::expect_message(for_loop_nb_check(x = fun20, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    # pre-existing file is never overwritten
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "FILE NAME ALREADY EXISTS", fixed = TRUE)
    # clean function (fun3 has clean for() sequences): no file written, NULL returned
    testthat::expect_null(object = for_loop_nb_check(x = fun3, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_forloop, "fun3_safer.R", fsep = "/")))
    # reported for() constructs with no checking line inserted: no file written either
    testthat::expect_null(object = for_loop_nb_check(x = fun5, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir_forloop, "fun5_safer.R", fsep = "/")))
    # file name determination: written as pkg::fun, the package name is not part of the file name (mean contains no for() construct, so a file is not written: use a reported function through pkg::fun)
    fun53 <- function(x){ for(i in 1:length(x)){ base::print(x = i) } }
    base::assign(x = "fun53_in_pkg", value = fun53, envir = base::globalenv())
    testthat::expect_null(object = for_loop_nb_check(x = fun53_in_pkg, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_forloop, "fun53_in_pkg_safer.R", fsep = "/")), label = "fun53_in_pkg_safer.R written for the bare name form")
    base::unlink(x = base::file.path(tempo_dir_forloop, "fun53_in_pkg_safer.R", fsep = "/"))
    # same, with the explicit pkg::fun written form: the package name is not part of the file name (stats::factanal contains a reported for() construct)
    testthat::expect_null(object = for_loop_nb_check(x = stats::factanal, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_forloop, "factanal_safer.R", fsep = "/")), label = "factanal_safer.R written for the pkg::fun form")
    base::unlink(x = base::file.path(tempo_dir_forloop, "factanal_safer.R", fsep = "/"))
    # inline definition and component writing are refused when export = TRUE (the name of the exported file could not be determined reliably)
    testthat::expect_error(for_loop_nb_check(x = function(y){ for(i in 1:length(y)){ base::print(x = i) } }, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    FUN_LIST <- base::list(fun = fun20)
    testthat::expect_error(for_loop_nb_check(x = FUN_LIST$fun, export = TRUE, out_path = tempo_dir_forloop, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    # non-existing out_path
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = "PATH_NOT_GOOD", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXIST:\n", fixed = TRUE)
    # out_path with a trailing /: the trailing / is removed first, then the path checked (the error message shows the path without the trailing /)
    testthat::expect_error(for_loop_nb_check(x = fun20, export = TRUE, out_path = "PATH_NOT_GOOD/", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "DOES NOT EXIST:\nPATH_NOT_GOOD\n", fixed = TRUE)
    # trailing / and \\ tolerated in out_path
    base::unlink(x = base::file.path(tempo_dir_forloop, "fun20_safer.R", fsep = "/"))
    testthat::expect_null(object = for_loop_nb_check(x = fun20, export = TRUE, out_path = base::paste0(tempo_dir_forloop, "/"), safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_forloop, "fun20_safer.R", fsep = "/")))
    base::unlink(x = base::file.path(tempo_dir_forloop, "fun20_safer.R", fsep = "/"))
    testthat::expect_null(object = for_loop_nb_check(x = fun20, export = TRUE, out_path = base::paste0(tempo_dir_forloop, "\\"), safer_check = FALSE, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir_forloop, "fun20_safer.R", fsep = "/")))
    # cleanup
    base::unlink(x = tempo_dir_forloop, recursive = TRUE, force = TRUE, expand = TRUE)
    base::rm(list = "fun53_in_pkg", envir = base::globalenv())
    ######## end export and out_path arguments

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # checks added by the correction and executed by the corrected function: the negative and decreasing checks only concern the numeric sequences (no error for factors or complex numbers, no pointless warning for decreasing characters or dates)
    fun_seq_kind <- function(x){
        for(v in x){
            base::invisible(x = v)
        }
    }
    fun_seq_kind_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_seq_kind, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::is.function(x = fun_seq_kind_safer))
    testthat::expect_no_error(object = fun_seq_kind_safer(x = base::c(1i, 2i))) # complex: no ordering, previously an error
    testthat::expect_no_warning(object = fun_seq_kind_safer(x = base::c(2i, 1i)))
    testthat::expect_no_error(object = fun_seq_kind_safer(x = base::factor(x = base::c("b", "a"), levels = base::c("a", "b"), labels = base::c("a", "b"), exclude = NA, ordered = FALSE, nmax = NA))) # factor: previously an error (NA in the negative check)
    testthat::expect_no_warning(object = fun_seq_kind_safer(x = base::factor(x = base::c("b", "a"), levels = base::c("a", "b"), labels = base::c("a", "b"), exclude = NA, ordered = FALSE, nmax = NA)))
    testthat::expect_no_warning(object = fun_seq_kind_safer(x = base::c("b", "a"))) # decreasing characters, like for(nm in base::names(x = x)): previously a warning of decreasing sequence
    testthat::expect_no_warning(object = fun_seq_kind_safer(x = base::as.Date(x = base::c("2024-02-01", "2024-01-01"), format = "%Y-%m-%d"))) # decreasing dates: previously a warning of decreasing sequence
    testthat::expect_no_warning(object = fun_seq_kind_safer(x = base::list(2, 1))) # list
    testthat::expect_warning(object = fun_seq_kind_safer(x = base::c(3, 1)), regexp = "FOR SEQUENCE STARTING DECREASING", fixed = TRUE) # numeric decreasing: still reported
    testthat::expect_warning(object = fun_seq_kind_safer(x = base::c(-1L, -2L, -3L)), regexp = "FOR SEQUENCE WITH ALL NEGATIVE VALUES", fixed = TRUE) # integer negative: still reported
    testthat::expect_warning(object = fun_seq_kind_safer(x = base::c(3, 1, 2)), regexp = "FOR SEQUENCE STARTING DECREASING", fixed = TRUE) # starts decreasing: first value greater than the second
    testthat::expect_no_warning(object = fun_seq_kind_safer(x = base::c(1, 3, 2))) # does not start decreasing
    testthat::expect_warning(object = fun_seq_kind_safer(x = base::c(1, NA)), regexp = "FOR SEQUENCE WITH NA VALUES", fixed = TRUE) # NA values: still reported
    testthat::expect_no_warning(object = fun_seq_kind_safer(x = base::expression(a, b))) # expression vector: base::anyNA() not applied (it returned an error)
    # expression vector as for() sequence, e.g., for(e in base::parse(text = code)), in a function indented with spaces and in a function indented with TABs: the corrected function runs like the submitted one (base::anyNA() of the checking line returned an error for an expression vector, also for the TAB-indented function, left unmodified before)
    for(tempo_indent in base::c("    ", "\t")){
        tempo_txt <- base::paste0("function(code){\n", tempo_indent, "n <- 0\n", tempo_indent, "for(e in base::parse(text = code)){\n", tempo_indent, tempo_indent, "n <- n + 1\n", tempo_indent, "}\n", tempo_indent, "n\n}", collapse = NULL, recycle0 = FALSE)
        fun_expr_seq <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        fun_expr_seq_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_expr_seq, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
        testthat::expect_true(object = base::is.function(x = fun_expr_seq_safer), label = base::paste0("expression sequence, indentation \"", tempo_indent, "\": corrected function returned", collapse = NULL, recycle0 = FALSE))
        testthat::expect_true(object = base::grepl(x = base::paste0(utils::capture.output(fun_expr_seq_safer, file = NULL, append = FALSE, type = NULL, split = FALSE), collapse = "\n", recycle0 = FALSE), pattern = base::paste0("\n", tempo_indent, "tempo_for_seq1 <- base::parse(text = code) ; ", collapse = NULL, recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = base::paste0("expression sequence, indentation \"", tempo_indent, "\": checking line inserted", collapse = NULL, recycle0 = FALSE))
        testthat::expect_identical(object = fun_expr_seq_safer(code = "a; b"), expected = fun_expr_seq(code = "a; b"))
        testthat::expect_identical(object = fun_expr_seq_safer(code = "a; b"), expected = 2)
        testthat::expect_warning(object = fun_expr_seq_safer(code = "a"), regexp = "FOR SEQUENCE WITH A SINGLE VALUE", fixed = TRUE) # the following checks still work
    }
    testthat::expect_true(object = base::grepl(x = base::paste0(utils::capture.output(fun_seq_kind_safer, file = NULL, append = FALSE, type = NULL, split = FALSE), collapse = "\n", recycle0 = FALSE), pattern = "base::anyNA(x = tempo_for_seq1, recursive = FALSE)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "all the arguments of anyNA() written in the checking line")
    # comments, line breaks and indentation of the submitted function kept in the returned function (srcref of the returned function)
    fun_com <- base::eval(expr = base::parse(text = "function(x){\n    # comment 1\n    for(i in 1:length(x)){ # comment 2\n        base::print(i)\n    }\n}", keep.source = TRUE)[[1]]) # warning: the short parse writing is required here: a fully-named writing with srcfile = NULL removes the srcref of the parsed code
    fun_com_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_com, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_print <- utils::capture.output(fun_com_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    testthat::expect_identical(object = tempo_print[base::c(1:2, 4:6)], expected = base::c("function(x){", "    # comment 1", "        base::print(i)", "    }", "}"))
    testthat::expect_true(object = base::grepl(x = tempo_print[3], pattern = "; for(i in tempo_for_seq1){ # comment 2", fixed = TRUE))
    # attributes of x, printed by R after its code: saferDev error that names them, raised before the code of x is recovered as printed (formerly, the printed attribute values were analyzed as code of x: raw error of the parser when they do not parse, e.g., a character string printed with its [1] prefix, and silent analysis when they parse, e.g., a formula or a function, with wrong results)
    fun_attr <- function(x){x}
    base::attr(x = fun_attr, which = "note") <- "hello"
    fun_attr_parse <- function(x){for(i in 1:length(x)){x[i]}}
    base::attr(x = fun_attr_parse, which = "fo") <- y ~ x
    base::attr(x = fun_attr_parse, which = "fn") <- function(z){z}
    testthat::expect_error(object = saferDev::for_loop_nb_check(x = fun_attr, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::for_loop_nb_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nnote\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::for_loop_nb_check(x = fun_attr_parse, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::for_loop_nb_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nfo\nfn\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # remedy given by the error message: once the attribute is removed, the function is checked. The attributes that print() does not show (source references and comment) are accepted
    base::attr(x = fun_attr, which = "note") <- NULL
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_attr, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = NULL)
    base::comment(x = fun_attr) <- "comment not printed by print()"
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_attr, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = NULL)
    # object inserted in the body of x that is not printed as R code: saferDev error with the message of the R parser (only its position is compared, because the parser messages are translated), instead of the raw error of the parser
    fun_body_env <- function(x){x}
    base::body(fun = fun_body_env, envir = base::environment(fun = fun_body_env)) <- base::call(name = "identity", base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L))
    testthat::expect_error(object = saferDev::for_loop_nb_check(x = fun_body_env, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::for_loop_nb_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS CODE, AS PRINTED BY R, DOES NOT PARSE (E.G., AN OBJECT INSERTED IN ITS BODY THAT IS NOT PRINTED AS R CODE, LIKE AN ENVIRONMENT, PRINTED <environment>).\n\nERROR MESSAGE OF THE R PARSER:\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::for_loop_nb_check(x = fun_body_env, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "\n\nERROR MESSAGE OF THE R PARSER:\n<text>:[0-9]+:[0-9]+: ", class = NULL, fixed = FALSE, inherit = TRUE, info = NULL, label = NULL)
    # option keep.parse.data set to FALSE (e.g., by base::sys.source() during the loading of a file): the parse data are kept anyway (same result as with the default TRUE), and the option is restored on exit
    fun_kpd <- function(x){for(i in 1:length(x)){x[i]}}
    res_kpd_ref <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_kpd, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    oop_kpd <- base::options(keep.parse.data = FALSE)
    res_kpd <- base::tryCatch(expr = base::list(fun = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_kpd, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), option = base::getOption(x = "keep.parse.data", default = NULL)), error = function(e){base::list(fun = e, option = NULL)}, finally = base::options(oop_kpd))
    testthat::expect_true(object = base::is.function(x = res_kpd$fun), info = NULL, label = NULL)
    testthat::expect_identical(object = utils::capture.output(res_kpd$fun, file = NULL, append = FALSE, type = NULL, split = FALSE), expected = utils::capture.output(res_kpd_ref, file = NULL, append = FALSE, type = NULL, split = FALSE), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_false(object = res_kpd$option, info = NULL, label = NULL)
    # quoted code: the for() loops inside quote(), bquote(), expression(), alist() and substitute(), code that R does not evaluate as written, get no checking line and are not reported, whatever the writing of the call (base::, base:::, backquotes, string, native pipe), with or without source references (formerly, the quoted loop was replaced by the checking lines, so that the quoted code returned by these calls changed). Warning: each function is built from its text with source references (source code checked), and without them (keep.source = FALSE and srcfile = NULL: deparsed code checked)
    tempo_txt_quote <- base::c(
        "function(){quote(for(i in 1:length(x)) x)}",
        "function(){base::quote(expr = {for(i in 1:length(x)) x})}",
        "function(n){base::bquote(for(i in 1:.(n)) x)}", # evaluated .() part of bquote()
        "function(){base::expression(for(i in 1:n) i)}",
        "function(){base::alist(a = for(i in x) i)}",
        "function(){base::substitute(expr = for(i in s) i, env = base::list(s = 1:3))}", # evaluated env argument of substitute()
        "function(){quote(base:::bquote(function(x){ for(i in 1:length(x)) x }))}", # nested quoting calls
        "function(){`quote`(for(i in x) i) ; \"expression\"(for(i in x) i) ; (for(i in x) i) |> quote()}", # backquoted and string function names, native pipe
        "function(x){base::bquote(for(i in 1:length(x)) .(x))}", # loop in the expr argument of bquote(), with a .() call
        "function(x){y ~ for(i in 1:length(x)) NULL}", # formula
        "function(x){base::bquote(f(..(for(i in 1:length(x)) NULL)))}", # ..() without splice = TRUE
        "function(x){base::substitute(a, NULL, for(i in 1:length(x)) NULL) ; base::bquote(x, w = for(i in 1:length(x)) NULL, wh = NULL)}", # arguments of calls whose argument matching fails (error of R)
        "function(x){(for(i in 1:length(x)) NULL) |> base::bquote(where = base::environment())}" # expr argument of bquote() written with the native pipe
    )
    for(i1 in base::seq_along(along.with = tempo_txt_quote)){
        for(tempo_keep in base::c(TRUE, FALSE)){
            fun_quote <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt_quote[i1], prompt = "?", keep.source = tempo_keep, srcfile = if(tempo_keep == TRUE){base::srcfilecopy(filename = "<text>", lines = tempo_txt_quote[i1], timestamp = base::Sys.time(), isFile = FALSE)}else{NULL}, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            tempo_label <- base::paste0(tempo_txt_quote[i1], " (source references: ", tempo_keep, ")", collapse = NULL, recycle0 = FALSE)
            testthat::expect_message(object = saferDev::for_loop_nb_check(x = fun_quote, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = tempo_label)
            testthat::expect_null(object = base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_quote, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = tempo_label)
        }
    }
    # loop outside the quoting call, on the lines of a multi-line quoting call indented with TABs: still reported and guarded (tempo_for_seq1, the quoted loop being no longer counted), while the quoted loop is kept, so that the corrected function returns the same quoted code
    tempo_txt_quote2 <- "function(x){\n\te <- base::quote(expr = for(i in 1:n)\n\t\t1) ; s <- 0 ; for(j in 1:length(x)) s <- s + j\n\tbase::list(e, s)\n}"
    fun_quote2 <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt_quote2, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_txt_quote2, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    msgs_quote2 <- testthat::capture_messages(code = saferDev::for_loop_nb_check(x = fun_quote2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res_quote2 <- base::unlist(x = base::strsplit(x = msgs_quote2, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    res_quote2 <- res_quote2[base::grepl(x = res_quote2, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
    testthat::expect_identical(object = base::substr(x = res_quote2, start = 1, stop = 5), expected = "3\tfor", info = NULL, label = NULL, expected.label = NULL)
    fun_quote2_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_quote2, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    tempo_print <- utils::capture.output(fun_quote2_safer, file = NULL, append = FALSE, type = NULL, split = FALSE)
    testthat::expect_identical(object = tempo_print[base::c(1:2, 4:5)], expected = base::c("function(x){", "\te <- base::quote(expr = for(i in 1:n)", "\tbase::list(e, s)", "}"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_true(object = base::startsWith(x = tempo_print[3], prefix = "\t\t1) ; s <- 0 ; tempo_for_seq1 <- 1:length(x) ; "), info = NULL, label = NULL)
    testthat::expect_identical(object = fun_quote2_safer(x = 1:3), expected = fun_quote2(x = 1:3), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_warning(object = fun_quote2_safer(x = base::character(length = 0)), regexp = "FOR SEQUENCE STARTING DECREASING", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL) # 1:length(x) is c(1, 0)
    # code evaluated by R inside bquote() and substitute(): the first argument of the .() calls of bquote() (and of its ..() calls if splice = TRUE or T is written), the where and splice arguments of bquote() and the env argument of substitute(), whatever their writing (partial, quoted or backquoted argument names, positions, native pipe with or without the placeholder _), are evaluated: their loops are reported and guarded, and the corrected function gives the same results and warns as the other corrected loops (formerly, the whole argument list of these calls was skipped). Each function is built from its text with source references and without them
    tempo_txt_eval <- base::c(
        "function(x){base::bquote(.(for(i in 1:length(x)) NULL))}", # .() of bquote()
        "function(x){base::substitute(expr = u, env = base::list(u = for(i in 1:length(x)) NULL))}", # env argument of substitute()
        "function(x){base::bquote(`expr` = a, \"wh\" = base::as.environment(x = base::list(b = for(i in 1:length(x)) NULL)))}", # where argument of bquote(), argument names between backquotes and quotes, partial name
        "function(x){base::bquote(f(..(base::list(for(i in 1:length(x)) NULL))), splice = TRUE)}", # ..() with splice = TRUE
        "function(x){base::list(u = for(i in 1:length(x)) NULL) |> base::substitute(expr = u, env = _)}", # env argument written with the placeholder
        "function(x){x |> base::substitute(base::list(u = for(i in 1:length(x)) NULL))}" # expr argument written with the native pipe (not checked), env argument written by position
    )
    for(i1 in base::seq_along(along.with = tempo_txt_eval)){
        for(tempo_keep in base::c(TRUE, FALSE)){
            fun_eval <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_txt_eval[i1], prompt = "?", keep.source = tempo_keep, srcfile = if(tempo_keep == TRUE){base::srcfilecopy(filename = "<text>", lines = tempo_txt_eval[i1], timestamp = base::Sys.time(), isFile = FALSE)}else{NULL}, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
            tempo_label <- base::paste0(tempo_txt_eval[i1], " (source references: ", tempo_keep, ")", collapse = NULL, recycle0 = FALSE)
            msgs_eval <- testthat::capture_messages(code = saferDev::for_loop_nb_check(x = fun_eval, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
            res_eval <- base::unlist(x = base::strsplit(x = msgs_eval, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
            res_eval <- res_eval[base::grepl(x = res_eval, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
            testthat::expect_identical(object = base::vapply(X = base::strsplit(x = res_eval, split = "\t", fixed = TRUE, perl = FALSE, useBytes = FALSE), FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1), USE.NAMES = FALSE), expected = "for", info = NULL, label = tempo_label, expected.label = NULL)
            fun_eval_safer <- base::suppressMessages(expr = saferDev::for_loop_nb_check(x = fun_eval, export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
            testthat::expect_identical(object = fun_eval_safer(x = 1:3), expected = fun_eval(x = 1:3), info = NULL, label = tempo_label, expected.label = NULL)
            testthat::expect_warning(object = fun_eval_safer(x = base::character(length = 0)), regexp = "FOR SEQUENCE STARTING DECREASING", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = tempo_label) # 1:length(x) is c(1, 0)
        }
    }
    # loop in the argument of .(): same correction as a loop in the argument of another call, enclosed in curly brackets with its checking lines
    tempo_txt_eval2 <- base::c("function(x){f(for(i in 1:length(x)) NULL)}", "function(x){base::bquote(.(for(i in 1:length(x)) NULL))}")
    tempo_print_eval2 <- base::vapply(X = tempo_txt_eval2, FUN = function(x){utils::capture.output(base::suppressMessages(expr = saferDev::for_loop_nb_check(x = base::eval(expr = base::parse(file = "", n = NULL, text = x, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = x, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), export = FALSE, out_path = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), file = NULL, append = FALSE, type = NULL, split = FALSE)[1]}, FUN.VALUE = base::character(length = 1), USE.NAMES = FALSE) # first line of each corrected function
    testthat::expect_identical(object = tempo_print_eval2[2], expected = base::paste0("function(x){base::bquote(.(", base::substr(x = tempo_print_eval2[1], start = 15, stop = base::nchar(x = tempo_print_eval2[1], type = "chars", allowNA = FALSE, keepNA = NA) - 2), "))}", collapse = NULL, recycle0 = FALSE), info = NULL, label = NULL, expected.label = NULL) # the code between the brackets of f(), from the character 15 (after "function(x){f(") to the last but two (before ")}"), is the code between the brackets of .()
    testthat::expect_true(object = base::startsWith(x = tempo_print_eval2[2], prefix = "function(x){base::bquote(.({ tempo_for_seq1 <- 1:length(x) ; if(base::length(x = tempo_for_seq1) == 0){"), info = NULL, label = NULL)
    ## end other tests

})
