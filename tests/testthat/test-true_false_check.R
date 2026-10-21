# This file tests true_false_check()
# Same structure as test-curly_bracket_check.R

testthat::test_that("true_false_check()", {

    ## data argument values
    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    # functions with and without T or F used as values
    fun3 <- function(x){ if(x > 1){ base::return(TRUE) } }
    fun4 <- function(x){ if(x == T){ base::return(x) } }
    fun5 <- function(x){ base::ifelse(test = x > 1, yes = TRUE, no = FALSE) }
    fun6 <- function(x){ base::ifelse(test = x > 1, yes = T, no = F) }
    fun7 <- function(x){ base::c(TRUE, FALSE) }
    fun8 <- function(x){ base::c(T, F) }
    fun9 <- function(x){ x <- T ; base::return(x) } # assignment value: not reported
    fun10 <- function(T){ if(T){ base::print("ok") } } # formal name: not reported
    fun11 <- function(df){ df$T ; df@F ; for(T in 1:3){ base::print(T) } } # names: not reported
    fun12 <- function(x){ base::f(x = T) ; base::f(T) } # named + positional argument values: reported
    fun13 <- function(n, m){ n[T] ; m[[F]] } # indexing values: reported
    fun14 <- function(x){ y ~ T ; ?T ; `T` <- 1 ; base::print("ok") } # formula, help and backquotes: not reported
    fun15 <- function(x){ x <- "T is here" ; base::print(x) } # string: not reported
    fun16 <- function(x){ x == T # T in comment: the code T before the comment is reported, but the comment itself is immune
    }
    fun16b <- function(x){ # only comments mentioning T and F
        # T and F in comment
        base::print("ok")
    }
    fun17 <- function(x){ base::T ; x == base::F } # fully qualified symbols: reported (fix: base::TRUE)
    fun18 <- function(x){ mean(x) } # unqualified function call: not checked (no colons pre-check): no T or F value: clean
    fun19 <- function(x){ local_fun <- function(y){ y == T } ; local_fun(x) } # internal function definition: no error, T reported
    fun20 <- function(x){ f1 <- utils::getFromNamespace(x = "sum", ns = "base", pos = , envir = ) ; f1(TRUE) } # getFromNamespace definition: no error, no T or F value: clean
    fun21 <- function(x){ T() } # function name T(): not a value symbol: not reported
    fun22 <- function(x){ T ; F } # bare value symbols: reported
    fun23 <- function(x){ x == T ; T <- 1 ; x == T } # local definition of T: not reported
    fun24 <- base::mean # UseMethod body: no T or F value: clean
    fun25 <- function(x){ base::print(\(y) y == T) } # lambda body: reported
    fun26 <- function(x){ T -> y ; base::ifelse(test = x, yes = T, no = FALSE) } # T -> y: T is an assignment value (first one not reported), the ifelse T is reported
    fun27 <- function(x){ y -> T ; base::ifelse(test = x, yes = T, no = FALSE) } # y -> T: T is a definition target, nothing reported
    fun28 <- function(df){ df$T <- 1 ; x == T } # df$T <- 1 is not a definition of a variable named T: the x == T usage is reported
    fun29 <- function(df){ df@F <- 1 ; x == F } # idem for the slot writing
    fun30 <- function(x){ x$F = 1 ; x == F } # idem for the = writing
    fun31 <- function(x){ ?(T) ; base::print(x) } # help with parenthesized topic: not reported
    fun32 <- function(x){ T ~ y ; base::print(x) } # formula left-hand side: not reported
    fun32b <- function(x){ y ~ x + T ; base::print(x) } # formula symbol anywhere in the formula: not reported
    fun32c <- function(x){ ?(((T))) ; base::print(x) } # deep-parenthesized help topic: not reported
    fun33 <- function(df, x){ df$mean(x) ; base::print("ok") } # component function call: not checked (no colons pre-check)
    fun34 <- function(df, x){ df$mean <- function(y){ y } ; mean(x) } # component assignment and bare mean(): not checked (no colons pre-check): clean
    fun35 <- function(obj, x){ obj$x(T) ; base::print(x) } # component function call: not checked (no colons pre-check), T still reported
    fun36 <- function(x){ df <- 1 ; ?(T) ; base::print(df) } # help parenthesized topic with a deparse without srcref: not reported either way
    fun37 <- function(x){ T ->> w2 ; base::ifelse(test = x, yes = T, no = FALSE) } # ->> behaves like ->: the ifelse T is reported once
    fun38 <- function(x){ T <<- 1 ; x == T } # T <<- 1 is a definition target: nothing reported
    fun39 <- function(x){ a <- b <- T ; base::ifelse(test = x, yes = T, no = FALSE) } # multi-assignment: the trailing T is an assignment value (not reported), the ifelse T is a genuine value (reported)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(true_false_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::true_false_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(true_false_check()) # the function has default values for all args except x.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(true_false_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(true_false_check(x = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))

    testthat::expect_error(true_false_check(x = fun3, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(true_false_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(true_false_check(x = fun3, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    # safer_check must be TRUE
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.0: unguarded lib_path check)
    testthat::expect_error(true_false_check(x = fun3, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_message(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    # end x
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = "", error_text = ""))
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    # true_false_check() only checks the T and F symbols used as values: no package-association pre-check (unlike all_args_here(), colons_check() does not have to be run first)
    testthat::expect_message(true_false_check(x = fun18, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # unqualified function call: not checked, no T or F value: clean
    testthat::expect_message(true_false_check(x = fun21, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # T used as function name: not a value symbol: not reported
    testthat::expect_message(true_false_check(x = fun24, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # UseMethod body: no T or F value: clean
    testthat::expect_no_error(true_false_check(x = fun19, safer_check = TRUE, lib_path = NULL, error_text = "")) # internal function definition: no error, T still reported (see below)
    testthat::expect_message(true_false_check(x = fun20, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # getFromNamespace function definition: no T or F value: clean
    testthat::expect_no_error(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = "")) # no function call at all: accepted
    # detection of T or F used as values
    testthat::expect_message(true_false_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(true_false_check(x = fun5, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(true_false_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(true_false_check(x = fun9, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # assignment value not reported
    testthat::expect_message(true_false_check(x = fun10, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # formal name not reported
    testthat::expect_message(true_false_check(x = fun11, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # names ($, @, loop variable) not reported
    testthat::expect_message(true_false_check(x = fun14, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # formula, help and backquotes not reported
    testthat::expect_message(true_false_check(x = fun15, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # strings are immune
    testthat::expect_message(true_false_check(x = fun16, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # code before a comment is reported, comments are ignored
    testthat::expect_message(true_false_check(x = fun16b, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # only comments mentioning T and F: immune
    testthat::expect_message(true_false_check(x = fun23, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # local definition of T: not reported
    testthat::expect_message(true_false_check(x = true_false_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # self check
    testthat::expect_message(true_false_check(x = colons_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(true_false_check(x = arg_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(true_false_check(x = curly_bracket_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    testthat::expect_message(true_false_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE)
    testthat::expect_message(true_false_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE)
    testthat::expect_message(true_false_check(x = fun8, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE)
    testthat::expect_message(true_false_check(x = fun12, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # named + positional argument values
    testthat::expect_message(true_false_check(x = fun13, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # indexing values
    testthat::expect_message(true_false_check(x = fun17, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # fully qualified symbols also reported
    testthat::expect_message(true_false_check(x = fun19, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE)
    testthat::expect_message(true_false_check(x = fun22, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # bare value symbols
    testthat::expect_message(true_false_check(x = fun25, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # lambda body
    testthat::expect_message(true_false_check(x = fun26, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # T -> y: T is an assignment value, but the ifelse T is a genuine value: reported
    msgs <- testthat::capture_messages(true_false_check(x = fun26, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res <- base::unlist(x = base::strsplit(x = msgs, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res_fields <- base::strsplit(x = res, split = "\t", fixed = TRUE)
    testthat::expect_true(object = base::length(x = res_fields) == 1, label = "fun26 single finding") # the T of T -> y must NOT be reported
    testthat::expect_message(true_false_check(x = fun27, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # y -> T: T is a definition target: nothing reported
    testthat::expect_message(true_false_check(x = fun28, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # df$T <- 1 is not a definition of a variable named T
    testthat::expect_message(true_false_check(x = fun29, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # idem for the slot writing
    testthat::expect_message(true_false_check(x = fun30, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # idem for the = writing
    testthat::expect_message(true_false_check(x = fun31, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # help with parenthesized topic: not reported
    testthat::expect_message(true_false_check(x = fun32, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # formula left-hand side: not reported
    testthat::expect_message(true_false_check(x = fun32b, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # formula symbol anywhere in the formula: not reported
    testthat::expect_message(true_false_check(x = fun32c, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # deep-parenthesized help topic: not reported
    testthat::expect_message(true_false_check(x = fun33, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # component function call: not checked (no colons pre-check)
    testthat::expect_message(true_false_check(x = fun34, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # component assignment and bare mean(): not checked (no colons pre-check): clean
    testthat::expect_message(true_false_check(x = fun35, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # component function call: not checked, T still reported
    testthat::expect_message(true_false_check(x = fun36, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE)
    fun36b <- base::eval(expr = base::parse(text = "function(x){ df <- 1 ; ?(T) ; base::print(df) }", file = "", n = NULL, prompt = "?", keep.source = FALSE, srcfile = NULL, encoding = "unknown")) # function built without srcref: deparsed as `?`((T)) by utils::capture.output()
    testthat::expect_message(true_false_check(x = fun36b, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # deparsed `?` call: the `?` function-call token is skipped by the identifier check, and the topic is in the parent-of-parent subtree: not reported
    testthat::expect_message(true_false_check(x = fun37, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # ->> behaves like ->
    msgs37 <- testthat::capture_messages(true_false_check(x = fun37, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res37 <- base::unlist(x = base::strsplit(x = msgs37, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res37 <- res37[base::grepl(x = res37, pattern = "^[0-9]+\\t", perl = TRUE)]
    testthat::expect_true(object = base::length(x = res37) == 1, label = "fun37 single finding") # the T of T ->> w2 must NOT be reported
    testthat::expect_message(true_false_check(x = fun38, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # T <<- 1 is a definition target: nothing reported
    testthat::expect_message(true_false_check(x = fun39, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE", fixed = TRUE) # multi-assignment: only the ifelse T reported
    msgs39 <- testthat::capture_messages(true_false_check(x = fun39, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res39 <- base::unlist(x = base::strsplit(x = msgs39, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res39 <- res39[base::grepl(x = res39, pattern = "^[0-9]+\\t", perl = TRUE)]
    testthat::expect_true(object = base::length(x = res39) == 1, label = "fun39 single finding") # the trailing T of a <- b <- T must NOT be reported
    testthat::expect_message(true_false_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun4(), SOME SYMBOLS T OR F", fixed = TRUE) # message normalization of the checked function name
    # exact findings for a representative case (only the LINE and SYMBOL columns are compared, because the CODE column depends on the srcref presence of the checked function). Warning: utils::capture.output(type = "message") does not work inside testthat (the message connection is redirected), so testthat::capture_messages() is used
    msgs <- testthat::capture_messages(true_false_check(x = fun6, safer_check = FALSE, lib_path = NULL, error_text = ""))
    res <- base::unlist(x = base::strsplit(x = msgs, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res_fields <- base::strsplit(x = res, split = "\t", fixed = TRUE)
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[1]}, FUN.VALUE = base::character(length = 1)), y = base::c("1", "1")), label = "fun6 exact line numbers")
    testthat::expect_true(object = base::identical(x = base::vapply(X = res_fields, FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1)), y = base::c("T", "F")), label = "fun6 exact symbols")
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests

    base::rm(list = base::ls())

})

# The tests of the export argument are in a separate block, so that they always run, even if the block above stops on an error
testthat::test_that("true_false_check() export argument", {

    ## data argument values
    fun4 <- function(x){ if(x == T){ base::return(x) } } # contains T only
    fun6 <- function(x){ base::ifelse(test = x > 1, yes = T, no = F) } # contains T and F
    fun3 <- function(x){ if(x > 1){ base::return(TRUE) } } # clean function
    str1 <- "FUN1"
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    ## end data argument values

    ## tests
    # export argument values
    testthat::expect_error(true_false_check(x = fun4, export = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun4, export = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun4, export = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun4, export = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun4, export = "a", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun4, export = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end export argument values

    # export = FALSE (default): the modified function is returned, NULL if everything is clean
    out_fun4 <- true_false_check(x = fun4, export = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.function(x = out_fun4), label = "export FALSE returns the modified function")
    testthat::expect_message(true_false_check(x = out_fun4, export = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # the returned function is clean
    testthat::expect_true(object = base::identical(x = out_fun4(TRUE), y = TRUE), label = "the returned function actually evaluates") # out_fun4(TRUE) must return TRUE, exactly like fun4(TRUE)
    testthat::expect_null(object = out_fun4(2), label = "the returned function behaves like the submitted one when the condition is FALSE") # fun4(2) returns NULL
    testthat::expect_true(object = base::is.null(x = true_false_check(x = fun3, export = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")), label = "export FALSE returns NULL when everything is clean")

    # export = TRUE: file written in a fresh temporary working directory, NULL returned, pre-existing file not overwritten
    ini_wd <- base::getwd()
    base::on.exit(expr = base::setwd(ini_wd), add = TRUE, after = TRUE) # restore the working directory even if a test fails
    tempo_dir <- base::tempfile(pattern = "tf_export_") # fresh directory at each test run, so that no pre-existing file can interfere
    base::dir.create(tempo_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    base::setwd(tempo_dir)
    out6 <- base::suppressMessages(true_false_check(x = fun6, export = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # suppressMessages() keeps the printed message out of the test output: the message itself is tested below with fun4
    testthat::expect_null(object = out6, label = "export TRUE returns NULL")
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun6_safer.R", fsep = "/")), label = "fun6_safer.R written")
    exported_lines <- base::readLines(con = base::file.path(tempo_dir, "fun6_safer.R", fsep = "/"))
    testthat::expect_true(object = base::any(base::grepl(x = exported_lines, pattern = "^fun6_safer <- function", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)), label = "exported file defines fun6_safer <- function")
    testthat::expect_true(object = base::any(base::grepl(x = exported_lines, pattern = "TRUE", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)) & base::any(base::grepl(x = exported_lines, pattern = "FALSE", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)), label = "exported file contains TRUE and FALSE") # fun6 contains both T and F
    # sourced file defines a working function
    base::sys.source(file = base::file.path(tempo_dir, "fun6_safer.R", fsep = "/"), envir = base::environment(fun = NULL))
    testthat::expect_true(object = base::identical(x = fun6_safer(2), y = fun6(2)), label = "sourced exported function behaves like the submitted one")
    testthat::expect_true(object = base::identical(x = fun6_safer(1), y = fun6(1)), label = "sourced exported function with T branch")
    # the message is still printed (tested with fun4, whose file does not exist yet)
    testthat::expect_message(true_false_check(x = fun4, export = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "MODIFIED CODE EXPORTED IN:", fixed = TRUE)
    # pre-existing file never overwritten
    testthat::expect_error(true_false_check(x = fun6, export = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "FILE NAME ALREADY EXISTS", fixed = TRUE)
    # pkg::fun submitted name: the file name is built from the last component of the submitted name (true_false_check_safer.R here, not saferDev_safer.R). The function is clean, so no file is written and NULL is returned
    testthat::expect_null(object = true_false_check(x = saferDev::true_false_check, export = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), label = "pkg::fun naming with a clean function: NULL")
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir, "true_false_check_safer.R", fsep = "/")), label = "no true_false_check_safer.R file written for the clean pkg::fun")
    # inline anonymous function with export = TRUE: error
    testthat::expect_error(true_false_check(x = function(y){ y == T }, export = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    # clean function: NULL returned, no file written
    testthat::expect_true(object = base::is.null(x = true_false_check(x = fun3, export = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")), label = "clean function with export TRUE: NULL, no file")
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir, "fun3_safer.R", fsep = "/")), label = "no file written for a clean function")
    # end export = TRUE tests

    # out_path argument: type checks (whatever the value of export for NULL and "")
    testthat::expect_error(true_false_check(x = fun3, export = TRUE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "CANNOT BE NULL", fixed = TRUE)
    testthat::expect_error(true_false_check(x = fun3, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "CANNOT BE NULL", fixed = TRUE)
    testthat::expect_error(true_false_check(x = fun3, export = TRUE, out_path = "", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    testthat::expect_error(true_false_check(x = fun3, export = FALSE, out_path = "", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "CANNOT CONTAIN EMPTY STRING", fixed = TRUE)
    testthat::expect_error(true_false_check(x = fun3, export = TRUE, out_path = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, export = TRUE, out_path = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, export = TRUE, out_path = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, export = TRUE, out_path = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(true_false_check(x = fun3, export = TRUE, out_path = base::c("a", "b"), safer_check = TRUE, lib_path = NULL, error_text = ""))
    # out_path ignored when export = FALSE (even a non-existing path)
    testthat::expect_no_error(true_false_check(x = fun3, export = FALSE, out_path = "PATH_NOT_GOOD", safer_check = TRUE, lib_path = NULL, error_text = ""))
    # non-existing out_path with export = TRUE
    testthat::expect_error(true_false_check(x = fun3, export = TRUE, out_path = "PATH_NOT_GOOD", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXISTS", fixed = TRUE)
    # trailing / and \\ tolerated in out_path
    base::unlink(x = base::file.path(tempo_dir, "fun4_safer.R", fsep = "/"))
    testthat::expect_null(object = true_false_check(x = fun4, export = TRUE, out_path = base::paste0(tempo_dir, "/"), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun4_safer.R", fsep = "/")))
    base::unlink(x = base::file.path(tempo_dir, "fun4_safer.R", fsep = "/"))
    testthat::expect_null(object = true_false_check(x = fun4, export = TRUE, out_path = base::paste0(tempo_dir, "\\"), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun4_safer.R", fsep = "/")))
    # component writing is refused when export = TRUE (the written form is not reliable for the file name)
    FUN_LIST <- base::list(fun = fun4)
    testthat::expect_error(true_false_check(x = FUN_LIST$fun, export = TRUE, out_path = tempo_dir, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME", fixed = TRUE)
    # end of tests: restore the working directory and remove the temporary directory
    base::setwd(ini_wd)
    base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE)

    ## end tests

    ## other tests
    ## end other tests

    base::rm(list = base::setdiff(x = base::ls(), y = "ini_wd")) # ini_wd is kept because used by the on.exit() handler above

})
