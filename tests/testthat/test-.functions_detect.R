testthat::test_that(".functions_detect()", {

    ## data argument values
    # from source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R")
    test <- function(
        text, 
        pattern
    ){
        # AIM
        # extract all function names
        # ARGUMENTS
        # text: vector of strings
        # pattern: regex to extract function names
        # RETURN
        # A list containing the functions names, each compartment being one of the string of the input vector
        # DEBUGGING
        # text = ini[1] ; pattern = pattern
        FUN1 <- function(){}
        #### Find all matches, including trailing '(' #
        matches <- gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text) # # test
        dt <- base::c(2:8)
        matched_strings <- regmatches(x = text, m = matches)[[1]]
        FUN1()
        # Remove trailing '(' from each match #
        tempo4 <- a$regmatches(x = text, m = matches)[[1]] ; sum(1:3) ; a$regmatches(x = 1)
        tempo5 <- a$count
        tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS OF col1 (", base::length(roc1()), "), col2 (", base::length(roc2), "), AND col3 (", base::length(roc3), "), SHOULD BE EQUAL\n")
        result <- sub("\\($##", "", matched_strings) ; range(1:3) # sub("\\($##", "", matched_strings)
        tempo_cat <- base::paste0("IAGE\nLENGTHS OF roc00() (", base::ks.test(roc4()), "), and roc0 (", base::length(roc5), ") SHOULL\n")
        return(baba) # base::sub("\\($##", "", matched_strings)
        base::return(bobo) # a$sub("\\($##", "", matched_strings)
    }
    arg_user_setting2_1 <- base::list(x =  as.name(x = "test"))
    arg_user_setting2_2 <- base::list(x =  as.name(x = "PACK::test"))
    arg_user_setting2_3 <- base::list(x =  c("::", "PACK", "test"))
    arg_user_setting2_4 <- base::list(x =  as.name(x = "fun4"))
    vec1 <- c("function", "if", "for", "while", "repeat", "else")
    str1 <- "blabla" # error saferDev:::.noclean_functions()
    str2 <- c("", "UseMethod(\"mean\")") # no error
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    fun2 <- function(){
        # blabla
    }
    fun3 <- function
    (){
        dt <- base::c(2:8)
    }
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(.functions_detect(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.functions_detect()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.functions_detect()) 
    x <- test
    arg_user_setting2 <- arg_user_setting2_1
    skipped_base <- vec1
    lib_path <- NULL 
    error_text <- ""
    testthat::expect_error(.functions_detect()) # R classical non traced error message due to error_text without default value (specific of my internal functions)
    testthat::expect_error(.functions_detect(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    rm(
        x, 
        arg_user_setting2, 
        skipped_base, 
        lib_path, 
        error_text
    )
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(.functions_detect(x = NULL, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = NULL, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(.functions_detect(x = character(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = integer(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = double(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = logical(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = complex(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = data.frame(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = list(), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = character(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = integer(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = double(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = logical(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = complex(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = data.frame(), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = list(), skipped_base = vec1, lib_path = NULL, error_text = ""))

    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = list(), lib_path = NULL, error_text = ""))

    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = character(), error_text = ""))
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(.functions_detect(x = NA, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = NA, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NA, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = NA))
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
    # x
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = NULL, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = NA, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = 1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = c(TRUE, FALSE), arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = str1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = mat1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = factor1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = expr1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_warning(.functions_detect(x = fun1, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""), regexp = "IS EMPTY OF FUNCTION", fixed = TRUE) # empty function: warning, not error
    # end x
    # arg_user_setting2
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = NULL, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = NA, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = 1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = c(TRUE, FALSE), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = str1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = mat1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = factor1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = expr1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = fun1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    # end arg_user_setting2
    # skipped_base
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = str1, lib_path = NULL, error_text = "")) 
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = fun1, lib_path = NULL, error_text = ""))
    # end skipped_base
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only is safer_check = TRUE in the enclosing function
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character

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
    # modification of arg_user_setting2$x for clean messages
    testthat::expect_no_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_3, skipped_base = vec1, lib_path = NULL, error_text = ""))
    result <- .functions_detect(x = test, arg_user_setting2 = arg_user_setting2_3, skipped_base = vec1, lib_path = NULL, error_text = "")$arg_user_setting$x
    expected <- "PACK::test()"
    testthat::expect_equal(result, expected)
    # end modification of arg_user_setting2$x for clean messages
    # primitive function: no R code, warning and NULL (formerly, its printed code, function (..., na.rm = FALSE)  .Primitive("sum"), was analyzed, so that colons_check() returned a function returning base::sum instead of the sum)
    testthat::expect_warning(object = result <- .functions_detect(x = base::sum, arg_user_setting2 = base::list(x = base::c("::", "base", "sum")), skipped_base = vec1, lib_path = NULL, error_text = ""), regexp = "THE TESTED FUNCTION base::sum() IS A PRIMITIVE FUNCTION, WHICH HAS NO R CODE.\n\nNOTHING TO CHECK.", class = NULL, fixed = TRUE, inherit = TRUE, all = FALSE, info = NULL, label = NULL)
    testthat::expect_null(object = result, info = NULL, label = NULL)
    # end primitive function
    # recovering the basic functions of R
    # stats and graphics are detached, then attached again at their initial positions, so that the search path of the next test files is not modified. Warning: the exclude and include.only arguments of base::attachNamespace() are not written, because include.only = NULL attaches an empty package environment
    search_ini <- base::search()
    pos_stats <- base::match(x = "package:stats", table = search_ini, nomatch = NA_integer_, incomparables = NULL)
    pos_graphics <- base::match(x = "package:graphics", table = search_ini, nomatch = NA_integer_, incomparables = NULL)
    base::on.exit(expr = {if( ! "package:stats" %in% base::search()){base::attachNamespace(ns = "stats", pos = pos_stats, depends = NULL)} ; if( ! "package:graphics" %in% base::search()){base::attachNamespace(ns = "graphics", pos = pos_graphics, depends = NULL)}}, add = TRUE, after = FALSE) # stats and graphics attached again if an error occurs while they are detached
    base::detach(name = "package:stats", pos = 2L, unload = FALSE, character.only = TRUE, force = FALSE)
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.functions_detect().\n\nTHE default base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\npackage:stats\nTHE FUNCTION CANNOT WORK WITH THIS NAME SPACE DETACHED. PLEASE, ATTACH IT.\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    base::attachNamespace(ns = "stats", pos = pos_stats, depends = NULL)
    base::detach(name = "package:stats", pos = 2L, unload = FALSE, character.only = TRUE, force = FALSE)
    base::detach(name = "package:graphics", pos = 2L, unload = FALSE, character.only = TRUE, force = FALSE)
    testthat::expect_error(.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = ""))
    result <- saferDev::get_message('.functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = "")', kind = "error", print_no = TRUE, text = NULL) 
    expected <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN saferDev:::.functions_detect().\n\nTHE default base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\npackage:stats\npackage:graphics\nTHE FUNCTION CANNOT WORK WITH THESE NAME SPACES DETACHED. PLEASE, ATTACH THEM.\n\n================\n\n\n"
    testthat::expect_equal(result, expected)
    base::attachNamespace(ns = "stats", pos = pos_stats, depends = NULL) # increasing positions: stats, then graphics
    base::attachNamespace(ns = "graphics", pos = pos_graphics, depends = NULL)
    testthat::expect_identical(object = base::search(), expected = search_ini, info = NULL, label = NULL, expected.label = NULL)
    # end recovering the basic functions of R

    # removal of comments
    # quotes read as the R parser does: # between quotes kept, apostrophes inside double quotes and inside comments, # at the beginning of a line of a string over several lines, raw string and backquoted name containing #
    text_comment <- 'function(x){\n    # comment with paste(1)\n    y <- "it\'s # not (a) comment" ; z <- sum(x) # c(1)\n    a <- "multi\n# line sum(2)\nb" ; b <- r"(#)" ; `a#b` <- mean(x) # it\'s\n}'
    fun_comment <- base::eval(expr = base::parse(file = "", n = NULL, text = text_comment, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = text_comment, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # with source references, i.e., with comments
    result <- saferDev:::.functions_detect(x = fun_comment, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_comment")), lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result$code[1:7], expected = base::c("function(x){", "", "y <- \"it's # not (a) comment\" ; z <- sum(x) ", "a <- \"multi", "# line sum(2)", "b\" ; b <- r\"(#)\" ; `a#b` <- mean(x) ", "}")) # line 8 is the environment of fun_comment
    testthat::expect_equal(object = result$fun_names, expected = base::list(c3 = base::c("not", "sum"), c5 = "sum", c6 = "mean"))
    testthat::expect_equal(object = result$code_line_nb, expected = base::c(3L, 5L, 6L))
    # end removal of comments

    # parse data of the printed code of x: internal functions, formulas and .Internal() calls
    # names inside a formula (one-sided or two-sided, written with `~`(), over two lines, after a TAB, at any depth) and name of the function called by .Internal() (on the same line or not, with the call argument name written): TRUE in fun_names_skip, which has the structure of fun_names
    code_skip <- "function(d){\n    stats::lm(formula = y ~ log(x) + offset(z), data = d)\n\tg <- ~ mean(.x) ;\th <- `~`(y, sqrt(x))\n    k <- y ~\n        poly(x, 2)\n    .Internal(toupper(d)) ; base::.Internal(call = tolower(d))\n    .Internal(\n        nchar(d))\n    sum(1)\n}"
    fun_skip <- base::eval(expr = base::parse(file = "", n = NULL, text = code_skip, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_skip, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the TABs and the line breaks
    result <- .functions_detect(x = fun_skip, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_skip")), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result$fun_names, expected = base::list(c2 = base::c("lm", "log", "offset"), c3 = base::c("mean", "sqrt"), c5 = "poly", c6 = base::c(".Internal", "toupper", ".Internal", "tolower"), c7 = ".Internal", c8 = "nchar", c9 = "sum"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = result$fun_names_skip, expected = base::list(c2 = base::c(FALSE, TRUE, TRUE), c3 = base::c(TRUE, TRUE), c5 = TRUE, c6 = base::c(FALSE, TRUE, FALSE, TRUE), c7 = FALSE, c8 = TRUE, c9 = FALSE), info = NULL, label = NULL, expected.label = NULL)
    result <- .functions_detect(x = test, arg_user_setting2 = arg_user_setting2_1, skipped_base = vec1, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = base::lengths(x = result$fun_names_skip, use.names = TRUE), expected = base::lengths(x = result$fun_names, use.names = TRUE), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_false(object = base::any(base::unlist(x = result$fun_names_skip, recursive = TRUE, use.names = FALSE), na.rm = FALSE), info = NULL, label = NULL)
    # names inside the brackets of the calls of quote(), bquote(), expression(), alist() and substitute() (code used as data, e.g., plotmath labels), written without package or after base:: or base:::, between backquotes or quotes, after a TAB, over several lines, nested, inside a formula or containing one: TRUE in fun_names_skip, while the names of these functions are FALSE, as the names of the code that R evaluates: the .() of bquote() and the env argument of substitute() (see the quoting rule below). Not the calls of a function of another package or after $, nor these names written as strings elsewhere (formerly, FALSE everywhere except in the formula, so that colons_check() and all_args_here() modified the quoted code)
    code_quote <- "function(r2, e){\n    e <- quote(sqrt(2)) ; base::bquote(R^2 == .(round(r2, 2)))\n\tf <- base:::expression(paste(\"a\", sin(x))) ; `alist`(a = , b = log(2))\n    \"substitute\"(mean(e), env = list(e = 1))\n    g <- quote(\n        quote(exp(1))\n    )\n    stats::lm(formula = y ~ quote(log(x))) ; quote(y ~ log(x)) ; sum(1)\n    pkg::quote(nchar(e)) ; e$quote(rev(e)) ; base::c(\"quote\", toupper(e))\n}"
    fun_quote <- base::eval(expr = base::parse(file = "", n = NULL, text = code_quote, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_quote, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the TABs and the line breaks
    result <- .functions_detect(x = fun_quote, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_quote")), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result$fun_names, expected = base::list(c2 = base::c("quote", "sqrt", "bquote", "round"), c3 = base::c("expression", "paste", "sin", "log"), c4 = base::c("mean", "list"), c5 = "quote", c6 = base::c("quote", "exp"), c8 = base::c("lm", "quote", "log", "quote", "log", "sum"), c9 = base::c("quote", "nchar", "quote", "rev", "c", "toupper")), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = result$fun_names_skip, expected = base::list(c2 = base::c(FALSE, TRUE, FALSE, FALSE), c3 = base::c(FALSE, TRUE, TRUE, TRUE), c4 = base::c(TRUE, FALSE), c5 = FALSE, c6 = base::c(TRUE, TRUE), c8 = base::c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE), c9 = base::c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)), info = NULL, label = NULL, expected.label = NULL)
    # value piped into a quoting call by the native pipe (paste0("a") |> quote() is quote(paste0("a"))), also through several pipes and with a comment after |>: TRUE in fun_names_skip. Not a quoting call piped into another function, nor a pipe into another function (formerly, FALSE, so that colons_check() and all_args_here() modified the quoted code)
    code_pipe <- "function(x){\n    a <- paste0(\"a\") |> quote()\n    b <- x |> nchar() |> base::quote() |> eval()\n    d <- toupper(x) |> # comment\n        quote()\n    e <- quote(sqrt(x)) |> eval() ; sum(1) |> rev()\n}"
    fun_pipe <- base::eval(expr = base::parse(file = "", n = NULL, text = code_pipe, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_pipe, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the pipes, otherwise deparsed as quote(paste0("a"))
    result <- .functions_detect(x = fun_pipe, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_pipe")), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result$fun_names, expected = base::list(c2 = base::c("paste0", "quote"), c3 = base::c("nchar", "quote", "eval"), c4 = "toupper", c5 = "quote", c6 = base::c("quote", "sqrt", "eval", "sum", "rev")), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = result$fun_names_skip, expected = base::list(c2 = base::c(TRUE, FALSE), c3 = base::c(TRUE, FALSE, FALSE), c4 = TRUE, c5 = FALSE, c6 = base::c(FALSE, TRUE, FALSE, FALSE, FALSE)), info = NULL, label = NULL, expected.label = NULL)
    # quoting rule, with and without source references: the names of the code that R evaluates inside the quoting calls are FALSE in fun_names_skip: argument of the .() calls of bquote() (and of the ..() calls if splice is written TRUE or T), at any depth, also inside a nested quote() or formula, where and splice arguments of bquote(), env argument of substitute(), with their arguments matched as R does (names, partial names, positions), also when piped, but none of them if the matching fails (error of R, e.g., the unused argument zz of substitute() or the where argument matched by w and wh). The rest of the expr argument of bquote() and substitute(), the .() and ..() markers, and the code inside quote(), expression() and alist() are TRUE, also with a comment between base:: and the name of the function (formerly, all the code inside the brackets of the quoting calls was TRUE, so that colons_check() did not report mean() in bquote(x + .(mean(y))), as saferDev 2.0.0 did, the arguments of a call whose matching fails were checked, and a comment after base:: prevented the recognition of quote())
    code_quoting <- base::c(
        "function(y){base::bquote(x + .(mean(y)))}",
        "function(y){base::bquote(x + .(z), where = list2env(list(z = 1)))}",
        "function(y){base::substitute(u + v, list(u = sum(y)))}",
        "function(y){base::bquote(f(..(lapply(y, as.name))), splice = TRUE)}",
        "function(y){base::bquote(f(..(g(y))))}",
        "function(y){base::quote(.(mean(y)))}",
        "function(y){base::bquote(quote(.(mean(y))))}",
        "function(y){base::bquote(r ~ .(f(y)) + g(z))}",
        "function(y){list(u = sum(y)) |> base::substitute(expr = u + v, env = _)}",
        "function(y){mean(y) |> base::bquote()}",
        "function(y){base::bquote(.(base::quote(h(y))))}",
        "function(y){base::bquote(.(as.name(y)) ~ x)}",
        "function(y){base::bquote(x + .(base::mean(y, na.rm = T)))}",
        "function(y){base::bquote(T)}",
        "function(y){base::substitute(expr = a, env = base::list(a = T))}",
        "function(a){base::bquote(.(if(a) 1 else 2) + x)}",
        "function(a){base::bquote(if(a) 1 else 2)}",
        "function(y){base::expression(f(.(y)), g(y))}",
        "function(y){base::bquote(where = list2env(list(z = 1)), expr = a + .(z))}",
        "function(y){base::substitute(env = list(u = sum(y)), expr = u + v)}",
        "function(y){\n    base::bquote(ex = f(..(g(y)), .()), wh = list2env(list(z = 1)), s = T) ; base::substitute(ex = u + v, en = list(u = sum(y)))\n    base::bquote(`.`(nchar(y)), # comment\n        where = new.env()) ; base::substitute(u, list(a = 1), zz = rev(1))\n}",
        "function(y){\n    base::bquote(x, w = mean(y), wh = 1) ; base::bquote(a, where = list2env(list(z = 1)), wher = 1)\n    base::bquote(a, new.env(), TRUE, nchar(y)) ; base::substitute(expr = a, env = list(a = 1), expr = b)\n    base::bquote(x = .(rev(y))) ; base::substitute(en = list(a = 1), e = sum(y)) ; mean(y) |> base::substitute(expr = a, env = list(a = 1))\n}",
        "function(y){g(base:: # comment\n    quote(h(y)))}"
    )
    names_eval <- base::list(base::c("bquote", "mean"), base::c("bquote", "list2env", "list"), base::c("substitute", "list", "sum"), base::c("bquote", "lapply"), "bquote", "quote", base::c("bquote", "mean"), base::c("bquote", "f"), base::c("list", "sum", "substitute"), "bquote", base::c("bquote", "quote"), base::c("bquote", "as.name"), base::c("bquote", "mean"), "bquote", base::c("substitute", "list"), "bquote", "bquote", "expression", base::c("bquote", "list2env", "list"), base::c("substitute", "list", "sum"), base::c("bquote", "list2env", "list", "g", "substitute", "list", "sum", "bquote", "nchar", "new.env", "substitute"), base::c("bquote", "bquote", "bquote", "substitute", "bquote", "substitute", "substitute"), base::c("g", "quote")) # names of the evaluated code
    names_skip <- base::list(base::character(length = 0), base::character(length = 0), base::character(length = 0), base::c("f", ".."), base::c("f", "..", "g"), "mean", "quote", "g", base::character(length = 0), "mean", "h", base::character(length = 0), base::character(length = 0), base::character(length = 0), base::character(length = 0), base::character(length = 0), base::character(length = 0), base::c("f", "g"), base::character(length = 0), base::character(length = 0), base::c("f", "..", "rev", "list"), base::c("mean", "list2env", "list", "new.env", "nchar", "list", "rev", "list", "sum", "mean", "list"), "h") # names of the code not evaluated
    for(i1 in base::seq_along(along.with = code_quoting)){
        for(i2 in base::c(TRUE, FALSE)){
            if(i2 == TRUE){
                tempo_srcfile <- base::srcfilecopy(filename = "<text>", lines = code_quoting[i1], timestamp = base::Sys.time(), isFile = FALSE)
            }else{
                tempo_srcfile <- NULL
            }
            fun_quoting <- base::eval(expr = base::parse(file = "", n = NULL, text = code_quoting[i1], prompt = "?", keep.source = i2, srcfile = tempo_srcfile, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # without source references, the code is printed deparsed, e.g., the value piped into substitute() becomes its env argument
            result <- .functions_detect(x = fun_quoting, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_quoting")), lib_path = NULL, error_text = "")
            tempo_names <- base::unlist(x = result$fun_names, recursive = TRUE, use.names = FALSE)
            tempo_skip <- base::unlist(x = result$fun_names_skip, recursive = TRUE, use.names = FALSE)
            testthat::expect_identical(object = base::list(base::sort(x = tempo_names[ ! tempo_skip], decreasing = FALSE), base::sort(x = tempo_names[tempo_skip], decreasing = FALSE)), expected = base::list(base::sort(x = names_eval[[i1]], decreasing = FALSE), base::sort(x = names_skip[[i1]], decreasing = FALSE)), info = NULL, label = base::paste0("quoting case ", i1, ", source references ", i2, collapse = NULL, recycle0 = FALSE), expected.label = NULL)
        }
    }
    # internal functions: formal arguments of x and of the functions created inside x, names assigned a function definition or a utils::getFromNamespace() result (=, <-, ->, after ; or {, line break after the operator, chained assignments, brackets, quotes and backquotes, lambda), and x itself (pkg::name() writing): TRUE in fun_names_internal, which has the structure of fun_names. Not the names assigned another value (c <- 3), nor the targets made of several names (lst$m), nor a definition that is not assigned (n <- base::list(function(z){z}), or function(w){w} returned by a function), nor the formal arguments of the functions defined inside x, outside these functions (y, z and w)
    code_internal <- "function(x, FUN){\n    a = function(y){y} ; b <- utils::getFromNamespace(x = \"mean\", ns = \"base\")\n    d <-\n        function(z){z}\n    e <- f <- function(z){z}\n    (function(z){z}) -> g\n    \"h\" <- function(z){z} ; `k` <- \\(z) z\n    c <- 3 ; lst$m <- function(z){z} ; n <- base::list(function(z){z})\n    function(z){function(w){w}}\n    FUN(x) ; a(1) ; b(1) ; d(1) ; e(1) ; f(1) ; g(1) ; h(1) ; k(1) ; c(1) ; m(1) ; n(1) ; y(1) ; z(1) ; w(1) ; fun_internal(1)\n}"
    fun_internal <- base::eval(expr = base::parse(file = "", n = NULL, text = code_internal, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_internal, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the writing of the assignments
    result <- .functions_detect(x = fun_internal, skipped_base = vec1, arg_user_setting2 = base::list(x = "saferDev::fun_internal()"), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result$fun_names, expected = base::list(c2 = "getFromNamespace", c8 = "list", c10 = base::c("FUN", "a", "b", "d", "e", "f", "g", "h", "k", "c", "m", "n", "y", "z", "w", "fun_internal")), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = result$fun_names_internal, expected = base::list(c2 = FALSE, c8 = FALSE, c10 = base::c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)), info = NULL, label = NULL, expected.label = NULL)
    result <- .functions_detect(x = test, arg_user_setting2 = arg_user_setting2_3, skipped_base = vec1, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = base::lengths(x = result$fun_names_internal, use.names = TRUE), expected = base::lengths(x = result$fun_names, use.names = TRUE), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = base::unlist(x = result$fun_names, recursive = TRUE, use.names = FALSE)[base::unlist(x = result$fun_names_internal, recursive = TRUE, use.names = FALSE)], expected = "FUN1", info = NULL, label = NULL, expected.label = NULL)
    fun_rec <- function(n){if(n > 0){fun_rec(n - 1)}else{0}}
    result <- .functions_detect(x = fun_rec, arg_user_setting2 = base::list(x = base::c("::", "PACK", "fun_rec")), skipped_base = vec1, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = base::unlist(x = result$fun_names_internal, recursive = TRUE, use.names = FALSE), expected = TRUE, info = NULL, label = "recursive call, x written PACK::fun_rec", expected.label = NULL)
    # scope of the internal functions, with and without source references: the formal arguments of a function defined inside x are internal functions only inside this function (formal arguments with their default values, and body), and a name assigned a function definition only inside the innermost function definition containing the assignment (the whole code of x for <<-). Formerly, these names were internal functions in the whole code of x, so that c(x, 1) (base::c()) and t(x) (base::t()) were not checked, while saferDev 2.0.0 reported them
    code_scope <- "function(x){\n    g <- function(y, c){\n        c(y)\n    }\n    a <- c(x, 1)\n    lapply(X = x, FUN = function(t){t + 1}) ; t(x)\n    h <- function(){\n        k <- function(z){z}\n        (function(z){z}) -> m\n        n <<- function(z){z}\n        k(1) ; m(1)\n    }\n    k(2) ; m(2) ; n(2) ; g(a, sum)\n    p <- function(s) s(1) ; s(2)\n}"
    for(i2 in base::c(TRUE, FALSE)){
        if(i2 == TRUE){
            tempo_srcfile <- base::srcfilecopy(filename = "<text>", lines = code_scope, timestamp = base::Sys.time(), isFile = FALSE)
        }else{
            tempo_srcfile <- NULL
        }
        fun_scope <- base::eval(expr = base::parse(file = "", n = NULL, text = code_scope, prompt = "?", keep.source = i2, srcfile = tempo_srcfile, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        result <- .functions_detect(x = fun_scope, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_scope")), lib_path = NULL, error_text = "")
        testthat::expect_identical(object = base::unlist(x = result$fun_names, recursive = TRUE, use.names = FALSE), expected = base::c("c", "c", "lapply", "t", "k", "m", "k", "m", "n", "g", "s", "s"), info = NULL, label = base::paste0("source references ", i2, collapse = NULL, recycle0 = FALSE), expected.label = NULL)
        testthat::expect_identical(object = base::unlist(x = result$fun_names_internal, recursive = TRUE, use.names = FALSE), expected = base::c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE), info = NULL, label = base::paste0("source references ", i2, collapse = NULL, recycle0 = FALSE), expected.label = NULL)
    }
    # function definitions of the quoted code (e.g., in bquote()), with and without source references: they create no function when x runs, so that their formal arguments, and the names assigned a function inside them, are not internal functions (mean and sd in the .() of the default values, c in the .() of f2 and f4, evaluated by bquote() in x with the basic functions, also inside function(b), and k in f3), and that a name assigned in an evaluated .() inside them is an internal function in the function that calls bquote() (rev, assigned in x by bquote()) (formerly, mean, sd, c and k were internal functions, so that colons_check() did not report mean, sd and c, while saferDev 2.0.0 did, and rev(1) was not, so that colons_check() changed it into base::rev(1), which changed the result)
    code_quoted_def <- "function(v){\n    f1 <- base::eval(base::bquote(function(x, mean = .(mean(v)), sd = .(sd(v))){\n        stats::dnorm(x = x, mean = mean, sd = sd, log = FALSE)\n    }))\n    f2 <- base::bquote(function(c) .(c(1, v)))\n    f3 <- base::bquote(function(a){k <- function(z){z} ; k(.(rev <- function(...){0}))})\n    f4 <- base::bquote(function(a){c <- function(z){z} ; .(c(1, v)) ; .(function(b){c(b)})})\n    rev(1) ; k(2)\n}"
    for(i2 in base::c(TRUE, FALSE)){
        if(i2 == TRUE){
            tempo_srcfile <- base::srcfilecopy(filename = "<text>", lines = code_quoted_def, timestamp = base::Sys.time(), isFile = FALSE)
        }else{
            tempo_srcfile <- NULL
        }
        fun_quoted_def <- base::eval(expr = base::parse(file = "", n = NULL, text = code_quoted_def, prompt = "?", keep.source = i2, srcfile = tempo_srcfile, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        result <- .functions_detect(x = fun_quoted_def, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_quoted_def")), lib_path = NULL, error_text = "")
        testthat::expect_identical(object = base::unlist(x = result$fun_names, recursive = TRUE, use.names = FALSE), expected = base::c("eval", "bquote", "mean", "sd", "dnorm", "bquote", "c", "bquote", "k", "bquote", "c", "c", "rev", "k"), info = NULL, label = base::paste0("source references ", i2, collapse = NULL, recycle0 = FALSE), expected.label = NULL)
        testthat::expect_identical(object = base::unlist(x = result$fun_names_skip, recursive = TRUE, use.names = FALSE), expected = base::c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), info = NULL, label = base::paste0("source references ", i2, collapse = NULL, recycle0 = FALSE), expected.label = NULL)
        testthat::expect_identical(object = base::unlist(x = result$fun_names_internal, recursive = TRUE, use.names = FALSE), expected = base::c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), info = NULL, label = base::paste0("source references ", i2, collapse = NULL, recycle0 = FALSE), expected.label = NULL)
    }
    # x itself only when its name designates x from the environment of x: not for the "x" placeholder written by the calling functions for a function object (e.g., x = fun1 in the args argument of base::do.call()), nor for a name whose value cannot be evaluated (promise returning an error). Formerly, the "x" placeholder made the calls of a function named x internal
    result <- .functions_detect(x = function(y){x(y)}, skipped_base = vec1, arg_user_setting2 = base::list(x = "x()"), lib_path = NULL, error_text = "")
    testthat::expect_false(object = base::any(base::unlist(x = result$fun_names_internal, recursive = TRUE, use.names = FALSE), na.rm = FALSE), info = NULL, label = NULL)
    tempo_env_promise <- base::new.env(hash = TRUE, parent = base::environment(fun = NULL), size = 29L)
    base::delayedAssign(x = "fun_promise", value = base::stop("PROMISE THAT RETURNS AN ERROR", call. = FALSE, domain = NULL), eval.env = tempo_env_promise, assign.env = tempo_env_promise)
    fun_promise <- function(y){fun_promise(y)}
    base::environment(fun = fun_promise) <- tempo_env_promise
    result <- .functions_detect(x = fun_promise, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_promise")), lib_path = NULL, error_text = "")
    testthat::expect_false(object = base::any(base::unlist(x = result$fun_names_internal, recursive = TRUE, use.names = FALSE), na.rm = FALSE), info = NULL, label = NULL)
    # printed code that does not parse (environment object inserted in the body): formulas, quoting calls and .Internal() calls not detected, and internal functions only found at the beginning of a line (name <- function(), and x itself, in the whole code of x
    fun_env <- function(x){x}
    base::body(fun = fun_env, envir = base::environment(fun = fun_env)) <- base::call(name = "{", base::str2lang(s = "f <- function(y){y}"), base::str2lang(s = "h <- y ~ log(x)"), base::str2lang(s = "k <- quote(sqrt(2))"), base::call(name = "g2", base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L)), base::str2lang(s = "f(1)"), base::str2lang(s = "fun_env(2)")) # base::str2lang() keeps no source reference: the code is printed deparsed
    result <- .functions_detect(x = fun_env, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_env")), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result[base::c("fun_names", "fun_names_skip", "fun_names_internal")], expected = base::list(fun_names = base::list(c6 = "log", c7 = base::c("quote", "sqrt"), c8 = "g2", c9 = "f", c10 = "fun_env"), fun_names_skip = base::list(c6 = FALSE, c7 = base::c(FALSE, FALSE), c8 = FALSE, c9 = FALSE, c10 = FALSE), fun_names_internal = base::list(c6 = FALSE, c7 = base::c(FALSE, FALSE), c8 = FALSE, c9 = TRUE, c10 = TRUE)), info = NULL, label = NULL, expected.label = NULL)
    # attributes printed after the code of the function (attr(,"foo") and its value), which does not parse: only the function is parsed
    code_attr <- "function(d){stats::lm(formula = y ~ offset(z), data = d)}"
    fun_attr <- base::eval(expr = base::parse(file = "", n = NULL, text = code_attr, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_attr, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the line of the code
    base::attr(x = fun_attr, which = "foo") <- "bar"
    result <- .functions_detect(x = fun_attr, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_attr")), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = base::list(fun_names = result$fun_names[1], fun_names_skip = result$fun_names_skip[1], fun_names_internal = result$fun_names_internal[1]), expected = base::list(fun_names = base::list(c1 = base::c("lm", "offset")), fun_names_skip = base::list(c1 = base::c(FALSE, TRUE)), fun_names_internal = base::list(c1 = base::c(FALSE, FALSE))), info = NULL, label = NULL, expected.label = NULL) # line 1 only: the next lines (environment and attribute lines) depend on the environment of fun_attr
    # no internal function: FALSE everywhere
    result <- .functions_detect(x = function(){base::mean(x = 1)}, skipped_base = vec1, arg_user_setting2 = base::list(x = "FUN_LIST$fun()"), lib_path = NULL, error_text = "")
    testthat::expect_false(object = base::any(base::unlist(x = result$fun_names_internal, recursive = TRUE, use.names = FALSE), na.rm = FALSE), info = NULL, label = NULL)
    # end parse data of the printed code of x: internal functions, formulas and .Internal() calls

    # removal of special functions
    testthat::expect_warning(.functions_detect(x = fun2, arg_user_setting2 = base::list(x =  as.name(x = "fun2")), skipped_base = vec1, lib_path = NULL, error_text = ""))
    testthat::expect_null(object = base::suppressWarnings(.functions_detect(x = fun2, arg_user_setting2 = base::list(x =  as.name(x = "fun2")), skipped_base = vec1, lib_path = NULL, error_text = ""), classes = "warning"))
    result <- saferDev::get_message('.functions_detect(x = fun2, arg_user_setting2 = base::list(x =  as.name(x = "fun2")), skipped_base = vec1, lib_path = NULL, error_text = "")', kind = "warning", print_no = TRUE, text = NULL) 
    expected <- "WARNING MESSAGE REPORTED:\nsimpleWarning: FROM saferDev:::.functions_detect().\n\nTHE TESTED FUNCTION fun2 IS EMPTY OF FUNCTION OR ONLY MADE OF COMMENTS OR ONLY MADE OF THE SKIPPED FUNCTIONS:\nfunction\nif\nfor\nwhile\nrepeat\nelse\n\nNOTHING TO CHECK.\n"
    testthat::expect_equal(result, expected)
    # end removal of special functions

    # trick to deal with end of lines between the name of the function and "("
    testthat::expect_no_error(.functions_detect(x = fun3, arg_user_setting2 = arg_user_setting2_4, skipped_base = vec1, lib_path = NULL, error_text = ""))
    # only after the name of a called function: not after a name ending a line, followed by a line starting with an expression between brackets (formerly, C was taken for a function call, so that colons_check() turned y <- C into y <- stats::C, and all_args_here() stopped with an internal error)
    code_newline <- "function(z){\n    C <- base::list(1, 2)\n    y <- C\n    (z)\n    base::c(y, paste0\n    (z))\n}"
    fun_newline <- base::eval(expr = base::parse(file = "", n = NULL, text = code_newline, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_newline, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the line breaks
    result <- .functions_detect(x = fun_newline, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_newline")), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result$fun_names, expected = base::list(c2 = "list", c5 = base::c("c", "paste0")), info = NULL, label = NULL, expected.label = NULL)
    # the ( is only added to detect the name: $code is returned without it, so that its brackets, read by all_args_here(), remain balanced (formerly, "base::c(y, paste0(", so that all_args_here() stopped with INTERNAL ERROR 3 of .fun_args_pos())
    testthat::expect_identical(object = result$code[base::c(3, 5)], expected = base::c("y <- C", "base::c(y, paste0"), info = NULL, label = NULL, expected.label = NULL)
    # name of a called function separated from its ( by a line of comment (formerly, not detected)
    code_newline2 <- "function(z){\n    base::nchar(paste0 # comment\n        # line of comment\n        (z))\n}"
    fun_newline2 <- base::eval(expr = base::parse(file = "", n = NULL, text = code_newline2, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_newline2, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # srcref required to keep the line breaks and the comments
    result <- .functions_detect(x = fun_newline2, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_newline2")), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result$fun_names, expected = base::list(c2 = base::c("nchar", "paste0")), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = result$code[2:4], expected = base::c("base::nchar(paste0 ", "", "(z))"), info = NULL, label = NULL, expected.label = NULL)
    # printed code that does not parse (environment object inserted in the body): any name ending a line followed by a line starting with ( is taken for a function call, and $code is also returned without the added (
    fun_env_newline <- function(x){x}
    base::body(fun = fun_env_newline, envir = base::environment(fun = fun_env_newline)) <- base::call(name = "{", base::str2lang(s = "y <- base::paste0"), base::str2lang(s = "(x)"), base::call(name = "g2", base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L))) # base::str2lang() keeps no source reference: the code is printed deparsed, with one statement per line
    result <- .functions_detect(x = fun_env_newline, skipped_base = vec1, arg_user_setting2 = base::list(x = base::as.name(x = "fun_env_newline")), lib_path = NULL, error_text = "")
    testthat::expect_identical(object = result$fun_names, expected = base::list(c3 = "paste0", c5 = "g2"), info = NULL, label = NULL, expected.label = NULL)
    testthat::expect_identical(object = result$code[3:4], expected = base::c("y <- base::paste0", "(x)"), info = NULL, label = NULL, expected.label = NULL)
    # end trick to deal with end of lines between the name of the function and "("
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # regression tests (quotes and comments, U58): the comments must be removed and the function names between quotes ignored, whatever the quotes before
    result <- base::suppressMessages(expr = saferDev::colons_check(x = fun_comment, export = FALSE, out_path = base::tempdir(check = FALSE), safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_equal(object = utils::capture.output(result, file = NULL, append = FALSE, type = "output", split = FALSE)[1:6], expected = base::c("function(x){", "    # comment with paste(1)", '    y <- "it\'s # not (a) comment" ; z <- base::sum(x) # c(1)', '    a <- "multi', "# line sum(2)", 'b" ; b <- r"(#)" ; `a#b` <- base::mean(x) # it\'s'))
    text_comment2 <- 'function(x){\n    # it\'s a comment with paste(1)\n    y <- base::paste0("it\'s # not (a) comment", x) # c(1)\n    `a#b` <- base::paste0(r"(#)", "#\'", x) # it\'s\n    base::paste0(y, `a#b`)\n}'
    fun_comment2 <- base::eval(expr = base::parse(file = "", n = NULL, text = text_comment2, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = text_comment2, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    result <- base::suppressMessages(expr = saferDev::all_args_here(x = fun_comment2, table_export = FALSE, fun_export = FALSE, out_path = base::tempdir(check = FALSE), df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::is.function(x = result))
    base::environment(fun = result) <- base::environment(fun = fun_comment2)
    testthat::expect_identical(object = result(1), expected = fun_comment2(1))
    testthat::expect_equal(object = utils::capture.output(result, file = NULL, append = FALSE, type = "output", split = FALSE)[2], expected = "    # it's a comment with paste(1)")
    # end regression tests (quotes and comments, U58)
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})

