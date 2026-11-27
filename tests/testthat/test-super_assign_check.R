# This file tests super_assign_check()
# Same structure as test-lambda_check.R
# Warning: the ->> operator is printed <<- in the code of a function without srcref. The functions containing ->> are therefore built with base::parse(text = ..., keep.source = TRUE) followed by base::eval(), to be sure that the srcref attribute is present, whatever the value of base::getOption("keep.source")

testthat::test_that("super_assign_check()", {

    ## data argument values
    int1 <- 1
    str1 <- "FUN1"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    # functions without superassignment (clean cases)
    fun3 <- function(x){ y <- x ; 2 -> z ; w = 3 ; base::return(y) } # <-, -> and = assignments
    fun4 <- function(x){ y <- "the <<- and ->> writings are just strings" ; x } # strings are immune
    fun5 <- function(x){ y <- 2 # a <<- writing in a comment is ignored
        x
    } # comments are ignored
    fun6 <- eval(parse(text = "function(x){ base::`<<-`\n    x }", keep.source = TRUE)) # backquoted `<<-` name: not an operator, immune
    fun7 <- eval(parse(text = "function(x){ x }", keep.source = TRUE)) # clean function with srcref
    fun8 <- function(x) x # function without braces
    # functions with superassignment
    fun10 <- eval(parse(text = "function(x){ y <<- x ; y }", keep.source = TRUE)) # <<-
    fun11 <- eval(parse(text = "function(x){ x ->> y ; y }", keep.source = TRUE)) # ->>
    fun12 <- eval(parse(text = "function(x){\n    y <- 1\n    base::lapply(X = 1:2, FUN = function(i){ y <<- i })\n    z <- \"<<-\" # <<- in a string and a comment\n    3 ->> w\n    y\n}", keep.source = TRUE)) # inside a nested function, plus ->>, plus immune writings on another line
    fun13 <- function(x){ y <<- x ; y } # function defined in the test file (srcref depends on keep.source)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(super_assign_check(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::super_assign_check()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    testthat::expect_error(super_assign_check()) # the function has default values for all args except x.
    ########  end argument with no default values

    ######## management of NULL arguments
    # all the arguments must be present
    testthat::expect_error(super_assign_check(x = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # all the arguments must be present
    testthat::expect_error(super_assign_check(x = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    testthat::expect_no_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")) # documented non-empty error_text usage (added to error messages)
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # all the arguments must be present
    testthat::expect_error(super_assign_check(x = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(super_assign_check(x = fun7, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(super_assign_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(super_assign_check(x = fun7, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.0: unguarded lib_path check)
    testthat::expect_error(super_assign_check(x = fun7, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    ######## end check of lib_path

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x
    testthat::expect_error(super_assign_check(x = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = -1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = 1:2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = str1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(super_assign_check(x = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end x
    ######## end argument checking with arg_check()

    #### end argument secondary checking

    #### main code
    # clean functions: message and NULL returned invisibly
    testthat::expect_message(super_assign_check(x = fun3, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # <-, -> and = assignments
    testthat::expect_message(super_assign_check(x = fun4, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # strings are immune
    testthat::expect_message(super_assign_check(x = fun5, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # comments are ignored
    testthat::expect_message(super_assign_check(x = fun6, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # backquoted `<<-` immune
    testthat::expect_message(super_assign_check(x = fun8, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # function without braces
    testthat::expect_message(super_assign_check(x = lambda_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # saferDev functions
    testthat::expect_message(super_assign_check(x = super_assign_check, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN", fixed = TRUE) # self check
    testthat::expect_message(super_assign_check(x = base::mean, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE base::mean(), EVERYTHING SEEMS CLEAN", fixed = TRUE) # message normalization of pkg::fun
    testthat::expect_null(object = base::suppressMessages(expr = super_assign_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    testthat::expect_invisible(call = base::suppressMessages(expr = super_assign_check(x = fun7, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    # superassignments: error
    testthat::expect_error(super_assign_check(x = fun10, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "THE SUPERASSIGNMENT OPERATORS <<- AND ->> ARE NOT ALLOWED IN SAFER-R FUNCTIONS", fixed = TRUE) # <<-
    testthat::expect_error(super_assign_check(x = fun11, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "THE SUPERASSIGNMENT OPERATORS <<- AND ->> ARE NOT ALLOWED IN SAFER-R FUNCTIONS", fixed = TRUE) # ->>
    testthat::expect_error(super_assign_check(x = fun12, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun12(), THE SUPERASSIGNMENT", fixed = TRUE) # nested function
    testthat::expect_error(super_assign_check(x = fun13, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "<<-", fixed = TRUE) # with or without srcref
    testthat::expect_error(super_assign_check(x = fun10, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1"), regexp = "ERROR IN saferDev::super_assign_check() INSIDE P1::F1", fixed = TRUE) # error_text added
    # exact findings for a representative case (only the LINE and SYMBOL columns are compared, because the CODE column depends on the indentation of the checked function)
    msg12 <- base::tryCatch(expr = super_assign_check(x = fun12, safer_check = FALSE, lib_path = NULL, error_text = ""), error = function(e){base::conditionMessage(c = e)}, finally = NULL)
    res <- base::unlist(x = base::strsplit(x = msg12, split = "\n", fixed = TRUE), recursive = TRUE, use.names = FALSE)
    res <- res[base::grepl(x = res, pattern = "^[0-9]+\\t", perl = TRUE)]
    res_fields <- base::strsplit(x = res, split = "\t", fixed = TRUE)
    testthat::expect_identical(object = base::vapply(X = res_fields, FUN = function(x){x[1]}, FUN.VALUE = base::character(length = 1)), expected = base::c("3", "5"), label = "fun12 exact line numbers (string and comment of line 4 not reported)")
    testthat::expect_identical(object = base::vapply(X = res_fields, FUN = function(x){x[2]}, FUN.VALUE = base::character(length = 1)), expected = base::c("<<-", "->>"), label = "fun12 exact symbols")
    testthat::expect_true(object = base::grepl(x = res[1], pattern = "base::lapply(X = 1:2, FUN = function(i){ y <<- i })", fixed = TRUE), label = "fun12 code column")
    # TAB-indented function: same report as with space indentation (only the line numbers of the parse data are used)
    txt_tab <- "function(x){\n\ty <- \"a\tb\" ;\tz <<- 1\n\tx ->> w\n}"
    fun_tab <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_tab, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_tab, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    msg_tab <- base::tryCatch(expr = saferDev::super_assign_check(x = fun_tab, safer_check = FALSE, lib_path = NULL, error_text = ""), error = function(e){base::conditionMessage(c = e)}, finally = NULL)
    res_tab <- base::unlist(x = base::strsplit(x = msg_tab, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    res_tab <- res_tab[base::grepl(x = res_tab, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
    testthat::expect_identical(object = res_tab, expected = base::c("2\t<<-\t\ty <- \"a\tb\" ;\tz <<- 1", "3\t->>\t\tx ->> w"), label = "TAB-indented function: lines, symbols and code")
    # superassignment written as a call of the operator, backquoted or between quotes, with or without base::, reported with the symbol <<-, whether x has source references or not (formerly, not reported with source references, nor after base:: without them). Not the string "<<-" elsewhere, nor the call of a list component named <<-
    txt_call <- "function(x){\n    base::`<<-`(y, 1)\n    `<<-`(z, 2) ; \"<<-\"(w, 3)\n    base::\x27<<-\x27(v, 4)\n    u <- base::c(\"<<-\", x$`<<-`(1))\n}"
    fun_call_src <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_call, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_call, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    fun_call_nosrc <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_call, prompt = "?", keep.source = FALSE, srcfile = NULL, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # printed deparsed: z <<- 2 and w <<- 3, but base::`<<-`(y, 1) and base::"<<-"(v, 4) unchanged
    msg_call_src <- base::tryCatch(expr = saferDev::super_assign_check(x = fun_call_src, safer_check = FALSE, lib_path = NULL, error_text = ""), error = function(e){base::conditionMessage(c = e)}, finally = NULL)
    msg_call_nosrc <- base::tryCatch(expr = saferDev::super_assign_check(x = fun_call_nosrc, safer_check = FALSE, lib_path = NULL, error_text = ""), error = function(e){base::conditionMessage(c = e)}, finally = NULL)
    res_call_src <- base::unlist(x = base::strsplit(x = msg_call_src, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    res_call_nosrc <- base::unlist(x = base::strsplit(x = msg_call_nosrc, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE), recursive = TRUE, use.names = FALSE)
    testthat::expect_identical(object = res_call_src[base::grepl(x = res_call_src, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)], expected = base::c("2\t<<-\t\tbase::`<<-`(y, 1)", "3\t<<-\t\t`<<-`(z, 2) ; \"<<-\"(w, 3)", "3\t<<-\t\t`<<-`(z, 2) ; \"<<-\"(w, 3)", "4\t<<-\t\tbase::\x27<<-\x27(v, 4)"), info = NULL, label = "operator called as a function, with source references", expected.label = NULL)
    testthat::expect_identical(object = res_call_nosrc[base::grepl(x = res_call_nosrc, pattern = "^[0-9]+\\t", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)], expected = base::c("3\t<<-\t\tbase::`<<-`(y, 1)", "4\t<<-\t\tz <<- 2", "5\t<<-\t\tw <<- 3", "6\t<<-\t\tbase::\"<<-\"(v, 4)"), info = NULL, label = "operator called as a function, without source references", expected.label = NULL)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # the error is the same through the package call
    testthat::expect_error(saferDev::super_assign_check(x = fun10, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "INSIDE fun10(), THE SUPERASSIGNMENT", fixed = TRUE)
    # attributes of x, printed by R after its code: saferDev error that names them, raised before the code of x is recovered as printed (formerly, the printed attribute values were analyzed as code of x: raw error of the parser when they do not parse, e.g., a character string printed with its [1] prefix, and silent analysis when they parse, e.g., a formula or a function, with wrong results)
    fun_attr <- function(x){x}
    base::attr(x = fun_attr, which = "note") <- "hello"
    fun_attr_parse <- function(x){x}
    base::attr(x = fun_attr_parse, which = "fo") <- y ~ x
    base::attr(x = fun_attr_parse, which = "fn") <- function(z){z}
    testthat::expect_error(object = saferDev::super_assign_check(x = fun_attr, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::super_assign_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nnote\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::super_assign_check(x = fun_attr_parse, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::super_assign_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\nfo\nfn\n\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    # remedy given by the error message: once the attribute is removed, the function is checked. The attributes that print() does not show (source references and comment) are accepted
    base::attr(x = fun_attr, which = "note") <- NULL
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::super_assign_check(x = fun_attr, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = NULL)
    base::comment(x = fun_attr) <- "comment not printed by print()"
    testthat::expect_null(object = base::suppressMessages(expr = saferDev::super_assign_check(x = fun_attr, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"), info = NULL, label = NULL)
    # object inserted in the body of x that is not printed as R code: saferDev error with the message of the R parser (only its position is compared, because the parser messages are translated), instead of the raw error of the parser
    fun_body_env <- function(x){x}
    base::body(fun = fun_body_env, envir = base::environment(fun = fun_body_env)) <- base::call(name = "identity", base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L))
    testthat::expect_error(object = saferDev::super_assign_check(x = fun_body_env, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "ERROR IN saferDev::super_assign_check().\n\nTHE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS CODE, AS PRINTED BY R, DOES NOT PARSE (E.G., AN OBJECT INSERTED IN ITS BODY THAT IS NOT PRINTED AS R CODE, LIKE AN ENVIRONMENT, PRINTED <environment>).\n\nERROR MESSAGE OF THE R PARSER:\n", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
    testthat::expect_error(object = saferDev::super_assign_check(x = fun_body_env, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "\n\nERROR MESSAGE OF THE R PARSER:\n<text>:[0-9]+:[0-9]+: ", class = NULL, fixed = FALSE, inherit = TRUE, info = NULL, label = NULL)
    ## end other tests

    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests

})
