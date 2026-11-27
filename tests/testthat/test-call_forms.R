testthat::test_that("call forms of the exported functions", {

    ## data argument values
    fun1 <- function(x){
        base::sum(x, na.rm = FALSE)
    }
    tempo_dir <- base::tempfile(pattern = "call_forms", tmpdir = base::tempdir(check = FALSE), fileext = "")
    base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    # every argument of each exported function written (safer_check = FALSE to save time)
    args_list <- base::list(
        all_args_here = base::list(x = fun1, table_export = FALSE, fun_export = FALSE, out_path = tempo_dir, df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""),
        arg_check = base::list(data = 1, class = "numeric", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = NULL, error_text = ""),
        colons_check = base::list(x = fun1, export = FALSE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = ""),
        curly_bracket_check = base::list(x = fun1, export = FALSE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = ""),
        env_check = base::list(pos = 1, name = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""),
        for_loop_nb_check = base::list(x = fun1, export = FALSE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = ""),
        get_message = base::list(data = "base::log(-1)", kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""),
        is_function_here = base::list(fun = "base::sum", safer_check = FALSE, lib_path = NULL, error_text = ""),
        is_package_here = base::list(req_package = "stats", safer_check = FALSE, lib_path = NULL, error_text = ""),
        lambda_check = base::list(x = fun1, export = FALSE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = ""),
        report = base::list(data = "a", output = "call_forms.txt", path = tempo_dir, append = TRUE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 2, safer_check = FALSE, lib_path = NULL, error_text = ""),
        super_assign_check = base::list(x = fun1, safer_check = FALSE, lib_path = NULL, error_text = ""),
        true_false_check = base::list(x = fun1, export = FALSE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = "")
    )
    testthat::expect_setequal(object = base::names(x = args_list), expected = base::getNamespaceExports(ns = "saferDev"))
    # code checkers (exported functions whose x argument is the function to check)
    checkers <- base::c("all_args_here", "colons_check", "curly_bracket_check", "for_loop_nb_check", "lambda_check", "super_assign_check", "true_false_check")
    # list containing fun1: x written as an expression that is not a name
    FUN_LIST <- base::list(fun = fun1)
    # function modified by colons_check() (mean() written without base::), built from a text so that this file contains no call without its package
    fun_cc <- base::eval(expr = base::parse(file = "", n = NULL, text = "function(x){mean(x = x)}", prompt = "?", keep.source = FALSE, srcfile = NULL, encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # writing of x in the calls built by base::lapply() and base::Map() (R internals, recovered here rather than written, to be independent of the R version)
    lapply_x <- base::lapply(X = base::list(1), FUN = function(x){base::deparse(expr = base::substitute(expr = x, env = base::environment(fun = NULL)), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)})[[1]]
    map_x <- base::Map(f = function(x){base::deparse(expr = base::substitute(expr = x, env = base::environment(fun = NULL)), width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)}, base::list(1))[[1]]
    ## end data argument values

    ## tests

    ######## call forms (the ... of the call are expanded in the frame that called the function, and the function can be called through its object)
    for(fn in base::names(x = args_list)){
        f <- base::get(x = fn, pos = -1L, envir = base::asNamespace(ns = "saferDev", base.OK = TRUE), mode = "function", inherits = FALSE)
        a <- args_list[[fn]]
        # through the function object (the call holds the function itself, not its name)
        testthat::expect_no_error(object = base::do.call(what = f, args = a, quote = FALSE, envir = base::environment(fun = NULL)), message = NULL, class = NULL)
        # through base::lapply(): first argument from X, the others through the ... of lapply()
        testthat::expect_no_error(object = base::do.call(what = base::lapply, args = base::c(base::list(X = base::list(a[[1]]), FUN = f), a[-1]), quote = FALSE, envir = base::environment(fun = NULL)), message = NULL, class = NULL)
        # through base::mapply() (base::Map() is a wrapper of it)
        testthat::expect_no_error(object = base::mapply(FUN = f, base::list(a[[1]]), MoreArgs = a[-1], SIMPLIFY = FALSE, USE.NAMES = TRUE), message = NULL, class = NULL)
        if(fn == "arg_check"){ # arg_check() with its default argument values returns its own error: at least one of options, class, typeof, mode, prop or length must be specified
            testthat::expect_error(object = base::Map(f = f, base::list(a[[1]])), regexp = "ERROR IN saferDev::<anonymous function>().\n\nAT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
        }else if(fn != "report"){ # report() has a second argument without default value (path)
            testthat::expect_no_error(object = base::Map(f = f, base::list(a[[1]])), message = NULL, class = NULL)
        }
        # through a function forwarding its ...
        wrapper <- function(...){
            f(...)
        }
        testthat::expect_no_error(object = base::do.call(what = wrapper, args = a, quote = FALSE, envir = base::environment(fun = NULL)), message = NULL, class = NULL)
    }
    # function forwarding empty ...
    wrapper2 <- function(...){
        saferDev::is_package_here(req_package = "stats", safer_check = FALSE, lib_path = NULL, error_text = "", ...)
    }
    testthat::expect_no_error(object = wrapper2(), message = NULL, class = NULL)
    # error messages
    result <- base::tryCatch(expr = base::do.call(what = saferDev::is_package_here, args = base::list(req_package = "NOT_A_PACKAGE_XYZ", safer_check = FALSE, lib_path = NULL, error_text = ""), quote = FALSE, envir = base::environment(fun = NULL)), error = function(e){base::conditionMessage(c = e)})
    testthat::expect_true(object = base::grepl(pattern = "ERROR IN saferDev::<anonymous function>()", x = result, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "do.call() with a function object")
    result <- base::tryCatch(expr = wrapper2(req_package = "NOT_A_PACKAGE_XYZ"), error = function(e){base::conditionMessage(c = e)})
    expected_result <- base::tryCatch(expr = (function(req_package){NULL})(req_package = 1, req_package = 2), error = function(e){base::conditionMessage(c = e)}) # error of R itself (argument matching, raised before the body of the function), in the language of the session
    testthat::expect_identical(object = result, expected = expected_result, info = NULL, label = "argument forwarded twice", expected.label = NULL)
    result <- base::tryCatch(expr = base::lapply(X = base::list("NOT_A_PACKAGE_XYZ"), FUN = saferDev::is_package_here, safer_check = FALSE, lib_path = NULL, error_text = ""), error = function(e){base::conditionMessage(c = e)})
    testthat::expect_true(object = base::grepl(pattern = "NOT_A_PACKAGE_XYZ", x = result, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), label = "error through lapply()")
    ######## end call forms

    ######## name of x in the messages of the code checkers
    # a single string: the writing of x in the call (name, pkg::name, pkg:::name or other expression, deparsed on one line), or x for a function object, which has no name. A name or an expression forwarded through the ... of a function is written as in a direct call (and not ..1, as written by base::match.call())
    expected_x <- base::c("fun1", "FUN_LIST$fun", "function(y) { base::sum(y, na.rm = FALSE) }", "x", lapply_x, map_x, "x", "fun1", "FUN_LIST$fun")
    for(fn in checkers){
        f <- base::get(x = fn, pos = -1L, envir = base::asNamespace(ns = "saferDev", base.OK = TRUE), mode = "function", inherits = FALSE)
        a <- args_list[[fn]]
        wrapper <- function(...){
            f(...)
        }
        # messages of the call forms, in the order of expected_x
        msg <- base::list(
            testthat::capture_messages(code = base::do.call(what = f, args = base::c(base::list(x = base::quote(expr = fun1)), a[-1]), quote = FALSE, envir = base::environment(fun = NULL))), # name
            testthat::capture_messages(code = base::do.call(what = f, args = base::c(base::list(x = base::quote(expr = FUN_LIST$fun)), a[-1]), quote = FALSE, envir = base::environment(fun = NULL))), # other expression
            testthat::capture_messages(code = base::do.call(what = f, args = base::c(base::list(x = base::quote(expr = function(y){base::sum(y, na.rm = FALSE)})), a[-1]), quote = FALSE, envir = base::environment(fun = NULL))), # inline definition, deparsed on several lines
            testthat::capture_messages(code = base::do.call(what = f, args = a, quote = FALSE, envir = base::environment(fun = NULL))), # function object
            testthat::capture_messages(code = base::do.call(what = base::lapply, args = base::c(base::list(X = base::list(fun1), FUN = f), a[-1]), quote = FALSE, envir = base::environment(fun = NULL))), # through base::lapply()
            testthat::capture_messages(code = base::Map(f = f, base::list(fun1))), # through base::Map(), with the default argument values
            testthat::capture_messages(code = base::do.call(what = wrapper, args = a, quote = FALSE, envir = base::environment(fun = NULL))), # function object forwarded through the ... of a function
            testthat::capture_messages(code = base::do.call(what = wrapper, args = base::c(base::list(x = base::quote(expr = fun1)), a[-1]), quote = FALSE, envir = base::environment(fun = NULL))), # name forwarded through the ... of a function
            testthat::capture_messages(code = base::do.call(what = wrapper, args = base::c(base::list(x = base::quote(expr = FUN_LIST$fun)), a[-1]), quote = FALSE, envir = base::environment(fun = NULL))) # expression forwarded through the ... of a function
        )
        for(i1 in base::seq_along(along.with = msg)){
            tempo_msg <- base::paste0(msg[[i1]], collapse = "", recycle0 = FALSE)
            tempo_label <- base::paste0(fn, "() with x written ", expected_x[i1], collapse = NULL, recycle0 = FALSE)
            testthat::expect_identical(object = base::sum(base::gregexpr(pattern = "INSIDE ", text = tempo_msg, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE), expected = 1L, info = NULL, label = tempo_label, expected.label = NULL) # a single message part naming x (and not one per element of the call)
            testthat::expect_true(object = base::grepl(pattern = base::paste0("INSIDE ", expected_x[i1], "(), ", collapse = NULL, recycle0 = FALSE), x = tempo_msg, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = tempo_label)
        }
    }
    ######## end name of x in the messages of the code checkers

    ######## export of the modified function: x must be designated by a name
    for(fn in checkers[checkers != "super_assign_check"]){ # super_assign_check() exports nothing
        f <- base::get(x = fn, pos = -1L, envir = base::asNamespace(ns = "saferDev", base.OK = TRUE), mode = "function", inherits = FALSE)
        a <- args_list[[fn]]
        a[[base::ifelse(test = fn == "all_args_here", yes = "fun_export", no = "export")]] <- TRUE
        wrapper <- function(...){
            f(...)
        }
        # function object, directly or forwarded through the ... of a function: no name for the file
        testthat::expect_error(object = base::do.call(what = f, args = a, quote = FALSE, envir = base::environment(fun = NULL)), regexp = "HERE IT IS:\nA FUNCTION OBJECT, WITHOUT ITS NAME (E.G., x = fun1 IN THE args ARGUMENT OF base::do.call(): USE x = base::quote(expr = fun1) INSTEAD)", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
        testthat::expect_error(object = base::do.call(what = wrapper, args = a, quote = FALSE, envir = base::environment(fun = NULL)), regexp = "HERE IT IS:\nA FUNCTION OBJECT, WITHOUT ITS NAME", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
        # other expression, written in the error message
        testthat::expect_error(object = base::do.call(what = base::lapply, args = base::c(base::list(X = base::list(fun1), FUN = f), a[-1]), quote = FALSE, envir = base::environment(fun = NULL)), regexp = base::paste0("HERE IT IS:\n", lapply_x, "\n", collapse = NULL, recycle0 = FALSE), class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
        testthat::expect_error(object = base::mapply(FUN = f, base::list(fun1), MoreArgs = a[-1], SIMPLIFY = FALSE, USE.NAMES = TRUE), regexp = base::paste0("HERE IT IS:\n", map_x, "\n", collapse = NULL, recycle0 = FALSE), class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = NULL)
        # name, directly (through base::quote(), as advised by the error message) or forwarded through the ... of a function: no error (fun1 is clean: no file written)
        testthat::expect_no_error(object = base::suppressMessages(expr = base::do.call(what = f, args = base::c(base::list(x = base::quote(expr = fun1)), a[-1]), quote = FALSE, envir = base::environment(fun = NULL)), classes = "message"), message = NULL, class = NULL)
        testthat::expect_no_error(object = base::suppressMessages(expr = base::do.call(what = wrapper, args = base::c(base::list(x = base::quote(expr = fun1)), a[-1]), quote = FALSE, envir = base::environment(fun = NULL)), classes = "message"), message = NULL, class = NULL)
    }
    # name forwarded through the ... of a function: the exported file is named after it, as in a direct call (and not ..1_safer.R)
    wrapper_cc <- function(...){
        saferDev::colons_check(...)
    }
    tempo_msg <- base::paste0(testthat::capture_messages(code = wrapper_cc(x = fun_cc, export = TRUE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = "")), collapse = "", recycle0 = FALSE)
    testthat::expect_true(object = base::grepl(pattern = "INSIDE fun_cc(), SOME :: OR ::: ARE MISSING BEFORE BASIC FUNCTIONS", x = tempo_msg, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = "name forwarded through the ...: message")
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun_cc_safer.R", fsep = "/")), info = NULL, label = "name forwarded through the ...: file name")
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir, "..1_safer.R", fsep = "/")), info = NULL, label = "name forwarded through the ...: no ..1_safer.R file")
    ######## end export of the modified function

    ######## nested call: safer functions called inside a function that is itself called through base::lapply()
    nested <- function(y){
        saferDev::super_assign_check(x = y, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE nested()")
        saferDev::lambda_check(x = y, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE nested()")
        saferDev::colons_check(x = y, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE nested()")
        saferDev::all_args_here(x = y, table_export = FALSE, fun_export = FALSE, out_path = NULL, df_name = "res.tsv", overwrite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE nested()")
        saferDev::curly_bracket_check(x = y, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE nested()")
        saferDev::true_false_check(x = y, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE nested()")
        saferDev::for_loop_nb_check(x = y, export = FALSE, out_path = NULL, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE nested()")
        saferDev::arg_check(data = y, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = TRUE, lib_path = NULL, error_text = " INSIDE nested()")
    }
    result <- NULL
    tempo_msg <- base::paste0(testthat::capture_messages(code = result <- base::lapply(X = base::list(fun1), FUN = nested)), collapse = "", recycle0 = FALSE)
    testthat::expect_identical(object = base::sum(base::gregexpr(pattern = "INSIDE y(), ", text = tempo_msg, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[[1]] > 0, na.rm = FALSE), expected = 7L, info = NULL, label = "nested call: x named y by the 7 code checkers", expected.label = NULL)
    for(fn in checkers[checkers != "all_args_here"]){
        testthat::expect_true(object = base::grepl(pattern = base::paste0("AFTER RUNNING saferDev::", fn, "().", collapse = NULL, recycle0 = FALSE), x = tempo_msg, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), info = NULL, label = base::paste0("nested call: name of ", fn, "()", collapse = NULL, recycle0 = FALSE))
    }
    testthat::expect_false(object = result[[1]]$problem, info = NULL, label = "nested call: arg_check() result")
    testthat::expect_identical(object = result[[1]]$object_name, expected = "y", info = NULL, label = "nested call: arg_check() object name", expected.label = NULL)
    ######## end nested call

    ######## internal functions called through their function object: named <anonymous function>() in their messages
    ns_saferdev <- base::asNamespace(ns = "saferDev", base.OK = TRUE)
    internal_list <- base::c(".all_args_here_fill", ".all_args_insert", ".base_op_check", ".colons_check_message", ".extract_all_fun_names", ".find_construct_body", ".fun_args_pos", ".functions_detect", ".has_odd_number_of_quotes", ".in_parenthesis_replacement", ".in_quotes_replacement", ".noclean_functions", ".pack_and_function_check")
    tempo_names <- base::ls(name = , pos = -1L, envir = ns_saferdev, all.names = TRUE, pattern = "^\\.[a-z]", sorted = TRUE)
    testthat::expect_setequal(object = internal_list, expected = tempo_names[base::vapply(X = tempo_names, FUN = function(x){base::is.function(x = base::get(x = x, pos = -1L, envir = ns_saferdev, mode = "any", inherits = FALSE))}, FUN.VALUE = TRUE, USE.NAMES = FALSE)])
    for(fn in internal_list){
        f <- base::get(x = fn, pos = -1L, envir = ns_saferdev, mode = "function", inherits = FALSE)
        if(fn == ".base_op_check"){ # error_text is its only argument
            testthat::expect_no_error(object = base::do.call(what = f, args = base::list(error_text = ""), quote = FALSE, envir = base::environment(fun = NULL)), message = NULL, class = NULL)
        }else{
            # only the reserved arguments written: error of the arguments without default value
            testthat::expect_error(object = base::do.call(what = f, args = base::list(lib_path = NULL, error_text = ""), quote = FALSE, envir = base::environment(fun = NULL)), regexp = "ERROR IN saferDev::<anonymous function>().", class = NULL, fixed = TRUE, inherit = TRUE, info = NULL, label = fn)
        }
    }
    ######## end internal functions called through their function object

    ######## lib_path: .libPaths() restored exactly, the site library not added back if it was removed
    ini_lib <- base::.libPaths(new = , include.site = TRUE)
    base::.libPaths(new = base::setdiff(x = ini_lib, y = base::.Library.site), include.site = FALSE)
    tempo_lib <- base::.libPaths(new = , include.site = TRUE)
    testthat::expect_no_error(object = saferDev::is_package_here(req_package = "stats", safer_check = FALSE, lib_path = tempo_dir, error_text = ""), message = NULL, class = NULL)
    testthat::expect_identical(object = base::.libPaths(new = , include.site = TRUE), expected = tempo_lib)
    base::.libPaths(new = ini_lib, include.site = FALSE)
    testthat::expect_identical(object = base::.libPaths(new = , include.site = TRUE), expected = ini_lib)
    ######## end lib_path

    ## end tests

    base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE)
    base::rm(list = base::ls())
})
