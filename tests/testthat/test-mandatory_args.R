# This file tests the "mandatory arg of safer-r functions" checking block present in every saferDev function
# This block fires only if the function is written without one of the mandatory formals (lib_path, safer_check, error_text for public functions; lib_path, error_text for internals; error_text for .base_op_check)
# It cannot be triggered by user values: the test uses a temporary function built from the same body but with the mandatory formal removed from formals()
# No # nocov markers are used, and no R source files are modified: the block is actually executed by the tests

testthat::test_that("mandatory arg block of safer-r functions (public functions)", {

    ## data argument values
    # full calls with all formals except the mandatory one to drop (lib_path for all these functions)
    full_calls <- base::list(
        all_args_here = base::quote(all_args_here(x = "a", export = FALSE, out_path = ".", df_name = "res.tsv", overwrite = FALSE, safer_check = FALSE, error_text = "")),
        arg_check = base::quote(arg_check(data = 1, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, error_text = "")),
        colons_check = base::quote(colons_check(x = "a", safer_check = FALSE, error_text = "")),
        env_check = base::quote(env_check(pos = 1, name = NULL, safer_check = FALSE, error_text = "")),
        get_message = base::quote(get_message(data = "a", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = FALSE, error_text = "")),
        is_function_here = base::quote(is_function_here(fun = sum, safer_check = FALSE, error_text = "")),
        is_package_here = base::quote(is_package_here(req_package = "base", safer_check = FALSE, error_text = "")),
        curl_bracket_check = base::quote(curl_bracket_check(x = "a", safer_check = FALSE, error_text = "")),
        report = base::quote(report(data = data.frame(a = 1), output = "log.txt", path = ".", append = FALSE, rownames_kept = FALSE, vector_cat = FALSE, noquote = TRUE, sep = 2, safer_check = FALSE, error_text = ""))
    )
    ## end data argument values

    ## initialization of tests
    # none
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    ######## mandatory arg of safer-r functions
    for(fn in base::names(x = full_calls)){
        f <- base::get(x = fn, envir = base::asNamespace("saferDev"))
        fml <- base::formals(fun = f)
        fml_dropped <- fml[base::setdiff(x = base::names(x = fml), y = "lib_path")] # drop lib_path: the block must fire
        tempo_env <- base::new.env(parent = base::environment(fun = f))
        tempo_env[[fn]] <- base::as.function(base::c(fml_dropped, base::list(base::body(f))), envir = tempo_env)
        cl <- full_calls[[fn]]
        res <- base::tryCatch(
            expr = base::eval(expr = cl, envir = tempo_env),
            error = function(e){base::conditionMessage(e)}
        )
        testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT IS MANDATORY IN SAFER-R FUNCTIONS:", fixed = TRUE), label = base::paste0(fn, ": mandatory-args block message"))
        testthat::expect_true(object = base::grepl(x = res, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path reported as missing mandatory arg"))
        testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE), label = base::paste0(fn, ": no error before the mandatory-args block"))
    }
    ######## end mandatory arg of safer-r functions

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # restore check: with all formals present, the block must NOT fire
    testthat::expect_no_error(object = saferDev::get_message(data = "a", kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})

testthat::test_that("mandatory arg block of safer-r functions (internal functions)", {

    ## data argument values
    # full calls with all formals except the mandatory one to drop (lib_path for all these internals)
    intern_calls <- base::list(
        .all_args_here_fill = base::quote(.all_args_here_fill(arg_full = base::list(), arg_full_names = "x", tempo_split = "x", three_dots_log = FALSE, i2 = 1L, col1_i2 = 1L, col2_i2 = "length", arg_user_setting_x = "x", warn = NULL, warn_count = 0L, error_text = "")),
        .colons_check_message = base::quote(.colons_check_message(list_fun = base::list(), list_fun_pos = base::list(), line_nb = base::integer(), ini = base::character(), arg_user_setting2 = base::list(), text = "BASIC", internal_fun_names = base::character(), error_text = "")),
        .extract_all_fun_names = base::quote(.extract_all_fun_names(text = "a", pattern = "a", error_text = "")),
        .functions_detect = base::quote(.functions_detect(x = "a", skipped_base = FALSE, arg_user_setting2 = base::list(), error_text = "")),
        .fun_args_pos = base::quote(.fun_args_pos(text = "a", pattern = "a", error_text = "")),
        .has_odd_number_of_quotes = base::quote(.has_odd_number_of_quotes(input_string = "a", pattern = "a", error_text = "")),
        .in_parenthesis_replacement = base::quote(.in_parenthesis_replacement(string = "a", pattern = "a", no_regex_pattern = FALSE, replacement = "b", perl = TRUE, open_pos = 1L, close_pos = 2L, error_text = "")),
        .in_quotes_replacement = base::quote(.in_quotes_replacement(string = "a", pattern = "a", no_regex_pattern = FALSE, replacement = "b", perl = TRUE, error_text = "")),
        .noclean_functions = base::quote(.noclean_functions(col1 = base::character(), col2 = base::character(), col3 = base::character(), ini = base::character(), error_text = "")),
        .pack_and_function_check = base::quote(.pack_and_function_check(fun = sum, error_text = ""))
    )
    ## end data argument values

    ## initialization of tests
    # none
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    ######## mandatory arg of safer-r functions
    for(fn in base::names(x = intern_calls)){
        f <- base::get(x = fn, envir = base::asNamespace("saferDev"))
        fml <- base::formals(fun = f)
        fml_dropped <- fml[base::setdiff(x = base::names(x = fml), y = "lib_path")] # drop lib_path: the block must fire
        tempo_env <- base::new.env(parent = base::environment(fun = f))
        tempo_env[[fn]] <- base::as.function(base::c(fml_dropped, base::list(base::body(f))), envir = tempo_env)
        cl <- intern_calls[[fn]]
        res <- base::tryCatch(
            expr = base::eval(expr = cl, envir = tempo_env),
            error = function(e){base::conditionMessage(e)}
        )
        testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT IS MANDATORY IN SAFER-R FUNCTIONS:", fixed = TRUE), label = base::paste0(fn, ": mandatory-args block message"))
        testthat::expect_true(object = base::grepl(x = res, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path reported as missing mandatory arg"))
        testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE), label = base::paste0(fn, ": no error before the mandatory-args block"))
    }
    ######## end mandatory arg of safer-r functions

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # .base_op_check() special case: error_text is used before the mandatory block for the error message text
    # drop error_text from formals but define error_text in the function env so the early paste0(error_text) resolves lexically before the block fires
    f <- base::get(x = ".base_op_check", envir = base::asNamespace("saferDev"))
    tempo_env <- base::new.env(parent = base::environment(fun = f))
    tempo_env[["error_text"]] <- ""
    tempo_env[[".base_op_check"]] <- base::as.function(base::c(base::formals(fun = f)[base::character()], base::list(base::body(f))), envir = tempo_env)
    res <- base::tryCatch(
        expr = base::eval(expr = base::quote(.base_op_check()), envir = tempo_env),
        error = function(e){base::conditionMessage(e)}
    )
    testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT IS MANDATORY IN SAFER-R FUNCTIONS:", fixed = TRUE), label = ".base_op_check: mandatory-args block message")
    testthat::expect_true(object = base::grepl(x = res, pattern = "error_text", fixed = TRUE), label = ".base_op_check: error_text reported as missing mandatory arg")
    testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE), label = ".base_op_check: no error before the mandatory-args block")
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
