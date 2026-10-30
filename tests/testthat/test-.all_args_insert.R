# This file tests the internal function .all_args_insert() directly, like test-.all_args_here_fill.R does for .all_args_here_fill()
# It covers the branches that the real caller (all_args_here()) cannot trigger: internal error blocks, non-standard argument values, and the specific validation branches of the correction logic

testthat::test_that(".all_args_insert()", {

    ## data argument values
    FUN1 <- function(x, y){
        missing_arg1 <- base::sum(1:2, na.rm)
        missing_arg2 <- base::paste0("a", "b")
    }
    FUN2 <- function(x, y){
        base::sum(x, y, na.rm = FALSE)
    }
    fd <- utils::getFromNamespace(x = ".functions_detect", ns = "saferDev", pos = , envir = )
    out_fun1 <- fd(x = FUN1, arg_user_setting = base::list(x = base::as.name(x = "FUN1")), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "")
    out_fun2 <- fd(x = FUN2, arg_user_setting = base::list(x = base::as.name(x = "FUN2")), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "")
    # the LINE_NB and FUN_POS values are derived from out$code (the printed function), so that the fixture is valid whatever the print style of the function (deparse style or srcref style depending on the way the test file is run)
    line_fun1 <- base::grep(x = out_fun1$code, pattern = "^missing_arg1", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    pos_fun1 <- base::nchar(x = base::sub(x = out_fun1$code[line_fun1], pattern = "^(.*base::)sum.*$", replacement = "\\1", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), type = "chars", allowNA = FALSE, keepNA = NA) + 1 # position of "sum" in the line
    line_fun1_2 <- base::grep(x = out_fun1$code, pattern = "^missing_arg2", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    pos_fun1_2 <- base::nchar(x = base::sub(x = out_fun1$code[line_fun1_2], pattern = "^(.*base::)paste0.*$", replacement = "\\1", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), type = "chars", allowNA = FALSE, keepNA = NA) + 1 # position of "paste0" in the line
    output_fun1 <- base::data.frame(LINE_NB = base::c(line_fun1, line_fun1_2), FUN_NAME = base::c("sum", "paste0"), FUN_ARGS = base::c("sum(1:2, na.rm)", "paste0(\"a\", \"b\")"), FUN_POS = base::c(pos_fun1, pos_fun1_2), DEF_ARGS = base::c("..., na.rm = FALSE", "..., collapse = NULL, recycle0 = FALSE"), MISSING_ARG_NAMES = base::c("na.rm", "collapse, recycle0"), MISSING_ARGS = base::c("na.rm = FALSE", "collapse = NULL, recycle0 = FALSE"), STATUS = base::c("sum(1:2, na.rm, na.rm = FALSE)", "paste0(\"a\", \"b\", collapse = NULL, recycle0 = FALSE)"), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    ## end data argument values

    #### function name
    # pkg::fun call form (function_name[1] == "::()" | ":::()")
    testthat::expect_error(saferDev:::.all_args_insert()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()")
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(.all_args_insert())
    testthat::expect_error(.all_args_insert(error_text = "")) # safer error message of arg with no default values (even if the same objects exist in the R scope)
    ########  end argument with no default values

    ######## management of NULL arguments
    testthat::expect_error(.all_args_insert(x = NULL, out = out_fun1, output = output_fun1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_insert(x = FUN1, out = NULL, output = output_fun1, lib_path = NULL, error_text = ""))
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = NULL, lib_path = NULL, error_text = ""))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(.all_args_insert(x = character(), out = out_fun1, output = output_fun1, lib_path = character(), error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = output_fun1, lib_path = base::c(NA, NA), error_text = "")) # all-NA value
    ######## end management of NA arguments

    ######## argument checking with arg_check()
    testthat::expect_error(.all_args_insert(x = "a", out = out_fun1, output = output_fun1, lib_path = NULL, error_text = "")) # x must be a function
    testthat::expect_error(.all_args_insert(x = FUN1, out = "a", output = output_fun1, lib_path = NULL, error_text = "")) # out must be a list
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = matrix(nrow = 0, ncol = 0), lib_path = NULL, error_text = "")) # output must be a data frame
    ######## end argument checking with arg_check()

    ######## other checkings: coherence between output and out$code (INTERNAL ERROR 1)
    tempo_output <- output_fun1
    tempo_output$LINE_NB[1] <- 100 # LINE_NB beyond the length of out$code
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    tempo_output <- output_fun1
    tempo_output$FUN_POS[1] <- 0 # FUN_POS not positive
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    tempo_output <- output_fun1
    tempo_output$LINE_NB[1] <- base::as.numeric(x = NA)
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    tempo_output <- output_fun1
    tempo_output$FUN_POS[1] <- base::as.numeric(x = NA)
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    ######## end other checkings: coherence between output and out$code (INTERNAL ERROR 1)

    ######## main code: no correction to apply (empty output, empty keep)
    testthat::expect_no_error(tempo_out1 <- .all_args_insert(x = FUN2, out = out_fun2, output = base::data.frame(LINE_NB = base::integer(), FUN_NAME = base::character(), FUN_ARGS = base::character(), FUN_POS = base::integer(), DEF_ARGS = base::character(), MISSING_ARG_NAMES = base::character(), MISSING_ARGS = base::character(), STATUS = base::character(), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out1$fun))
    testthat::expect_true(object = base::is.null(x = tempo_out1$fun_txt))
    testthat::expect_true(object = base::length(x = tempo_out1$inserted) == 0)
    testthat::expect_true(object = base::length(x = tempo_out1$left) == 0)
    # STATUS being only empty or GOOD (no proposal): nothing applied
    tempo_output <- output_fun1
    tempo_output$STATUS <- base::c("GOOD", "")
    testthat::expect_no_error(tempo_out2 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out2$fun))
    testthat::expect_true(object = base::length(x = tempo_out2$left) == 0)
    # MISSING_ARGS empty: nothing applied
    tempo_output <- output_fun1
    tempo_output$MISSING_ARGS <- base::c("", "")
    testthat::expect_no_error(tempo_out3 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out3$fun))
    ######## end main code: no correction to apply

    ######## main code: helpers of the tests below
    # function built with its source references (srcref), whatever the keep.source option of the session running the tests
    mk_fun <- function(txt){
        base::eval(expr = base::parse(file = "", n = NULL, text = txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL))
    }
    # one row of the output table of all_args_here(), for the call fun_name( of the line starting by line_start in out$code (FUN_ARGS is not used by .all_args_insert())
    mk_row <- function(out, line_start, fun_name, def_args, missing_args, status){
        tempo_line <- base::grep(x = out$code, pattern = base::paste0("^", line_start, collapse = NULL, recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)[1]
        tempo_pos <- base::regexpr(pattern = base::paste0("::", fun_name, "(", collapse = NULL, recycle0 = FALSE), text = out$code[tempo_line], ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[1] + 2
        base::data.frame(LINE_NB = tempo_line, FUN_NAME = fun_name, FUN_ARGS = "", FUN_POS = tempo_pos, DEF_ARGS = def_args, MISSING_ARG_NAMES = "", MISSING_ARGS = missing_args, STATUS = status, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    }
    fd_fun <- function(f, name){
        fd(x = f, arg_user_setting = base::list(x = base::as.name(x = name)), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "")
    }
    ######## end main code: helpers of the tests below

    ######## main code: insertions (comments, line breaks, indentation and strings kept)
    txt_com <- base::c(
        "function(x, y){",
        "    # first comment",
        "    missing_arg1 <- base::sum(1:2) # inline comment",
        "        missing_arg2 <- base::paste0(\"a  b\", \"c\")   # misaligned line, string with consecutive spaces",
        "    missing_arg3 <- base::round(",
        "\t\tx # tab indented value written without name",
        "    )",
        "    missing_arg4 <- base::date()",
        "}"
    )
    FUN_COM <- mk_fun(txt = txt_com)
    out_com <- fd_fun(f = FUN_COM, name = "FUN_COM")
    tempo_output <- base::rbind(
        mk_row(out = out_com, line_start = "missing_arg1", fun_name = "sum", def_args = "..., na.rm = FALSE", missing_args = "na.rm = FALSE", status = "sum(1:2, na.rm = FALSE)"),
        mk_row(out = out_com, line_start = "missing_arg2", fun_name = "paste0", def_args = "..., collapse = NULL, recycle0 = FALSE", missing_args = "collapse = NULL, recycle0 = FALSE", status = "paste0(\"a b\", \"c\", collapse = NULL, recycle0 = FALSE)"), # STATUS built from the normalized code (consecutive spaces collapsed)
        mk_row(out = out_com, line_start = "missing_arg3", fun_name = "round", def_args = "x, digits = 0, ...", missing_args = "x = x, digits = 0", status = "round(x = x, digits = 0)"), # call written on several lines, with a value written without name
        mk_row(out = out_com, line_start = "missing_arg4", fun_name = "date", def_args = "z = 1", missing_args = "z = 1", status = "date(z = 1)"), # call without argument (fake DEF_ARGS)
        make.row.names = FALSE, stringsAsFactors = FALSE
    )
    testthat::expect_no_error(tempo_out4 <- .all_args_insert(x = FUN_COM, out = out_com, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::length(x = tempo_out4$inserted) == 4)
    testthat::expect_true(object = base::length(x = tempo_out4$left) == 0)
    testthat::expect_equal(object = base::strsplit(x = tempo_out4$fun_txt, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]], expected = base::c(
        "function(x, y){",
        "    # first comment",
        "    missing_arg1 <- base::sum(1:2, na.rm = FALSE) # inline comment",
        "        missing_arg2 <- base::paste0(\"a  b\", \"c\", collapse = NULL, recycle0 = FALSE)   # misaligned line, string with consecutive spaces",
        "    missing_arg3 <- base::round(",
        "\t\tx = x, digits = 0 # tab indented value written without name",
        "    )",
        "    missing_arg4 <- base::date(z = 1)",
        "}"
    ))
    testthat::expect_false(object = base::is.null(x = base::attr(x = tempo_out4$fun, which = "srcref", exact = TRUE))) # the returned function carries its source references, thus prints with its comments
    # nested calls on the same line, and STATUS not parsable (call written on several lines with several statements inside {}, joined into a single line in the STATUS): both applied
    txt_nest <- base::c(
        "function(v){",
        "    res <- base::switch(v, a = {",
        "        i <- 1 # comment inside the call",
        "        base::sum(i, base::mean(1:2))",
        "    }, b = 2)",
        "}"
    )
    FUN_NEST <- mk_fun(txt = txt_nest)
    out_nest <- fd_fun(f = FUN_NEST, name = "FUN_NEST")
    tempo_output <- base::rbind(
        mk_row(out = out_nest, line_start = "res <-", fun_name = "switch", def_args = "EXPR, ...", missing_args = "EXPR = v", status = "switch(a = { i <- 1 base::sum(i, base::mean(1:2)) }, b = 2,EXPR = v)"), # not parsable
        mk_row(out = out_nest, line_start = "base::sum", fun_name = "sum", def_args = "..., na.rm = FALSE", missing_args = "na.rm = FALSE", status = "sum(i, base::mean(1:2), na.rm = FALSE)"),
        mk_row(out = out_nest, line_start = "base::sum", fun_name = "mean", def_args = "x, ...", missing_args = "x = 1:2", status = "mean(x = 1:2)"),
        make.row.names = FALSE, stringsAsFactors = FALSE
    )
    testthat::expect_no_error(tempo_out5 <- .all_args_insert(x = FUN_NEST, out = out_nest, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::length(x = tempo_out5$inserted) == 3)
    testthat::expect_equal(object = base::strsplit(x = tempo_out5$fun_txt, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]], expected = base::c(
        "function(v){",
        "    res <- base::switch(EXPR = v, a = {",
        "        i <- 1 # comment inside the call",
        "        base::sum(i, base::mean(x = 1:2), na.rm = FALSE)",
        "    }, b = 2)",
        "}"
    ))
    ######## end main code: insertions

    ######## main code: corrections not applied (left routing, with their reason)
    # MISSING_ARGS not parsable, DEF_ARGS not parsable, MISSING_ARGS with a value without name, MISSING_ARGS with an empty argument: unreadable
    for(tempo_miss in base::c("na.rm = FALSE)", "FALSE", "na.rm = FALSE, ")){
        tempo_output <- mk_row(out = out_com, line_start = "missing_arg1", fun_name = "sum", def_args = "..., na.rm = FALSE", missing_args = tempo_miss, status = "sum(1:2, na.rm = FALSE)")
        testthat::expect_no_error(tempo_out6 <- .all_args_insert(x = FUN_COM, out = out_com, output = tempo_output, lib_path = NULL, error_text = ""))
        testthat::expect_true(object = base::is.null(x = tempo_out6$fun))
        testthat::expect_true(object = base::grepl(x = tempo_out6$left, pattern = "\n    REASON: the MISSING_ARGS or DEF_ARGS column of the call cannot be read", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    }
    tempo_output <- mk_row(out = out_com, line_start = "missing_arg1", fun_name = "sum", def_args = "..., na.rm = FALSE)", missing_args = "na.rm = FALSE", status = "sum(1:2, na.rm = FALSE)")
    testthat::expect_no_error(tempo_out6 <- .all_args_insert(x = FUN_COM, out = out_com, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::grepl(x = tempo_out6$left, pattern = "\n    REASON: the MISSING_ARGS or DEF_ARGS column of the call cannot be read", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # call not located: FUN_POS not on the function name, printed code of x not the one of out (position beyond the line), call with an empty argument (e.g., "c(1, )")
    tempo_output <- mk_row(out = out_com, line_start = "missing_arg1", fun_name = "sum", def_args = "..., na.rm = FALSE", missing_args = "na.rm = FALSE", status = "sum(1:2, na.rm = FALSE)")
    tempo_output$FUN_POS <- 3
    testthat::expect_no_error(tempo_out7 <- .all_args_insert(x = FUN_COM, out = out_com, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out7$fun))
    testthat::expect_true(object = base::grepl(x = tempo_out7$left, pattern = "\n    REASON: the call cannot be located in the code of the function", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    tempo_x_lines <- txt_com
    tempo_x_lines[3] <- "    x" # same line indexing as out_com$code (srcref printing), but line shorter than FUN_POS
    FUN_DIFF <- mk_fun(txt = tempo_x_lines)
    tempo_output <- mk_row(out = out_com, line_start = "missing_arg1", fun_name = "sum", def_args = "..., na.rm = FALSE", missing_args = "na.rm = FALSE", status = "sum(1:2, na.rm = FALSE)")
    testthat::expect_no_error(tempo_out8 <- .all_args_insert(x = FUN_DIFF, out = out_com, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::grepl(x = tempo_out8$left, pattern = "\n    REASON: the call cannot be located in the code of the function", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    FUN_EMPTY_ARG <- mk_fun(txt = base::c("function(x){", "    res <- base::sum(1, )", "}"))
    out_empty_arg <- fd_fun(f = FUN_EMPTY_ARG, name = "FUN_EMPTY_ARG")
    tempo_output <- mk_row(out = out_empty_arg, line_start = "res <-", fun_name = "sum", def_args = "..., na.rm = FALSE", missing_args = "na.rm = FALSE", status = "sum(1, , na.rm = FALSE)")
    testthat::expect_no_error(tempo_out9 <- .all_args_insert(x = FUN_EMPTY_ARG, out = out_empty_arg, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::grepl(x = tempo_out9$left, pattern = "\n    REASON: the call cannot be located in the code of the function", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # positional bare symbol duplicated as named argument (e.g., "sum(1:2, na.rm, na.rm = FALSE)"), and corrected call different from the STATUS proposal
    tempo_output <- output_fun1
    testthat::expect_no_error(tempo_out10 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.function(x = tempo_out10$fun)) # the other correction (paste0) is still applied
    testthat::expect_true(object = base::length(x = tempo_out10$inserted) == 1)
    testthat::expect_true(object = base::grepl(x = tempo_out10$left, pattern = "\n    REASON: an argument is written without its name", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    tempo_output$STATUS[2] <- "paste0(\"a\", \"b\", collapse = \"x\", recycle0 = FALSE)" # differs from the MISSING_ARGS
    testthat::expect_no_error(tempo_out11 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out11$fun))
    testthat::expect_true(object = base::any(base::grepl(x = tempo_out11$left, pattern = "\n    REASON: the corrected call does not match the proposal of the STATUS column", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = FALSE))
    ######## end main code: corrections not applied

    ## end tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
