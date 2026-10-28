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
    # STATUS being only messages or GOOD (no proposal): nothing applied
    tempo_output <- output_fun1
    tempo_output$STATUS <- base::c("GOOD", "HAS TO BE FULLY WRITTEN")
    testthat::expect_no_error(tempo_out2 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out2$fun))
    testthat::expect_true(object = base::length(x = tempo_out2$left) == 0)
    # MISSING_ARGS empty: nothing applied
    tempo_output <- output_fun1
    tempo_output$MISSING_ARGS <- base::c("", "")
    testthat::expect_no_error(tempo_out3 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out3$fun))
    ######## end main code: no correction to apply

    ######## main code: validations 1-3 (left routing)
    # validation 1: STATUS not parsable
    tempo_output <- output_fun1
    tempo_output$STATUS[1] <- "sum(1:2, na.rm" # not parsable
    testthat::expect_no_error(tempo_out4 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.function(x = tempo_out4$fun)) # the other correction (paste0) is still applied
    testthat::expect_true(object = base::length(x = tempo_out4$left) == 1)
    testthat::expect_true(object = base::all(base::grepl(x = tempo_out4$left, pattern = "\n    REASON: the proposed call does not parse as R code", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = FALSE)) # the reason is written below each correction not applied
    testthat::expect_true(object = base::length(x = tempo_out4$inserted) == 1)
    # validation 1: STATUS with empty argument value " = )" (parses as missing arg): applied, because empty values are allowed by the safer-r rules (default value that cannot be copied, or no default value)
    tempo_output <- output_fun1
    tempo_output$STATUS[1] <- "sum(1:2, na.rm = )" # the " = )" empty value
    testthat::expect_no_error(tempo_out5 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.function(x = tempo_out5$fun))
    testthat::expect_true(object = base::length(x = tempo_out5$left) == 0)
    testthat::expect_true(object = base::length(x = tempo_out5$inserted) == 2)
    testthat::expect_true(object = base::grepl(x = tempo_out5$fun_txt, pattern = "sum(1:2, na.rm = )", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE))
    # validation 1: STATUS that does not parse
    tempo_output$STATUS[1] <- "sum(1:2, na.rm = FALSE" # missing closing bracket
    testthat::expect_no_error(tempo_out5 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::length(x = tempo_out5$left) == 1)
    testthat::expect_true(object = base::length(x = tempo_out5$inserted) == 1)
    # validation 1: LINE_NB beyond the length of out$code: this is caught by the coherence check of the second round of checking (INTERNAL ERROR 1), not routed to left
    tempo_output <- output_fun1
    tempo_output$LINE_NB <- base::c(99, 99)
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    # validation 1: FUN_POS = 0: idem, caught by the coherence check (INTERNAL ERROR 1)
    tempo_output <- output_fun1
    tempo_output$FUN_POS <- base::c(0, 0)
    testthat::expect_error(.all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    # validation 2: FUN_ARGS not parsable (multi-line call truncated)
    tempo_output <- output_fun1
    tempo_output$FUN_ARGS[1] <- "sum(1:2,"
    testthat::expect_no_error(tempo_out8 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.function(x = tempo_out8$fun)) # the other correction (paste0) is still applied
    testthat::expect_true(object = base::length(x = tempo_out8$left) == 1)
    testthat::expect_true(object = base::all(base::grepl(x = tempo_out8$left, pattern = "\n    REASON: the call is written on several lines", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = FALSE)) # the reason is written below each correction not applied
    testthat::expect_true(object = base::length(x = tempo_out8$inserted) == 1)
    # validation 3: FUN_ARGS not at FUN_POS in the out$code line
    tempo_output <- output_fun1
    tempo_output$FUN_POS[1] <- 3 # the FUN_ARGS "sum(1:2, na.rm)" is not at position 3 of the line "missing_arg1 <- base::sum(1:2, na.rm)"
    testthat::expect_no_error(tempo_out9 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.function(x = tempo_out9$fun)) # the other correction (paste0) is still applied
    testthat::expect_true(object = base::length(x = tempo_out9$left) == 1)
    testthat::expect_true(object = base::all(base::grepl(x = tempo_out9$left, pattern = "\n    REASON: the call cannot be located in the printed code", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = FALSE)) # the reason is written below each correction not applied
    testthat::expect_true(object = base::length(x = tempo_out9$inserted) == 1)
    ######## end main code: validations 1-3 (left routing)

    ######## main code: validation 4 (overlapping span: only the innermost correction applied, outer routed to left)
    FUN_NESTED <- function(v){
        res <- base::paste0(base::mean(1:2))
    }
    out_nested <- fd(x = FUN_NESTED, arg_user_setting = base::list(x = base::as.name(x = "FUN_NESTED")), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "")
    line_nested <- base::grep(x = out_nested$code, pattern = "^res <-", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    pos_paste0 <- base::nchar(x = base::sub(x = out_nested$code[line_nested], pattern = "^(.*base::)paste0.*$", replacement = "\\1", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), type = "chars", allowNA = FALSE, keepNA = NA) + 1 # position of "paste0" in the line
    pos_mean <- pos_paste0 + base::nchar(x = "paste0(base::", type = "chars", allowNA = FALSE, keepNA = NA) # position of "mean" in the line (after "base::paste0(base::")
    tempo_output <- base::data.frame(LINE_NB = base::c(line_nested, line_nested), FUN_NAME = base::c("mean", "paste0"), FUN_ARGS = base::c("mean(1:2)", "paste0(base::mean(1:2))"), FUN_POS = base::c(pos_mean, pos_paste0), DEF_ARGS = base::c("x, trim = 0, na.rm = FALSE", "..., collapse = NULL, recycle0 = FALSE"), MISSING_ARG_NAMES = base::c("x", "collapse, recycle0"), MISSING_ARGS = base::c("x = ", "collapse = NULL, recycle0 = FALSE"), STATUS = base::c("mean(x = 1:2)", "paste0(base::mean(1:2), collapse = NULL, recycle0 = FALSE)"), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    testthat::expect_no_error(tempo_out10 <- .all_args_insert(x = FUN_NESTED, out = out_nested, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.function(x = tempo_out10$fun))
    testthat::expect_true(object = base::length(x = tempo_out10$inserted) == 1) # only the innermost (mean) applied
    testthat::expect_true(object = base::length(x = tempo_out10$left) == 1) # the outer paste0 proposal routed to left
    testthat::expect_true(object = base::all(base::grepl(x = tempo_out10$left, pattern = "\n    REASON: another call is nested inside this call", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = FALSE)) # the reason is written below each correction not applied
    ######## end main code: validation 4 (overlapping span)

    ######## main code: validation 5 (positional bare symbol duplicated as named arg: routed to left)
    FUN_POSARG <- function(v){
        res <- base::sum(v, na.rm)
    }
    out_posarg <- fd(x = FUN_POSARG, arg_user_setting = base::list(x = base::as.name(x = "FUN_POSARG")), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "")
    line_posarg <- base::grep(x = out_posarg$code, pattern = "^res <-", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    pos_sum <- base::nchar(x = base::sub(x = out_posarg$code[line_posarg], pattern = "^(.*base::)sum.*$", replacement = "\\1", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), type = "chars", allowNA = FALSE, keepNA = NA) + 1 # position of "sum" in the line
    tempo_output <- base::data.frame(LINE_NB = line_posarg, FUN_NAME = "sum", FUN_ARGS = "sum(v, na.rm)", FUN_POS = pos_sum, DEF_ARGS = "..., na.rm = FALSE", MISSING_ARG_NAMES = "na.rm", MISSING_ARGS = "na.rm = FALSE", STATUS = "sum(v, na.rm, na.rm = FALSE)", row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    testthat::expect_no_error(tempo_out11 <- .all_args_insert(x = FUN_POSARG, out = out_posarg, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out11$fun))
    testthat::expect_true(object = base::length(x = tempo_out11$left) == 1)
    testthat::expect_true(object = base::all(base::grepl(x = tempo_out11$left, pattern = "\n    REASON: an argument is written without its name", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = FALSE)) # the reason is written below each correction not applied
    # validation 5 with an empty named set: the STATUS has no named argument (all positional): applied without triggering validation 5
    tempo_output <- output_fun1
    tempo_output$STATUS[1] <- "sum(1:2, na.rm)" # identical to FUN_ARGS: applied, no change
    testthat::expect_no_error(tempo_out12 <- .all_args_insert(x = FUN1, out = out_fun1, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.function(x = tempo_out12$fun))
    testthat::expect_true(object = base::length(x = tempo_out12$left) == 0) # both corrections applied
    testthat::expect_true(object = base::length(x = tempo_out12$inserted) == 2)
    ######## end main code: validation 5 (positional bare symbol duplicated as named arg)

    ######## main code: final guard (corrected code does not parse: all corrections discarded) # nocov
    # This branch cannot be triggered by argument values: each applied STATUS and FUN_ARGS is individually validated as parsable, and a parsable span is replaced by another parsable span at the same position, so the reassembled code cannot fail to parse without a bug in the validations above.
    # The # nocov markers are set in R/intern_.all_args_insert.R accordingly.
    ######## end main code: final guard

    ######## main code: comments and indentation preserved (corrections inserted into the printed lines of x, not into the normalized out$code)
    # the function is built with its source references (srcref), whatever the keep.source option of the session running the tests
    txt_com <- base::c(
        "function(x, y){",
        "    # first comment",
        "    missing_arg1 <- base::sum(1:2, na.rm) # inline comment",
        "        missing_arg2 <- base::paste0(\"a\", \"b\")   # misaligned line",
        "}"
    )
    FUN_COM <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_com, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_com, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL))
    out_com <- fd(x = FUN_COM, arg_user_setting = base::list(x = base::as.name(x = "FUN_COM")), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "")
    line_com1 <- base::grep(x = out_com$code, pattern = "^missing_arg1", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    line_com2 <- base::grep(x = out_com$code, pattern = "^missing_arg2", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    pos_com1 <- base::regexpr(pattern = "sum(", text = out_com$code[line_com1], ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[1]
    pos_com2 <- base::regexpr(pattern = "paste0(", text = out_com$code[line_com2], ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[1]
    tempo_output <- output_fun1
    tempo_output$LINE_NB <- base::c(line_com1, line_com2)
    tempo_output$FUN_POS <- base::c(pos_com1, pos_com2)
    tempo_output$STATUS[1] <- "sum(1:2, na.rm = FALSE)" # valid proposal (the STATUS of output_fun1 is routed to left by validation 5)
    testthat::expect_no_error(tempo_out13 <- .all_args_insert(x = FUN_COM, out = out_com, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::length(x = tempo_out13$inserted) == 2)
    testthat::expect_true(object = base::length(x = tempo_out13$left) == 0)
    tempo_txt13 <- base::strsplit(x = tempo_out13$fun_txt, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
    testthat::expect_equal(object = tempo_txt13[2], expected = "    # first comment") # full comment line kept
    testthat::expect_equal(object = tempo_txt13[3], expected = base::paste0("    missing_arg1 <- base::", tempo_output$STATUS[1], " # inline comment", collapse = NULL, recycle0 = FALSE)) # indentation and inline comment kept
    testthat::expect_equal(object = tempo_txt13[4], expected = base::paste0("        missing_arg2 <- base::", tempo_output$STATUS[2], "   # misaligned line", collapse = NULL, recycle0 = FALSE)) # unusual indentation and spaces kept
    testthat::expect_false(object = base::is.null(x = base::attr(x = tempo_out13$fun, which = "srcref", exact = TRUE))) # the returned function carries its source references, thus prints with its comments
    ######## end main code: comments and indentation preserved

    ######## main code: validation 6 (span not located in the printed lines of x: out not built from x)
    tempo_x_lines <- txt_com # same line indexing as out_com$code (srcref printing)
    tempo_x_lines[line_com1] <- "    x" # shorter than FUN_POS + nchar(FUN_ARGS): span beyond the line
    tempo_x_lines[line_com2] <- base::sub(pattern = "paste0", replacement = "paste1", x = tempo_x_lines[line_com2], ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE) # same length but different text
    FUN_DIFF <- base::eval(expr = base::parse(file = "", n = NULL, text = tempo_x_lines, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = tempo_x_lines, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL))
    testthat::expect_no_error(tempo_out14 <- .all_args_insert(x = FUN_DIFF, out = out_com, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out14$fun))
    testthat::expect_true(object = base::length(x = tempo_out14$left) == 2)
    testthat::expect_true(object = base::all(base::grepl(x = tempo_out14$left, pattern = "\n    REASON: the call cannot be located in the printed code", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = FALSE)) # the reason is written below each correction not applied
    ######## end main code: validation 6

    ######## main code: validation 7 (string literal with consecutive spaces: routed to left, because the STATUS carries the collapsed string)
    txt_str <- base::c(
        "function(x){",
        "    res <- base::paste0(\"a  b\", \"c\")",
        "}"
    )
    FUN_STR <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_str, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_str, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL))
    out_str <- fd(x = FUN_STR, arg_user_setting = base::list(x = base::as.name(x = "FUN_STR")), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "")
    line_str <- base::grep(x = out_str$code, pattern = "^res <-", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    pos_str <- base::regexpr(pattern = "paste0(", text = out_str$code[line_str], ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)[1]
    tempo_output <- base::data.frame(LINE_NB = line_str, FUN_NAME = "paste0", FUN_ARGS = "paste0(\"a b\", \"c\")", FUN_POS = pos_str, DEF_ARGS = "..., collapse = NULL, recycle0 = FALSE", MISSING_ARG_NAMES = "collapse, recycle0", MISSING_ARGS = "collapse = NULL, recycle0 = FALSE", STATUS = "paste0(\"a b\", \"c\", collapse = NULL, recycle0 = FALSE)", row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    testthat::expect_no_error(tempo_out15 <- .all_args_insert(x = FUN_STR, out = out_str, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.null(x = tempo_out15$fun))
    testthat::expect_true(object = base::length(x = tempo_out15$left) == 1)
    testthat::expect_true(object = base::all(base::grepl(x = tempo_out15$left, pattern = "\n    REASON: a string of the call contains consecutive spaces", fixed = TRUE, ignore.case = FALSE, perl = FALSE, useBytes = FALSE), na.rm = FALSE)) # the reason is written below each correction not applied
    # runs of spaces outside string literals: the correction is applied
    txt_sp <- base::c(
        "function(x){",
        "    res <- base::paste0(\"a\",    \"c\")",
        "}"
    )
    FUN_SP <- base::eval(expr = base::parse(file = "", n = NULL, text = txt_sp, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = txt_sp, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL))
    out_sp <- fd(x = FUN_SP, arg_user_setting = base::list(x = base::as.name(x = "FUN_SP")), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "")
    tempo_output$FUN_ARGS <- "paste0(\"a\", \"c\")"
    tempo_output$STATUS <- "paste0(\"a\", \"c\", collapse = NULL, recycle0 = FALSE)"
    testthat::expect_no_error(tempo_out16 <- .all_args_insert(x = FUN_SP, out = out_sp, output = tempo_output, lib_path = NULL, error_text = ""))
    testthat::expect_true(object = base::is.function(x = tempo_out16$fun))
    testthat::expect_true(object = base::grepl(x = tempo_out16$fun_txt, pattern = "    res <- base::paste0(\"a\", \"c\", collapse = NULL, recycle0 = FALSE)", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE))
    ######## end main code: validation 7

    ## end tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
