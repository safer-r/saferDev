test_that("all_args_here()", {
    source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R")
    # expected3 <- read.table("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/all_args_here_on_all_args_here_res.tsv", sep = "\t", header = TRUE) # too complicate to get the same
    FUN1 <- function(x, y){
        code_for_col <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any")
        code_for_col2 <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y)))
        middle_bracket <- base::do.call(what = base::c, args = code_for_col)
        middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())
    }

    FUN2 <- function(x, y){
        middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())
    }

  # Simple examples
    result1 <- saferDev::get_message("all_args_here(x = test2)", kind = "error", print.no = TRUE, text = NULL)
    # warning LINE 13 can be LINE 22
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN all_args_here() OF THE saferDev PACKAGE\nTHE TESTED FUNCTION test2 SEEMS TO HAVE A WRITTING ERROR IN LINE 13 AND FUNCTION length.\nPLEASE, RUN THE TESTED FUNCTION FIRST.\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    result2 <- saferDev::get_message("all_args_here(x = mean)", kind = "error", print.no = TRUE, text = NULL)
    expected2 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN all_args_here() OF THE saferDev PACKAGE\nCANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE 2:\n\nUseMethod(\"mean\")\n\nPLEASE, RUN saferDev::colons_check(mean) FIRST,\nADD THE MISSING <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> FOR FUNCTION STARTING BY A DOT)\nAND RERUN saferDev::all_args_here(mean)\n\n================\n\n\n"
    testthat::expect_equal(result2, expected2)

    testthat::expect_error(all_args_here(x = "a"))
    testthat::expect_error(all_args_here(x = test2, export = "a"))
    testthat::expect_error(all_args_here(x = test2, path_out = 1))
    testthat::expect_error(all_args_here(x = test2, path_out = "a"))
    testthat::expect_error(all_args_here(x = test2, df_name = 1))
    testthat::expect_error(all_args_here(x = test2, overwrite = "a"))
    testthat::expect_error(all_args_here(x = test2, lib_path = "a"))
    testthat::expect_error(all_args_here(x = test2, safer_check = "a"))

  # sophisticated example

    testthat::expect_no_error(all_args_here(
        x = all_args_here, # R function
        export = TRUE, # export the data frame into a .tsv file?
        path_out = ".", # pathway of the folder where to export the data frame
        df_name = "a.tsv", # name of the exported data frame file
        overwrite = TRUE, # Overwrite potential df_name file already existing in path_out?
        lib_path = NULL, # absolute pathways of the directories containing the required packages if not in the default directories
        safer_check = FALSE # perform some "safer" checks? Warning : always set this argument to FALSE if all_args_here() is used inside another safer function.
    ))

    result3 <- all_args_here(
        x = FUN1, # R function
        export = FALSE, # export the data frame into a .tsv file?
        path_out = ".", # pathway of the folder where to export the data frame
        df_name = "a.tsv", # name of the exported data frame file
        overwrite = TRUE, # Overwrite potential df_name file already existing in path_out?
        lib_path = NULL, # absolute pathways of the directories containing the required packages if not in the default directories
        safer_check = FALSE # perform some "safer" checks? Warning : always set this argument to FALSE if all_args_here() is used inside another safer function.
    )
    expected3 <- data.frame(
        LINE_NB = c(2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 5, 5), 
        FUN_NAME = c("as.vector", "unlist", "mapply", "rep", "length", "as.vector", "unlist", "mapply", "rep", "length", "do.call", "do.call", "parent.frame"), 
        FUN_ARGS = c('as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any")', 'unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE)', 'mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)', 'rep(x = y, base::length(x = x))', 'length(x = x)', 'as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y)))', 'unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y))', 'mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y)', 'rep(x = y, base::length(x = x))', 'length(x = x)', 'do.call(what = base::c, args = code_for_col)', 'do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())', 'parent.frame()'), 
        FUN_POS = c(23, 43, 60, 94, 111, 24, 44, 61, 95, 112, 25, 26, 100), 
        DEF_ARGS = c('x, mode = "any"', 'x, recursive = TRUE, use.names = TRUE', 'FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE', 'x, ...', 'x', 'x, mode = "any"', 'x, recursive = TRUE, use.names = TRUE', 'FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE', 'x, ...', 'x', 'what, args, quote = FALSE, envir = parent.frame()', 'what, args, quote = FALSE, envir = parent.frame()', 'n = 1'), 
        MISSING_ARG_NAMES = c('', '', '', '', '', 'mode', 'recursive, use.names', 'MoreArgs, SIMPLIFY, USE.NAMES', '', '', 'quote, envir', '', 'n'), 
        MISSING_ARGS = c('', '', '', '', '', 'mode = "any"', 'recursive = TRUE, use.names = TRUE', 'MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE', '', '', 'quote = FALSE, envir = parent.frame()', '', 'n = 1'), 
        STATUS = c('GOOD', 'GOOD', 'GOOD', 'GOOD', 'GOOD', 'as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y)), mode = \"any\")', 'unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = x, y = y), recursive = TRUE, use.names = TRUE)', 'mapply( x = x,  y = y, FUN = function(x, y){base::rep(x = y, base::length(x = x))}, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)', 'GOOD', 'GOOD', 'do.call(what = base::c,  args = code_for_col, quote = FALSE, envir = parent.frame())', 'GOOD', 'parent.frame(n = 1)')
        # a space between mapply( and x because FUN was the first arg in the initial writting, replaced by x in the final writting, which is preceeded by a space after string split.
    )
    testthat::expect_equal(result3, expected3)

    result4 <- all_args_here(
        x = FUN2, # R function
        export = FALSE, # export the data frame into a .tsv file?
        path_out = ".", # pathway of the folder where to export the data frame
        df_name = "a.tsv", # name of the exported data frame file
        overwrite = FALSE, # Overwrite potential df_name file already existing in path_out?
        lib_path = NULL, # absolute pathways of the directories containing the required packages if not in the default directories
        safer_check = FALSE # perform some "safer" checks? Warning : always set this argument to FALSE if all_args_here() is used inside another safer function.
    )
    expected4 <- data.frame(
        LINE_NB = c(2,2), 
        FUN_NAME = c("do.call", "parent.frame"), 
        FUN_ARGS = c("do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())", "parent.frame()"), 
        FUN_POS = c(26, 100), 
        DEF_ARGS = c("what, args, quote = FALSE, envir = parent.frame()", "n = 1"), 
        MISSING_ARG_NAMES = c("", "n"), 
        MISSING_ARGS = c("", "n = 1"), 
        STATUS = c("GOOD", "parent.frame(n = 1)")
    )
    testthat::expect_equal(result4, expected4)
})

