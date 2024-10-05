#' @title all_args_here
#' @description
#' Verify that all the functions used inside a function are written with all their arguments. For instance: base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE) and not paste0(letters[1:2]).
#' @param x a function name, written without quotes and brackets.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param export Single logical value. Export the data frame in a .tsv file?
#' @param path_out Single string indicating the name of the file and path where to export the data frame. Ignored if export is FALSE
#' @returns 
#' A data frame indicating the missing arguments or a message saying that everything seems fine.
#' If export argument is TRUE, then the data frame is exported as res.tsv instead of being returned.
#' Data frame: 
#' $LINE_NB: the line number in the function code (starting at the "<- function" line, i.e., without counting the #' header lines)
#' $FUN_NAME: the function name.
#' $FUN_ARGS: the written arguments of $FUN_NAME. "NOT_CONSIDERED" means that the function is between quotes or after $
#' $FUN_POS: the position of the first character of the function name in the $LINE_NB line of the code.
#' $DEF_ARGS: the defaults arguments of $FUN_NAME. "NO_ARGS" means that the function has no arguments
#' $MISSING_ARG_NAMES: the missing argument names in $FUN_ARGS.
#' $MISSING_ARGS: the missing arguments with their values in $FUN_ARGS.
#' $NEW: the new proposed argument writting for $FUN_NAME. "INACTIVATED" means that no proposal is returned for this function, for the reason explained in the "details" section. "GOOD" means that all the arguments are already written.
#' @details
#' More precisely, all_args_here() verifies that all the strings before an opening bracket "(" are written with all their arguments. Thus, it cannot check function names written without brackets, like in the FUN argument of some functions, e.g., sapply(1:3, FUN = as.character).
#' 
#' The perl regex used to detect a function name is: "[a-zA-Z.]{1}[a-zA-Z0-9._]*\\s*\\(".
#' 
#' Function names preceeded by $ and any space are not considered (pattern "\\$ *[a-zA-Z.]{1}[a-zA-Z0-9._]* *\\(")
#'  
#' The following R functions using bracket are not considered: "function", "if", "for", "while" and "repeat".
#' 
#' Most of the time, all_args_here() does not check inside comments, but some unexpected writting could dupe all_args_here().
#' 
#' The returned line numbers are indicative, depending on which source is checked. For instance, saferDev::report (compiled) has not the same line numbers as its source file (https://github.com/safer-r/saferDev/blob/main/R/report.R). Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the "<- function" line, i.e., without counting the #' header lines potentially present in source files.
#' 
#' Warnings: 
#' 1) The function could not properly work if any comma is present in default argument values. Please, report here https://github.com/safer-r/saferDev/issues if it is the case.
#' 
#' 2) Results are only suggestions, as it is difficult to anticipate all the exceptions with argument writting.
#' 
#' 3) The following functions are not treated:
#'     as.list: because x arg and ..., with x without default value but not mandatory (?!), and because different args depending on input.
#' 
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' all_args_here(mean)
#' all_args_here(all_args_here)
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R") ; all_args_here(test2, export = TRUE)
#' source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; all_args_here(a, export = TRUE)
#' @export
all_args_here <- function(
    x, 
    safer_check = TRUE,
    export = FALSE,
    path_out = "."
){
    # DEBUGGING
    # x = .expand_R_libs_env_var ; safer_check = TRUE
    # library(saferGraph) ; x = close2 ; safer_check = TRUE 
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; safer_check = TRUE
    # library(saferDev) ; x = get_message ; safer_check = TRUE
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\all_args_here.R") ; x = all_args_here ; safer_check = TRUE
    # arg.user.setting = base::list(x = as.name(x = "test2"), safer_check = TRUE, export = TRUE)
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; x = a ; safer_check = TRUE ; export = TRUE # use the folling line before out <- 
    # arg.user.setting = base::list(x = as.name(x = "a"), safer_check = TRUE, export = TRUE)
    # package name
    package.name <- "saferDev"
    # end package name
    # function name
    function.name <- base::paste0(base::as.list(x = base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function.name[1] == "::()" | function.name[1] == ":::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(x = base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name




    #### second round of checking and data preparation
    ######## reserved words (to avoid bugs)
    reserved_word <- "NOT_CONSIDERED"
    ######## end reserved words (to avoid bugs)
    ######## new environment
    ######## end new environment
    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment
    ######## warning initiation
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    ######## end warning initiation
    ######## other checkings
    ######## end other checkings
    #### end second round of checking and data preparation



    # main code
    # inactivated functions
    inactivated <- c(
        "as.list" # because x arg and ..., with x without default value but not mandatory (?!), and because different args depending on input 
    )
    # end inactivated functions
    out <- .functions_detect(
        x = x, 
        arg.user.setting = arg.user.setting, 
        function.name = function.name, 
        package.name = package.name
    )
    code_line_nb <- out$code_line_nb # vector of line numbers in code where functions are
    fun_names <-  out$fun_names # list of function names for each line of code
    fun_names_pos <-  out$fun_names_pos # list of pos (1st character) of function names for each line of code

    code <- out$code # vector of strings of the tested function code
    fun_1_line <- base::paste(out$code, collapse = " ") # assemble the code of the tested  function (without comments) in a single line
    if(grepl(x = fun_1_line, pattern = reserved_word)){
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") THE RESERVED WORD \"", base::paste(reserved_word, collapse = " "), "\" HAS BEEN DETECTED IN THE CODE OF THE INPUT FUNCTION\nWHICH COULD HAMPER THE ACCURACY OF THE OUTPUT TABLE")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }
    # cumulative nchar of each non empty lines of code 
    cum_nchar_code_line <- base::cumsum(base::nchar(out$code) + 1) # +1 because of the 1 space added between lines of codes fused # will serve to know the position of first character of fun_names in fun_1_line
    cum_nchar_code_line <- base::c(0, cum_nchar_code_line[-base::length(cum_nchar_code_line)]) # because pos in fun_names_pos will be added to this
    cum_nchar_code_line <- cum_nchar_code_line[code_line_nb]
    # end cumulative nchar of each non empty lines of code 
    # replacement of all the ) between quotes
    tempo <- .in_quotes_replacement(string = fun_1_line, pattern = "\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, function.name = function.name, package.name = package.name)
    fun_1_line_replace <- tempo$string # code of the tested function that will serve to better detect functions in it
    pos_rep <- tempo$pos # replaced positions in fun_1_line
    # end replacement of all the ) between quotes
    # replacement of all the ( between quotes
    tempo <- .in_quotes_replacement(string = fun_1_line_replace, pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, function.name = function.name, package.name = package.name)
    fun_1_line_replace <- tempo$string
    pos_rep <- base::sort(base::c(pos_rep, tempo$pos))
    # end replacement of all the ( between quotes
    # recovery of the functions, in the tested function, with written arguments inside ()
    arg_string_for_col3 <- fun_names # like fun_names but added with all what is between ()
    arg_string <- fun_names # like arg_string_for_col3 but with only the arguments
    mid_bracket_pos_in_fun_1_line <- base::lapply(X = fun_names, FUN = function(x){base::lapply(X = x, FUN = function(y){NULL})}) # list of lists, will be used to get inside ( and ) positions, from fun_1_line

# caca <- NULL ###########################
    for(i1 in 1:base::length(fun_names)){
        tempo_pos_in_code <- base::as.integer(base::sub(pattern = "^c", replacement = "", x = base::names(fun_names)[i1], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))
        tempo_which <- base::which(code_line_nb %in% tempo_pos_in_code)
        for(i2 in 1:base::length(fun_names[[i1]])){
            # if(i1 == 18 & i2 == 4){stop()} ############ caca
            # line of code 
            fun_pos_start <- fun_names_pos[[i1]][i2] + cum_nchar_code_line[tempo_which]
            fun_pos_stop <- fun_pos_start + base::nchar(fun_names[[i1]][i2]) - 1
            tempo_fun <- substr(fun_1_line_replace, fun_pos_start, fun_pos_stop)
            if(tempo_fun != fun_names[[i1]][i2]){
                tempo.cat <- base::paste0("INTERNAL ERROR 3 IN ", function.name, " OF THE ", package.name, " PACKAGE\ntempo_fun MUST BE IDENTICAL TO fun_names[[i1]][i2]\n\ntempo_fun: ", tempo_fun, "\n\nfun_names[[i1]][i2]: ", fun_names[[i1]][i2], "\n\ni1: ", i1, "\n\ni2: ", i2)
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            tempo_str_after <- base::substr(x = fun_1_line_replace, start = fun_pos_stop + 1, stop = base::nchar(fun_1_line_replace))
            if(base::grepl(x = tempo_str_after, pattern = "^[\\s\\r\\n]*\\(", perl = TRUE)){ # detection that it is a function of interest because ( after function name not removed
                tempo_pos <- .fun_args_pos(text = fun_1_line_replace, pattern = paste0(fun_names[[i1]][i2], "[\\s\\r\\n]*\\("), function.name = function.name, package.name = package.name) # positions of 1st letter of the function name and opening and closing brackets # Warning: fun_1_line_replace used because the input string must be cleaned form brackets between quotes
                tempo_str_before <- base::substr(x = fun_1_line_replace, start = 1, stop = fun_pos_start - 1)
                tempo_log <- base::grepl(x = tempo_str_before, pattern = "\\$ *$")
                if(tempo_log){ # remove functions preceeded by $, like a$fun()
                    arg_string_for_col3[[i1]][i2] <- ""
                    fun_names[[i1]][i2] <- ""
                    arg_string[[i1]][i2] <- ""
                    base::substr(x = fun_1_line_replace, start = 1, stop = fun_pos_start - 1) <- base::paste(base::rep(" ", fun_pos_start - 1), collapse = "")
                }else{
                    arg_string_for_col3[[i1]][i2] <- base::substr(x = fun_1_line, start = tempo_pos$begin_fun, stop = tempo_pos$end) # add the "function(args)" string into arg_string_for_col3. I use fun_1_line because I want unaltered values of args here (fun_1_line_replace have quoted () replaced by spaces)
                    arg_string[[i1]][i2] <- base::substr(x = fun_1_line, start = tempo_pos$begin + 1, stop = tempo_pos$end - 1) # idem arg_string_for_col3 but inside () of the function (just the arguments written)
                    if( ! base::is.null(tempo_pos$middle_bracket_pos)){ # I have to use if(){}, otherwise mid_bracket_pos_in_fun_1_line[[i1]][[i2]] disappears
                        mid_bracket_pos_in_fun_1_line[[i1]][[i2]] <- base::unlist(tempo_pos$middle_bracket_pos) # positions of the () inside a function
                    }
                    base::substr(x = fun_1_line_replace, start = 1, stop = tempo_pos$begin - 1) <- base::paste(base::rep(" ", tempo_pos$begin - 1), collapse = "") # trick that replaces function name by the same number of spaces. This, to avoid to take always the first paste0 for instance in the fun_1_line_replace string when several are present in fun_names
                }
                # caca <- c(caca, fun_1_line_replace) ##################
            }else{
                arg_string_for_col3[[i1]][i2] <- reserved_word
                arg_string[[i1]][i2] <- ""
            }
        }
    }
    # utils::write.table(caca, file = base::paste0(path_out, "/resi.tsv"), row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "") ########################
    # end recovery of the functions, in the tested function, with written arguments inside ()
    # recovery of the positions of inside () in col3
    mid_bracket_pos_in_col3 <- base::lapply(X = fun_names, FUN = function(x){base::lapply(X = x, FUN = function(y){NULL})}) # list of lists, will be used to get inside ( and ) positions, from col3
    for(i1 in 1:base::length(fun_names)){
        for(i2 in 1:base::length(fun_names[[i1]])){
            pattern2 <- base::paste0(fun_names[[i1]][i2], "[\\s\\r\\n]*\\(") # function detection in 
            # pattern2 <- paste0("[a-zA-Z.][a-zA-Z0-9._]* *\\$ *", fun_names[[i1]][i2], "[\\s\\r\\n]*\\(") # function like a$fun()
            if(base::grepl(x = arg_string_for_col3[[i1]][i2], pattern = pattern2)){ # because of "NOT_CONSIDERED" in some cases
                # detection of inside () between quotes
                tempo1 <- .in_quotes_replacement(string = arg_string_for_col3[[i1]][i2], pattern = "\\(", no_regex_pattern = "(", replacement = " ", perl = TRUE, function.name = function.name, package.name = package.name)
                tempo2 <- .in_quotes_replacement(string =tempo1$string, pattern = "\\)", no_regex_pattern = ")", replacement = " ", perl = TRUE, function.name = function.name, package.name = package.name)
                tempo_pos <- .fun_args_pos(text = tempo2$string, pattern = pattern2,     function.name = function.name, package.name = package.name) # positions of 1st letter of the function name and opening and closing brackets # Warning: fun_1_line_replace used because the input string must be cleaned form brackets between quotes
                if( ! base::is.null(tempo_pos$middle_bracket_pos)){ # I have to use if(){}, otherwise mid_bracket_pos_in_fun_1_line[[i1]][[i2]] disappears
                    mid_bracket_pos_in_col3[[i1]][[i2]] <- base::unlist(tempo_pos$middle_bracket_pos) # positions of the () inside a function
                }
            }
        }
    }
    # end recovery of the positions of inside () in col3
    # preparation of columns
    code_for_col <- code[code_line_nb]
    code_for_col <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::length(x))}, x = fun_names, y = code_for_col)))
    col1 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::length(x))}, x = fun_names, y = code_line_nb))) # code line number
    col2 <- base::as.vector(base::unlist(fun_names)) # all the function names inside the tested functions (functions between quotes are already removed thanks to fun_1_line_replace)
    col3 <- base::as.vector(base::unlist(arg_string_for_col3)) # as col2 but with its arguments between ()
    col4 <- base::as.vector(base::unlist(fun_names_pos)) # as col2 but position in the code string of 1st character of function name. From col4, we can have pos of opening ( with col4 + nchar(col2) and pos of closing ) with col4 + nchar(col3)
    middle_bracket <- base::do.call(base::c,  mid_bracket_pos_in_col3) #  concatenate the sublists into a single list -> flatten the outer list while keeping the result as a list
    middle_bracket_open_in_col3 <- base::lapply(X = middle_bracket, FUN = function(x){if( ! is.null(x)){x[seq(1, length(x), by = 2)]}else{NULL}})
    middle_bracket_close_in_col3 <- base::lapply(X = middle_bracket, FUN = function(x){if( ! is.null(x)){x[seq(2, length(x), by = 2)]}else{NULL}})
    if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col1) == base::length(col4) & base::length(col1) == base::length(code_for_col) & base::length(col1) == base::length(middle_bracket))){
        tempo.cat <- base::paste0("INTERNAL ERROR 3 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), col3 (", base::length(col3), "), col4 (", base::length(col4), "), code_for_col (", base::length(code_for_col), "), AND middle_bracket (", base::length(middle_bracket), "), SHOULD BE EQUAL\n")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    tempo.log <- base::as.vector(base::unlist(base::mapply(
        FUN = function(x, y){
            if(y != ""){
                if(grepl(x = y, pattern = base::paste0("^", x, "[\\s\\r\\n]*\\(.*\\)$"), perl = TRUE) | grepl(x = y, pattern = base::paste0("^", reserved_word, "$"), perl = FALSE)){
                    base::return(FALSE)
                }else{
                    base::return(TRUE) # TRUE = problem: does not start by what is expected, i.e., base::paste0("^", x, "[\\s\\r\\n]*\\(.*\\)$"
                }
            }else{
                base::return(FALSE)
            }
        }, 
        x = col2, 
        y = col3
    )))
    if(base::any(tempo.log, na.rm = TRUE)){
        tempo.cat <- base::paste0("INTERNAL ERROR 5 IN ", function.name, " OF THE ", package.name, " PACKAGE\ncol3 MUST BE MADE OF STRINGS STARTING BY\n\"<FUNCTION_NAME>[\\s\\r\\n]*\\(\"\nAND FINISHING BY\")\"\nHERE IT IS:\n\n", base::paste(col3, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    # data.frame(POS_IN_CODE = col1, FUN = col2, FUN_OBS_ARGS = col3, base::as.vector(base::unlist(arg_string_for_col5)))
    # removal of detected function preceeded by $, which are "" in col2
    tempo_log <- col2 == "" 
    if(base::any(tempo_log, na.rm = TRUE)){
        code_for_col <- code_for_col[ ! tempo_log]
        col1 <- col1[ ! tempo_log]
        col2 <- col2[ ! tempo_log]
        col3 <- col3[ ! tempo_log]
        col4 <- col4[ ! tempo_log]
        middle_bracket <- middle_bracket[ ! tempo_log]
        middle_bracket_open_in_col3 <- middle_bracket_open_in_col3[ ! tempo_log]
        middle_bracket_close_in_col3 <- middle_bracket_close_in_col3[ ! tempo_log]
    }
    # end removal of detected function preceeded by $, which are "" in col2
    # end preparation of columns
    # two new columns for arg proposal
    if( (base::length(col1) == 0)){
        base::cat("\n\nEVERYTHING SEEMS CLEAN\n\n")
    }else{
        col5 <- NULL # all arguments of the function with default value
        col6 <- NULL # missing arg names
        col7 <- NULL # potential missing args with values
        col8 <- NULL # reconstructed function with all arg
        for(i2 in 1:base::length(col1)){
            if(col3[i2] != reserved_word){
                # check if the function exists
                if(col4[i2] <= 3){
                    tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE ", col1[i2], ":\n\n", base::paste(paste0(substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1), col3[i2]), collapse = "\n"), "\n\nPLEASE, RUN saferDev::colons_check(", arg.user.setting$x, ") FIRST,\nADD THE MISSING <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> FOR FUNCTION STARTING BY A DOT)\nAND RERUN saferDev::all_args_here(", arg.user.setting$x, ")")
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
                tempo_string <- base::substr(x = code_for_col[i2], start = col4[i2] - 2, stop = col4[i2] - 1)
                if(tempo_string != "::"){
                    tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nCANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE ", col1[i2], ":\n\n", base::paste(paste0(substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1), col3[i2]), collapse = "\n"), "\n\nPLEASE, RUN saferDev::colons_check(", arg.user.setting$x, ") FIRST,\nADD THE MISSING <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> FOR FUNCTION STARTING BY A DOT)\nAND RERUN saferDev::all_args_here(", arg.user.setting$x, ")")
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
                tempo_string <- base::substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1)
                tempo_string2 <- saferDev:::.extract_all_fun_names(text = tempo_string, pattern = "[a-zA-Z][a-zA-Z0-9.]*:{2,3}$")$string # before 
                is_function_here(fun = paste0(tempo_string2, col2[i2]), lib.path = NULL, safer_check = FALSE) # check that exists
                # end check if the function exists
                # recovering default args of the function
                if(base::is.primitive(base::get(col2[i2]))){
                    if(base::all(base::typeof(base::get(col2[i2])) == "special", na.rm = TRUE)){
                        arg_full <- NULL
                    }else{
                        arg_full <- base::as.list(base::formals(base::args(name = col2[i2]))) # convert pairlist into list
                    }
                }else{
                    arg_full <- base::as.list(base::formals(fun = col2[i2])) # all the argument of the function in col2[i2] with default values # convert pairlist into list
                }
                # end recovering default args of the function
                if(base::is.null(arg_full)){
                    col5 <- base::c(col5, "NO_ARGS")
                    col6 <- base::c(col6, "")
                    col7 <- base::c(col7, "")
                    col8 <- base::c(col8, "")
                }else{
                    # all arguments of the function with default value in col5
                    tempo <- base::sapply(X = arg_full, FUN = function(x){base::paste0(ifelse(base::all(base::typeof(x) == "symbol", na.rm =TRUE), "", " = "), base::deparse(x))})
                    tempo <- base::paste(base::paste0(base::names(tempo), tempo), collapse = ", ")
                    col5 <- base::c(col5, tempo)
                    # end all arguments of the function with default value in col5
                    # arguments: replacement of all the commas between quotes
                    tempo_col3 <- col3[i2]
                    tempo <- .in_quotes_replacement(string = tempo_col3, pattern = ",", no_regex_pattern = ",", replacement = " ", perl = TRUE, function.name = function.name, package.name = package.name)
                    tempo_col3 <- tempo$string
                    pos_rep2 <- tempo$pos # replaced positions in obs_args
                    # end arguments: replacement of all the commas between quotes
                    # arguments: replacement of all the commas inside () of subfunctions
                    if(length(middle_bracket_open_in_col3[[i2]]) > 0){
                        if(length(middle_bracket_open_in_col3[[i2]]) != length(middle_bracket_close_in_col3[[i2]])){
                            tempo.cat <- base::paste0("INTERNAL ERROR 6 IN ", function.name, " OF THE ", package.name, " PACKAGE\nmiddle_bracket_open_in_col3 AND middle_bracket_close_in_col3 MUST HAVE THE SAME LENGTH IN LOOP ", i2, "\n\nmiddle_bracket_open_in_col3 (", length(middle_bracket_open_in_col3), "):\n", base::paste(middle_bracket_open_in_col3, collapse = " "), "\n\nmiddle_bracket_close_in_col3 (", length(middle_bracket_close_in_col3), "):\n", base::paste(middle_bracket_close_in_col3, collapse = " "))
                            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                        }
                        for(i6 in 1:length(middle_bracket_open_in_col3[[i2]])){
                            tempo <- .in_parenthesis_replacement(string = tempo_col3, pattern = ",", no_regex_pattern = ",", replacement = " ", perl = TRUE, open_pos = middle_bracket_open_in_col3[[i2]][i6], close_pos = middle_bracket_close_in_col3[[i2]][i6], function.name = function.name, package.name = package.name)
                            tempo_col3 <- tempo$string
                            pos_rep2 <- c(pos_rep2, tempo$pos) # replaced positions in obs_args
                        }
                    }
                    # end arguments: replacement of all the commas inside () of subfunctions
                    # recovering obs arguments
                    obs_args <- base::sub(pattern =  base::paste0("^", col2[i2], "[\\s\\r\\n]*\\("), replacement = "", x = tempo_col3, perl = TRUE) # removal of function name and (
                    obs_args <- base::sub(pattern =  "\\)$", replacement = "", x = obs_args, perl = FALSE) # removal of trailing )
                    # end recovering obs arguments
                    # splitting the arguments using commas
                    tempo_split <- base::strsplit(x = obs_args, split = " *, *", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]] # separation of args
                    # tempo_split <- gsub(pattern = "^[\\s;]+", replacement = "", x = tempo_split) # removing leading ; and space, ; because of line: fun_1_line <- base::paste(out$code, collapse = ";")
                    # tempo_split <- gsub(pattern = "[\\s;]+$", replacement = "", x = tempo_split) # removing trailing ; and space, ; because of line: fun_1_line <- base::paste(out$code, collapse = ";")
                    # end splitting the arguments using commas
                    # rewriting the commas inside args
                    if( ! base::is.null(pos_rep2)){
                        # resseting the pos of the removed commas to fit obs_args
                        pos_rep2 <- pos_rep2 - base::nchar(col2[i2]) - 1 # -1 for the opening (
                        if(base::any(pos_rep2 <= 0, na.rm = TRUE)){
                            tempo.cat <- base::paste0("INTERNAL ERROR 6 IN ", function.name, " OF THE ", package.name, " PACKAGE\nPOSITIONS OF REMOVED COMMAS CANOT  OR LESS\n\npos_rep2 (", length(pos_rep2), "):\n", base::paste(pos_rep2, collapse = " "), "\n\nARGUMENT STRING obs_args:\n", base::paste(obs_args, collapse = " "))
                            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                        }
                        # end resseting the pos of the removed commas to fit obs_args
                        for(i6 in 1:base::length(tempo_split)){
                            tempo_log <- pos_rep2 >= 1 & pos_rep2 <= nchar(tempo_split[i6])
                            if(base::any(tempo_log, na.rm = TRUE)){
                                substring(text = tempo_split[i6], first = pos_rep2[tempo_log], last = pos_rep2[tempo_log]) <- ","
                            }
                            pos_rep2 - nchar(tempo_split[i6]) - 1 # -1 because of the comma that separates each tempo_split
                        }
                    }
                    # end rewriting the commas inside args
                    # working on each observed arg
                    arg_full_names <-  base::names(arg_full)
                    three_dots_log <- arg_full_names == "..."
                    # checking 
                    if(base::length(tempo_split) > base::length(arg_full_names) & ! base::any(three_dots_log, na.rm = TRUE)){
                        tempo.cat <- base::paste0("INTERNAL ERROR 6 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTH OF tempo_split MUST LOWER OR EQUAL TO LENGTH OF arg_full_names IF ... IS NOT AN ARGUMENT OF THE FUNCTION\n\nFUNCTION: ", col2[i2], "\n\ntempo_split (", length(tempo_split), "):\n", base::paste(tempo_split, collapse = " "), "\n\narg_full_names (", length(arg_full_names), "):\n", base::paste(arg_full_names, collapse = " "))
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                    }
                    # end checking
                    tempo_out <- .all_args_here_fill(
                        arg_full = arg_full, 
                        arg_full_names = arg_full_names, 
                        tempo_split = tempo_split, 
                        three_dots_log = three_dots_log, 
                        col2_i2 = col2[i2],
                        function.name = function.name, 
                        package.name = package.name 
                    )
                    col6 <- base::c(col6, tempo_out$col6)
                    col7 <- base::c(col7, tempo_out$col7)
                    if(base::any(inactivated %in% col2[i2], na.rm = TRUE)){
                        col8 <- base::c(col8, "INACTIVATED")
                    }else{
                        col8 <- base::c(col8, tempo_out$col8)
                    }
                    # end working on each observed arg
                }
            }else{
                col5 <- base::c(col5, "")
                col6 <- base::c(col6, "")
                col7 <- base::c(col7, "")
                col8 <- base::c(col8, "")
            }
            # if(i2 != length(col8)){stop(paste0("caca ", i2))}
        }
        output <- base::data.frame(LINE_NB = col1, FUN_NAME = col2, FUN_ARGS = col3, FUN_POS = col4, DEF_ARGS = col5, MISSING_ARG_NAMES = col6, MISSING_ARGS = col7, NEW = col8)
        if(export == TRUE){
            if(grepl(x = path_out, pattern = "/$")){
                path_out <- base::sub(pattern = "/$", replacement = "", x = path_out, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
            }else if(grepl(x = path_out, pattern = "\\\\$")){
                path_out <- base::sub(pattern = "\\\\$", replacement = "", x = path_out, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
            }
            utils::write.table(output, file = base::paste0(path_out, "/res.tsv"), row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "")
        }else{
            base::return(output)
        }
    }

    # end two new columns for arg proposal
    # end main code
    # output
    # warning output
    if( ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    # end output
}



