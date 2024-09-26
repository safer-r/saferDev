#' @title all_args_here
#' @description
#' Verify that all the functions used inside a function are written with all their arguments. For instance: base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE) and not paste0(letters[1:2]).
#' @param x a function name, written without quotes and brackets.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns 
#' A table-like message indicating the missing arguments or a message saying that everything seems fine.
#' Table-like: column 1, the line number in the function code (starting at the "<- function" line, i.e., without counting the #' header lines); column 2,  the function name; column 3, the code preceeding the function name; column 4, the missing arguments with default values
#' @details
#' - More precisely, all_args_here() verifies that all the strings before an opening bracket "(" are written with all their arguments. Thus, it cannot check function names written without brackets, like in the FUN argument of some functions, e.g., sapply(1:3, FUN = as.character).
#' 
#' - The perl regex used to detect a function name is: "[a-zA-Z.]{1}[a-zA-Z0-9._]*\\s*\\(".
#' 
#' - Function names preceeded by $ and any space are not considered (pattern "\\$ *[a-zA-Z.]{1}[a-zA-Z0-9._]* *\\(")
#'  
#' - The following R functions using bracket are not considered: "function", "if", "for", "while" and "repeat".
#' 
#' - Most of the time, all_args_here() does not check inside comments, but some unexpected writting could dupe all_args_here().
#' 
#' - The returned line numbers is indicative, depending on which source is checked. For instance, saferDev::report (compiled) has not the same line numbers as its source file (https://github.com/safer-r/saferDev/blob/main/R/report.R). Notably, compiled functions do not have comments anymore, compared to the same source function sourced into the working environment. In addition, the counting starts at the "<- function" line, i.e., without counting the #' header lines potentially present in source files.
#' 
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' all_args_here(mean)
#' all_args_here(all_args_here)
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; all_args_here(test)
#' @export
all_args_here <- function(
    x, 
    safer_check = TRUE
){
    # DEBUGGING
    # x = .expand_R_libs_env_var ; safer_check = TRUE
    # library(saferGraph) ; x = close2 ; safer_check = TRUE 
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; safer_check = TRUE
    # library(saferDev) ; x = get_message ; safer_check = TRUE
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\all_args_here.R") ; x = all_args_here ; safer_check = TRUE
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R") ; x = test ; safer_check = TRUE # use the folling line before out <- 
    # arg.user.setting = base::list(x = as.name(x = "test"), safer_check = TRUE)
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
    out <- .functions_detect(
        x = x, 
        safer_check = safer_check,
        arg.user.setting = arg.user.setting, 
        function.name = function.name, 
        package.name = package.name
    )
    code_line_nb_wo_op <- out$code_line_nb_wo_op # vector of line numbers in ini
    fun_name_wo_op <-  out$fun_name_wo_op # list of function names for each line of ini
    fun_name_wo_op_pos <-  out$fun_name_pos_wo_op # list of pos (1st character) of function names for each line of ini

    ini <- out$ini # vector of strings of the tested function code
    fun_1_line <- base::paste(out$ini, collapse = ";") # assemble the code of the tested  function (without comments) in a single line
    if(grepl(x = fun_1_line, pattern = reserved_word)){
        warn.count <- warn.count + 1
        tempo.warn <- base::paste0("(", warn.count,") THE RESERVED WORD \"", base::paste(reserved_word, collapse = " "), "\" HAS BEEN DETECTED IN THE CODE OF THE INPUT FUNCTION\nWHICH COULD HAMPER THE ACCURACY OF THE OUTPUT TABLE")
        warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
    }
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
    arg_string_for_col3 <- fun_name_wo_op # like fun_name_wo_op but added with all what is between ()
    arg_string_for_col5 <- fun_name_wo_op # will be used to get the arguments
    middle_bracket_pos_col3 <- base::lapply(X = fun_name_wo_op, FUN = function(x){base::lapply(X = x, FUN = function(y){base::list()})}) # list of lists, will be used to get inside ( and ) positions

    for(i1 in 1:base::length(fun_name_wo_op)){
        for(i2 in 1:base::length(fun_name_wo_op[[i1]])){
            pattern1 <- base::paste0(fun_name_wo_op[[i1]][i2], "[\\s\\r\\n]*\\(") # function detection in 
            # pattern2 <- paste0("[a-zA-Z.][a-zA-Z0-9._]* *\\$ *", fun_name_wo_op[[i1]][i2], "[\\s\\r\\n]*\\(") # function like a$fun()
            if(base::grepl(x = fun_1_line_replace, pattern = pattern1)){ 
                tempo_pos <- .fun_args_pos(text = fun_1_line_replace, pattern = pattern1) # positions of 1st letter of the function name and opening and closing brackets # Warning: fun_1_line_replace used because the input string must be cleaned form brackets between quotes
                tempo_str_before <- substr(x = fun_1_line, start = 1, stop = tempo_pos$begin_fun - 1)
                tempo_log <- grepl(x = tempo_str_before, pattern = "\\$ *$")
                if(tempo_log){ # remove functions preceeded by $, like a$fun()
                    arg_string_for_col3[[i1]][i2] <- ""
                    fun_name_wo_op[[i1]][i2] <- ""
                    arg_string_for_col5[[i1]][i2] <- ""
                    substr(x = fun_1_line_replace, start = 1, stop = tempo_pos$begin - 1) <- paste(rep(" ", tempo_pos$begin - 1), collapse = "")
                }else{
                    arg_string_for_col3[[i1]][i2] <- substr(x = fun_1_line, start = tempo_pos$begin_fun, stop = tempo_pos$end) # add the "function(args)" string into arg_string_for_col3. I use fun_1_line because I want unaltered values of args here (fun_1_line_replace have quoted () replaced by spaces)
                    arg_string_for_col5[[i1]][i2] <- substr(x = fun_1_line, start = tempo_pos$begin + 1, stop = tempo_pos$end - 1) # idem arg_string_for_col3 but inside () of the function (just the arguments written)
                    middle_bracket_pos_col3[[i1]][[i2]] <- tempo_pos$middle_bracket_pos # positions of the () inside a function in 
                    substr(x = fun_1_line_replace, start = 1, stop = tempo_pos$begin - 1) <- paste(rep(" ", tempo_pos$begin - 1), collapse = "") # trick that replaces characters between start = tempo_pos$begin_fun and stop = tempo_pos$end by the same number of spaces. This, to avoid to take always the first paste0 for instance in the fun_1_line_replace string when several are present in fun_name_wo_op
                }
            }else{
                arg_string_for_col3[[i1]][i2] <- reserved_word
                arg_string_for_col5[[i1]][i2] <- ""
            }
            # substr(x = fun_1_line, start = fun_pos, stop = fun_close_paren_pos) <- paste(rep(" ", nchar( arg_string_for_col3[[i1]][i2])), collapse = "") # remove the first fonction in the line, in case of identical function names in a code line. Like, that, the next round for the next same function can be easily tested for "between quotes" 
        }
    }
    # end recovery of the functions, in the tested function, with written arguments inside ()
    # preparation of columns
    col1 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::length(x))}, x = fun_name_wo_op, y = code_line_nb_wo_op))) # code line number
    col2 <- base::as.vector(base::unlist(fun_name_wo_op)) # all the function names inside the tested functions (functions between quotes are already removed thanks to fun_1_line_replace)
    col3 <- base::as.vector(base::unlist(arg_string_for_col3)) # as col2 but with its arguments between ()
    if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3))){
        tempo.cat <- base::paste0("INTERNAL ERROR 3 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), col3 (", base::length(col3), "), AND col5 (", base::length(col5), "), SHOULD BE EQUAL\n")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
    }
    if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col2) == base::length(col3))){
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), AND col3 (", base::length(col3), "), SHOULD BE EQUAL\n")
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
        col1 <- col1[ ! tempo_log]
        col2 <- col2[ ! tempo_log]
        col3 <- col3[ ! tempo_log]
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
                arg_full <- base::formals(fun = col2[i2]) # all the argument of the function in col2[i2] with default values
                arg_full <- arg_full[names(arg_full) != "..."] # removal of the "..." argument
                if(base::is.null(arg_full)){
                    col5 <- base::c(col5, "")
                    col6 <- base::c(col6, "")
                    col7 <- base::c(col7, "")
                }else{
                    # all arguments of the function with default value in col5
                    tempo <- base::sapply(X = arg_full, FUN = function(x){ base::paste0(ifelse(base::all(x == "", na.rm =TRUE), "", " = "), x)})
                    tempo <- base::paste( base::paste0( base::names(tempo), tempo), collapse = ", ")
                    col5 <- base::c(col5, tempo)
                    # end all arguments of the function with default value in col5
                    # recovering obs arg
                    tempo_str <- base::sub(pattern =  base::paste0("^", col2[i2], "[\\s\\r\\n]*\\("), replacement = "", x = col3[i2], perl = TRUE) # removal of function name and (
                    tempo_str <- base::sub(pattern =  "\\)$", replacement = "", x = tempo_str, perl = FALSE) # removal of trailing )
                    # end recovering obs arg
                    # replacement of all the commas between quotes
                    tempo <- .in_quotes_replacement(string = tempo_str, pattern = ",", no_regex_pattern = ",", replacement = " ", perl = TRUE, function.name = function.name, package.name = package.name)
                    tempo_str <- tempo$string
                    pos_rep2 <- tempo$pos # replaced positions in fun_1_line
                    # end replacement of all the commas between quotes
                    # working on each observed arg
                    tempo_split <- base::strsplit(x = tempo_str, split = ",")[[1]] # separation of args
                    arg_full_names <-  base::names(arg_full)
                    good_args <- NULL
                    missing_args <- NULL
                    missing_args_names <- NULL
                    count <- 1 # count of the tempo_split args without arg name before
                    for(i5 in 1:base::length(arg_full_names)){
                        pattern3 <- base::paste0("^[\\s\\r\\n]*", arg_full_names[i5], "[\\s\\r\\n]*=") # looking for the arg name
                        tempo.log <- grepl(x = tempo_split, pattern = pattern3, perl = TRUE)
                        missing_args_log <- FALSE
                        if(base::sum(tempo.log, na.rm = TRUE) == 1){
                            good_args <- base::c(good_args, tempo_split[tempo.log]) # good args reordered in case
                            # remove the ( and ) positions inside brackets in .fun_args_pos() and give couple of brackets # done
                            # done bit factorise while loop in .fun_args_pos() # done
                            # then, detect any comma inside couple of brackets
                            # replace commas inside ()
                            # split using ","
                            # the begining of each split string should start by pattern3 <- base::paste0("^[\\s\\r\\n]*", arg_full_names[i4], "[\\s\\r\\n]*=")
                            # go back to unmodified string to extract the arguments to export
                        }else if(base::sum(tempo.log, na.rm = TRUE) == 0){
                            missing_args_log <- TRUE
                            missing_args_names <- base::c(missing_args_names, arg_full_names[i5])
                            if(base::length(arg_full[[i5]]) == 0){
                                tempo_missing_args <- base::paste0(arg_full_names[i5], " = ", arg_full[[i5]])
                            }else if(arg_full[[i5]] == ""){
                                if(count <= base::length(tempo_split)){
                                    tempo_missing_args <- base::paste0(arg_full_names[i5], " = ", tempo_split[count])
                                    count <- count + 1
                                }else{
                                    tempo.cat <- base::paste0("INTERNAL ERROR 6 IN ", function.name, " OF THE ", package.name, " PACKAGE\nPROBLEM IN count THAT CANNOT BE MORE THAN THE NUMBER OF STRINGS IN ntempo_split\n\ncount:\n", base::paste(count, collapse = "\n"), "\n\ntempo_split:\n", base::paste(tempo_split[tempo.log], collapse = "\n"))
                                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                                }
                            }else{
                                tempo_missing_args <- base::paste0(arg_full_names[i5], " = ", arg_full[[i5]])
                            }
                        }else{
                            tempo.cat <- base::paste0("INTERNAL ERROR 7 IN ", function.name, " OF THE ", package.name, " PACKAGE\npattern3 DETECTED SEVERAL TIMES IN ARGUMENTS:\n\npattern3:\n", base::paste(pattern3, collapse = "\n"), "\n\ntempo_split:\n", base::paste(tempo_split[tempo.log], collapse = "\n"))
                            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                        }
                        if(missing_args_log == TRUE){
                            missing_args <- base::c(missing_args, tempo_missing_args)
                            good_args <- base::c(good_args, tempo_missing_args)
                        }
                    }
                    # end working on each observed arg
                }
                # col5 done above
                col6 <- base::c(col6, base::paste(missing_args_names, collapse = ", "))
                col7 <- base::c(col6, base::paste(good_args, collapse = ", "))
            }else{
                col5 <- base::c(col5, "")
                col6 <- base::c(col6, "")
                col7 <- base::c(col7, "")
            }
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



