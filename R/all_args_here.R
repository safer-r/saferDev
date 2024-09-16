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
#' - The perl regex used to detect a function name is: "[a-zA-Z.][a-zA-Z0-9._]*\\s*\\(".
#' 
#' - Function names preceeded by $ and any space are not considered (pattern "\\$ *[a-zA-Z.][a-zA-Z0-9._]* *\\(")
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
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # library(saferDev) ; x = get_message ; safer_check = TRUE # Warning: does not return the same number of code lines than the previsou example
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\all_args_here.R") ; x = all_args_here ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R") ; x = test ; safer_check = TRUE # Warning: x = saferDev::get_message does not return the same number of code lines
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
    # main code
    out <<- saferDev:::.functions_detect(
        x = x, 
        safer_check = safer_check,
        arg.user.setting = arg.user.setting, 
        function.name = function.name, 
        package.name = package.name
    )
    code_line_nb_wo_op <- out$code_line_nb_wo_op # vector of line numbers in ini
    fun_name_wo_op <-  out$fun_name_wo_op # list of function names for each line of ini
    ini <- out$ini # vector of strings of the tested function code
    fun_1_line <- base::paste(out$ini, collapse = ";") # assemble the code of the function tested (without comments) in a single line
    # removal of all the ) between quotes
    fun_1_line_split <- base::strsplit(fun_1_line, split = "\\)")[[1]]
    tempo_output_1_line <- fun_1_line_split[1]
    for(i1 in 2:base::length(fun_1_line_split)){
        # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
        double.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = tempo_output_1_line, pattern = '"') # here FALSE means even number of quotes, thus that ")" is not between quotes, thus has to be kept. TRUE means that ")" is between quotes, thus has to be removed
        simple.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = tempo_output_1_line, pattern = "'") # idem
        odds.quotes.log <- double.quote.test |  simple.quote.test # remove ")" ?
        if(odds.quotes.log == TRUE){
            tempo_output_1_line <- paste0(tempo_output_1_line, fun_1_line_split[i1])
        }else{
            tempo_output_1_line <- paste0(tempo_output_1_line, ")", fun_1_line_split[i1])
        }
    }
    # end removal of all the ) between quotes
    # removal of all the ( between quotes
    fun_1_line_split2 <- base::strsplit(tempo_output_1_line, split = "\\(")[[1]]
    output_1_line <- fun_1_line_split2[1]
    for(i1 in 2:base::length(fun_1_line_split2)){
        # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
        double.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = output_1_line, pattern = '"') # here FALSE means even number of quotes, thus that ")" is not between quotes, thus has to be kept. TRUE means that "(" is between quotes, thus has to be removed
        simple.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = output_1_line, pattern = "'") # idem
        odds.quotes.log <- double.quote.test |  simple.quote.test # remove "(" ?
        if(odds.quotes.log == TRUE){
            output_1_line <- paste0(output_1_line, fun_1_line_split2[i1])
        }else{
            output_1_line <- paste0(output_1_line, "(", fun_1_line_split2[i1])
        }
    }
    # end removal of all the ( between quotes
    # recovery of the functions with written arguments inside ()
    arg_string <- fun_name_wo_op # like fun_name_wo_op but for all what is between ()
    open_paren_pos <- base::as.vector(base::gregexpr(pattern = "\\(", text = output_1_line)[[1]])
    close_paren_pos <- base::as.vector(base::gregexpr(pattern = "\\)",  text = output_1_line)[[1]])
    for(i1 in 1:base::length(fun_name_wo_op)){
        for(i2 in 1:base::length(fun_name_wo_op[[i1]])){
            pattern1 <- paste0(fun_name_wo_op[[i1]][i2], "[\\s\\r\\n]*\\(")
            if(grepl(x = output_1_line, pattern = pattern1)){
                fun_pos <- base::as.vector(base::gregexpr(pattern = pattern1,  text = output_1_line, perl = TRUE)[[1]][1]) # position of the 1st character of fun_name_wo_op[[i1]][i2] in output_1_line
                fun_open_paren_pos <- open_paren_pos[open_paren_pos > fun_pos][1] # ( of the fonction
                if(base::length(fun_open_paren_pos) != 1 | any(is.na(fun_open_paren_pos), na.rm = TRUE) | is.null(fun_open_paren_pos)){
                    tempo.cat <- base::paste0("INTERNAL ERROR 2 IN ", function.name, " OF THE ", package.name, " PACKAGE\nfun_open_paren_pos SHOULD BE A SINGLE VALUE\nfun_open_paren_pos: ", base::paste(fun_open_paren_pos, collapse = " "))
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
                }
                # detection of the closing ) of the function
                tempo_close_pos <- close_paren_pos[close_paren_pos > fun_open_paren_pos][1]
                tempo_log <- open_paren_pos > fun_open_paren_pos & open_paren_pos < tempo_close_pos
                if(any(tempo_log, na.rm = TRUE)){
                    shift_pos <- sum(tempo_log, na.rm = TRUE) # number of () inside the function argument part
                    fun_close_paren_pos <- close_paren_pos[which(close_paren_pos == tempo_close_pos) + shift_pos]
                }else{
                    fun_close_paren_pos <- tempo_close_pos
                }
                # end detection of the closing ) of the function
                arg_string[[i1]][i2] <- substr(x = output_1_line, start = fun_pos, stop = fun_close_paren_pos) # add the "function(args)" string into arg_string
            }else{
                arg_string[[i1]][i2] <- NA
            }
            # substr(x = output_1_line, start = fun_pos, stop = fun_close_paren_pos) <- paste(rep(" ", nchar( arg_string[[i1]][i2])), collapse = "") # remove the first fonction in the line, in case of identical function names in a code line. Like, that, the next round for the next same function can be easily tested for "between quotes" 
        }
    }
    # end recovery of the functions with written arguments inside ()
    # preparation of columns
    col1 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::length(x))}, x = fun_name_wo_op, y = code_line_nb_wo_op)))
    col2 <- base::as.vector(base::unlist(fun_name_wo_op))
    col3 <- base::as.vector(base::unlist(arg_string))
    if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col2) == base::length(col3))){
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), AND col3 (", base::length(col3), "), SHOULD BE EQUAL\n")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    col_res <- saferDev:::.clean_functions(col1 = col1, col2 = col2, col3 = col3, ini = ini)
    col1 <- col_res$col1 # line number
    col2 <- col_res$col2 # function
    col3 <- col_res$col3 # code inside ()
    if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col2) == base::length(col3))){
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), AND col3 (", base::length(col3), "), SHOULD BE EQUAL\n")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    tempo.log <- base::as.vector(base::unlist(base::mapply(
        FUN = function(x, y){
            if( ! base::is.na(y)){
                if(grepl(x = y, pattern = base::paste0("^", x, "[\\s\\r\\n]*\\(.*\\)$", perl = TRUE))){
                    base::return(FALSE)
                }else{
                    base::return(TRUE)
                }
            }
        }, 
        x = col2, 
        y = col3
    )))
    if(base::any(tempo.log, na.rm = TRUE)){
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\ncol3 MUST BE MADE OF STRINGS STARTING BY \"<FUNCTION_NAME>[\\s\\r\\n]*\\(\" AND FINISHING BY \")\":\n", , base::paste(col3, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end preparation of columns
    # two new columns for arg proposal
    if( (base::length(col1) == 0)){
        base::cat("\n\nEVERYTHING SEEMS CLEAN\n\n")
    }else{
        col4 <- NULL # all arguments of the function with default value
        col5 <- NULL # 
        col6 <- NULL # potential missing args with values
        for(i2 in 1:base::length(col1)){
            arg_full <- base::formals(fun = col2[i2])
            if(base::is.null(arg_full)){
                col4 <- base::c(col4, "")
                col5 <- base::c(col5, "")
            }else{
                arg_full_names <- names(arg_full)
                tempo.str <- base::sub(pattern =  base::paste0("^", col2[i2], "[\\s\\r\\n]*\\("), replacement = "", x = col3[i2], perl = TRUE) # removal of function anme and (
                tempo.str <- base::sub(pattern =  "\\($", replacement = "", x = tempo.str, perl = FALSE) # removal of trailing )
                tempo.split <- base::strsplit(x = tempo.str, split = ",")[[1]] # separation of args

                for(i4 in 1:base::length(arg_full_names))
                    pattern2 <- base::paste0("^[\\s\\r\\n]*", arg_full_names[i4], "[\\s\\r\\n]*=")
                    tempo.log <- grepl(x = tempo.split, pattern = pattern2, perl = TRUE)
                    if(base::any(tempo.log, na.rm = TRUE)){

                        caca <- TRUE
                        # col3 = "gregexpr(pattern = base::paste0(pattern, \"\\\\#\"), text = text)" 
                        # remplacer each inside function like paste0(pattern, \"\\\\#\") par ___1___ (checker existe pas), etc. 2, 3, in the string col3
                        # I have no choice because of same imbricated functions (same aerguments) : paste0(paste0(, collapse = )) 
                        # then split using "," or look if pattern2 present in the full string

                    }
            }
        }
    }

    # end two new columns for arg proposal
    # end main code
    # output
    # warning output
    base::options(warning.length = out$ini.warning.length)
    # end warning output
    # end output
}



