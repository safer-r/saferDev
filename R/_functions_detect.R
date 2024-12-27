#' @title .functions_detect
#' @description
#' Detect all the functions names used inside a function.
#' @param x a function name, written without quotes and brackets.
#' @param arg_user_setting Argument user settings list.
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @returns 
#'  A list:
#' $code: vector of strings of the code of the tested function.
#' $all_basic_funs: vector or strings of names of all the basic R functions.
#' $fun_names: list of names of all the functions, not considering base::c("function", "if", "for", "while", "repeat"). Compartment names indicate the code line number of the functions in $code.
#' $fun_names_pos: list of position of the first character of each $fun_names. Compartment names indicate the code line number of the functions in $code.
#' $code_line_nb: vector of integers of the code line numbers of code for each non empty compartment of $fun_names and $fun_names_pos.
#' $internal_fun_names: vector of string of names of internal functions in the code of the tested function.
#' $arg_user_setting: list of arg user settings of the tested function.
#' @details
#' - Does not check if the functions inside the code exist.
#' - Use the regex pattern "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(" to detect a function in the code.
#' - $all_basic_funs are all the functions in base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base")
#' 
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test.R") ; .functions_detect(x = test, arg_user_setting = base::list(x =  as.name(x = "test")), function_name = "F1", package_name = "P1")
#' }
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.functions_detect <- function(
    x, 
    arg_user_setting, 
    function_name, 
    package_name,
    internal_error_report_link
){
    # DEBUGGING
    # x = x ; arg_user_setting = arg_user_setting ; function_name = function_name ; package_name = package_name
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\dev\\other\\test2.R") ; x = test2 ; arg_user_setting = base::list(x = as.name(x = "test2"), export = TRUE) ; function_name = "F1" ; package_name = "P1"
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; x = BACKBONE ; arg_user_setting = base::list(x = as.name(x = "BACKBONE"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE) ; function_name = "F1" ; package_name = "P1"
    # FUN1 <- function(x, y){middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())} ; x = FUN1 ; arg_user_setting = base::list(x = as.name(x = "FUN1"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE) ; function_name = "F1" ; package_name = "P1"
    # FUN1 <- function(x, y){FUN2 <- function(x){x = 1}} ; x = FUN1 ; arg_user_setting = base::list(x = as.name(x = "FUN1"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE) ; function_name = "F1" ; package_name = "P1"
    # main code
    # modification of arg_user_setting$x for clean messages
    if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
        arg_user_setting$x <- base::paste0(base::as.character(x = arg_user_setting$x)[3], "()")
    }
    # end modification of arg_user_setting$x for clean messages
    # recovering the basic functions of R
    s <- base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base") # basic base::search() scope
    if(base::any( ! s %in% base::search())){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .functions_detect() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nTHE base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\n",
            base::paste(s[ ! s %in% base::search()], collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun <- base::unlist(base::sapply(X = s, FUN = function(x){base::ls(x, all.names = TRUE)})) # all the basic functions of R in all the scope
    # end recovering the basic functions of R
    # recovering the input function string
    code <- utils::capture.output(x) # no lines must be removed because it is to catch the lines of the full code
    code_line_nb <- 1:base::length(code)
    # code <- base::paste0(code, collapse = " \\n ") # recovering as single string separated by \\n (and not \n to avoid the eval(\n) when printing the error message)
    code <- base::gsub(x = code, pattern = " +", replacement = " ") # removal of multiple spaces
    code <- base::sub(x = code, pattern = "^ +", replacement = "") # removal of multiple spaces in the beginning od strings
    # end recovering the input function string

    # removal of empty lines
    empty_line.log <- base::grepl(code, pattern = "^\\s*$")
    # end removal of empty lines

    # removal of comments
    comment_line.log <- base::grepl(code, pattern = "^\\s*#") # removal of the lines starting by #
    if(base::length(code) == 0){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nTHE TESTED FUNCTION ", arg_user_setting$x, " IS EMPTY OR ONLY MADE OF COMMENTS")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    comment.log <- base::grepl(x = code, pattern = "#")
    if(base::any(comment.log, na.rm = TRUE)){
        comment.line.to.rm <- base::which(comment.log) # elements among code that have #
        lines <- code[comment.log]
        for(i2 in 1:base::length(lines)){
            lines.split <- base::strsplit(lines[i2], split = "#")[[1]]
            # detection of the first left # that is not between quotes
            count <- 1
            tempo.line <- lines.split[1]
            while.loop <- TRUE
            while(while.loop == TRUE & count < base::length(lines.split)){
                # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
                double.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = tempo.line, pattern = '"') # here FALSE means even number of quotes, thus that # is not between quotes, thus has to be removed. TRUE means that # is between quotes, thus has to be kept
                simple.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = tempo.line, pattern = "'") # idem
                odds.quotes.log <- double.quote.test |  simple.quote.test # lines to keep among commented lines
                if(odds.quotes.log == TRUE){
                    count <- count + 1
                    tempo.line <- base::paste0(tempo.line, "#", lines.split[count])
                }else{
                     while.loop <- FALSE
                }
            }
            # end detection of the first left # that is not between quotes
            lines[i2] <- tempo.line
        }
        code[comment.line.to.rm] <- lines
    }
    # end removal of comments
    # catch the internal function name created inside the tested function
    internal_fun_names <- base::unlist(base::lapply(X = code, FUN = function(x){
        output <- base::sub(pattern = "^\\s*(([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*)\\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.*", replacement = "\\1", x = x, perl = TRUE)
        # ^\\s* means in perl: 0 or any spaces at the begining of the string
        # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
        # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
        #  \\s*<-[\\s\\r\\n]*function[\\s\\r\\n]*\\(.* 0 or any space, assignation symbol, 0 or any (space, carriage return, end of line), an opening parenthesis, and any character
        if( ! output == x){
            base::return(output)
        }
    })) # To achieve the extraction of the function names, you need to wrap the part of the pattern that matches the function name in parentheses () to create a capturing group
    # end catch the internal function name created inside the tested function
    # trick to deal with end of lines between the name of the function and "("
    if(base::length(code) > 1){
        for (i2 in 2:base::length(code)) {
            # Check if the current string starts with spaces followed by a '('
            if (base::grepl("^\\s*\\(", code[i2])) {
                # Check if the previous string ends with the specified pattern
                if (base::grepl("([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*$", code[i2 - 1])) {
                # Append a '(' to the previous string
                code[i2 - 1] <- base::paste0(code[i2 - 1], "(")
                }
            }
        }
    }
    # end trick to deal with end of lines between the name of the function and "("
    # all function names in x
    pattern1 <- "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*\\s*\\(" # pattern to detect a function name, a$fun( is removed in .noclean_functions()
    # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
    # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
    # \\s*\\( means 0 or any space in perl, and an opening parenthesis
    # I could have used [\\s\\r\\n]* meaning any space or end of line or carriage return between the name and "(" but finally, another strategy used
        # - `this does not work well, as it does not take dots: "\\b[a-zA-Z\\.\\_]{1}[a-zA-Z0-9\\.\\_]+\\b", because of `\\b`: These are word boundaries. It ensures that the pattern matches only a complete word and not a part of a word.
        # - `[a-zA-Z.]{1}`: This portion of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), or a period (`.`) a single time ({1}).
        # - `[a-zA-Z0-9._]*`: This part of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), number (`0-9`), period (`.`), or underscore (`_`), repeated zero or more times (`*`). This represents the possible characters inside an R function name.
        # - `\\b`: Again, these are word boundaries, making sure the pattern captures the entire word and not just part of it.
        # -  not used: `(?= *\\()`: This is a lookahead assertion. It checks that the preceding pattern is followed by any spaces and a parenthesis (`\\(`), but doesn't include the spaces and parenthesis in the match. This is because, in R code, a function call is usually followed by a parenthesis, but the parenthesis is not part of the function name.

    fun_name <- base::list()
    fun_name_pos <- base::list()
    for(i1 in 1:base::length(code)){
        tempo <- saferDev:::.extract_all_fun_names(text = code[i1], pattern = pattern1) # recover all the function names, followed by "(", present in code, using a perl pattern
        fun_name <- base::c(fun_name, base::list(tempo$string))
        fun_name_pos <- base::c(fun_name_pos, base::list(tempo$pos))
    }
    # tempo <- base::lapply(code, FUN = function(x){saferDev:::.extract_all_fun_names(text = x, pattern = pattern1)})
    # removal of special functions
    tempo_log <- base::lapply(fun_name, FUN = function(x){ ! x %in% base::c("function", "if", "for", "while", "repeat")})
    fun_name_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name, y = tempo_log, SIMPLIFY = FALSE)
    fun_name_pos_wo_op <- base::mapply(FUN = function(x, y){x[y]}, x = fun_name_pos, y = tempo_log, SIMPLIFY = FALSE)
    # end removal of special functions
    # removal of empty string
    tempo.log <- base::sapply(fun_name_wo_op, FUN = function(x){base::length(x) == 0}) # detection of string with empty function names
    fun_name_wo_op <- fun_name_wo_op[ ! tempo.log]
    fun_name_pos_wo_op <- fun_name_pos_wo_op[ ! tempo.log]
    code_line_nb <- code_line_nb[( ! tempo.log) & ( ! comment_line.log) & ( ! empty_line.log)]
    if( ! (base::length(fun_name_wo_op) == base::length(fun_name_pos_wo_op) & base::length(fun_name_wo_op) == base::length(code_line_nb))){
        tempo.cat <- base::paste0("INTERNAL ERROR 2 IN .functions_detect() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nfun_name_wo_op: ", base::length(fun_name_wo_op), "\nfun_name_pos_wo_op: ", base::length(fun_name_pos_wo_op), "\ncode_line_nb: ", base::length(code_line_nb), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else if(base::any(base::is.na(code_line_nb))){
        tempo.cat <- base::paste0("INTERNAL ERROR 3 IN .functions_detect() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\ncode_line_nb SHOULD NOT CONTAIN NA.\ncode_line_nb:\n", base::paste(code_line_nb, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else{
        # with that, now the code line of code is indicated in as compartment names
        base::names(fun_name_wo_op) <- base::paste0("c", code_line_nb)
        base::names(fun_name_pos_wo_op) <- base::paste0("c", code_line_nb)
    }
    # end removal of empty string
    test.log <- base::mapply(FUN = function(x, y){base::length(x) != base::length(y)}, x = fun_name_wo_op, y = fun_name_pos_wo_op, SIMPLIFY = TRUE)
    if(base::any(test.log, na.rm = TRUE)){
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN .functions_detect() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL IN COMPARTMENTS ", base::paste(base::which(test.log), collapse = ", "), " OF fun_name_wo_op AND fun_name_pos_wo_op", base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # fun_name_wo_op_uni <- base::unlist(base::unique(fun_name_wo_op)) # in case
    # end all function names in x
    #### output
    output <- base::list(
        code = code, 
        all_basic_funs = fun, 
        fun_names = fun_name_wo_op, 
        fun_names_pos = fun_name_pos_wo_op, 
        code_line_nb = code_line_nb, 
        internal_fun_names = internal_fun_names,
        arg_user_setting = arg_user_setting
    )
    base::return(output)
    #### end output
}

