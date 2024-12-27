#' @title .fun_args_pos
#' @description
#' Return the positions of 1st letter of the function name and opening and closing parenthesis, as well as positions of the internal parenthesis.
#' @param text A string.
#' @param pattern: A perl regex to extract function name and (), using generally paste0(<FUNCTION_NAME>, "[\\s\\r\\n]*\\(").
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @returns A list containing two positions:
#' $begin_fun: position of 1st letter of the function name.
#' $begin: position of the "(" of the function.
#' $end: position of the closing ")" of the function.
#' $middle_bracket_pos: list of positions of the couple of brackets in the middle of the begin and end positions. In each compartment, the first number is the position of ( and the second the position of ). NULL if no inside brackets.
#' @details
#' Warning: the string must be cleaned form brackets between quotes. Use .in_quotes_replacement() for that.
#' Warning: quotes in strings are escaped, so that position of ( in \"a( is 3, not 4.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' # Warning : examples only with strings that must be cleaned form brackets between quotes
#' .fun_args_pos(text = "a$regmatches(x = text, m = matches)[[1]]", pattern = paste0("regmatches", "[\\s\\r\\n]*\\("), function_name = "F1", package_name = "P1")
#' .fun_args_pos(text = ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)', pattern = paste0("paste0", "[\\s\\r\\n]*\\("), function_name = "F1", package_name = "P1")
#' }
#' @keywords internal
#' @rdname internal_function
.fun_args_pos <- function(
    text, 
    pattern,
    function_name, 
    package_name,
    internal_error_report_link
){
    # DEBUGGING
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
    # text = ' "a" ; paste0("I", paste0(sum(1:3), collapse = " "), min(1) ) ; range(2)' ; pattern = paste0("paste0", "[\\s\\r\\n]*\\(") ; function_name = "F1" ; package_name = "P1"
    # text = 'base::gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text)' ; pattern = 'gregexpr[\\s\\r\\n]*\\(' ; function_name = "F1" ; package_name = "P1"
    check_pos <- function(x){
        if(base::length(x) != 1 | base::any(base::is.na(x), na.rm = TRUE) | base::is.null(x) | base::any(x < 0 , na.rm = TRUE)){
            tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .fun_args_pos() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nINTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF ", base::match.call(expand.dots = FALSE)$x, "\ntext: ", base::paste(text, collapse = "\n"), "\npattern: ", base::paste(pattern, collapse = "\n"), "\nfun_pos: ", base::paste(fun_pos, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }

    while_loop <- function(
        start,
        all_pos, 
        open_pos,
        close_pos,
        function_name,
        package_name,
        internal_error_report_link, 
        text,
        pattern
    ){
        count <- 1 # 1 because ( of the function is already opened. When count == 0, we will have the closing )
        final_pos <- base::which(all_pos == start) #start by the first ( but will be incremented
        loop.nb <- 1 
        while(count != 0 & loop.nb < base::length(all_pos)){
            final_pos <- final_pos + 1
            if(all_pos[final_pos] %in% open_pos){
                count <- count + 1
            }
            if(all_pos[final_pos] %in% close_pos){
                count <- count - 1
            }
            loop.nb <- loop.nb + 1
        }
        if(count != 0){
            tempo.cat <- base::paste0("INTERNAL ERROR 1 IN while_loop() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nINTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION OF THE CLOSING BRACKET IN ", base::match.call(expand.dots = FALSE)$x, "\ntext: ", base::paste(text, collapse = "\n"), "\npattern: ", base::paste(pattern, collapse = "\n"), "\ncount: ", base::paste(count, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }else{
            base::return(final_pos)
        }
    }

    open_paren_pos <- base::as.vector(base::gregexpr(pattern = "\\(", text = text)[[1]])
    close_paren_pos <- base::as.vector(base::gregexpr(pattern = "\\)",  text = text)[[1]])
    # left position
    fun_pos <- base::as.vector(base::gregexpr(pattern = pattern,  text = text, perl = TRUE)[[1]][1]) # position of the 1st character of fun in text
    check_pos(x = fun_pos)
    # end left position
    fun_open_paren_pos <- open_paren_pos[open_paren_pos > fun_pos][1] # position of ( of the fonction
    check_pos(x = fun_open_paren_pos)
    # detection of the closing ) of the function
    all_pos <- base::sort(base::c(open_paren_pos, close_paren_pos))
    
    final_pos <- while_loop(
        start = fun_open_paren_pos,
        all_pos = all_pos, 
        open_pos = open_paren_pos, 
        close_pos = close_paren_pos, 
        function_name = function_name,
        package_name = package_name,
        internal_error_report_link = internal_error_report_link, 
        text = text,
        pattern = pattern
    )
    fun_close_paren_pos <- all_pos[final_pos]
    # end detection of the closing ) of the function
    # middle brackets
    tempo_log <- all_pos > fun_open_paren_pos & all_pos < fun_close_paren_pos
    if(base::any(tempo_log, na.rm = TRUE)){
        all_pos_inside <- all_pos[tempo_log] # all positions of the brackets inside fun(    )
        open_paren_pos_inside <- all_pos_inside[all_pos_inside %in% open_paren_pos]
        count_open_paren_pos_inside <- base::length(open_paren_pos_inside)
        close_paren_pos_inside <- all_pos_inside[all_pos_inside %in% close_paren_pos]
        count_close_paren_pos_inside <- base::length(open_paren_pos_inside)
        if(count_open_paren_pos_inside != count_close_paren_pos_inside | count_open_paren_pos_inside == 0){ #count_open_paren_pos_inside == 0 because tempo_log above has some TRUE
            tempo.cat <- base::paste0("INTERNAL ERROR 2 IN while_loop() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nTHE .fun_args_pos() INTERNAL FUNCTION DID NOT PROPERLY DETECT THE POSITION ALL THE BRACKETS INSIDE THE FUN(    ) BRACKETS IN ", base::match.call(expand.dots = FALSE)$x, "\ntext: ", base::paste(text, collapse = "\n"), "\npattern: ", base::paste(pattern, collapse = "\n"), "\nCOUNT OF OPENED BRACKETS: ", count_open_paren_pos_inside, "\nCOUNT OF CLOSING BRACKETS: ", count_close_paren_pos_inside, "\nCHECK THAT THE STRING HAS ALL THE BRACKETS BETWEEN QUOTES REMOVED", base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        middle_bracket_pos <- base::vector(mode = "list", length = count_open_paren_pos_inside)
        for(i2 in 1:count_open_paren_pos_inside){
            final_pos2 <- while_loop(
                start = open_paren_pos_inside[i2],
                all_pos = all_pos_inside, 
                open_pos = open_paren_pos_inside, 
                close_pos = close_paren_pos_inside, 
                function_name = function_name,
                package_name = package_name, 
                internal_error_report_link = internal_error_report_link, 
                text = text,
                pattern = pattern
            )
            middle_bracket_pos[[i2]] <- base::c(open_paren_pos_inside[i2], all_pos_inside[final_pos2])
        }
    }else{
        middle_bracket_pos <- NULL
    }
    # end middle brackets
    output <- base::list(
        begin_fun = fun_pos, 
        begin = fun_open_paren_pos,
        end = fun_close_paren_pos,
        middle_bracket_pos = middle_bracket_pos
    )
    base::return(output)
}




