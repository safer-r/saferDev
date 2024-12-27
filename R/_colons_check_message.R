
#' @title .colons_check_message
#' @description
#' Create the message for the colons_check() function.
#' @param list.fun list of names of all the functions.
#' @param list.fun.pos list of position of first character of names of all the functions in ini.
#' @param line.nb vector of corresponding line number.
#' @param ini vector of string of the initial function code analyzed.
#' @param arg_user_setting list of arg user settings.
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @param text either "BASIC" or "OTHER".
#' @param internal_fun_names vector of string of names of internal functions in the function code analyzed.
#' @returns
#'  A list:
#'  $output.cat: the message (string).
#'  $colon_not_here: logical vector. Does list.fun contain function names without :: or ::: ?
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.colons_check_message <- function(
    list.fun, 
    list.fun.pos, 
    line.nb, 
    ini, 
    arg_user_setting, 
    function_name, 
    package_name, 
    internal_error_report_link, 
    text,
    internal_fun_names
){
    # DEBUGGING
    # list.fun = in_basic_fun ; list.fun.pos = in_basic_fun_names_pos ; line.nb = in_basic_code_line_nb ; ini = out$code ; arg_user_setting = out$arg_user_setting ; function_name = function_name ; package_name = package_name ; text = "BASIC" ; internal_fun_names = out$internal_fun_names
    # list.fun = in_other_fun ; list.fun.pos = in_other_fun_names_pos ; line.nb = in_other_code_line_nb ; ini = out$code ; arg_user_setting = out$arg_user_setting ; function_name = function_name ; package_name = package_name ; text = "OTHER" ; internal_fun_names = out$internal_fun_names

    output.cat <- NULL
    colon_not_here <- FALSE # reminder: no colon problem with internal functions
    # check the identical structure of list.fun and list.fun.pos
    ident_str <- function(list.fun, list.fun.pos, error_nb, internal_error_report_link){
        if( ! (base::length(x = list.fun) == base::length(x = list.fun.pos) & base::all(base::sapply(X = list.fun, FUN = function(x){base::length(x = x)}) == base::sapply(X = list.fun.pos, FUN = function(x){base::length(x = x)}), na.rm = TRUE))){
            tempo.cat <- base::paste0("INTERNAL ERROR ", error_nb, " IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLISTS list.fun AND list.fun.pos SHOULD HAVE IDENTICAL STRUCTURES\nBUT LENGTHS ARE RESPECTIVELY:\n", base::length(x = list.fun), "\n", base::length(x = list.fun.pos), "\nAND NUMBER OF ELEMENT IN EACH COMPARTMENT ARE RESPECTIVELY:\n", 
                base::paste(base::sapply(X = list.fun, FUN = function(x){base::length(x = x)}), collapse = " "), "\n", base::paste(base::sapply(X = list.fun, FUN = function(x){base::length(x = x)}), collapse = " "), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end check the identical structure of list.fun and list.fun.pos
    if(base::length(text) != 1 & base::any( ! text %in% base::c("BASIC", "OTHER"))){
        tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nTHE text ARGUMENT OF .colons_check_message() MUST BE \"BASIC\" OR \"OTHER\".\nTHE PROBLEM IS:\n",
            base::paste(text, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    ident_str(list.fun = list.fun, 
        list.fun.pos = list.fun.pos, 
        error_nb = 2, 
        internal_error_report_link = internal_error_report_link
    )
    # remove internal functions in other functions (list.fun and list.fun.pos)
    if(text == "OTHER" & base::length(internal_fun_names) > 0){
        empty_compart_log <- ! logical(length = base::length(x = list.fun)) # all TRUE at the beginning
        for(i2 in 1:base::length(internal_fun_names)){
            tempo_log <- base::lapply(X = list.fun, FUN = function(x){
                x == internal_fun_names[i2]
            })
            if(i2 == 1){
                intern_fun_log <- tempo_log
            }else{
                intern_fun_log <- base::mapply(FUN = function(x, y){x | y}, x = tempo_log, y = intern_fun_log, SIMPLIFY = FALSE)
            }
        }
        # remove internal functions elements
        list.fun <- base::mapply(FUN = function(x, y){y[ ! x]}, x = intern_fun_log, y = list.fun, SIMPLIFY = FALSE)
        list.fun.pos <- base::mapply(FUN = function(x, y){y[ ! x]}, x = intern_fun_log, y = list.fun.pos, SIMPLIFY = FALSE)
        # end remove internal functions elements
        ident_str(list.fun = list.fun, 
            list.fun.pos = list.fun.pos, 
            error_nb = 3, 
            internal_error_report_link = internal_error_report_link
        )
        # remove empty compartment
        tempo_log2 <- base::sapply(X = list.fun, FUN = function(x){base::length(x = x) == 0}) # test if empty compartment
        list.fun <- list.fun[ ! tempo_log2]
        list.fun.pos <- list.fun.pos[ ! tempo_log2]
        line.nb <- line.nb[ ! tempo_log2]
        # end remove empty compartment
        output.cat <- base::paste0(
            "INSIDE ", arg_user_setting$x, "(), ", base::ifelse(base::length(x = list.fun) == 0, "ONLY", ""), "INTERNAL FUNCTION", base::ifelse(base::length(internal_fun_names) == 1, "", "S"), " DETECTED:\n", 
            base::paste(internal_fun_names, collapse = "\n")
        )
        # reminder: no colon problem with internal functions
    }
    # end remove internal functions in other functions (list.fun and list.fun.pos)
    if(base::length(x = list.fun) != 0){
        # pattern2 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", fun.uni, "\\s*\\("), collapse = "|") # to split string according to function name as splitter. Pattern (?<![A-Za-z0-9._]) means "must not be preceeded by any alphanum or .or _
        # pattern3 <- base::paste(base::paste0("(?<![A-Za-z0-9._])", fun.uni, "\\s*\\($"), collapse = "|") # same as pattern2 but used to know if the seeked function is at the end of the string
        basic_ini <- ini[line.nb]
        if( ! (base::length(list.fun) == base::length(list.fun.pos) & base::length(list.fun) == base::length(line.nb) & base::length(list.fun) == base::length(basic_ini))){
            tempo.cat <- base::paste0("INTERNAL ERROR 2 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nIN .colons_check_message(), LENGTHS SHOULD BE IDENTICAL\nlist.fun: ", base::length(list.fun), "\nlist.fun.pos: ", base::length(list.fun.pos), "\nline.nb: ", base::length(line.nb), "\nbasic_ini: ", base::length(basic_ini), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        res <- list.fun.pos
        for(i1 in 1:base::length(basic_ini)){
            res[[i1]] <- base::mapply(FUN = function(x , y){z <- base::substr(x = x, start = 1, stop = y - 1)}, x = basic_ini[i1], y = list.fun.pos[[i1]], SIMPLIFY = TRUE, USE.NAMES = FALSE)
        }
        # res <- base::strsplit(x = basic_ini, split = pattern2, perl = TRUE) # in res, all the strings should finish by ::
        # tempo.log <- ! base::grepl(x = basic_ini, pattern = pattern3, perl = TRUE) # strings of basic_ini that does not finish by the function name
        # in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
        # if(base::sum(tempo.log, na.rm = TRUE) > 0){
        #     res[tempo.log] <- base::lapply(X = res[tempo.log], FUN = function(x){x[-base::length(x)]})
        # }
        # end in each compartment of res, the last split section is removed because nothing to test at the end (end of code)
        res2 <- base::lapply(X = res, FUN = function(x){base::substr(x, base::nchar(x)-1, base::nchar(x))}) # base::nchar(x)-1 takes only :: if the strings ends by :::
        base::names(res2) <- NULL
        if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = res, FUN = function(x){base::length(x)}))){
            tempo.cat <- base::paste0("INTERNAL ERROR 3 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " "), "\nres: ", base::paste(base::sapply(X = res, FUN = function(x){base::length(x)}), collapse = " "), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        colon_not_here <- base::lapply(X = res2, FUN = function(x){ ! x %in% "::"}) # no need to check for ":::" because base::nchar(x)-1 takes only :: if the strings ends by :::
        if( ! base::all(base::sapply(X = res2, FUN = function(x){base::length(x)}) == base::sapply(X = colon_not_here, FUN = function(x){base::length(x)}))){
            tempo.cat <- base::paste0("INTERNAL ERROR 4 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS SHOULD BE IDENTICAL\nres2: ", base::paste(base::sapply(X = res2, FUN = function(x){base::length(x)}), collapse = " "), "\ncolon_not_here: ", base::paste(base::sapply(X = colon_not_here, FUN = function(x){base::length(x)}), collapse = " "), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(base::any(base::unlist(colon_not_here))){
            col1 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){base::rep(y, base::sum(x))}, x = colon_not_here, y = line.nb, SIMPLIFY = TRUE)))
            col2 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = list.fun, SIMPLIFY = TRUE)))
            col3 <- base::as.vector(base::unlist(base::mapply(FUN = function(x, y){y[x]}, x = colon_not_here, y = res, SIMPLIFY = TRUE)))
            if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col2) == base::length(col3))){
                tempo.cat <- base::paste0("INTERNAL ERROR 5 IN .colons_check_message() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nLENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), AND col3 (", base::length(col3), "), SHOULD BE EQUAL\n", base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
            # removal of functions between quotes and after $
            tempo.log <- saferDev:::.noclean_functions(col1 = col1, col2 = col2, col3 = col3, ini = ini) # function names are inside quotes or after $ ?
            if(base::sum(tempo.log, na.rm = TRUE) > 0){
                col1 <- col1[ ! tempo.log] # keep clean functions
                col2 <- col2[ ! tempo.log] # keep clean functions
                col3 <- col3[ ! tempo.log] # keep clean functions 
            }
            # end removal of functions between quotes and after $
            if(base::length(col1) > 0){
                tempo.pos <- base::paste0(col1, "\t", col2, "\t\t", col3)
                output.cat <- base::paste0(
                    base::ifelse(test = base::is.null(output.cat), yes = "", no = base::paste0(output.cat, "\n\n")),
                    "INSIDE ", arg_user_setting$x, "(), SOME :: OR ::: ARE MISSING AT ", text, " FUNCTION POSITIONS:\n\n", 
                    "LINE\tFUN\t\tSTRING_BEFORE\n",
                    base::paste(tempo.pos, collapse = "\n")
                )
            }
        }
    }
    base::return(base::list(output.cat = output.cat, colon_not_here = base::unlist(colon_not_here)))
}
