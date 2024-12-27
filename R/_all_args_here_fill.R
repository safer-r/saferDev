#' @title .all_args_here_fill
#' @description
#' Get the $MISSING_ARG_NAMES, $MISSING_ARGS and $NEW of all_args_here()
#' @param arg_full list of all arguments of the function with default value
#' @param arg_full_names vector of strings of the names of the arguments of the function
#' @param tempo_split vector of strings of the observed argument writting of the function.
#' @param three_dots_log vector of logical. Is ... present among arg_full_names 
#' @param i2 loop number
#' @param col1_i2 code line number of the checked function
#' @param col2_i2 name of the checked sub function
#' @param arg_user_setting_x name of the checked function
#' @param function_name Name of the function using this internal function.
#' @param package_name Name of the package of the function using this internal function.
#' @param internal_error_report_link String of the report link for internal error messages.
#' @param warn warning string.
#' @param warn_count warning count.
#' @returns
#'  A list:
#'    $col6: the $MISSING_ARG_NAMES.
#'    $col7: the $MISSING_ARGS.
#'    $col8: the $STATUS.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @keywords internal
#' @rdname internal_function
.all_args_here_fill <- function(
    arg_full,
    arg_full_names, 
    tempo_split, 
    three_dots_log, 
    i2, 
    col1_i2, 
    col2_i2,
    arg_user_setting_x, 
    function_name, 
    package_name,
    internal_error_report_link, 
    warn,
    warn_count
){
    # DEBUGGING
    # arg_full = arg_full ; arg_full_names = arg_full_names ; tempo_split = tempo_split ; three_dots_log = three_dots_log ; i2 = i2 ; col1_i2 = col1[i2] ; col2_i2 = col2[i2] ; function_name = function_name ; package_name = package_name 
    #  arg_full = list(definition = "sys.function(sys.parent())", call = "sys.call(sys.parent())", expand.dots = TRUE, envir = "parent.frame(2L)") ; arg_full_names = c("definition", "call", "expand.dots", "envir") ; tempo_split = "expand.dots = FALSE" ;  three_dots_log = c(FALSE, FALSE, FALSE, FALSE) ; col2_i2 = "match.call" ; col3_i2 = "match.call(expand.dots = FALSE)" ; function_name = "F1" ; package_name = "P1"
    #  arg_full = list(definition = sys.function(sys.parent()), call = sys.call(sys.parent()), expand.dots = TRUE, envir = parent.frame(2L)) ; arg_full_names = c("definition", "call", "expand.dots", "envir") ; tempo_split = c("sys.function(sys.parent())", "expand.dots = FALSE", "sys.call(sys.parent())") ;  three_dots_log = c(FALSE, FALSE, FALSE, FALSE) ; col2_i2 = "match.call" ; col3_i2 = "match.call(sys.function(sys.parent()), expand.dots = FALSE, sys.call(sys.parent()))" ; function_name = "F1" ; package_name = "P1"
    #  arg_full = list(... = "", collapse = " ", recycle0 = FALSE) ; arg_full_names = c("...", "collapse", "recycle0") ; tempo_split = c("AA", "collapse = \" \"", "BB", "recycle0 = FALSE") ; three_dots_log = c(TRUE, FALSE, FALSE) ; col2_i2 = "paste0" ; col3_i2 = 'paste0("AA", collapse = " ", "BB", recycle0 = FALSE)' ; function_name = "F1" ; package_name = "P1"
    #  arg_full = list(... = "", collapse = " ", recycle0 = FALSE) ; arg_full_names = c("...", "collapse", "recycle0") ; tempo_split = c("AA", "collapse = \" \"", "BB") ; three_dots_log = c(TRUE, FALSE, FALSE) ; col2_i2 = "paste0" ; col3_i2 = 'paste0("AA", collapse = " ", "BB")' ; function_name = "F1" ; package_name = "P1"
    pattern1 <- "^\\s*([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]*[\\s\\r\\n]*=" # looking for the arg name
    good_args <- NULL
    missing_args <- NULL
    missing_args_names <- NULL
    obs_arg_log <- base::logical()
    if(base::any(three_dots_log, na.rm = TRUE)){
        arg_full_names <- arg_full_names[ ! three_dots_log]
        arg_full <- arg_full[ ! three_dots_log]
    }
    if(base::length(arg_full) == 0){
        # col5 <- base::c(col5, "...") #inactivated because already filled above
        col6 <- ""
        col7 <- ""
        col8 <- ""
    }else{
        # scan for args names present in tempo_split
        good_count <- 0 # to define if all the args are written (not considering ...)
        if(base::length(tempo_split) == 0 & base::length(arg_full_names) > 0){
            missing_args_names <- arg_full_names
        }else{
            obs_arg_log <- base::rep(TRUE, base::length(tempo_split)) # will help for counting the tempo_split args without arg name before. All the remaining TRUE will be values that need an arg name
            for(i3 in 1:base::length(arg_full_names)){
                pattern3 <- base::paste0("^[\\s\\r\\n]*", arg_full_names[i3], "[\\s]*=") # looking for the arg name
                tempo.log <- base::grepl(x = tempo_split, pattern = pattern3, perl = TRUE)
                if(base::sum(tempo.log, na.rm = TRUE) == 1){ # arg i3 has its names written in the args between ()
                    good_args <- base::c(good_args, tempo_split[tempo.log])
                    obs_arg_log <- obs_arg_log & ! tempo.log # remove the position of the taken arg in tempo_split
                    good_count <- good_count + 1
                }else if(base::sum(tempo.log, na.rm = TRUE) == 0){ # arg i3 has not its names written in the args between ()
                    missing_args_names <- base::c(missing_args_names, arg_full_names[i3])
                }else{
                    tempo.cat <- base::paste0("INTERNAL ERROR 1 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\npattern3 DETECTED SEVERAL TIMES IN ARGUMENTS:\n\npattern3:\n", base::paste(pattern3, collapse = "\n"), "\n\ntempo_split:\n", base::paste(tempo_split[tempo.log], collapse = "\n"), "\n\nCHECK IF THE ARGUMENT IS PRESENT SEVERAL TIMES IN LINE ", col1_i2, ", INSIDE ", col2_i2, base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
            }
        }
        # end scan for args names present in tempo_split
        # removal of arguments without arg name before in obs_arg_log
        supp_args_in_three_dots <- NULL
        if(base::any(three_dots_log, na.rm = TRUE)){
            if(base::length(x = obs_arg_log) != 0){ # no need to add & base::length(tempo_split) != 0 because both have the same length
                for(i3 in 1:base::length(tempo_split)){
                    if(base::grepl(x = tempo_split[i3], pattern = pattern1, perl = TRUE) & obs_arg_log[i3] == TRUE){ # obs_arg_log[i3] == TRUE means values that need an arg name but detection of a = with only arg name rule before
                        obs_arg_log[i3] <- FALSE # remove this arg from the args that need an arg name
                        supp_args_in_three_dots <- base::c(supp_args_in_three_dots, tempo_split[i3])
                    }
                }
            }
        }
        # end removal of arguments without arg name before in obs_arg_log
        # detection of arguments that starts by the same string in the sub function
        tempo_col8_end <- NULL
        same_begin <- base::unlist(base::lapply(
            FUN = function(x){
                tempo_log <- base::grepl(x = arg_full_names, pattern = base::paste0("^", x), perl = FALSE)
                if(base::sum(tempo_log, na.rm = TRUE) > 1){ 
                    base::return(arg_full_names[tempo_log][base::which.min(base::nchar(arg_full_names[tempo_log]))])
                }
            }, 
            X = arg_full_names
        ))
        if( ! base::is.null(same_begin)){
            tempo_col8_end <- base::paste0("WARNING: SEVERAL ARGUMENT NAMES OF THE FUNCTION BEGINNING WITH ", base::paste0(same_begin[ ! base::is.null(same_begin)], collapse = " ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        }
        # end detection of arguments that starts by the same string in the sub function
        # checking if arg name are not fully written
        arg_full_symbol_type <- base::sapply(X = arg_full, FUN = function(x){base::all(base::typeof(x) == "symbol", na.rm =TRUE)}) # to check if any arg without optional value are completed with obs arg values
        if(base::any(arg_full_symbol_type, na.rm =TRUE) & base::length(tempo_split) == 0){
            tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nTHE TESTED FUNCTION ", arg_user_setting_x, " SEEMS TO HAVE A WRITTING ERROR IN LINE ",  col1_i2, " AND FUNCTION ", col2_i2, ".\nPLEASE, RUN THE TESTED FUNCTION FIRST.")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        tempo_col8 <- NULL
        if(( ! base::is.null(missing_args_names)) & base::length(tempo_split) != 0){
            for(i3 in 1:base::length(tempo_split)){
                if(base::grepl(x = tempo_split[i3], pattern = pattern1, perl = TRUE)){
                    tempo_arg_name <- base::strsplit(tempo_split[i3], split = "[\\s\\r\\n]*=", perl = TRUE)[[1]][1]
                    tempo_arg_name <- base::gsub(pattern = "^[\\s]*", replacement = "", x = tempo_arg_name) # removing leading space
                    if( ! base::is.null(same_begin)){
                        if( ! tempo_arg_name %in% same_begin){
                            tempo.log <- base::grepl(x = missing_args_names, pattern = base::paste0("^", tempo_arg_name), perl = FALSE)
                            if(base::sum(tempo.log, na.rm = TRUE) > 1){
                                tempo.cat <- base::paste0("INTERNAL ERROR 2 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nIN LINE ", i2, " IN THE ", col2_i2, " FUNCTION\ntempo_arg_name DETECTS SEVERAL TIMES ARGUMENT NAMES:\n\ntempo_arg_name:\n", tempo_arg_name, "\n\nmissing_args_names:\n", base::paste(missing_args_names, collapse = "\n"), "\n\nmissing_args_names[tempo.log]:\n", base::paste(missing_args_names[tempo.log], collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
                            }
                            if(base::sum(tempo.log, na.rm = TRUE) == 1){
                                tempo_col8 <- base::c(
                                    tempo_col8, 
                                    base::paste0(
                                        base::ifelse(test = base::is.null(tempo_col8), yes = "", no = " ; "), 
                                        base::paste0(tempo_arg_name, " ARG NAME HAS TO BE FULLY WRITTEN ", missing_args_names[tempo.log])
                                    )
                                )
                            }
                        }
                    }
                }
            }
            if(( ! base::is.null(tempo_col8)) & ! base::is.null(tempo_col8_end)){
                tempo_col8 <- base::paste0(tempo_col8, " & ", tempo_col8_end, collapse = NULL, recycle0 = FALSE)
            }
        }
        # end checking if arg name are not fully written
        # when ... is present or not
            # Of note, when ... is present, argument values must be preceeded by their arg name. This means that values without arg names of the scanned function are ...
            # Otherwise, the first value without names must take the first arg name not already used, the second value without names must take the second, etc., then finish by the none used arg names with their default values
        missing_arg_log <- arg_full_names %in% missing_args_names
        if(base::any(three_dots_log, na.rm = TRUE) & base::all( ! arg_full_symbol_type, na.rm =TRUE)){ # ... present but no mandatory args with value to set 
            missing_args <-  base::unlist(base::mapply(FUN = function(x, y){base::paste0(x, " = ", if(base::is.null(y)){"NULL"}else{y})}, x = arg_full_names[missing_arg_log], y = arg_full[missing_arg_log], SIMPLIFY = TRUE)) # missing arg values with names
            good_args <- base::c(
                tempo_split[ ! tempo_split %in% good_args], # arg values without names
                good_args, # obs arg values with names
                base::paste0(" ", missing_args) # missing arg values with names #a space added to finally have  comma followed by a space
            )
        }else{
            count_good_args <- 0
            final <- NULL
            missing_args <-  NULL
            for(i3 in 1:base::length(arg_full_names)){ # here I cannot have more args than base::length(arg_full_names)
                if(missing_arg_log[i3] == TRUE){
                    if(base::sum(obs_arg_log) > 0){ # this means that remains obs arg with no arg names written
                        tempo <- base::paste0(arg_full_names[i3], " = ", tempo_split[base::which(obs_arg_log == TRUE)[1]])
                        obs_arg_log[base::which(obs_arg_log == TRUE)[1]] <- FALSE
                    }else{
                        tempo <- base::paste0(arg_full_names[i3], " = ", if(base::is.null(base::deparse(arg_full[[i3]]))){"NULL"}else{base::deparse(arg_full[[i3]])})
                    }
                    missing_args <- base::c(missing_args, tempo)
                    final <- base::c(final, base::ifelse(test = i3 == 1, yes = tempo, no = base::paste0(" ", tempo))) # take the first pos always of the args with no arg names
                }else{
                    count_good_args <- count_good_args + 1
                    final <- base::c(final, good_args[count_good_args])
                }
                arg_full_symbol_type[i3] <- FALSE
            }
            good_args <- final
            if(base::any(arg_full_symbol_type)){
                tempo.cat <- base::paste0("INTERNAL ERROR 3 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nARGUMENT WITHOUT OPTIONAL VALUES (MANDATORY ARGS) CANNOT REMAIN WITHOUT VALUE:\n\narg_full_symbol_type:\n", base::paste(arg_full_symbol_type, collapse = "\n"), "\n\narg_full_names:\n", base::paste(arg_full_names, collapse = "\n"), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(( ! base::any(three_dots_log, na.rm = TRUE)) & base::any(obs_arg_log, na.rm =TRUE)){
                tempo.cat <- base::paste0("INTERNAL ERROR 4 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\nCANNOT HAVE OBS ARGUMENT NOT INCORPORATED YET IF ! base::any(three_dots_log, na.rm = TRUE) IS TRUE:\n\nthree_dots_log:\n", base::paste(three_dots_log, collapse = " "), "\n\nobs_arg_log:\n", base::paste(obs_arg_log, collapse = " "), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(count_good_args > base::length(tempo_split)){
                tempo.cat <- base::paste0("INTERNAL ERROR 5 IN .all_args_here_fill() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE\ncount_good_args + 1 CANNOT BE MORE THAN length(tempo_split):\n\nlength(tempo_split): ", base::length(tempo_split), "\n\ncount_good_args + 1: ", count_good_args + 1, base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)))
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn_count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            if(base::any(three_dots_log, na.rm = TRUE) & base::any(obs_arg_log, na.rm =TRUE)){ # obs values not yet in good_args
                if(count_good_args + 1 <= base::length(tempo_split)){
                    good_args <- base::c(good_args, tempo_split[obs_arg_log])
                }
            }
        }
        # end when ... is present or not
        # col5 done above
        col6 <- base::paste(missing_args_names, collapse = ", ") # if NULL return ""
        col7 <- base::paste(missing_args, collapse = ", ")  # if NULL return ""
        tempo <- base::paste0(
            col2_i2, 
            "(", 
            base::ifelse(test = ! base::is.null(supp_args_in_three_dots), yes = base::gsub(pattern = "^[\\s]*", replacement = "", x = base::paste0(supp_args_in_three_dots, collapse = ","), perl = TRUE), no = ""), 
            base::ifelse(test = ( ! base::is.null(supp_args_in_three_dots)) & ( ! base::is.null(good_args)) , yes = ",", no = ""), 
            base::ifelse(test = ! base::is.null(good_args), yes = base::paste0(good_args, collapse = ","), no = ""),
            ")"
        )
        if(base::length(arg_full_names) == good_count){
            col8 <- "GOOD"
        }else{
            col8 <- tempo
        }
        if( ! base::is.null(tempo_col8)){
            col8 <- tempo_col8
        }
    }
    base::return(base::list(col6 = col6, col7 = col7, col8 = col8))
}

