#' @title all_args_here
#' @description
#' Verify that all the functions used inside a function are written with all their arguments. For instance: base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE) and not paste0(letters[1:2]).
#' @param x Function name, written without quotes and brackets.
#' @param export Single logical value. Export the data frame into a .tsv file? If TRUE, the data frame is not returned by the function but only exported.
#' @param path_out Single character string indicating the absolute pathway of the folder where to export the data frame. path_out = "." means the R working directory set by the user. Ignored if export is FALSE
#' @param df_name Single character string indicating the name of the exported data frame file. Ignored if export is FALSE.
#' @param overwrite Single logical value. Overwrite potential df_name file already existing in path_out? Ignored if export is FALSE.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful to overcome R execution using system with non admin rights for R package installation in the default directories. Ignored if NULL (default): only the pathways specified by .libPaths() are used for package calling. Specify the right path if the function returns a package path error.
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if all_args_here() fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>".
#' @returns 
#' A data frame indicating the missing arguments or a message saying that everything seems fine.
#' If export argument is TRUE, then the data frame is exported as res.tsv instead of being returned.
#' Data frame: 
#'  $LINE_NB: the line number in the function code (starting at the "<- function" line, i.e., without counting the #' header lines)
#'  $FUN_NAME: the function name.
#'  $FUN_ARGS: the written arguments of $FUN_NAME. "NOT_CONSIDERED" means that the function is between quotes or after $
#'  $FUN_POS: the position of the first character of the function name in the $LINE_NB line of the code.
#'  $DEF_ARGS: the defaults arguments of $FUN_NAME. "NO_ARGS" means that the function has no arguments
#'  $MISSING_ARG_NAMES: the missing argument names in $FUN_ARGS.
#'  $MISSING_ARGS: the missing arguments with their values in $FUN_ARGS.
#'  $STATUS: either "GOOD", meaning that all the arguments are already written, or a new proposal of arguments writting, or indicates if some arguments are not fully written (abbreviation is discouraged), or nothing.
#' 
#' An additionnal message "EVERYTHING SEEMS CLEAN" if the $STATUS column is only made of "" and "GOOD".
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
#' 2) Proposals in the STATUS column are only suggestions, as it is difficult to anticipate all the exceptions with arguments writting.
#' 
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that return an error
#' all_args_here(mean)
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R") ; all_args_here(test2, export = TRUE)
#' }
#' FUN2 <- function(x, y){middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())}
#' all_args_here(FUN2, safer_check = FALSE)
#' source("https://raw.githubusercontent.com/safer-r/.github/refs/heads/main/profile/backbone.R")
#' all_args_here(BACKBONE, export = TRUE, safer_check = FALSE)
#' @export
all_args_here <- function(
    x, 
    export = FALSE,
    path_out = ".",
    df_name = "res.tsv", 
    overwrite = FALSE,
    lib_path = NULL, 
    safer_check = TRUE,
    error_text = ""
){
    # DEBUGGING
    # x = .expand_R_libs_env_var ; export = FALSE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE
    # library(saferGraph) ; x = close2 ; export = FALSE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; export = TRUE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # library(saferDev) ; x = get_message ; export = TRUE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\all_args_here.R") ; x = all_args_here ; export = TRUE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # arg_user_setting = base::list(x = as.name(x = "test2"), export = TRUE,  path_out = ".",  df_name = "res.tsv",  overwrite = TRUE,  lib_path = NULL,  safer_check = TRUE, error_text = "")
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R") ; x = test2 ; export = TRUE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # use the folling line before out <- 
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "test2"), export = TRUE,  path_out = ".",  df_name = "res.tsv",  overwrite = TRUE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "export",  "path_out",  "df_name",  "overwrite", "lib_path", "safer_check")
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\.github\\profile\\backbone.R") ; x = BACKBONE ; export = TRUE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = "" # use the folling line before out <- 
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "BACKBONE"), export = TRUE,  path_out = ".",  df_name = "res.tsv",  overwrite = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "") ; arg_names <- c("x", "export",  "path_out",  "df_name",  "overwrite", "lib_path", "safer_check")
    # FUN1 <- function(x, y){middle_bracket2 <- base::do.call(what = base::c, args = code_for_col, quote = FALSE, envir = base::parent.frame())} ; x = FUN1 ; export = FALSE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "export",  "path_out",  "df_name",  "overwrite", "lib_path", "safer_check")
    # x = saferDev::colons_check ; export = FALSE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "colons.check" ; arg_user_setting = base::list(x = as.name(x = "colons.check"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "export",  "path_out",  "df_name",  "overwrite", "lib_path", "safer_check")
    # x = saferDev::all_args_here ; export = FALSE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- "all_args_here" ; arg_user_setting = base::list(x = as.name(x = "all_args_here"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "export",  "path_out",  "df_name",  "overwrite", "lib_path", "safer_check")
    # x = .functions_detect ; export = FALSE ; path_out = "." ; df_name = "res.tsv" ; overwrite = FALSE ; lib_path = NULL ; safer_check = TRUE ; error_text = ""
    # function_name <- ".functions_detect" ; arg_user_setting = base::list(x = as.name(x = ".functions_detect"), export = FALSE,  path_out = ".",  df_name = "res.tsv",  overwrite = FALSE,  lib_path = NULL,  safer_check = TRUE, error_text = "") ; arg_names <- c("x", "export",  "path_out",  "df_name",  "overwrite", "lib_path", "safer_check")

    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user)
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation
    # basic error text start
    error_text_start <- base::paste0(
        "ERROR IN ", 
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name, 
        collapse = NULL, 
        recycle0 = FALSE
    )
    # end basic error text start
    # check of the error_text argument
    if( ! (base::all(base::typeof(x = error_text) == "character", na.rm = TRUE) & base::length(x = error_text) == 1)){ # no need to test is.null(error_text) because typeof(x = NULL) == "character" returns FALSE
        tempo_cat <- base::paste0(
            error_text_start, 
            "\nTHE error_text ARGUMENT MUST BE A SINGLE CHARACTER STRING (CAN BE \"\").\nHERE IT IS:\n", 
            base::paste0(error_text, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    # end check of the error_text argument
    # basic error text start updated
    error_text_start <- base::paste0(
        error_text_start, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    # end basic error text start updated
    # internal error text
    intern_error_text_start <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    # end internal error text
    #### end error_text initiation

    #### environment checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if( ! base::is.null(x = lib_path)){
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = FALSE)){ # no na.rm = TRUE with typeof
            tempo_cat <- base::paste0(
                error_text_start, 
                "DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                base::paste0(lib_path, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA
            tempo_cat <- base::paste0(
                error_text_start, 
                "DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\n", 
                base::paste0(lib_path, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib_path <- base:::.libPaths(new = , include.site = TRUE)
        }
    }else{
        lib_path <- base:::.libPaths(new = , include.site = TRUE) # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    ######## end check of lib_path

    ######## safer_check argument checking
    if( ! (base::all(safer_check %in% base::c(TRUE, FALSE), na.rm = FALSE) & base::length(x = safer_check) == 1 & base::all(base::is.logical(x = safer_check), na.rm = TRUE))){
        tempo_cat <- base::paste0(
            error_text_start, 
            "safer_check ARGUMENT MUST BE EITHER TRUE OR FALSE.\nHER IT IS:\n", 
            base::paste0(safer_check, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end safer_check argument checking

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
            fun = base::c(
                "saferDev:::.base_op_check", 
                "saferDev::arg_check",
                "saferDev:::.functions_detect", # requires saferDev::arg_check, saferDev:::.extract_all_fun_names, saferDev:::.has_odd_number_of_quotes
                "saferDev:::.in_quotes_replacement", # requires saferDev::arg_check, saferDev:::.has_odd_number_of_quotes
                "saferDev:::.has_odd_number_of_quotes", # from saferDev:::.functions_detect, saferDev:::.in_quotes_replacement
                "saferDev:::.fun_args_pos", 
                "saferDev:::.extract_all_fun_names", # from this function and from saferDev:::.functions_detect
                "saferDev::is_function_here", 
                "saferDev:::.in_parenthesis_replacement", 
                "saferDev:::.all_args_here_fill"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
            internal_error_report_link = internal_error_report_link
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "x"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = FALSE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values

    ######## management of NA arguments
    if(base::length(x = arg_user_setting) != 0){
        tempo_log <- base::suppressWarnings(expr = base::sapply(X = base::lapply(X = arg_user_setting, FUN = function(x){base::is.na(x = x)}), FUN = function(x){base::any(x = x, na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE), classes = "warning") & base::lapply(X = arg_user_setting, FUN = function(x){base::length(x = x)}) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA because base::is.na() used here
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT JUST BE NA:", 
                base::paste0(arg_names[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    ######## management of NULL arguments
    tempo_arg <-base::c(
        "x", 
        "export", 
        "path_out", 
        "df_name", 
        "overwrite", 
        # "lib_path", # because can be NULL
        "safer_check",
        "error_text"
    )
    tempo_log <- base::sapply( X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            "\nCANNOT BE NULL", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- arg_check(data = x, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = lib_path, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- arg_check(data = export, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = lib_path, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- arg_check(data = path_out, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = lib_path, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- arg_check(data = df_name, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = lib_path, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- arg_check(data = overwrite, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = lib_path, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # safer_check already checked above
    # error_text already checked above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <-base::c(
        "path_out", 
        "df_name",
        "lib_path"
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  # for character argument that can also be NULL, if NULL -> considered as character
    if(base::any(tempo_log, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT MODE \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
    ######## end management of "" in arguments of mode character

    #### end argument primary checking

    #### second round of checking and data preparation

    ######## reserved words
    reserved_words <- base::c("NOT_CONSIDERED")
    ######## end reserved words

    ######## new environment
    ######## end new environment

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    if(export == TRUE){
        if( ! base::all(base::dir.exists(paths = path_out), na.rm = TRUE)){
            tempo.cat <- base::paste0(
                error_text_start,  
                "DIRECTORY PATH INDICATED IN THE path_out ARGUMENT DOES NOT EXISTS:\n", 
                base::paste(path_out, sep = " ", collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        # removal of trailing / or \\ in the path
        if(base::grepl(x = path_out, pattern = "/$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            path_out <- base::sub(pattern = "/$", replacement = "", x = path_out, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }else if(base::grepl(x = path_out, pattern = "\\\\$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            path_out <- base::sub(pattern = "\\\\$", replacement = "", x = path_out, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }
        # end removal of trailing / or \\ in the path
        if( ! base::dir.exists(paths = path_out)){
            tempo.cat <- base::paste0(
                error_text_start,
                "DIRECTORY PATH INDICATED IN THE path_out ARGUMENT DOES NOT EXISTS:\n", 
                path_out, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(overwrite == FALSE){
            if(base::file.exists(base::paste0(path_out, "/", df_name, collapse = NULL, recycle0 = FALSE))){
                tempo.cat <- base::paste0(
                    error_text_start,
                    "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n", 
                    base::paste0(path_out, "/", df_name, collapse = NULL, recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code

    ######## warning
    ######## end warning

    # arg_user_setting$x <- base::as.character(arg_user_setting$x)
    arg_user_setting$x <- base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L) # because arg_user_setting$x is str(arg_user_setting$x) "language saferDev::colons_check". When I use it as string, like as.character(arg_user_setting$x), it splits  "::"           "saferDev"     "colons_check"
    path_out <- base::paste0(path_out, "/", df_name, collapse = NULL, recycle0 = FALSE)
    out <- .functions_detect(
        x = x, 
        arg_user_setting = arg_user_setting, 
        error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
        internal_error_report_link = internal_error_report_link
    )
    code_line_nb <- out$code_line_nb # vector of line numbers in code where functions are
    fun_names <-  out$fun_names # list of function names for each line of code
    fun_names_pos <-  out$fun_names_pos # list of pos (1st character) of function names for each line of code

    code <- out$code # vector of strings of the tested function code
    fun_1_line <- base::paste(out$code, collapse = " ", recycle0 = FALSE, sep = " ") # assemble the code of the tested  function (without comments) in a single line
    if(base::grepl(x = fun_1_line, pattern = reserved_words, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
        warn_count <- warn_count + 1
        tempo_warn <- base::paste0("(", warn_count,") THE RESERVED WORD \"", base::paste(reserved_words, collapse = " ", recycle0 = FALSE), "\" HAS BEEN DETECTED IN THE CODE OF THE INPUT FUNCTION\nWHICH COULD HAMPER THE ACCURACY OF THE OUTPUT TABLE", collapse = NULL, recycle0 = FALSE)
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # cumulative nchar of each non empty lines of code 
    cum_nchar_code_line <- base::cumsum(x = base::nchar(x = out$code, type = "chars", allowNA = FALSE, keepNA = NA) + 1) # +1 because of the 1 space added between lines of codes fused # will serve to know the position of first character of fun_names in fun_1_line
    cum_nchar_code_line <- base::c(0, cum_nchar_code_line[-base::length(x = cum_nchar_code_line)]) # because pos in fun_names_pos will be added to this
    cum_nchar_code_line <- cum_nchar_code_line[code_line_nb]
    # end cumulative nchar of each non empty lines of code 
    # replacement of all the ) between quotes
    tempo <- .in_quotes_replacement(
        string = fun_1_line, 
        pattern = "\\)", 
        no_regex_pattern = ")", 
        replacement = " ", 
        perl = TRUE, 
        error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
        internal_error_report_link = internal_error_report_link
    )
    fun_1_line_replace <- tempo$string # code of the tested function that will serve to better detect functions in it
    pos_rep <- tempo$pos # replaced positions in fun_1_line
    # end replacement of all the ) between quotes
    # replacement of all the ( between quotes
    tempo <- .in_quotes_replacement(
        string = fun_1_line_replace, 
        pattern = "\\(", 
        no_regex_pattern = "(", 
        replacement = " ", 
        perl = TRUE, 
        error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
        internal_error_report_link = internal_error_report_link
    )
    fun_1_line_replace <- tempo$string
    pos_rep <- base::sort(x = base::c(pos_rep, tempo$pos), decreasing = FALSE)
    # end replacement of all the ( between quotes
    # recovery of the functions, in the tested function, with written arguments inside ()
    arg_string_for_col3 <- fun_names # like fun_names but added with all what is between ()
    arg_string <- fun_names # like arg_string_for_col3 but with only the arguments
    mid_bracket_pos_in_fun_1_line <- base::lapply(X = fun_names, FUN = function(x){base::lapply(X = x, FUN = function(y){NULL})}) # list of lists, will be used to get inside ( and ) positions, from fun_1_line

    for(i1 in 1:base::length(x = fun_names)){
        tempo_pos_in_code <- base::as.integer(x = base::sub(pattern = "^c", replacement = "", x = base::names(x = fun_names)[i1], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))
        tempo_which <- base::which(x = code_line_nb %in% tempo_pos_in_code, arr.ind = FALSE, useNames = TRUE)
        for(i2 in 1:base::length(x = fun_names[[i1]])){
            fun_pos_start <- fun_names_pos[[i1]][i2] + cum_nchar_code_line[tempo_which]
            fun_pos_stop <- fun_pos_start + base::nchar(x = fun_names[[i1]][i2], type = "chars", allowNA = FALSE, keepNA = NA) - 1
            tempo_fun <- base::substr(x = fun_1_line_replace, start = fun_pos_start, stop = fun_pos_stop)
            if(tempo_fun != fun_names[[i1]][i2]){
                tempo.cat <- base::paste0(
                    "INTERNAL ERROR 1 IN ", 
                    intern_error_text_start,
                    "tempo_fun MUST BE IDENTICAL TO fun_names[[i1]][i2]\n\ntempo_fun: ", 
                    tempo_fun, 
                    "\n\nfun_names[[i1]][i2]: ", 
                    fun_names[[i1]][i2], 
                    "\n\ni1: ", 
                    i1, 
                    "\n\ni2: ", 
                    i2, 
                    intern_error_text_end, 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo_str_after <- base::substr(x = fun_1_line_replace, start = fun_pos_stop + 1, stop = base::nchar(x = fun_1_line_replace, type = "chars", allowNA = FALSE, keepNA = NA))
            if(base::grepl(x = tempo_str_after, pattern = "^[\\s\\r\\n]*\\(", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)){ # detection that it is a function of interest because ( after function name not removed
                tempo_pos <- .fun_args_pos(
                    text = fun_1_line_replace, 
                    pattern = base::paste0(fun_names[[i1]][i2], "[\\s\\r\\n]*\\(", collapse = NULL, recycle0 = FALSE), 
                    error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                    internal_error_report_link = internal_error_report_link) # positions of 1st letter of the function name and opening and closing brackets # Warning: fun_1_line_replace used because the input string must be cleaned form brackets between quotes
                tempo_str_before <- base::substr(x = fun_1_line_replace, start = 1, stop = fun_pos_start - 1)
                tempo_log <- base::grepl(x = tempo_str_before, pattern = "\\$ *$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                if(tempo_log){ # remove functions preceeded by $, like a$fun()
                    arg_string_for_col3[[i1]][i2] <- ""
                    fun_names[[i1]][i2] <- ""
                    arg_string[[i1]][i2] <- ""
                    base::substr(x = fun_1_line_replace, start = 1, stop = fun_pos_start - 1) <- base::paste(base::rep(x = " ", fun_pos_start - 1), collapse = "", recycle0 = FALSE, sep = " ")
                }else{
                    arg_string_for_col3[[i1]][i2] <- base::substr(x = fun_1_line, start = tempo_pos$begin_fun, stop = tempo_pos$end) # add the "function(args)" string into arg_string_for_col3. I use fun_1_line because I want unaltered values of args here (fun_1_line_replace have quoted () replaced by spaces)
                    arg_string[[i1]][i2] <- base::substr(x = fun_1_line, start = tempo_pos$begin + 1, stop = tempo_pos$end - 1) # idem arg_string_for_col3 but inside () of the function (just the arguments written)
                    if( ! base::is.null(x = tempo_pos$middle_bracket_pos)){ # I have to use if(){}, otherwise mid_bracket_pos_in_fun_1_line[[i1]][[i2]] disappears
                        mid_bracket_pos_in_fun_1_line[[i1]][[i2]] <- base::unlist(x = tempo_pos$middle_bracket_pos, recursive = TRUE, use.names = TRUE) # positions of the () inside a function
                    }
                    base::substr(x = fun_1_line_replace, start = 1, stop = tempo_pos$begin - 1) <- base::paste(base::rep(x = " ", tempo_pos$begin - 1), collapse = "", recycle0 = FALSE, sep = " ") # trick that replaces function name by the same number of spaces. This, to avoid to take always the first paste0 for instance in the fun_1_line_replace string when several are present in fun_names
                }
            }else{
                arg_string_for_col3[[i1]][i2] <- reserved_words
                arg_string[[i1]][i2] <- ""
            }
        }
    }
    # end recovery of the functions, in the tested function, with written arguments inside ()
    # recovery of the positions of inside () in col3
    mid_bracket_pos_in_col3 <- base::lapply(X = fun_names, FUN = function(x){base::lapply(X = x, FUN = function(y){NULL})}) # list of lists, will be used to get inside ( and ) positions, from col3
    for(i1 in 1:base::length(x = fun_names)){
        for(i2 in 1:base::length(x = fun_names[[i1]])){
            pattern2 <- base::paste0(fun_names[[i1]][i2], "[\\s\\r\\n]*\\(", collapse = NULL, recycle0 = FALSE) # function detection in 
            # pattern2 <- paste0("[a-zA-Z.][a-zA-Z0-9._]* *\\$ *", fun_names[[i1]][i2], "[\\s\\r\\n]*\\(") # function like a$fun()
            if(base::grepl(x = arg_string_for_col3[[i1]][i2], pattern = pattern2, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # because of "NOT_CONSIDERED" in some cases
                # detection of inside () between quotes
                tempo1 <- .in_quotes_replacement(
                    string = arg_string_for_col3[[i1]][i2], 
                    pattern = "\\(", 
                    no_regex_pattern = "(", 
                    replacement = " ", 
                    perl = TRUE, 
                    error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                    internal_error_report_link = internal_error_report_link
                )
                tempo2 <- .in_quotes_replacement(
                    string =tempo1$string, 
                    pattern = "\\)", 
                    no_regex_pattern = ")", 
                    replacement = " ", 
                    perl = TRUE, 
                    error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                    internal_error_report_link = internal_error_report_link
                )
                tempo_pos <- .fun_args_pos(
                    text = tempo2$string, 
                    pattern = pattern2,     
                    error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                    internal_error_report_link = internal_error_report_link) # positions of 1st letter of the function name and opening and closing brackets # Warning: fun_1_line_replace used because the input string must be cleaned form brackets between quotes
                if( ! base::is.null(x = tempo_pos$middle_bracket_pos)){ # I have to use if(){}, otherwise mid_bracket_pos_in_fun_1_line[[i1]][[i2]] disappears
                    mid_bracket_pos_in_col3[[i1]][[i2]] <- base::unlist(x = tempo_pos$middle_bracket_pos, recursive = TRUE, use.names = TRUE) # positions of the () inside a function
                }
            }
        }
    }
    # end recovery of the positions of inside () in col3
    # preparation of columns
    code_for_col <- code[code_line_nb]
    code_for_col <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = fun_names, y = code_for_col, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any")
    col1 <- base::as.vector(x = base::unlist(x = base::mapply(FUN = function(x, y){base::rep(x = y, base::length(x = x))}, x = fun_names, y = code_line_nb, MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE), recursive = TRUE, use.names = TRUE), mode = "any") # code line number
    col2 <- base::as.vector(x = base::unlist(x = fun_names, recursive = TRUE, use.names = TRUE), mode = "any") # all the function names inside the tested functions (functions between quotes are already removed thanks to fun_1_line_replace)
    col3 <- base::as.vector(x = base::unlist(x = arg_string_for_col3, recursive = TRUE, use.names = TRUE), mode = "any") # as col2 but with its arguments between ()
    col4 <- base::as.vector(x = base::unlist(x = fun_names_pos, recursive = TRUE, use.names = TRUE), mode = "any") # as col2 but position in the code string of 1st character of function name. From col4, we can have pos of opening ( with col4 + nchar(col2) and pos of closing ) with col4 + nchar(col3)
    middle_bracket <- base::do.call(what = base::c, args = mid_bracket_pos_in_col3, quote = FALSE, envir = base::parent.frame(n = 1)) #  concatenate the sublists into a single list -> flatten the outer list while keeping the result as a list
    middle_bracket_open_in_col3 <- base::lapply(X = middle_bracket, FUN = function(x){if( ! base::is.null(x = x)){x[base::seq(1, base::length(x = x), by = 2)]}else{NULL}})
    middle_bracket_close_in_col3 <- base::lapply(X = middle_bracket, FUN = function(x){if( ! base::is.null(x = x)){x[base::seq(2, base::length(x = x), by = 2)]}else{NULL}})
    if( ! (base::length(x = col1) == base::length(x = col2) & base::length(x = col1) == base::length(x = col3) & base::length(x = col1) == base::length(x = col4) & base::length(x = col1) == base::length(x = code_for_col) & base::length(x = col1) == base::length(x = middle_bracket))){
        tempo.cat <- base::paste0(
            "INTERNAL ERROR 2 IN ", 
            intern_error_text_start,
            "LENGTHS OF col1 (", 
            base::length(x = col1), 
            "), col2 (", 
            base::length(x = col2), 
            "), col3 (", 
            base::length(x = col3), 
            "), col4 (", 
            base::length(x = col4), 
            "), code_for_col (", 
            base::length(x = code_for_col), 
            "), AND middle_bracket (", 
            base::length(x = middle_bracket), 
            "), SHOULD BE EQUAL.", 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    tempo.log <- base::as.vector(x = base::unlist(x = base::mapply(
        FUN = function(x, y){
            if(y != ""){
                if(base::grepl(x = y, pattern = base::paste0("^", x, "[\\s\\r\\n]*\\(.*\\)$", collapse = NULL, recycle0 = FALSE), ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) | base::grepl(x = y, pattern = base::paste0("^", reserved_words, "$", collapse = NULL, recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
                    base::return(FALSE)
                }else{
                    base::return(TRUE) # TRUE = problem: does not start by what is expected, i.e., base::paste0("^", x, "[\\s\\r\\n]*\\(.*\\)$"
                }
            }else{
                base::return(FALSE)
            }
        }, 
        x = col2, 
        y = col3,
        MoreArgs = NULL, 
        SIMPLIFY = TRUE, 
        USE.NAMES = TRUE
    ), recursive = TRUE, use.names = TRUE), mode = "any")
    if(base::any(tempo.log, na.rm = TRUE)){
        tempo.cat <- base::paste0("
            INTERNAL ERROR 3 IN ", 
            intern_error_text_start,
            "col3 MUST BE MADE OF STRINGS STARTING BY\n\"<FUNCTION_NAME>[\\s\\r\\n]*\\(\"\nAND FINISHING BY\")\"\nHERE IT IS:\n\n", 
            base::paste0(col3, collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
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
    if( (base::length(x = col1) == 0)){
        base::cat("\n\nEVERYTHING SEEMS CLEAN\n\n", file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
    }else{
        col5 <- NULL # all arguments of the function with default value
        col6 <- NULL # missing arg names
        col7 <- NULL # potential missing args with values
        col8 <- NULL # reconstructed function with all arg
        for(i2 in 1:base::length(x = col1)){
            if(col3[i2] != reserved_words){
                # check if the function exists
                tempo_cat_fun_exists <- base::paste0(
                    error_text_start, 
                    "CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE ", 
                    col1[i2], 
                    ":\n\n", 
                    base::paste0(base::paste0(base::substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1), col3[i2], collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE), 
                    "\n\nPLEASE, RUN saferDev::colons_check(", arg_user_setting$x, ") FIRST,\nADD THE MISSING <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> FOR FUNCTION STARTING BY A DOT)\nAND RERUN saferDev::all_args_here(", arg_user_setting$x, 
                    ").", 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                if(col4[i2] <= 3){
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat_fun_exists, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
                tempo_string <- base::substr(x = code_for_col[i2], start = col4[i2] - 2, stop = col4[i2] - 1)
                if(tempo_string != "::"){
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat_fun_exists, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
                tempo_string <- base::substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1)
                tempo_package_name_colons <- .extract_all_fun_names(
                        text = tempo_string, 
                        pattern = "[a-zA-Z][a-zA-Z0-9.]*:{2,3}$",
                        error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
                        internal_error_report_link = internal_error_report_link
                    )$string # before 
                is_function_here(
                    fun = base::paste0(tempo_package_name_colons, col2[i2], collapse = NULL, recycle0 = FALSE), 
                    lib_path = lib_path, 
                    safer_check = FALSE, 
                    error_text = paste0("INSIDE saferDev::is_function_here() INSIDE ", function_name, " OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)
                ) # check that exists
                tempo_package_name <- base::sub(pattern =  ":+$", replacement = "", x = tempo_package_name_colons, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                # end check if the function exists
                # recovering default args of the function
                tempo_log <- base::exists(x = col2[i2], where = -1, envir = base::getNamespace(name = tempo_package_name), frame = NULL, mode = "any", inherits = FALSE)
                if(tempo_log){
                    tempo_fun2 <- base::get(x = col2[i2], envir = base::asNamespace(ns = tempo_package_name, base.OK = TRUE), mode = "function", inherits = FALSE, pos = -1L)
                }else{
                    tempo.cat <- base::paste0(
                        error_text_start, 
                        "CANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT CORRECTLY ASSOCIATED TO ITS PACKAGE IN LINE ", 
                        col1[i2], 
                        ":\n\n", 
                        base::paste0(base::paste0(base::substr(x = code_for_col[i2], start = 1, stop = col4[i2] - 1), col3[i2], collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE), 
                        "\n\nPLEASE, RUN utils::getAnywhere('", 
                        col2[i2], 
                        "')$where FIRST,\nCORRECT THE LINE ", 
                        col1[i2], 
                        " IN ", 
                        arg_user_setting$x, 
                        ",\nSAVE AND REIMPORT ", 
                        arg_user_setting$x, 
                        "\nAND RERUN saferDev::all_args_here(", 
                        arg_user_setting$x, 
                        ").", 
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
                if(base::is.primitive(x = tempo_fun2)){
                    tempo_all_args <- base::as.list(x = base::suppressWarnings(expr = base::formals(fun = base::args(name = col2[i2]), envir = base::asNamespace(ns = tempo_package_name, base.OK = TRUE)), classes = "warning"))
                    if(base::all(base::typeof(x = tempo_fun2) %in% base::c("special", "symbol"), na.rm = TRUE)){
                        if(base::length(x = tempo_all_args) == 0){
                            arg_full <- NULL # all args of the function with default values
                        }else{
                            arg_full <- tempo_all_args # convert pairlist into list
                        }
                    }else{
                        arg_full <- tempo_all_args
                    }
                }else{
                    arg_full <- base::as.list(x = base::formals(fun = col2[i2], envir = base::asNamespace(ns = tempo_package_name, base.OK = TRUE))) # all the argument of the function in col2[i2] with default values # convert pairlist into list
                }
                # end recovering default args of the function
                if(base::is.null(x = arg_full)){
                    col5 <- base::c(col5, "NO_ARGS")
                    col6 <- base::c(col6, "")
                    col7 <- base::c(col7, "")
                    col8 <- base::c(col8, "")
                }else{
                    # all arguments of the function with default value in col5
                    tempo <- base::sapply(X = arg_full, FUN = function(x){base::paste0(base::ifelse(test = base::all(base::typeof(x = x) == "symbol", na.rm =TRUE), yes = "", no = " = "), base::deparse(expr = x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = NULL, recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE)
                    tempo <- base::paste(base::paste0(base::names(x = tempo), tempo, collapse = NULL, recycle0 = FALSE), collapse = ", ", recycle0 = FALSE, sep = " ")
                    col5 <- base::c(col5, tempo)
                    # end all arguments of the function with default value in col5
                    # arguments: replacement of all the commas between quotes
                    tempo_col3 <- col3[i2]
                    tempo <- .in_quotes_replacement(
                        string = tempo_col3, 
                        pattern = ",", 
                        no_regex_pattern = ",", 
                        replacement = " ", 
                        perl = TRUE, 
                        error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                        internal_error_report_link = internal_error_report_link
                    )
                    tempo_col3 <- tempo$string
                    pos_rep2 <- tempo$pos # replaced positions in obs_args
                    # end arguments: replacement of all the commas between quotes
                    # arguments: replacement of all the commas inside () of subfunctions
                    if(base::length(x = middle_bracket_open_in_col3[[i2]]) > 0){
                        if(base::length(x = middle_bracket_open_in_col3[[i2]]) != base::length(x = middle_bracket_close_in_col3[[i2]])){
                            tempo.cat <- base::paste0(
                                "INTERNAL ERROR 4 IN ", 
                                intern_error_text_start, 
                                "middle_bracket_open_in_col3 AND middle_bracket_close_in_col3 MUST HAVE THE SAME LENGTH IN LOOP ", 
                                i2, 
                                "\n\nmiddle_bracket_open_in_col3 (", 
                                base::length(x = middle_bracket_open_in_col3), 
                                "):\n", 
                                base::paste0(middle_bracket_open_in_col3, collapse = " ", recycle0 = FALSE), 
                                "\n\nmiddle_bracket_close_in_col3 (", 
                                base::length(x = middle_bracket_close_in_col3), 
                                "):\n", 
                                base::paste0(middle_bracket_close_in_col3, collapse = " ", recycle0 = FALSE), 
                                "\n\ni2:\n", 
                                i2, 
                                intern_error_text_end, 
                                collapse = NULL, 
                                recycle0 = FALSE
                            )
                            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                        }
                        for(i6 in 1:base::length(x = middle_bracket_open_in_col3[[i2]])){
                            tempo <- .in_parenthesis_replacement(
                                string = tempo_col3, 
                                pattern = ",", 
                                no_regex_pattern = ",", 
                                replacement = " ", 
                                perl = TRUE, 
                                open_pos = middle_bracket_open_in_col3[[i2]][i6], 
                                close_pos = middle_bracket_close_in_col3[[i2]][i6], 
                                error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                                internal_error_report_link = internal_error_report_link)
                            tempo_col3 <- tempo$string
                            pos_rep2 <- base::c(pos_rep2, tempo$pos) # replaced positions in obs_args
                        }
                    }
                    # end arguments: replacement of all the commas inside () of subfunctions
                    # recovering obs arguments
                    obs_args <- base::sub(pattern =  base::paste0("^", col2[i2], "[\\s\\r\\n]*\\(", collapse = NULL, recycle0 = FALSE), replacement = "", x = tempo_col3, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) # removal of function name and (
                    obs_args <- base::sub(pattern =  "\\)$", replacement = "", x = obs_args, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # removal of trailing )
                    # end recovering obs arguments
                    # splitting the arguments using commas
                    tempo_split <- base::strsplit(x = obs_args, split = ",", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]] # separation of args
                    # tempo_split <- gsub(pattern = "^[\\s;]+", replacement = "", x = tempo_split) # removing leading ; and space, ; because of line: fun_1_line <- base::paste(out$code, collapse = ";")
                    # tempo_split <- gsub(pattern = "[\\s;]+$", replacement = "", x = tempo_split) # removing trailing ; and space, ; because of line: fun_1_line <- base::paste(out$code, collapse = ";")
                    # end splitting the arguments using commas
                    # rewriting the commas inside args
                    if( ! base::is.null(x = pos_rep2)){
                        # resseting the pos of the removed commas to fit obs_args
                        pos_rep2 <- pos_rep2 - base::nchar(x = col2[i2], type = "chars", allowNA = FALSE, keepNA = NA) - 1 # -1 for the opening (
                        if(base::any(pos_rep2 <= 0, na.rm = TRUE)){
                            tempo.cat <- base::paste0(
                                "INTERNAL ERROR 5 IN ", 
                                intern_error_text_start,
                                "POSITIONS OF REMOVED COMMAS CANOT  OR LESS\n\npos_rep2 (", 
                                base::length(x = pos_rep2), 
                                "):\n", 
                                base::paste0(pos_rep2, collapse = " ", recycle0 = FALSE), 
                                "\n\nARGUMENT STRING obs_args:\n", 
                                base::paste0(obs_args, collapse = " ", recycle0 = FALSE), 
                                "\n\ni2:\n", 
                                i2, 
                                intern_error_text_end, 
                                collapse = NULL, 
                                recycle0 = FALSE
                            )
                            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                        }
                        # end resseting the pos of the removed commas to fit obs_args
                        for(i6 in 1:base::length(x = tempo_split)){
                            tempo_log <- pos_rep2 >= 1 & pos_rep2 <= base::nchar(x = tempo_split[i6], type = "chars", allowNA = FALSE, keepNA = NA)
                            if(base::any(base::is.na(x = tempo_log), na.rm = TRUE)){
                                tempo.cat <- base::paste0(
                                    "INTERNAL ERROR 6 IN ", 
                                    intern_error_text_start, 
                                    "tempo_log CONTAINS NA:\n", 
                                    base::paste0(tempo_log, collapse = " ", recycle0 = FALSE), 
                                    intern_error_text_end, 
                                    collapse = NULL, 
                                    recycle0 = FALSE
                                )
                                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                            }
                            if(base::any(tempo_log, na.rm = TRUE)){
                                for(i7 in 1:base::length(x = tempo_log)){
                                    if(tempo_log[i7]){
                                        base::substr(x = tempo_split[i6], start = pos_rep2[i7], stop = pos_rep2[i7]) <- ","
                                    }
                                }
                            }
                            pos_rep2 <- pos_rep2 - base::nchar(x = tempo_split[i6], type = "chars", allowNA = FALSE, keepNA = NA) - 1 # -1 because of the comma that separates each tempo_split
                        }
                    }
                    # end rewriting the commas inside args
                    # working on each observed arg
                    arg_full_names <-  base::names(x = arg_full)
                    three_dots_log <- arg_full_names == "..."
                    # checking 
                    if(base::length(x = tempo_split) > base::length(x = arg_full_names) & ! base::any(three_dots_log, na.rm = TRUE)){
                        tempo.cat <- base::paste0(
                            "INTERNAL ERROR 7 IN ", 
                            intern_error_text_start, 
                            "LENGTH OF tempo_split MUST LOWER OR EQUAL TO LENGTH OF arg_full_names IF ... IS NOT AN ARGUMENT OF THE FUNCTION\n\nFUNCTION: ", 
                            col2[i2], 
                            "\n\ntempo_split (", 
                            base::length(x = tempo_split), 
                            "):\n", 
                            base::paste0(tempo_split, collapse = "\n", recycle0 = FALSE), 
                            "\n\narg_full_names (", 
                            base::length(x = arg_full_names), 
                            "):\n", 
                            base::paste0(arg_full_names, collapse = "\n", , recycle0 = FALSE), 
                            "\n\ni2:\n", 
                            i2, 
                            intern_error_text_end, 
                            collapse = NULL, 
                            recycle0 = FALSE
                        )
                        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }
                    # end checking
                    tempo_out <- .all_args_here_fill(
                        arg_full = arg_full, 
                        arg_full_names = arg_full_names, 
                        tempo_split = tempo_split, 
                        three_dots_log = three_dots_log, 
                        i2 = i2, 
                        col1_i2 = col1[i2],
                        col2_i2 = col2[i2],
                        arg_user_setting_x = arg_user_setting$x, 
                        error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
                        internal_error_report_link = internal_error_report_link, 
                        warn = warn,
                        warn_count = warn_count
                    )
                    col6 <- base::c(col6, tempo_out$col6)
                    col7 <- base::c(col7, tempo_out$col7)
                    col8 <- base::c(col8, tempo_out$col8)
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
    }
    # end two new columns for arg proposal
    #### end main code

    #### output
    if(base::all(col8 %in%base::c("", "GOOD"), na.rm = FALSE)){
        tempo.cat <- base::paste0("INSIDE ", base::as.character(x = out$arg_user_setting$x), "(), EVERYTHING SEEMS CLEAN.", collapse = NULL, recycle0 = FALSE)
        if(export == TRUE){
            tempo.cat <- base::paste0("RESULT EXPORTED IN:\n", path_out, "\nBUT ", tempo.cat, collapse = NULL, recycle0 = FALSE)
        }
        tempo.cat <- base::paste0("AFTER RUNNING ", function_name, " OF THE ", package_name, " PACKAGE:\n", tempo.cat, collapse = NULL, recycle0 = FALSE)
        base::on.exit(expr = base::cat(base::paste0("\n\n", tempo.cat, "\n\n", collapse = NULL, recycle0 = FALSE), file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE), add = TRUE, after = TRUE)
    }
    output <- base::data.frame(LINE_NB = col1, FUN_NAME = col2, FUN_ARGS = col3, FUN_POS = col4, DEF_ARGS = col5, MISSING_ARG_NAMES = col6, MISSING_ARGS = col7, STATUS = col8, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    if(export == TRUE){
        utils::write.table(x = output, file = path_out, row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "")
    }else{
        base::return(output)
    }
    #### end output

    #### warning output
    if( ! base::is.null(x = warn)){
        base::on.exit(expr = base::warning(base::paste0(
            base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
            warn, 
            collapse = NULL, 
            recycle0 = FALSE
        ), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL), add = FALSE, after = TRUE)
    }
        base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
    #### end warning output
}
