#' @title is_function_here
#' @description
#' Check if required functions are present in installed packages. This controls modifications in of function names package versions.
#' @param fun Character vector of the names of the required functions, preceded by the name of the package they belong to and a double or triple colon. Example: c("ggplot2::geom_point", "grid::gpar"). Warning: do not write "()" at the end of the function
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful to overcome R execution using system with non admin rights for R package installation in the default directories. Ignored if NULL (default): only the pathways specified by .libPaths() are used for package calling. Specify the right path if the function returns a package path error.
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>".
#' @returns  An error message if at least one of the checked packages is missing in lib_path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
#' @seealso \code{\link{exists}} et \code{\link[methods]{findFunction}}
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' \dontrun{ # Example that return an error
#' is_function_here(fun = "ggplot2::notgood") # commented because this example returns an error
#' is_function_here(fun = c("ggplot2::geom_point", "grid::gpar"))
#' is_function_here(fun = "c")
#' }
#' is_function_here(fun = "base::c")
#' @export


is_function_here <- function(
    fun, 
    lib_path = NULL,
    safer_check = TRUE,
    error_text = ""
){
    # DEBUGGING
    # fun = "ggplot2::geom_point" ; lib_path = "C:/Program Files/R/R-4.3.1/library" ; safer_check = TRUE
    # fun = "saferDev:::.colons_check_message" ; lib_path = "C:/Program Files/R/R-4.3.1/library" ; safer_check = TRUE

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
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
            internal_error_report_link = internal_error_report_link
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "fun"
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

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = fun, class = "vector", typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, lib_path = lib_path, safer_check = FALSE, error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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

    # management of NULL arguments
    tempo_arg <-base::c(
        "fun",
        # "lib_path", # inactivated because can be NULL
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

    ######## management of "" in arguments of mode character
    tempo_arg <-base::c(
        "fun",
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
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation
    
    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    tempo.log <- base::grepl(x = fun, pattern = "^[a-zA-Z][a-zA-Z0-9.]*(:{2}[a-zA-Z]|:{3}\\.[a-zA-Z._])[a-zA-Z0-9._]*$")
    # [a-zA-Z][a-zA-Z0-9.]+ means any single alphabet character (package name cannot start by dot or underscore or num), then any alphanum and dots
    # (:{2}[a-zA-Z]|:{3}\\.[a-zA-Z._]) means either double colon and any single alphabet character or triple colon followed by a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
    # [a-zA-Z0-9._]* any several of these characters or nothing
    if( ! base::all(tempo.log)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\" OR \":::.\":\n", base::paste(fun[ ! tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    tempo.log <- base::grepl(x = fun, pattern = "^.+\\(\\)$")
    if(base::any(tempo.log)){
        tempo.cat <- base::paste0("ERROR IN ", function_name, " OF THE ", package_name, " PACKAGE\nTHE STRING IN fun ARGUMENT MUST NOT FINISH BY \"()\":\n", base::paste(fun[tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    pkg.fun.name.list <- base::strsplit(x = fun, split = ":{2,3}") # package in 1 and function in 2
    pkg.name <- base::sapply(X = pkg.fun.name.list, FUN = function(x){x[1]})
    pkg.log <- pkg.name %in% base::rownames(utils::installed.packages(lib.loc = lib_path))
    if( ! base::all(pkg.log)){
        tempo <- pkg.name[ ! pkg.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            " OF THE ",
            package_name,
            " PACKAGE",
            "\nREQUIRED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n"))), 
            "MUST BE INSTALLED IN", 
            base::ifelse(base::length(lib_path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            base::paste(lib_path, collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    fun.log <- base::sapply(X = pkg.fun.name.list, FUN = function(x){base::exists(x[2], envir = base::asNamespace(x[1]))})
    if( ! base::all(fun.log)){
        tempo <- fun[ ! fun.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            " OF THE ",
            package_name,
            " PACKAGE",
            "\nREQUIRED FUNCTION",
            base::ifelse(base::length(tempo) == 1L, " IS ", "S ARE "), 
            "MISSING IN THE INSTALLED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n"))),
            "\n\nIN", 
            base::ifelse(base::length(lib_path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            base::paste(lib_path, collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    #### end main code

    #### output
    #### end warning output

    #### warning output
    #### end warning output
}
