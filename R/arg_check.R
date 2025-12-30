#' @title arg_check
#' @description
#' Check expected values of an argument of functions: class, type, mode, length, restricted values panel, kind of numeric values in addition to the distinction between 'integer' and 'double' (proportion only? Inf values authorized? negative values authorized? Integers of type 'double'?)
#' @param data Object to test.
#' @param class Single character string. Either one of the class() result or "vector" or "ggplot2" (i.e., objects of class c("gg", "ggplot")) or NULL. See the warning section below.
#' @param typeof Single character string. Either one of the typeof() result or NULL.
#' @param mode Single character string. Either one of the mode() result (for non-vector object) or NULL.
#' @param length Single numeric value indicating the length of the object. Not considered if NULL.
#' @param prop Single logical value. Are the numeric values between 0 and 1 (proportion)? If TRUE, can be used alone, without considering class, etc.
#' @param double_as_integer_allowed Single logical value. If TRUE, no error is reported in the cheking message if argument is set to typeof == "integer" or class == "integer", while the reality is typeof == "double" or class == "numeric" but the numbers strictly have zero as modulo (remainder of a division). This means that i <- 1, which is typeof(i) -> "double" is considered as integer with double_as_integer_allowed = TRUE. WARNING: data mod 1 == 0L but not isTRUE(all.equal(data mod 1, 0)) is used here because the argument checks for integers stored as double (does not check for decimal numbers that are approximate integers).
#' @param options Vector of character strings or integers indicating all the possible option values for the data argument, or NULL. Numbers of type "double" are accepted if they have a 0 modulo.
#' @param all_options_in_data Single logical value. If TRUE, all of the options must be present at least once in the data argument, and nothing else. If FALSE, some or all of the options must be present in the data argument, and nothing else. Ignored if options base::is.null.
#' @param na_contain Single logical value. Can the data argument contain NA?
#' @param neg_values Single logical value. Are negative numeric values authorized? Warning: the default setting is TRUE, meaning that, in that case, no check is performed for the presence of negative values. The neg_values argument is activated only when set to FALSE. In addition, (1) neg_values = FALSE can only be used when class, typeof or mode arguments are not NULL, otherwise return an error message, (2) the presence of negative values is not checked with neg_values = FALSE if the tested object is a factor and the following checking message is returned "OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR".
#' @param inf_values Single logical value. Are infinite numeric values authorized (Inf or -Inf)? Identical remarks as for the neg_values argument.
#' @param print Single logical value. Print the message if $problem is TRUE? Warning: set by default to FALSE, which facilitates the control of the checking message output when using arg_check() inside functions. See the example section.
#' @param data_name Single character string indicating the name of the object to test. If NULL, use what is assigned to the data argument for the returned message.
#' @param data_arg Single logical value. Is the tested object a function argument? If TRUE (default), "ARGUMENT" is written in messages, otherwise "OBJECT".
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) correct lib_path argument value 2) required functions and related packages effectively present in local R lybraries and 3) R classical operators (like "<-") not overwritten by another package because of the R scope. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "". Of note, in arg_check(), error_text is also used at the end of the string returned when no problem is detected.
#' @returns 
#' A list containing:
#' 
#'- §problem: logical. Is there any problem detected?
#' 
#'- §text: message indicating the details of the problem, or the absence of problem.
#' 
#' - §object.name: value of the data_name argument (i.e., name of the checked object if provided, NULL otherwise).
#' @details
#' If options == NULL, then at least class or type or mode or length argument must be non-null.
#'  
#' If options is non-null, then class, type and mode must be NULL, and length can be NULL or specified.
#'  
#' The function tests what is written in its arguments, even if what is written is incoherent. For instance, arg_check(data = factor(1), class = "factor", mode = "character") will return a problem, whatever the object tested in the data argument, because no object can be class "factor" and mode "character" (factors are class "factor" and mode "numeric"). Of note, length of object of class "environment" is always 0.
#'  
#' If the tested object base::is.null, then the function will always return a checking problem.
#'  
#' Argument "class" with value "vector" means that the object is tested for class(data) returning only "numeric", "integer", "character", "logical", "complex" or "expression". Please, use another value of class (e.g., class = "call" or class = "list") for other types and class of objects
#'  
#' Since R >= 4.0.0, class(matrix()) returns "matrix" "array", and not "matrix" alone as before. However, use argument class = "matrix" to check for matrix object (of class "matrix" "array" in R >= 4.0.0) and use argument class = "array" to check for array object (of class "array" in R >= 4.0.0).
#' @seealso \code{\link{match.arg}} et \code{\link{arg_test}}.
#' @author \href{gael.millot@pasteur.fr}{Gael Millot}
#' @author \href{yushi.han2000@gmail.com}{Yushi Han}
#' @author \href{wanghaiding442@gmail.com}{Haiding Wang}
#' @examples
#' test <- matrix(1:3)
#' \dontrun{ # Example that return an error
#' arg_check(data = test, print = TRUE, class = "vector", mode = "numeric")  # error: THE test ARGUMENT MUST BE CLASS vector
#' }
#' arg_check(data = test, print = TRUE, class = "matrix", mode = "numeric")
#' arg_check(data = test, print = TRUE, class = "matrix", mode = "numeric", error_text = " using saferDev::arg_check()")
#' 
#' @export
arg_check <- function(
    data, 
    class = NULL, 
    typeof = NULL, 
    mode = NULL, 
    length = NULL, 
    prop = FALSE, 
    double_as_integer_allowed = FALSE, 
    options = NULL, 
    all_options_in_data = FALSE, 
    na_contain = FALSE, 
    neg_values = TRUE, 
    inf_values = TRUE, 
    print = FALSE, 
    data_name = NULL, 
    data_arg = TRUE, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # data = mean ; class = NULL ; typeof = NULL ; mode = NULL ; length = NULL ; prop = FALSE ; double_as_integer_allowed = FALSE ; options = "a" ; all_options_in_data = FALSE ; na_contain = FALSE ; neg_values = TRUE ; inf_values = TRUE ; print = TRUE ; data_name = NULL ; data_arg = TRUE ; lib_path = NULL ; safer_check = TRUE ; error_text = " IN P1::F1."
    # vec1 <- -1:3 ; mat3 <- base::matrix(letters[1:5]) ; data = vec1 ; class = "numeric" ; typeof = mat3 ; mode = NULL ; length = NULL ; prop = FALSE ; double_as_integer_allowed = FALSE ; options = NULL ; all_options_in_data = FALSE ; na_contain = FALSE ; neg_values = TRUE ; inf_values = TRUE ; print = TRUE ; data_name = NULL ; data_arg = TRUE ; lib_path = NULL ; safer_check = TRUE ; error_text = " IN P1::F1."

    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name,
        collapse = NULL, 
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    # not required
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present below 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "data"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <- base::c(
        # "data", # inactivated because can be NULL
        # "class", # inactivated because can be NULL
        # "typeof", # inactivated because can be NULL 
        # "mode", # inactivated because can be NULL
        # "length", # inactivated because can be NULL
        "prop", 
        "double_as_integer_allowed", 
        # "options", # inactivated because can be NULL
        "all_options_in_data", 
        "na_contain", 
        "neg_values", 
        "inf_values", 
        "print", 
        # "data_name", # inactivated because can be NULL
        "data_arg", 
        "safer_check"
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        # "data", # inactivated because can be empty
        "class", 
        "typeof", 
        "mode", 
        "length",
        "prop", 
        "double_as_integer_allowed", 
        "options", 
        "all_options_in_data", 
        "na_contain", 
        "neg_values", 
        "inf_values", 
        "print", 
        "data_name",
        "data_arg", 
        "safer_check", 
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval, 
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval, 
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ), 
                FUN = function(x){
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if(safer_check == TRUE){
        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
            if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
                if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                    lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
                }
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                    base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
                tempo_log <- ! base::dir.exists(paths = lib_path)
                tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
                tempo_cat_b[tempo_cat_b == ""] <- "\"\""
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH",
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""), 
                    " INDICATED IN THE lib_path ARGUMENT DO", 
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"), 
                    " NOT EXIST:\n", 
                    base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                ini_lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base:::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "saferDev:::.base_op_check"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    # this part is specifically changed for arg_check because arg not checked with arg_check above. See below where exactly
    tempo_arg <-base::c(
        "class",
        "typeof",
        "mode",
        "data_name"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start,
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " MUST BE MADE OF CHARACTER STRINGS:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
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
    
    #### end argument secondary checking
    
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

    # management of special classes
    basic.class <- base::c(
        "NULL", # must be added here even if already dealt above
        "logical", 
        "integer", 
        "numeric", 
        # "complex", 
        "character"
        # "matrix", 
        # "array", 
        # "data.frame", 
        # "list", 
        # "factor", 
        # "table", 
        # "expression", 
        # "name", 
        # "symbol", 
        # "function", 
        # "uneval", 
        # "environment", 
        # "ggplot2", 
        # "ggplot_built", 
        # "call"
    )
    tempo.arg.base <- base::c( # no base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) used with arg_check() to be sure to deal with the correct environment
        "class", 
        "typeof", 
        "mode", 
        "length", 
        "prop", 
        "double_as_integer_allowed", 
        "options", 
        "all_options_in_data", 
        "na_contain", 
        "neg_values", 
        "inf_values", 
        "print", 
        "data_name",
        "data_arg"
        # "safer_check", # already checked above
        # "lib_path", # already checked above
        # "error_text" # already checked above
    )
    tempo.class <- base::list( # no base::get() used to be sure to deal with the correct environment
        base::class(x = class), 
        base::class(x = typeof), 
        base::class(x = mode), 
        base::class(x = length), 
        base::class(x = prop), 
        base::class(x = double_as_integer_allowed), 
        base::class(x = options), 
        base::class(x = all_options_in_data), 
        base::class(x = na_contain), 
        base::class(x = neg_values), 
        base::class(x = inf_values), 
        base::class(x = print), 
        base::class(x = data_name),
        base::class(x = data_arg)
    )
    tempo <- ! base::sapply(X = base::lapply(X = tempo.class, FUN = function(x){x %in% basic.class}), FUN = function(x){base::all(x, na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE)
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat1 <- tempo.arg.base[tempo]
        tempo_cat2 <- base::sapply(X = tempo.class[tempo], FUN = function(x){base::paste0(x, collapse = " ", recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE)
        tempo.sep <- base::sapply(X = base::mapply(x = " ", y = base::max(base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA), na.rm = TRUE) - base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA) + 3, FUN = function(x, y){base::rep(x = x, times = y)}, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE), FUN = function(x){base::paste0(x, collapse = "", recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE)
        tempo_cat <- base::paste0(
            error_text_start, 
            "ANY ARGUMENT EXCEPT data MUST BE CLASS \"logical\", \"integer\", \"numeric\", \"character\" OR NULL.\nPROBLEMATIC ARGUMENT", 
            base::ifelse(test = base::length(x = tempo_cat1) > 1, yes = "S", no = ""), 
            " AND ASSOCIATED CLASS", 
            base::ifelse(test = base::length(x = tempo_cat1) > 1, yes = "ES ARE", no = " IS"), 
            ":\n", 
            base::paste0(tempo_cat1, tempo.sep, tempo_cat2, collapse = "\n", recycle0 = FALSE),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # end management of special classes
    # management of the logical arguments
    log_args <- base::c(
        "prop", 
        "double_as_integer_allowed", 
        "all_options_in_data", 
        "na_contain", 
        "neg_values", 
        "inf_values", 
        "print", 
        "data_arg"
        # "safer_check", # already checked above
    )
    tempo_class <- base::list( # no base::get() used to be sure to deal with the correct environment 
        base::class(x = prop), 
        base::class(x = double_as_integer_allowed), 
        base::class(x = all_options_in_data), 
        base::class(x = na_contain), 
        base::class(x = neg_values), 
        base::class(x = inf_values), 
        base::class(x = print), 
        base::class(x = data_arg)
    )
    tempo_length <- base::c( # no base::get() used to be sure to deal with the correct environment 
        base::length(x = prop), 
        base::length(x = double_as_integer_allowed), 
        base::length(x = all_options_in_data), 
        base::length(x = na_contain), 
        base::length(x = neg_values), 
        base::length(x = inf_values), 
        base::length(x = print), 
        base::length(x = data_arg)
    )
    tempo_log1 <- base::sapply(X = tempo_class, FUN = function(x){base::all(x == "logical", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE)
    tempo_log2 <- tempo_length == 1
    tempo_log <- ! (tempo_log1 & tempo_log2)
    if(base::any(tempo_log, na.rm = TRUE)){ 
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE\n", 
            base::paste0(log_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            "\nARGUMENT",
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""),
            " MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # end management of the logical arguments
    # other checkings of the arguments by order
    if( ! base::is.null(x = data_name)){
        if( ! (base::length(x = data_name) == 1L & base::all(base::class(x = data_name) == "character", na.rm = TRUE))){ # base::all() without na.rm -> ok because base::class(NA) is "logical"
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE data_name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT:\n", 
                base::paste0(data_name, collapse = "\n", recycle0 = FALSE),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(base::is.null(x = options) & base::is.null(x = class) & base::is.null(x = typeof) & base::is.null(x = mode) &  base::all(prop == FALSE, na.rm = TRUE) & base::is.null(x = length)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "AT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop).",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if( ! base::is.null(x = options) & ( ! base::is.null(x = class) | ! base::is.null(x = typeof) | ! base::is.null(x = mode) | base::all(prop == TRUE, na.rm = TRUE))){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED.",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(neg_values == FALSE & base::is.null(x = class) & base::is.null(x = typeof) & base::is.null(x = mode)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(inf_values == FALSE & base::is.null(x = class) & base::is.null(x = typeof) & base::is.null(x = mode)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL.",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if( ! base::is.null(x = class)){ # may add "formula" and "Date" as in https://renenyffenegger.ch/notes/development/languages/R/functions/class
        if( ! base::all(class %in% base::c("vector", "logical", "integer", "numeric", "complex", "character", "matrix", "array", "data.frame", "list", "factor", "table", "expression", "name", "symbol", "function", "uneval", "environment", "ggplot2", "ggplot_built", "call") & base::length(x = class) == 1L, na.rm = TRUE)){ # length == 1L here because of base::class(base::matrix()) since R4.0.0  # base::all() without na.rm -> ok because class cannot be NA (tested above)
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE class ARGUMENT MUST BE ONE OF THESE VALUE:\n\"vector\"\n\"logical\"\n\"integer\"\n\"numeric\"\n\"complex\"\n\"character\"\n\"matrix\"\n\"array\"\n\"data.frame\"\n\"list\"\n\"factor\"\n\"table\"\n\"expression\"\n\"name\"\n\"symbol\"\n\"function\"\n\"environment\"\n\"ggplot2\"\n\"ggplot_built\"\n\"call\"",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(neg_values == FALSE & ! base::any(class %in% base::c("vector", "numeric", "integer", "matrix", "array", "data.frame", "table"), na.rm = TRUE)){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE class ARGUMENT CANNOT BE OTHER THAN\n\"vector\"\n\"numeric\"\n\"integer\"\n\"matrix\"\n\"array\"\n\"data.frame\"\n\"table\"\nIF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(inf_values == FALSE & ! base::any(class %in% base::c("vector", "numeric", "matrix", "array", "data.frame", "table"), na.rm = TRUE)){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE class ARGUMENT CANNOT BE OTHER THAN\n\"vector\"\n\"numeric\"\n\"matrix\"\n\"array\"\n\"data.frame\"\n\"table\"\nIF ABSENCE OF INFINITE VALUE IS CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.\n\"integer\" IS NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE NOT INTEGERS.", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if( ! base::is.null(x = typeof)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (base::all(typeof %in% base::c("logical", "integer", "double", "complex", "character", "list", "expression", "symbol", "closure", "special", "builtin", "environment", "S4", "language"), na.rm = TRUE) & base::length(x = typeof) == 1L)){ # "language" is the type of object of class "call" # base::all() without na.rm -> ok because typeof cannot be NA (tested above)
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE typeof ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\"\n\"integer\"\n\"double\"\n\"complex\"\n\"character\"\n\"list\"\n\"expression\"\n\"name\"\n\"symbol\"\n\"closure\"\n\"special\"\n\"builtin\"\n\"environment\"\n\"S4\"\n\"language\"",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(neg_values == FALSE & ! typeof %in% base::c("double", "integer")){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE typeof ARGUMENT CANNOT BE OTHER THAN \"double\" OR \"integer\" IF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(inf_values == FALSE & typeof != "double"){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE typeof ARGUMENT CANNOT BE OTHER THAN \"double\" IF ABSENCE OF INFINITE VALUE ARE CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.\n\"integer\" IS NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE NOT INTEGERS.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if( ! base::is.null(x = mode)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (base::all(mode %in% base::c("logical", "numeric", "complex", "character", "list", "expression", "name", "symbol", "function", "environment", "S4", "call"), na.rm = TRUE) & base::length(x = mode) == 1L)){ # base::all() without na.rm -> ok because mode cannot be NA (tested above)
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE mode ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\"\n\"numeric\"\n\"complex\"\n\"character\"\n\"list\"\n\"expression\"\n\"name\"\n\"symbol\"\n\"function\"\n\"environment\"\n\"S4\"\n\"call\"",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(neg_values == FALSE & mode != "numeric"){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(inf_values == FALSE & mode != "numeric"){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF ABSENCE OF INFINITE VALUE ARE CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.\nOTHER VALUES ARE NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE ONLY \"numeric\".",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if( ! base::is.null(x = length)){
        if( ! (base::is.numeric(x = length) & base::length(x = length) == 1L & base::all( ! base::grepl(x = length, pattern = "\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = TRUE))){ # base::is.na() already arg_checked for length
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE length ARGUMENT MUST BE A SINGLE INTEGER VALUE.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(prop == TRUE){
        if( ! base::is.null(x = class)){
            if( ! base::any(class %in% base::c("vector", "numeric", "matrix", "array", "data.frame", "table"), na.rm = TRUE)){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE class ARGUMENT CANNOT BE OTHER THAN\nNULL\n\"vector\"\n\"numeric\"\n\"matrix\"\n\"array\"\n\"data.frame\"\n\"table\"\nIF prop ARGUMENT IS TRUE.",
                    collapse = NULL, 
                    recycle0 = FALSE
                ) # not integer because prop
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! base::is.null(x = mode)){
            if(mode != "numeric"){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE mode ARGUMENT CANNOT BE OTHER THAN NULL OR \"numeric\" IF prop ARGUMENT IS TRUE.",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
        if( ! base::is.null(x = typeof)){
            if(typeof != "double"){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE typeof ARGUMENT CANNOT BE OTHER THAN NULL OR \"double\" IF prop ARGUMENT IS TRUE.",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
        }
    }
    # data_name and error_text tested at the beginning
    # end other checkings of the arguments by order
    ######## end other checkings
    
    #### end second round of checking and data preparation

    #### main code
    if(base::is.null(x = data_name)){
        data_name <- base::deparse(expr = base::substitute(expr = data, env = base::environment(fun = NULL)), width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L)
    }
    problem <- FALSE
    text_ok <- base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, base::ifelse(test = base::is.null(x = data_name), yes = "", no = " "), base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), base::ifelse(test = error_text == "", yes = ".", no = error_text), collapse = NULL, recycle0 = FALSE)
    text <- text_ok
    if(( ! base::is.null(x = options)) & (base::all(base::typeof(x = data) == "character", na.rm = TRUE) | base::all(base::typeof(x = data) == "integer", na.rm = TRUE) | base::all(base::typeof(x = data) == "double", na.rm = TRUE))){ # base::all() without na.rm -> ok because base::typeof() never returns NA
        test.log <- TRUE
        if(base::all(base::typeof(x = data) == "double", na.rm = TRUE)){
            if( ! base::all(data %% 1 == 0L, na.rm = TRUE)){ # double but integer like ?
                problem <- TRUE
                text <- base::paste0(
                    "ERROR",
                     base::ifelse(test = error_text == "", yes = "", no = error_text), 
                     "\n\nTHE ", 
                    data_name, 
                    " ", 
                    base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), 
                    " MUST BE SOME OF THESE OPTIONS:\n", 
                    base::paste0(options, collapse = "\n", recycle0 = FALSE), 
                    "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER.",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                test.log <- FALSE
            }
        }
        if(test.log == TRUE){
            text <- ""
            if( ! base::all(data %in% options, na.rm = TRUE)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                problem <- TRUE
                text <- base::paste0(
                    "ERROR",
                     base::ifelse(test = error_text == "", yes = "", no = error_text), 
                     "\n\nTHE ", 
                     data_name, 
                     " ", 
                     base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), 
                     " MUST BE SOME OF THESE OPTIONS:\n", 
                     base::paste0(options, collapse = "\n", recycle0 = FALSE), 
                     "\nTHE PROBLEMATIC ELEMENTS OF ", 
                     data_name, 
                     " ARE:\n", 
                     base::paste0(base::unique(x = data[ ! (data %in% options)], incomparables = FALSE), collapse = "\n", recycle0 = FALSE), 
                     collapse = NULL, 
                     recycle0 = FALSE
                )
            }
            if(all_options_in_data == TRUE){
                if( ! base::all(options %in% data, na.rm = TRUE)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    problem <- TRUE
                    text <- base::paste0(
                        base::ifelse(test = text == "", yes = "", no = base::paste0(text, "\n", collapse = NULL, recycle0 = FALSE)), 
                        base::ifelse(test = error_text == "", yes = "ERROR", no = base::paste0("ERROR IN ", error_text, collapse = NULL, recycle0 = FALSE)), 
                        "\nTHE ", 
                        data_name, 
                        " ", 
                        base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), 
                        " MUST BE MADE OF ALL THESE OPTIONS:\n", 
                        base::paste0(options, collapse = "\n", recycle0 = FALSE), 
                        "\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE:\n",  
                        base::paste0(base::unique(x = options[ ! (options %in% data)], incomparables = FALSE), collapse = "\n", recycle0 = FALSE),
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                }
            }
            if( ! base::is.null(x = length)){
                if(base::length(x = data) != length){
                    problem <- TRUE
                    text <- base::paste0(
                        base::ifelse(test = text == "", yes = "", no = base::paste0(text, "\n", collapse = NULL, recycle0 = FALSE)), 
                        base::ifelse(test = error_text == "", yes = "ERROR", no = base::paste0("ERROR IN ", error_text, collapse = NULL, recycle0 = FALSE)), 
                        "\nTHE LENGTH OF ", 
                        data_name, 
                        " MUST BE ", 
                        length, 
                        " AND NOT ", 
                        base::length(x = data),
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                }
            }
            if(text == ""){
                text <- base::paste0(
                    "NO PROBLEM DETECTED FOR THE ", 
                    data_name, 
                    " ", 
                    base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), 
                    base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
            }
        }
    }else if( ! base::is.null(x = options)){
        problem <- TRUE
        text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\nTHE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE SOME OF THESE OPTIONS:\n", base::paste0(options, collapse = "\n", recycle0 = FALSE), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER.", collapse = NULL, recycle0 = FALSE)
    }
    arg.names <- base::c("class", "typeof", "mode", "length")
    if( ! base::is.null(x = class)){
        if(class == "matrix"){ # because of base::class(base::matrix()) since R4.0.0
            class <- base::c("matrix", "array")
        }else if(class == "factor" & base::all(base::class(x = data) %in% base::c("factor", "ordered"), na.rm = TRUE)){ # to deal with ordered factors # base::all() without na.rm -> ok because base::class(NA) is "logical"
            class <- base::c("factor", "ordered")
        }
    }
    if(base::is.null(x = options)){
        for(i2 in 1:base::length(x = arg.names)){
            if( ! base::is.null(x = base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE))){
                # script to execute
                tempo.script <- '
                    problem <- TRUE ;
                    if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                        text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\nTHE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE ", collapse = NULL, recycle0 = FALSE) ;
                    }else{
                        text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE) ; 
                    }
                    text <- base::paste0(text, base::toupper(arg.names[i2]), " ", if(base::all(base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE) %in% base::c("matrix", "array"), na.rm = TRUE)){"matrix"}else if(base::all(base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE) %in% base::c("factor", "ordered"), na.rm = TRUE)){"factor"}else{base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE)}, collapse = NULL, recycle0 = FALSE)
                '
                # no need of na.rm = TRUE for base::all() because %in% does not output NA
                # end script to execute
                if(base::typeof(x = data) == "double" & double_as_integer_allowed == TRUE & ((arg.names[i2] == "class" & base::all(base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE) == "integer", na.rm = TRUE)) | (arg.names[i2] == "typeof" & base::all(base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE) == "integer", na.rm = TRUE)))){ # data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names # base::typeof(data) == "double" means no factor allowed
                    if( ! base::all(data %% 1 == 0L, na.rm = TRUE)){ # to check integers (use %%, meaning the remaining of a division): see the precedent line. base::isTRUE(base::all.equal(data%%1, base::rep(0, base::length(data)))) not used because we strictly need zero as a result. Warning: na.rm = TRUE required here for base::all()
                        base::eval(expr = base::parse(text = tempo.script, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # execute tempo.script
                    }
                }else if( ! base::any(base::all(base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE) %in% base::c("vector", "ggplot2"), na.rm = TRUE), na.rm = TRUE) & ! base::all(base::eval(expr = base::parse(text = base::paste0(arg.names[i2], "(data)", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) %in% base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE), na.rm = TRUE)){ # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::any() because get base::get(arg.names) does not contain NA
                    base::eval(expr = base::parse(text = tempo.script, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # execute tempo.script
                }else if(arg.names[i2] == "class" & base::all(base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE) == "vector", na.rm = TRUE) & ! (base::all(base::class(x = data) %in% "numeric", na.rm = TRUE) | base::all(base::class(x = data) %in% "integer", na.rm = TRUE) | base::all(base::class(x = data) %in% "character", na.rm = TRUE) | base::all(base::class(x = data) %in% "logical", na.rm = TRUE) | base::all(base::class(x = data) %in% "complex", na.rm = TRUE) | base::all(base::class(x = data) %in% "expression", na.rm = TRUE))){ # test class == "vector". base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) check if user has used the argument class = "vector". If TRUE and base::length(data) > 1, the class "numeric" "integer" "character" "logical" "complex" "expression" should be returned. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names. Other classes "list", "name", "symbol", "function", "environment", "S4", "call" return a list if length of data > 1
                    base::eval(expr = base::parse(text = tempo.script, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # execute tempo.script
                }else if(arg.names[i2] == "class" & base::all(base::get(x = arg.names[i2], pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = FALSE) == "ggplot2", na.rm = TRUE) & ! (base::all(base::class(x = data) %in% base::c("gg", "ggplot"), na.rm = TRUE) | base::all(base::class(x = data) %in% base::c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"), na.rm = TRUE))){ # test ggplot object (before and after Rv4.5.2) # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    base::eval(expr = base::parse(text = tempo.script, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # execute tempo.script
                }
            }
        }
    }
    if(prop == TRUE & base::all(base::typeof(x = data) == "double", na.rm = TRUE)){ # base::all() without na.rm -> ok because base::typeof(NA) is "logical"
        if(base::is.null(x = data) | base::any(data < 0 | data > 1, na.rm = TRUE)){ # works if data base::is.null # Warning: na.rm = TRUE required here for base::any() # base::typeof(data) == "double" means no factor allowed
            problem <- TRUE
            if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
            }else{
                text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
            }
            text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", collapse = NULL, recycle0 = FALSE)
        }
    }else if(prop == TRUE){
        problem <- TRUE
        if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
            text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
        }else{
            text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
        }
        text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE DECIMAL VALUES BETWEEN 0 AND 1.", collapse = NULL, recycle0 = FALSE)
    }
    if(base::all(base::class(x = data) %in% "expression", na.rm = TRUE)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        data <- base::as.character(x = data) # to evaluate the presence of NA
    }
    if(na_contain == FALSE & (base::mode(x = data) %in% base::c("logical", "numeric", "complex", "character", "list"))){ # before it was ! (base::class(data) %in% base::c("function", "environment"))
        if(base::any(base::is.na(x = data), na.rm = TRUE) == TRUE){ # not on the same line because when data is class envir or function , do not like that # normally no NA with base::is.na()
            problem <- TRUE
            if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
            }else{
                text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
            }
            text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " CONTAINS NA WHILE NOT AUTHORIZED.", collapse = NULL, recycle0 = FALSE)
        }
    }
    if(neg_values == FALSE & base::all(base::mode(x = data) %in% "numeric", na.rm = TRUE) & ! base::any(base::class(x = data) %in% "factor", na.rm = TRUE)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        if(base::any(data < 0, na.rm = TRUE)){ # Warning: na.rm = TRUE required here for base::any()
            problem <- TRUE
            if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
            }else{
                text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
            }
            text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE MADE OF NON NEGATIVE NUMERIC VALUES.", collapse = NULL, recycle0 = FALSE)
        }
    }else if(neg_values == FALSE){
        problem <- TRUE
        if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
            text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
        }else{
            text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
        }
        text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE MADE OF NON NEGATIVE VALUES BUT IS ", base::ifelse(test = base::any(base::class(x = data) %in% "factor", na.rm = TRUE), yes = "A FACTOR", no = "NOT EVEN MODE NUMERIC."), collapse = NULL, recycle0 = FALSE) # no need of na.rm = TRUE
    }
    if(inf_values == FALSE & base::all(base::typeof(x = data) %in% "double", na.rm = TRUE) & ! base::any(base::class(x = data) %in% "factor", na.rm = TRUE)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        if(base::any(base::is.infinite(x = data), na.rm = TRUE)){ # Warning: na.rm = TRUE required here for base::any()
            problem <- TRUE
            if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
            }else{
                text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
            }
            text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE MADE OF NON INFINITE NUMERIC VALUES.", collapse = NULL, recycle0 = FALSE)
        }
    }else if(inf_values == FALSE){
        problem <- TRUE
        if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
            text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
        }else{
            text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
        }
        text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE MADE OF NON INFINITE VALUES BUT IS ", base::ifelse(test = base::any(base::class(x = data) %in% "factor", na.rm = TRUE), yes = "A FACTOR", no = "NOT EVEN TYPE DOUBLE."), collapse = NULL, recycle0 = FALSE) # no need of na.rm = TRUE
    }
    if(print == TRUE & problem == TRUE){
        base::cat(base::paste0("\n\n================\n\n", text, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE)
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    output <- base::list(problem = problem, text = text, object.name = data_name)
    base::return(output)
    # end output

}
