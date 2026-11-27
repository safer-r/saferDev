#' @title Check the Expected Features of an Argument
#' @description
#' Checks the expected features of an object, typically an argument of a function: class, type, mode, length, allowed values, and for numeric data, proportions, negative values, infinite values and integers stored as double. The problems detected are returned, not raised as errors (except for \code{data} made of \code{NA} only).
#' @param data Object to test. It cannot be made of \code{NA} only (error, as for every argument of the safer-r functions), whatever \code{na_contain}.
#' @param class Vector of character strings, compared exactly (same values, in the same order) to \code{class(data)}: e.g., \code{class = c("matrix", "array")} for matrices and \code{class = c("ordered", "factor")} for ordered factors. \code{NULL}: class not tested.
#' @param typeof Single character string, compared to \code{typeof(data)}. Several values always give a problem, since \code{typeof(data)} is a single string. \code{NULL}: type not tested.
#' @param mode Single character string, compared to \code{mode(data)}. Several values always give a problem, since \code{mode(data)} is a single string. \code{NULL}: mode not tested.
#' @param length Single integer value, compared to \code{length(data)}. \code{NULL}: length not tested.
#' @param prop Single logical value. If \code{TRUE}, \code{data} must be of type \code{"double"}, with all its values in [-1, 1], or in [0, 1] with \code{neg_values = FALSE}. To also check the class of a proportion object (a matrix for instance), use a second \code{arg_check()} call with \code{prop = FALSE}.
#' @param options Vector of character strings or integers: the allowed values of \code{data}. Numbers of type \code{"double"} (in \code{options} or \code{data}) are accepted if they are integer values, up to a tolerance of about 1.5e-8 (e.g., \code{0.57 * 100}, slightly below 57). \code{NULL}: values not tested.
#' @param double_as_integer_allowed Single logical value. If \code{TRUE}, \code{class = "integer"} and \code{typeof = "integer"} also accept a \code{data} of type \code{"double"} made of integer values (e.g., \code{data = 1}). See the details for approximate integers and dates.
#' @param all_options_in_data Single logical value. If \code{FALSE}, \code{data} must be made of some of the \code{options} elements and nothing else (e.g., \code{data = c(1, 1, 2)} with \code{options = c(1, 2, 3)}). If \code{TRUE}, \code{data} must contain all the \code{options} elements at least once, and nothing else (e.g., \code{data = c(1, 1, 2, 3)}). Ignored if \code{options} is \code{NULL}.
#' @param na_contain Single logical value. Can \code{data} contain \code{NA}?
#' @param neg_values Single logical value. Are negative values authorized? Warning: \code{TRUE} (default) means that no check is performed. \code{FALSE} requires the \code{typeof} argument set to \code{"double"} or \code{"integer"}, the \code{mode} argument set to \code{"numeric"}, or \code{prop = TRUE} (error otherwise). A factor tested with \code{typeof} or \code{mode} then gives the problem \code{"... MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR"}.
#' @param inf_values Single logical value. Are infinite values (\code{Inf}, \code{-Inf}) authorized? Warning: \code{TRUE} (default) means that no check is performed. \code{FALSE} requires the \code{typeof} argument set to \code{"double"} or the \code{mode} argument set to \code{"numeric"} (error otherwise). A factor then gives the problem \code{"... MUST BE MADE OF NON INFINITE VALUES BUT IS A FACTOR"}.
#' @param print Single logical value. If \code{TRUE}, the text of the problems is also displayed (with \code{message()}) when \code{$problem} is \code{TRUE}. \code{FALSE} by default, to let the calling function handle the problems (see the examples).
#' @param data_name Single character string: name of the tested object in the returned text. If \code{NULL}, the expression passed to \code{data} is used.
#' @param data_arg Single logical value. Is the tested object an argument of a function? \code{"ARGUMENT"} (\code{TRUE}, default) or \code{"OBJECT"} is written in the returned text.
#' @param safer_check Single logical value. If \code{TRUE} (default), checks before running that the required packages and functions are installed and that the classical R operators (like \code{<-}) are not masked by another package or by a user-defined object of the global environment. These checks take time at each call: set \code{FALSE} in loops and inside other safer-r functions, to avoid repeating them.
#' @param lib_path Vector of character strings: paths of existing extra library folders containing the required packages (e.g., packages installed without admin rights). They are searched after the current \code{.libPaths()} during the call (the current libraries keep priority), and \code{.libPaths()} is restored on exit. If \code{NULL} (default), only the current \code{.libPaths()} are used. Only checked and used when \code{safer_check} is \code{TRUE}.
#' @param error_text Single character string added to the error messages and to the returned text (e.g., \code{" INSIDE pkg1::fun1()"}) to name the calling functions. \code{NULL} is converted into \code{""}.
#' @returns A list:
#' \itemize{
#'   \item \code{problem}: single logical value, \code{TRUE} if a problem is detected.
#'   \item \code{text}: the problems detected (text starting with \code{"ERROR"}), or a text saying that no problem is detected.
#'   \item \code{object_name}: \code{data_name} if provided, otherwise the expression passed to \code{data} as a string (e.g., \code{"vec1"}).
#' }
#' @details
#' At least one of \code{class}, \code{typeof}, \code{mode}, \code{length}, \code{options} and \code{prop = TRUE} must be set. \code{options} and \code{prop = TRUE} exclude each other, and require \code{class}, \code{typeof} and \code{mode} to be \code{NULL}. \code{length} can be combined with all of them. Wrong argument values or combinations raise an error.
#'
#' The function tests what is written, even if incoherent: \code{arg_check(data = factor(1), class = "factor", mode = "character")} returns a problem, since factors are of mode \code{"numeric"}.
#'
#' To accept several possibilities for the tested object, combine several \code{arg_check()} calls:
#'
#' \preformatted{
#' tempo1 <- saferDev::arg_check(data = data, mode = "character")
#' tempo2 <- saferDev::arg_check(data = data, class = "factor")
#' tempo3 <- saferDev::arg_check(data = data, class = "integer")
#' if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE)\{
#'     print("data ARGUMENT MUST BE A VECTOR OF (1) STRINGS, (2) FACTORS OR (3) INTEGERS")
#' \}
#' }
#'
#' Warning: \code{double_as_integer_allowed = TRUE} only accepts exact integer values stored as double: approximate integers (e.g., \code{1 + 1e-10} or \code{3 - 1e-10}) are problems. Objects of type \code{"double"} that are not numeric (e.g., dates) are problems when tested with \code{options}, and are not accepted as integers by \code{double_as_integer_allowed = TRUE}.
#' @seealso \code{\link{class}}, \code{\link{typeof}}, \code{\link{mode}}.
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @examples
#' # See also https://safer-r.github.io/saferDev/articles/arg_check.html
#' test <- base::matrix(data = 1:3, nrow = 3, ncol = 1, byrow = FALSE, dimnames = NULL)
#' # $problem is TRUE: class(test) is c("matrix", "array"), compared exactly to the class argument
#' saferDev::arg_check(data = test, class = "vector", typeof = NULL, mode = "numeric", length = NULL,
#'     prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE,
#'     na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = TRUE, data_name = NULL,
#'     data_arg = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
#' # $problem is TRUE as well, for the same reason
#' saferDev::arg_check(data = test, class = "matrix", typeof = NULL, mode = "numeric", length = NULL,
#'     prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE,
#'     na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = TRUE, data_name = NULL,
#'     data_arg = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
#' # $problem is FALSE (error_text is added to the returned text)
#' saferDev::arg_check(data = test, class = base::c("matrix", "array"), typeof = NULL,
#'     mode = "numeric", length = NULL, prop = FALSE, options = NULL,
#'     double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE,
#'     neg_values = TRUE, inf_values = TRUE, print = TRUE, data_name = NULL, data_arg = TRUE,
#'     safer_check = TRUE, lib_path = NULL, error_text = " using saferDev::arg_check()")
#' # Inside a function, with print = FALSE (default): the function stops with the returned text
#' fun1 <- function(x){
#'     tempo <- saferDev::arg_check(data = x, class = NULL, typeof = "double", mode = NULL,
#'         length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE,
#'         all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE,
#'         print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = TRUE, lib_path = NULL,
#'         error_text = "")
#'     if(tempo$problem == TRUE){
#'         base::stop(tempo$text, call. = FALSE, domain = NULL)
#'     }
#'     base::return(x)
#' }
#' fun1(x = 2)
#' base::try(expr = fun1(x = -2), silent = FALSE,
#'     outFile = base::getOption(x = "try.outFile", default = base::stderr()))
#'
#' @export
arg_check <- function(
    data, 
    class = NULL, 
    typeof = NULL, 
    mode = NULL, 
    length = NULL, 
    prop = FALSE, 
    options = NULL, 
    double_as_integer_allowed = FALSE, 
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
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = )) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below. Warning: envir is left empty on purpose: its default parent.frame(2L), evaluated inside base::match.call(), is the frame that called this function, where the ... of the call are expanded (base::parent.frame(n = 2L) written here would be evaluated one frame too far, which breaks the calls through base::lapply(), base::vapply(), base::apply(), base::Filter() or a function forwarding ..., base::sapply() only working by coincidence)
    if(base::is.name(x = tempo_settings[[1]]) | base::is.call(x = tempo_settings[[1]])){
        function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE)
    }else{
        function_name <- "<anonymous function>()" # the function was called through its object (e.g., base::do.call(what = <function object>, args = ...)), so that its name is not in the call
    }
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
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
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

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"), 
            " MANDATORY IN SAFER-R FUNCTIONS:\n", 
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    no_def_args <- base::c(
        "data"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE), 
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
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
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
    # Mandatory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
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
                    base::all(x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
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
    # warning: in arg_check(), the two if(safer_check == TRUE){} lines of this section are activated because lib_path is only used inside if(safer_check == TRUE){}: the lib_path check is only executed if safer_check is TRUE.
    # arg_check() is called thousands of times with safer_check = FALSE inside the other safer-r functions, with a lib_path already checked by the calling function, and lib_path is not used in the main code of arg_check().
    # Executing the lib_path check at each call multiplies the execution time of the calling functions by about five (measured on 2026-10-30: 351 s instead of 72 s for saferDev::all_args_here(x = saferDev::is_function_here)).
    if(safer_check == TRUE){ # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
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
                ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = FALSE), add = TRUE, after = TRUE) # return to the previous libPaths(). include.site = FALSE: exact restore, since ini_lib_path already contains the site library if it was in use
                base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = FALSE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    } # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = -1, envir = )
        .pack_and_function_check(
            fun = base::c(
                "saferDev:::.base_op_check"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = -1, envir = )

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
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
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.  #  need to test is.null() here
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
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame. # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
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

    # authorized modes for the arguments
    basic_mode <- base::c(
        "NULL", # must be added here even if already dealt above
        "logical", 
        "numeric", 
        "character"
    )
    tempo_arg_base <- base::c( # no base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) used with arg_check() to be sure to deal with the correct environment
        # "class", 
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
    tempo_mode <- base::list( # no base::get() used to be sure to deal with the correct environment
        base::mode(x = typeof), 
        base::mode(x = mode), 
        base::mode(x = length), 
        base::mode(x = prop), 
        base::mode(x = double_as_integer_allowed), 
        base::mode(x = options), 
        base::mode(x = all_options_in_data), 
        base::mode(x = na_contain), 
        base::mode(x = neg_values), 
        base::mode(x = inf_values), 
        base::mode(x = print), 
        base::mode(x = data_name),
        base::mode(x = data_arg)
    )
    tempo <- ! base::sapply(X = base::lapply(X = tempo_mode, FUN = function(x){x %in% basic_mode}), FUN = function(x){base::all(x, na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE)
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat1 <- tempo_arg_base[tempo]
        tempo_cat2 <- base::sapply(X = tempo_mode[tempo], FUN = function(x){base::paste0(x, collapse = " ", recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE)
        tempo_sep <- base::sapply(X = base::mapply(x = " ", y = base::max(base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA), na.rm = TRUE) - base::nchar(x = tempo_cat1, type = "chars", allowNA = FALSE, keepNA = NA) + 3, FUN = function(x, y){base::rep(x = x, times = y)}, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE), FUN = function(x){base::paste0(x, collapse = "", recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE)
        tempo_cat <- base::paste0(
            error_text_start, 
            "ANY ARGUMENT EXCEPT data AND class MUST BE MODE \"logical\", \"integer\", \"numeric\", \"character\" OR NULL.\nPROBLEMATIC ARGUMENT", 
            base::ifelse(test = base::length(x = tempo_cat1) > 1, yes = "S", no = ""), 
            " AND ASSOCIATED MODE", 
            base::ifelse(test = base::length(x = tempo_cat1) > 1, yes = "ES ARE", no = " IS"), 
            ":\n", 
            base::paste0(tempo_cat1, tempo_sep, tempo_cat2, collapse = "\n", recycle0 = FALSE),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # end authorized modes for the arguments
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
    if(prop == TRUE & ( ! base::is.null(x = class) | ! base::is.null(x = typeof) | ! base::is.null(x = mode))){ # prop = TRUE already checks that data is of type "double" with values between -1 (or 0 if neg_values = FALSE) and 1, so it must be used alone
        tempo_cat <- base::paste0(
            error_text_start,
            "THE class, typeof AND mode ARGUMENTS MUST BE NULL IF THE prop ARGUMENT IS TRUE.\nprop = TRUE ALREADY CHECKS THAT THE TESTED OBJECT IS MADE OF DECIMAL VALUES (TYPE \"double\") BETWEEN -1 AND 1, OR BETWEEN 0 AND 1 IF neg_values IS FALSE.",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if( ! base::is.null(x = options)){
        if( ! base::all(base::typeof(x = options) %in% base::c("character", "integer", "double"), na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE options ARGUMENT MUST BE TYPE \"character\", \"integer\", OR \"double\".",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(base::all(base::typeof(x = options) == "double", na.rm = TRUE)){
            if( ! base::is.numeric(x = options)){ # base::is.numeric() is FALSE for the objects of type double that do not support %% (Date, POSIXct, difftime)
                tempo_cat <- base::paste0(
                    error_text_start,
                    "IF THE options ARGUMENT IS TYPE \"double\", IT MUST BE NUMERIC (NOT DATES, DATE-TIMES OR TIME DIFFERENCES).",
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }
            tempo_modulo <- base::suppressWarnings(expr = options[ ! (base::is.na(x = options) & ! base::is.nan(x = options))] %% 1, classes = "warning") # modulo of the values of options, NA excluded: NA can be an allowed value of data, as in the options of type character or integer (NaN is kept, because it is not an integer value). Warning: base::suppressWarnings() removes the warning "probable complete loss of accuracy in modulus" of R for the very large values (e.g., 1e20), whose modulo 0 is exact, every double value beyond 2^52 being an integer value
            if( ! base::isTRUE(
                x = base::all.equal.numeric(
                    target = base::rep(x = 0L, times = base::length(x = tempo_modulo)), 
                    current = base::pmin(tempo_modulo, 1 - tempo_modulo, na.rm = FALSE), # distance to the nearest integer: the values slightly below an integer (e.g., 3 - 1e-10) are accepted, as those slightly above
                    tolerance = base::sqrt(x = base::.Machine$double.eps), 
                    scale = NULL, 
                    countEQ = FALSE, 
                    formatFUN = , 
                    check.attributes = FALSE, # only the values are compared, not the attributes of options (e.g., names, dim or class)
                    check.class = FALSE, 
                    giveErr = FALSE
                )
            )){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "IF THE options ARGUMENT IS TYPE \"double\", IT MUST BE MADE OF INTEGER VALUES, NOT DECIMAL VALUES.",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(base::any(base::abs(x = options) > base::.Machine$integer.max, na.rm = TRUE)){
                options <- base::round(x = base::as.vector(x = options, mode = "double"), digits = 0) # integer values outside the integer range of R (e.g., 3e9): kept as double values (attributes removed, as by base::as.integer() below), because base::as.integer() would convert them into NA, which would match the NA of data. base::round(): see below
            }else{
                options <- base::as.integer(x = base::round(x = options, digits = 0)) # base::round() because the modulo test above accepts the values close to an integer value (e.g., -2 + 1e-10 or 3 - 1e-10), while base::as.integer() truncates toward 0 (-1 instead of -2, 2 instead of 3)
            }
        }
    }
    if(neg_values == FALSE & base::is.null(x = typeof) & base::is.null(x = mode) & prop == FALSE){ # with prop = TRUE, neg_values = FALSE sets the range to 0 and 1 (instead of -1 and 1)
        tempo_cat <- base::paste0(
            error_text_start,
            "THE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF typeof AND mode ARGUMENTS ARE NULL AND prop IS FALSE.",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(inf_values == FALSE & base::is.null(x = typeof) & base::is.null(x = mode)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF typeof AND mode ARGUMENTS ARE NULL.",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(neg_values == FALSE & ! base::is.null(x = typeof)){ # combination checking: negative values can only be checked for numeric data (single "double" or "integer" typeof value), whatever the strings written in the typeof argument
        if( ! (base::all(typeof %in% base::c("double", "integer"), na.rm = TRUE) & base::length(x = typeof) == 1L)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE typeof ARGUMENT MUST BE A SINGLE \"double\" OR \"integer\" VALUE IF ABSENCE OF NEGATIVE VALUES IS CONTROLLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(inf_values == FALSE & ! base::is.null(x = typeof)){ # combination checking: infinite values can only be checked for double data (single "double" typeof value), whatever the strings written in the typeof argument. "integer" is not allowed because objects with infinite values are not integers
        if( ! (base::all(typeof %in% "double", na.rm = TRUE) & base::length(x = typeof) == 1L)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE typeof ARGUMENT MUST BE A SINGLE \"double\" VALUE IF ABSENCE OF INFINITE VALUES IS CONTROLLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.\n\"integer\" IS NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE NOT INTEGERS.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(neg_values == FALSE & ! base::is.null(x = mode)){ # combination checking: negative values can only be checked for numeric data (single "numeric" mode value), whatever the strings written in the mode argument
        if( ! (base::all(mode %in% "numeric", na.rm = TRUE) & base::length(x = mode) == 1L)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE mode ARGUMENT MUST BE A SINGLE \"numeric\" VALUE IF ABSENCE OF NEGATIVE VALUES IS CONTROLLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(inf_values == FALSE & ! base::is.null(x = mode)){ # combination checking: infinite values can only be checked for numeric data (single "numeric" mode value), whatever the strings written in the mode argument
        if( ! (base::all(mode %in% "numeric", na.rm = TRUE) & base::length(x = mode) == 1L)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE mode ARGUMENT MUST BE A SINGLE \"numeric\" VALUE IF ABSENCE OF INFINITE VALUES IS CONTROLLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE.\nOTHER VALUES ARE NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE ONLY \"numeric\".",
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

    # data_name and error_text tested at the beginning
    # end other checkings of the arguments by order
    ######## end other checkings
    
    #### end second round of checking and data preparation

    #### main code
    if(base::is.null(x = data_name)){
        data_name <- base::deparse1(expr = base::substitute(expr = data, env = base::environment(fun = NULL)), collapse = " ", width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L) # base::deparse1(): single string, even for an expression longer than the width.cutoff limit
    }
    problem <- FALSE
    text_ok <- base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, base::ifelse(test = base::is.null(x = data_name), yes = "", no = " "), base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), base::ifelse(test = error_text == "", yes = ".", no = error_text), collapse = NULL, recycle0 = FALSE)
    text <- text_ok
    if(( ! base::is.null(x = options)) & (base::all(base::typeof(x = data) == "character", na.rm = TRUE) | base::all(base::typeof(x = data) == "integer", na.rm = TRUE) | (base::all(base::typeof(x = data) == "double", na.rm = TRUE) & base::is.numeric(x = data)))){ # base::all() without na.rm -> ok because base::typeof() never returns NA. base::is.numeric() is FALSE for the objects of type double that do not support %% (Date, POSIXct, difftime): problem returned below instead of an R error
        test_log <- TRUE
        tempo_data_opt <- data # for the options argument only
        if(base::all(base::typeof(x = tempo_data_opt) == "double", na.rm = TRUE)){
            tempo_modulo <- base::suppressWarnings(expr = tempo_data_opt[ ! (base::is.na(x = tempo_data_opt) & ! base::is.nan(x = tempo_data_opt))] %% 1, classes = "warning") # modulo of the values of data, NA excluded, as in data of type integer: NA values are then tested by the na_contain argument, and against the options below (NaN is kept, because it is not an integer value). base::suppressWarnings(): see the options argument above
            if( ! base::isTRUE(
                x = base::all.equal.numeric(
                    target = base::rep(x = 0L, times = base::length(x = tempo_modulo)), 
                    current = base::pmin(tempo_modulo, 1 - tempo_modulo, na.rm = FALSE), # distance to the nearest integer: the values slightly below an integer (e.g., 3 - 1e-10) are accepted, as those slightly above
                    tolerance = base::sqrt(x = base::.Machine$double.eps), 
                    scale = NULL, 
                    countEQ = FALSE, 
                    formatFUN = , 
                    check.attributes = FALSE, # only the values are compared, not the attributes of data (e.g., names, dim or class)
                    check.class = FALSE, 
                    giveErr = FALSE
                )
            )){
                problem <- TRUE
                if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
                }
                text <- base::paste0(
                    text, 
                    "THE ",  
                    data_name, 
                    " ", 
                    base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), 
                    " MUST BE SOME OF THESE OPTIONS:\n", 
                    base::paste0(options, collapse = "\n", recycle0 = FALSE), 
                    "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER, OR TYPE DOUBLE WITH A 0 MODULO.",
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                test_log <- FALSE
            }else if(base::any(base::abs(x = tempo_data_opt) > base::.Machine$integer.max, na.rm = TRUE)){
                tempo_data_opt <- base::round(x = base::as.vector(x = tempo_data_opt, mode = "double"), digits = 0) # integer values outside the integer range of R (e.g., 3e9): kept as double values (attributes removed, as by base::as.integer() below), because base::as.integer() would convert them into NA, which would match the NA of the options. base::round(): see below
            }else{
                tempo_data_opt <- base::as.integer(x = base::round(x = tempo_data_opt, digits = 0)) # base::round() because the modulo test above accepts the values close to an integer value (e.g., -2 + 1e-10 or 3 - 1e-10), while base::as.integer() truncates toward 0 (-1 instead of -2, 2 instead of 3)
            }
        }
        if(test_log == TRUE){
            if( ! base::all(tempo_data_opt %in% options, na.rm = TRUE)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                problem <- TRUE
                if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
                }
                tempo_res <- tempo_data_opt[ ! (tempo_data_opt %in% options)]
                text <- base::paste0(
                    text, 
                    "THE ", 
                    data_name, 
                    " ", 
                    base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), 
                    " MUST BE SOME OF THESE OPTIONS:\n", 
                    base::paste0(options, collapse = "\n", recycle0 = FALSE), 
                    "\nTHE PROBLEMATIC ELEMENT",
                    base::ifelse(test = base::length(x = tempo_res) == 1, yes = "", no = "S"), 
                    " OF ", 
                    data_name, 
                    base::ifelse(test = base::length(x = tempo_res) == 1, yes = " IS:\n", no = " ARE:\n"),  
                    base::paste0(base::unique(x = tempo_res, incomparables = FALSE), collapse = "\n", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
            }
            if(all_options_in_data == TRUE){
                if( ! base::all(options %in% tempo_data_opt, na.rm = TRUE)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    problem <- TRUE
                    if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                        text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
                    }else{
                        text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
                    }
                    tempo_res <- options[ ! (options %in% tempo_data_opt)]
                    text <- base::paste0(
                        text, 
                        "THE ", 
                        data_name, 
                        " ", 
                        base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), 
                        " MUST BE MADE OF ALL THESE OPTIONS:\n", 
                        base::paste0(options, collapse = "\n", recycle0 = FALSE), 
                        "\nTHE MISSING ELEMENT", 
                        base::ifelse(test = base::length(x = tempo_res) == 1, yes = "", no = "S"), 
                        " IN ", 
                        data_name, 
                        base::ifelse(test = base::length(x = tempo_res) == 1, yes = " IS:\n", no = " ARE:\n"), 
                        base::paste0(base::unique(x = tempo_res, incomparables = FALSE), collapse = "\n", recycle0 = FALSE),
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                }
            }
            if( ! base::is.null(x = length)){
                if(base::length(x = tempo_data_opt) != length){
                    problem <- TRUE
                    if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                        text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
                    }else{
                        text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
                    }
                    text <- base::paste0(
                        text, 
                        "THE LENGTH OF ", 
                        data_name, 
                        " MUST BE ", 
                        length, 
                        " AND NOT ", 
                        base::length(x = tempo_data_opt),
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                }
            }
        }
    }else if( ! base::is.null(x = options)){
        problem <- TRUE
        text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\nTHE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE SOME OF THESE OPTIONS:\n", base::paste0(options, collapse = "\n", recycle0 = FALSE), base::ifelse(test = base::all(base::typeof(x = data) == "double", na.rm = TRUE), yes = "\nBUT IS TYPE DOUBLE AND NOT NUMERIC (E.G., A DATE, A DATE-TIME OR A TIME DIFFERENCE).", no = "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER, OR TYPE DOUBLE WITH A 0 MODULO."), collapse = NULL, recycle0 = FALSE) # only the objects of type double that are not numeric (Date, POSIXct, difftime) reach this branch with type double: the numeric ones are handled above
    }
    arg_names <- base::c("class", "typeof", "mode", "length")
    if(base::is.null(x = options)){
        for(i2 in base::seq_along(along.with = arg_names)){
            if( ! base::is.null(x = base::get(x = arg_names[i2], pos = -1L, envir = base::as.environment(x = -1), mode = "any", inherits = FALSE))){
                # script to execute
                tempo_script <- '
                    problem <- TRUE ;
                    if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                        text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\nTHE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE ", collapse = NULL, recycle0 = FALSE) ;
                    }else{
                        text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE) ; 
                    }
                    text <- base::paste0(
                        text, 
                        base::toupper(arg_names[i2]), 
                        " ", 
                        base::paste0(base::get(x = arg_names[i2], pos = -1L, envir = base::as.environment(x = -1), mode = "any", inherits = FALSE), collapse = ", ", recycle0 = FALSE),
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                '
                # no need of na.rm = TRUE for base::all() because %in% does not output NA
                # end script to execute
                if(base::typeof(x = data) == "double" & base::is.numeric(x = data) & double_as_integer_allowed == TRUE & ((arg_names[i2] == "class" & base::all(base::get(x = arg_names[i2], pos = -1L, envir = base::as.environment(x = -1), mode = "any", inherits = FALSE) == "integer", na.rm = TRUE)) | (arg_names[i2] == "typeof" & base::all(base::get(x = arg_names[i2], pos = -1L, envir = base::as.environment(x = -1), mode = "any", inherits = FALSE) == "integer", na.rm = TRUE)))){ # data of type double & double_as_integer_allowed == TRUE and (class = "integer" | typeof = "integer") # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg_names # base::typeof(data) == "double" means no factor allowed # base::is.numeric(x = data) excludes the objects of type double that do not support %% (Date, POSIXct, difftime)
                    if( ! base::all(base::suppressWarnings(expr = data %% 1, classes = "warning") == 0L, na.rm = TRUE)){ # to check integers (use %%, meaning the remaining of a division): see the precedent line. base::isTRUE(base::all.equal(data%%1, base::rep(0, base::length(data)))) not used because we strictly need zero as a result. Warning: na.rm = TRUE required here for base::all(). base::suppressWarnings(): see the options argument above
                        base::eval(expr = base::parse(text = tempo_script, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # execute tempo.script
                    }
                }else if(arg_names[i2] == "length"){
                    if( ! (base::length(x = data) == base::get(x = arg_names[i2], pos = -1L, envir = base::as.environment(x = -1), mode = "any", inherits = FALSE))){ # numeric comparison for the length argument (single integer value, checked above)
                        base::eval(expr = base::parse(text = tempo_script, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # execute tempo.script
                    }
                }else if( ! base::identical(
                    x = base::as.character(x = base::eval(expr = base::parse(text = base::paste0(arg_names[i2], "(data)", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))), 
                    y = base::as.character(x = base::get(x = arg_names[i2], pos = -1L, envir = base::as.environment(x = -1), mode = "any", inherits = FALSE)), 
                    num.eq = TRUE, 
                    single.NA = TRUE, 
                    attrib.as.set = TRUE, 
                    ignore.bytecode = TRUE, 
                    ignore.environment = FALSE, 
                    ignore.srcref = TRUE, 
                    extptr.as.ref = FALSE
                )){ # test the three base::c("class", "typeof", "mode") arguments with their corresponding function, exactly (same values, in the same order). base::as.character() used on both sides to remove any potential attribute of class(data), typeof(data) and mode(data). No need of na.rm = TRUE because base::as.character() is used
                    base::eval(expr = base::parse(text = tempo_script, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # execute tempo.script
                }
            }
        }
    }
    if(prop == TRUE & base::all(base::typeof(x = data) == "double", na.rm = TRUE)){ # base::all() without na.rm -> ok because base::typeof(NA) is "logical"
        if(base::is.null(x = data) | base::any(data < base::ifelse(test = neg_values, yes = -1, no = 0) | data > 1, na.rm = TRUE)){ # works if data base::is.null # Warning: na.rm = TRUE required here for base::any() # base::typeof(data) == "double" means no factor allowed # lower limit: -1 if negative values are authorized, 0 otherwise
            problem <- TRUE
            if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
            }else{
                text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
            }
            text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE DECIMAL VALUES BETWEEN ", base::ifelse(test = neg_values, yes = "-1", no = "0"), " AND 1.", collapse = NULL, recycle0 = FALSE)
        }
    }else if(prop == TRUE){
        problem <- TRUE
        if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
            text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
        }else{
            text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
        }
        text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE DECIMAL VALUES BETWEEN ", base::ifelse(test = neg_values, yes = "-1", no = "0"), " AND 1.", collapse = NULL, recycle0 = FALSE)
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
    if(( ! base::is.null(x = mode) && base::all(mode == "numeric", na.rm = TRUE) && base::length(x = mode) == 1L) | ( ! base::is.null(x = typeof) && base::all(typeof %in% base::c("double", "integer"), na.rm = TRUE) && base::length(x = typeof) == 1L)){
        if(neg_values == FALSE & ! base::any(base::class(x = data) %in% "factor", na.rm = TRUE) & (base::all(base::mode(x = data) %in% "numeric", na.rm = TRUE) | base::all(base::typeof(x = data) %in% base::c("double", "integer"), na.rm = TRUE))){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
            if(base::any(data < 0, na.rm = TRUE)){ # Warning: na.rm = TRUE required here for base::any()
                problem <- TRUE
                if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
                }else{
                    text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
                }
                text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE MADE OF NON NEGATIVE VALUES.", collapse = NULL, recycle0 = FALSE)
            }
        }else if(neg_values == FALSE){
            problem <- TRUE
            if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
            }else{
                text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
            }
            text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE MADE OF NON NEGATIVE VALUES BUT IS ", base::ifelse(test = base::any(base::class(x = data) %in% "factor", na.rm = TRUE), yes = "A FACTOR", no = "NOT EVEN NUMERIC."), collapse = NULL, recycle0 = FALSE) # no need of na.rm = TRUE
        }
    }
    if(( ! base::is.null(x = mode) && base::all(mode == "numeric", na.rm = TRUE) && base::length(x = mode) == 1L) | ( ! base::is.null(x = typeof) && base::all(typeof %in% "double", na.rm = TRUE) && base::length(x = typeof) == 1L)){
        if(inf_values == FALSE & ! base::any(base::class(x = data) %in% "factor", na.rm = TRUE) & (base::all(base::mode(x = data) %in% "numeric", na.rm = TRUE) | base::all(base::typeof(x = data) %in% "double", na.rm = TRUE))){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
            if(base::any(base::is.infinite(x = data), na.rm = TRUE)){ # Warning: na.rm = TRUE required here for base::any()
                problem <- TRUE
                if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                    text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
                }else{
                    text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
                }
                text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE MADE OF NON INFINITE VALUES.", collapse = NULL, recycle0 = FALSE)
            }
        }else if(inf_values == FALSE){
            problem <- TRUE
            if(base::identical(x = text, y = text_ok, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = error_text), "\n\n", collapse = NULL, recycle0 = FALSE)
            }else{
                text <- base::paste0(text, " AND ", collapse = NULL, recycle0 = FALSE)
            }
            text <- base::paste0(text, "THE ", data_name, " ", base::ifelse(test = data_arg, yes = "ARGUMENT", no = "OBJECT"), " MUST BE MADE OF NON INFINITE VALUES BUT IS ", base::ifelse(test = base::any(base::class(x = data) %in% "factor", na.rm = TRUE), yes = "A FACTOR", no = "NOT EVEN MODE NUMERIC OR TYPE DOUBLE."), collapse = NULL, recycle0 = FALSE) # no need of na.rm = TRUE
        }
    }
    if(print == TRUE & problem == TRUE){
        base::message(base::paste0("\n\n================\n\n", text, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    output <- base::list(problem = problem, text = text, object_name = data_name)
    base::return(output)
    #### end output

}
