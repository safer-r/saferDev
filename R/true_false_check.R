#' @title Check and Write \code{TRUE} and \code{FALSE} in Full
#' @description
#' Checks that the logical constants \code{TRUE} and \code{FALSE} are written in full when used as values, and not abbreviated as \code{T} or \code{F}, which are ordinary objects that can be overwritten (e.g., \code{T <- 0}). Prints the reported symbols and returns a corrected version of the function.
#' @param x Function to check, written without quotes and brackets (e.g., \code{fun1} or \code{pkg::fun1}).
#' @param export Single logical value. If \code{TRUE}, the corrected function is written into the \code{<name>_safer.R} file of \code{out_path} instead of being returned (e.g., \code{fun1_safer.R} for \code{x = fun1} or \code{x = pkg::fun1}), as \code{fun1_safer <- function(...)}, which \code{base::source()} recreates. Nothing is written if nothing has to be corrected, but an error is returned if the file already exists, or if \code{x} is not a function name (e.g., \code{function(x)\{x\}} or \code{FUN_LIST$fun}).
#' @param out_path Single character string: path of an existing folder where the file is written. Required when \code{export} is \code{TRUE}; otherwise not used, but a non-\code{NULL} value must still be a single non-empty character string. \code{NULL} by default.
#' @inheritParams is_function_here
#' @returns
#' The corrected function, which keeps the environment of \code{x}, or \code{NULL}, invisibly, if nothing has to be corrected or if \code{export} is \code{TRUE}.
#'
#' A message is also printed: a table with one row per reported symbol (column 1: line number; column 2: symbol; column 3: code of the line), or a sentence saying that everything seems clean.
#' @details
#' Reported: \code{T} or \code{F} used as a value, e.g., \code{f(log = T)}, \code{f(T)}, \code{if(T)}, \code{while(T)}, \code{x == T}, \code{x[T]} or \code{z <- T & y}.
#'
#' Not reported: \code{T} or \code{F} used as a name or as the whole assigned value, e.g., \code{T <- 1}, \code{x <- T}, \code{function(T)}, \code{f(T = 1)}, \code{df$T}, \code{for(T in 1:3)}, \code{T()}, \code{?T} or \code{y ~ T}, and the \code{T} and \code{F} inside \code{quote()}, \code{bquote()}, \code{expression()}, \code{alist()} and \code{substitute()}, whose code R does not evaluate as written (e.g., the plotmath label \code{expression(T[max])}), except the parts that R evaluates: \code{.()} in \code{bquote()} (\code{..()} too if \code{splice = TRUE} is written), and the \code{where} and \code{splice} arguments of \code{bquote()} and \code{env} of \code{substitute()}. In the other functions that use the code of their arguments, \code{T} and \code{F} are replaced, even when they are names (e.g., a column named \code{T} in \code{base::subset(x = d, subset = T > 30)}, which becomes \code{TRUE > 30}). Strings, comments and backquoted names (\code{`T`}) are ignored. If \code{T} or \code{F} is defined anywhere in the function (argument, loop variable or assignment), none of its uses is reported.
#'
#' Correction: each reported \code{T} is replaced by \code{TRUE} and each \code{F} by \code{FALSE} (\code{base::T} also becomes \code{TRUE}). The rest of the code, comments and indentation included, is unchanged.
#'
#' Source references: line 1 is the line of the \code{function} keyword. Comments and indentation are kept only if \code{x} carries its source references (attribute \code{srcref}). Functions created with \code{options(keep.source = FALSE)} (default of Rscript) and functions of installed packages (e.g., \code{saferDev::colons_check}) have none: their deparsed code is analyzed, without comments and with line numbers that differ from the source file.
#' @seealso The code checkers of saferDev, in the recommended order of use: \code{\link{super_assign_check}}, \code{\link{lambda_check}}, \code{\link{colons_check}}, \code{\link{all_args_here}}, \code{\link{curly_bracket_check}}, \code{\link{true_false_check}}, \code{\link{for_loop_nb_check}} (see \code{vignette("saferDev")}).
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @examples
#' # Without source references ("Run examples" link, Rscript), line numbers and comments differ.
#' # See also https://safer-r.github.io/saferDev/articles/true_false_check.html
#' fun1 <- function(x){ if(x == TRUE){ base::return(x) } }
#' saferDev::true_false_check(x = fun1, export = FALSE, out_path = NULL, safer_check = TRUE,
#'     lib_path = NULL, error_text = "") # nothing to correct: NULL returned, invisibly
#' fun2 <- function(x){ if(x == T){ base::return(x) } }
#' fun2_safer <- saferDev::true_false_check(x = fun2, export = FALSE, out_path = NULL,
#'     safer_check = TRUE, lib_path = NULL, error_text = "") # T reported
#' fun2_safer # corrected function
#' # Write the corrected function in the fun2_safer.R file of a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::true_false_check(x = fun2, export = TRUE, out_path = tempo_dir, safer_check = TRUE,
#'     lib_path = NULL, error_text = "")
#' base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
#'
#' @export
true_false_check <- function(
    x,
    export = FALSE,
    out_path = NULL,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){
    # DEBUGGING
    # x = fun1 ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "true_false_check" ; arg_user_setting = base::list(x = as.name(x = "fun1"), export = FALSE, out_path = NULL, lib_path = NULL , safer_check = TRUE, error_text = " INSIDE A")
    # x = .base_op_check ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # internal function with function body made of a single .Primitive() call
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\true_false_check.R") ; x = true_false_check ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::true_false_check does not return the same number of code lines

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
    intern_error_text_start <- base::paste0(
        package_function_name,
        base::ifelse(test = error_text == "", yes = ".", no = error_text),
        "\n\n",
        collapse = NULL,
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
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
    # optional section: remove the code if none of your arguments has no default value
    no_def_args <- base::c(
        "x"
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
    tempo_arg <-base::c(
        "x",
        "export",
        # "out_path", # inactivated because can be NULL
        # "lib_path", # inactivated because can be NULL
        "safer_check"
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
        "x",
        "export",
        "out_path",
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
    # if(safer_check == TRUE){ # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
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
    # } # this line must be activated if lib_path is only used inside if(safer_check == TRUE){}. Which is not the case using saferDev::arg_check()
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = -1, envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check",
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check"
                # end internal functions required in this code
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
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = x, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = export, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = out_path)){
        tempo <- saferDev::arg_check(data = out_path, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    tempo_arg <- base::c(
        "out_path"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            "\nIf saferMake::saferMake() HAS BEEN USED TO MODIFY YOUR FUNCTION, PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE FOR YOUR ARGUMENTS\n.HERE, ARGUMENTS ARE TYPE:\n", 
            base::paste0(base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ": \"", base::typeof(x = base::get(x = x, pos = -1L, envir = base::parent.frame(n = 3), mode = "any", inherits = FALSE)), "\"", collapse = NULL, recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE), collapse = "\n", recycle0 = FALSE), # parent.frame(n = 3) because the get() is evaluated inside the FUN closure of sapply. But sapply(FUN) calls lapply(FUN) internally !! So parent.frame(n = 1) is the internal lapply() frame called by sapply, parent.frame(n = 2) is sapply's frame, parent.frame(n = 3) is BACKBONE's frame. NB: paste0() and typeof() are primitives and add no frame.
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
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
    # x forwarded through the ... argument of another function (e.g., fun2 <- function(...){saferDev::true_false_check(...)} called with x = fun1): base::match.call() writes x ..1 (or ..2, etc.) in arg_user_setting, instead of its writing in the call. This writing is recovered from the promise of x by base::substitute() (x is not reassigned before this line)
    if(base::is.symbol(x = arg_user_setting$x) && base::grepl(x = base::as.character(x = arg_user_setting$x), pattern = "^\\.\\.[0-9]+$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
        arg_user_setting$x <- base::substitute(expr = x, env = base::environment(fun = NULL))
    }
    if(export == TRUE){
        if(base::is.null(x = out_path)){
            tempo_cat <- base::paste0(
                error_text_start,
                "THE out_path ARGUMENT MUST BE THE PATHWAY OF AN EXISTING FOLDER WHEN export IS TRUE.",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        # removal of trailing / or \\ in the path
        if(base::grepl(x = out_path, pattern = "/$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            out_path <- base::sub(pattern = "/$", replacement = "", x = out_path, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }else if(base::grepl(x = out_path, pattern = "\\\\$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            out_path <- base::sub(pattern = "\\\\$", replacement = "", x = out_path, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }
        # end removal of trailing / or \\ in the path
        if( ! base::dir.exists(paths = out_path)){
            tempo_cat <- base::paste0(
                error_text_start,
                "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXIST:\n", 
                out_path, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        out_dir <- out_path # folder path after trailing / or \\ removal
        # determination of the file name: name of the submitted function, as written by the user, with _safer added at the end
        # arg_user_setting$x is x as written in the call: a name (as.name(x = "test")), a call (like the parsed call "saferDev::test"), or a function object when the call holds the function itself (e.g., x = fun1 in the args argument of base::do.call()). The function object is tested first, because base::as.character() cannot convert it
        if(( ! base::is.function(x = arg_user_setting$x)) && (base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::")){
            out_fun_name <- base::as.character(x = arg_user_setting$x)[3] # form "package::function" or "package:::function"
        }else if(base::is.symbol(x = arg_user_setting$x)){
            out_fun_name <- base::as.character(x = arg_user_setting$x)[1]
        }else{
            # multi-component written name (like FUN_LIST$fun), inline definition (like function(x){x}), function object, or other unexpected writing: cannot determine a file name reliably. FUN_LIST$fun would give fun_safer.R, but a same name can designate different functions depending on the object browsed by $: the written form is not reliable for the file name
            if(base::is.function(x = arg_user_setting$x)){
                tempo_x <- "A FUNCTION OBJECT, WITHOUT ITS NAME (E.G., x = fun1 IN THE args ARGUMENT OF base::do.call(): USE x = base::quote(expr = fun1) INSTEAD)"
            }else{
                tempo_x <- base::paste0(base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = "\n", recycle0 = FALSE)
            }
            tempo_cat <- base::paste0(
                error_text_start,
                "THE x ARGUMENT MUST BE DESIGNATED BY A FUNCTION NAME (NOT BY AN INLINE FUNCTION DEFINITION, NOR BY A COMPONENT WRITING LIKE obj$fun) WHEN THE export ARGUMENT IS TRUE.\nHERE IT IS:\n",
                tempo_x,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        out_file_name <- base::paste0(out_fun_name, "_safer.R", collapse = NULL, recycle0 = FALSE)
        out_file_path <- base::file.path(out_dir, out_file_name, fsep = "/") # consistent between OS, contrary to base::paste0(out_dir, "/", out_file_name, collapse = NULL, recycle0 = FALSE)
        if(base::file.exists(out_file_path)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n", 
                out_file_path, 
                "\n\nPLEASE, RENAME OR REMOVE THIS FILE, OR CHANGE THE out_path ARGUMENT, AND RERUN ", 
                package_function_name, 
                ".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }else{
        out_file_path <- NULL
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # attributes of x printed by R after its code (all the attributes except srcref, srcfile, wholeSrcref and comment, which print() does not show): the code of x is recovered below as printed, so that the printed attribute values would be analyzed as code of x, silently when they parse (e.g., a formula, a function or a call, which could even be run), or with the parse error below. Same message in super_assign_check(), lambda_check(), curly_bracket_check(), true_false_check() and for_loop_nb_check()
    tempo_attr <- base::setdiff(x = base::names(x = base::attributes(x = x)), y = base::c("srcref", "srcfile", "wholeSrcref", "comment"))
    if(base::length(x = tempo_attr) > 0){
        tempo_cat <- base::paste0(
            error_text_start,
            "THE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS ATTRIBUTES, PRINTED BY R AFTER ITS CODE, WOULD BE ANALYZED AS CODE.\nATTRIBUTES TO REMOVE BEFORE THE CHECK (E.G., base::attr(x = fun, which = \"note\") <- NULL):\n",
            base::paste0(tempo_attr, collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # recover the code of the checked function
    code <- utils::capture.output(x, file = NULL, append = FALSE, type = NULL, split = FALSE)
    # removal of the attribute lines printed for compiled functions (e.g., "<bytecode: 0x...>", "<environment: ...>", "<srcref: ...>"). Warning: the pattern cannot be simply a line starting by "<", because a valid R code line can start by "<" (binary operator continuation)
    code <- code[ ! base::grepl(x = code, pattern = "^<(bytecode|environment|srcref): ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)]
    # warning: R only stores the parse data when the option keep.parse.data is TRUE (see lambda_check()). The option is set here and restored on exit
    oop <- base::options(keep.parse.data = TRUE)
    base::on.exit(expr = base::options(oop), add = TRUE, after = TRUE)
    code_text <- base::paste0(code, collapse = "\n", recycle0 = FALSE)
    pd <- base::tryCatch(
        expr = utils::getParseData(x = base::parse(file = "", n = NULL, text = code_text, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_text, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA), # warning: an explicit srcfile is required, because srcfile = NULL removes the source references of the parsed code, so that utils::getParseData() returns NULL
        error = function(e){e}, # code of x, as printed by R, that does not parse: error below
        finally =
    )
    if(base::inherits(x = pd, what = "error", which = FALSE)){
        # the printed code of x, without attributes (checked above), does not parse when an object that is not printed as R code is inserted in its body (e.g., an environment, printed <environment>). Same message in super_assign_check(), lambda_check(), curly_bracket_check(), true_false_check() and for_loop_nb_check()
        tempo_cat <- base::paste0(
            error_text_start,
            "THE FUNCTION OF THE x ARGUMENT CANNOT BE CHECKED: ITS CODE, AS PRINTED BY R, DOES NOT PARSE (E.G., AN OBJECT INSERTED IN ITS BODY THAT IS NOT PRINTED AS R CODE, LIKE AN ENVIRONMENT, PRINTED <environment>).\n\nERROR MESSAGE OF THE R PARSER:\n",
            base::conditionMessage(c = pd),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # conversion of the col1 and col2 columns of the parse data into character positions of the code lines, because the replacements below use substr() on these lines: utils::getParseData() counts a TAB as a jump to the next multiple of 8 (the TABs of the code are kept)
    tab_lines <- base::which(x = base::grepl(x = code, pattern = "\t", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE), arr.ind = FALSE, useNames = TRUE)
    for(i3 in base::seq_along(along.with = tab_lines)){
        tempo_chars <- base::strsplit(x = code[tab_lines[i3]], split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
        tempo_cols <- base::integer(length = base::length(x = tempo_chars)) # parse data column of each character of the line
        tempo_col <- 0L
        for(i4 in base::seq_along(along.with = tempo_chars)){
            tempo_col <- tempo_col + 1L
            if(tempo_chars[i4] == "\t"){
                tempo_col <- ((tempo_col + 7L) %/% 8L) * 8L
            }
            tempo_cols[i4] <- tempo_col
        }
        tempo_log1 <- pd$line1 == tab_lines[i3]
        tempo_log2 <- pd$line2 == tab_lines[i3]
        pd$col1[tempo_log1] <- base::match(x = pd$col1[tempo_log1], table = tempo_cols, nomatch = NA_integer_, incomparables = NULL)
        pd$col2[tempo_log2] <- base::match(x = pd$col2[tempo_log2], table = tempo_cols, nomatch = NA_integer_, incomparables = NULL)
        if(base::anyNA(x = pd$col1[tempo_log1], recursive = FALSE) | base::anyNA(x = pd$col2[tempo_log2], recursive = FALSE)){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (in UTF-8 and single-byte locales, each column of the parse data is the column of a character of its line).
            tempo_cat <- base::paste0("INTERNAL ERROR 1 IN ", intern_error_text_start, "A COLUMN OF THE PARSE DATA DOES NOT MATCH ANY CHARACTER OF LINE ", tab_lines[i3], " OF THE RECOVERED CODE", intern_error_text_end, collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    # end conversion of the col1 and col2 columns of the parse data into character positions of the code lines
    # terminal tokens (comments excluded), sorted by source position
    term_rows <- base::which(x = pd$terminal == TRUE & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
    term_rows <- term_rows[base::order(pd$line1[term_rows], pd$col1[term_rows], na.last = TRUE, decreasing = FALSE, method = "auto")]
    # modification of arg_user_setting$x for clean messages: a single string "<name>()", with <name> the writing of x in the call (name, pkg::name, pkg:::name, or other expression deparsed on one line, like dots[[1L]][[1L]] through base::Map()), or "x" for a function object, which has no name (e.g., x = fun1 in the args argument of base::do.call())
    if(base::is.function(x = arg_user_setting$x)){
        arg_user_setting$x <- "x()"
    }else if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
        arg_user_setting$x <- base::paste0(
            base::as.character(x = arg_user_setting$x)[2],
            base::as.character(x = arg_user_setting$x)[1],
            base::as.character(x = arg_user_setting$x)[3],
            "()",
            collapse = NULL,
            recycle0 = FALSE
        )
    }else if(base::is.symbol(x = arg_user_setting$x)){
        arg_user_setting$x <- base::paste0(
            arg_user_setting$x,
            "()",
            collapse = NULL,
            recycle0 = FALSE
        )
    }else{
        arg_user_setting$x <- base::paste0(
            base::paste0(base::trimws(x = base::deparse(expr = arg_user_setting$x, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), which = "left", whitespace = "[ \\t]"), collapse = " ", recycle0 = FALSE),
            "()",
            collapse = NULL,
            recycle0 = FALSE
        )
    }
    # end modification of arg_user_setting$x for clean messages
    # true_false_check() only checks the T and F symbols used as values: no package-association pre-check of the function calls (unlike all_args_here(), which performs this pre-check, colons_check() does not have to be run first in true_false_check())
    # detection of the T and F symbols used as names in the checked function code (formal name, loop variable, definition target), because the value usages of these symbols refer to user-defined objects and are not reported. Warning: conservative rule: if a name T or F is defined anywhere in the checked function code, all the value usages of this symbol are not reported, whatever the position in the code
    tf_names <- base::character(length = 0)
    term_nb <- base::length(x = term_rows)
    for(i in base::seq_len(length.out = term_nb)){
        k <- term_rows[i]
        if(pd$text[k] %in% base::c("T", "F")){
            # formal name: the symbol is defined as a formal of a function
            if(pd$token[k] == "SYMBOL_FORMALS"){
                tf_names <- base::c(tf_names, pd$text[k], recursive = TRUE, use.names = TRUE)
            # definition target (T <- ... or T = ...) or loop variable (for(T in ...)): the symbol is defined as a name. Warning: the symbol must not be a component or slot name, like in df$T <- 1, which is not a definition of a variable named T. Warning: with the -> and ->> operators, the definition target is located after the assignment operator, like in y -> T. Warning: the tokens $ and @ must be compared through their text (pd$text), because their token names are quoted ('$' and '@')
            }else if(pd$token[k] == "SYMBOL" & ((i < term_nb & pd$token[term_rows[i + 1]] %in% base::c("LEFT_ASSIGN", "EQ_ASSIGN", "IN")) | (i > 1 & pd$token[term_rows[i - 1]] == "RIGHT_ASSIGN")) & (i < 2 | ! (pd$text[term_rows[i - 1]] %in% base::c("$", "@")))){
                tf_names <- base::c(tf_names, pd$text[k], recursive = TRUE, use.names = TRUE)
            }
        }
    }
    # end detection of the T and F symbols used as names in the checked function code
    # scan of the T and F symbols used as values
    findings_line <- base::integer(length = 0)
    findings_symbol <- base::character(length = 0)
    findings_code <- base::character(length = 0)
    findings_col1 <- base::integer(length = 0)
    findings_col2 <- base::integer(length = 0)
    findings_tr <- base::integer(length = 0)
    # code not evaluated by R (quoting rule): only the code that R evaluates is checked, the rest being code used as data (e.g., the plotmath label expression(T[max])). State of each row of the parse data, determined from the top of the checked function, which is evaluated: 1 for the evaluated code (checked), 2 for the code of the expr argument of an evaluated bquote() (3 if its splice argument is written TRUE or T), 4 for the code that R does not evaluate. The code in states 2, 3 and 4 is not checked:
    # - in state 1, the arguments of quote(), expression() and alist(), the expr argument of substitute() and the formulas (~) are in state 4, and the expr argument of bquote() is in state 2 or 3. The env argument of substitute(), the where and splice arguments of bquote(), and the rest of the code stay in state 1
    # - in states 2 and 3, bquote() walks the whole expression, also inside the nested quote(), bquote() and formulas: the first argument of each .() call, and of each ..() call in state 3, is evaluated (state 1). The rest stays in state 2 or 3
    # - in state 4, nothing is evaluated, .() included
    # Same rule as in .functions_detect() (colons_check() and all_args_here()), curly_bracket_check(), true_false_check() and for_loop_nb_check(). Function names written alone, after base:: or base::: (quoting functions only), backquoted or as a string ("quote"(x) is a call of quote()). Warning: the string tokens are also used for the strings that are not function names (f("quote"), "quote" = 1): the function name token must be followed by the ( of its own call, i.e., a ( whose parent is the call expression, which is the parent of the expression of the function name. Arguments matched as R does (exact names, then partial names, then positions): if the matching fails (error of R), all the arguments are in state 4. Native pipe: x |> f(y) is f(x, y), or f(y) with x in place of the placeholder _ (e.g., x |> substitute(expr = a, env = _)), so that the piped value is moved as an argument of the call. Warning: the parse tree is described by the pd$id and pd$parent columns, whose values must be converted into pd row numbers through a lookup vector, because the two numbering systems are different (the id numbering skips the non terminal rows)
    id2row <- base::seq_len(length.out = base::nrow(x = pd))
    base::names(x = id2row) <- base::as.character(x = pd$id)
    parent_row <- id2row[base::as.character(x = pd$parent)] # row number of the parent of each parse tree node. Warning: NA for the top expressions (parent id 0) and for the root node
    quote_fun_names <- base::c("quote", "bquote", "expression", "alist", "substitute")
    tempo_fun_names <- base::c(quote_fun_names, ".", "..") # with the .() and ..() calls of bquote()
    tempo_quote <- "'" # single quote of the function names written as strings between single quotes
    tempo_fun_texts <- base::c(tempo_fun_names, base::paste0("`", tempo_fun_names, "`", collapse = NULL, recycle0 = FALSE), base::paste0("\"", tempo_fun_names, "\"", collapse = NULL, recycle0 = FALSE), base::paste0(tempo_quote, tempo_fun_names, tempo_quote, collapse = NULL, recycle0 = FALSE), recursive = TRUE, use.names = TRUE)
    tempo_rows <- base::which(x = pd$token %in% base::c("SYMBOL_FUNCTION_CALL", "STR_CONST") & pd$text %in% tempo_fun_texts, arr.ind = FALSE, useNames = TRUE)
    qc_row <- base::integer(length = 0) # rows of the call expressions of these functions
    qc_kind <- base::character(length = 0) # name of the function of each call
    for(r1 in base::seq_along(along.with = tempo_rows)){
        r <- tempo_rows[r1]
        tempo_pos <- base::match(x = r, table = term_rows, nomatch = NA_integer_, incomparables = NULL) # position of the function name token among the terminal tokens (never NA: the SYMBOL_FUNCTION_CALL and STR_CONST tokens are terminal tokens other than comments)
        if(tempo_pos < base::length(x = term_rows)){ # a string can be the last terminal token of the code (e.g., function(x) "quote"), but not a function name
            tempo_kind <- base::rep(x = tempo_fun_names, times = 4)[base::match(x = pd$text[r], table = tempo_fun_texts, nomatch = NA_integer_, incomparables = NULL)]
            tempo_sib_rows <- base::which(x = parent_row == parent_row[r] & pd$token != "COMMENT" & id2row != r, arr.ind = FALSE, useNames = TRUE) # other tokens of the expression of the function name, comments excluded: none, or the package and the :: or ::: (base::quote), but not a component or a call of another package (obj$quote, pkg::quote), nor base::.(x), which is not a .() call of bquote()
            if(pd$token[term_rows[tempo_pos + 1]] == "'('" & base::isTRUE(x = parent_row[term_rows[tempo_pos + 1]] == parent_row[parent_row[r]]) & base::all(pd$text[tempo_sib_rows] %in% base::c("base", "`base`", "::", ":::"), na.rm = FALSE) & (base::length(x = tempo_sib_rows) == 0 | tempo_kind %in% quote_fun_names)){
                qc_row <- base::c(qc_row, parent_row[parent_row[r]], recursive = TRUE, use.names = FALSE)
                qc_kind <- base::c(qc_kind, tempo_kind, recursive = TRUE, use.names = FALSE)
            }
        }
    }
    # arguments of these calls
    qa_call <- base::integer(length = 0) # position in qc_row of the call of each argument
    qa_row <- base::integer(length = 0) # row of the value of each argument (NA if empty, e.g., alist(a = ))
    qa_first <- base::logical(length = 0) # first argument of the call
    qa_state <- base::integer(length = 0) # state of the argument when the call of a quoting function is in state 1
    bfs_parent <- parent_row # parent rows of the parse tree walked below, the piped values being moved as children of the call
    for(c1 in base::seq_along(along.with = qc_row)){
        tempo_kids <- base::which(x = parent_row == qc_row[c1] & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
        tempo_kids <- tempo_kids[base::order(pd$line1[tempo_kids], pd$col1[tempo_kids], na.last = TRUE, decreasing = FALSE, method = "auto")]
        tempo_kids <- tempo_kids[-base::c(1, 2, base::length(x = tempo_kids))] # tokens between the ( and the ) of the call: the first two children are the expression of the function name and the (
        tempo_comma <- pd$token[tempo_kids] == "','"
        tempo_group <- base::cumsum(x = tempo_comma) # rank of the argument of each token, minus 1
        tempo_nb <- base::ifelse(test = base::length(x = tempo_kids) == 0, yes = 0, no = base::sum(tempo_comma, na.rm = FALSE) + 1) # number of arguments
        tempo_arg_names <- base::rep(x = "", times = tempo_nb) # "" for the arguments without name
        tempo_arg_rows <- base::rep(x = NA_integer_, times = tempo_nb)
        for(i2 in base::seq_len(length.out = tempo_nb)){
            tempo_g <- tempo_kids[tempo_group == i2 - 1 & ! tempo_comma] # tokens of the argument: none, value, name and =, or name, = and value
            if(base::length(x = tempo_g) >= 2){
                tempo_txt <- pd$text[tempo_g[1]]
                if(pd$token[tempo_g[1]] == "STR_CONST" | base::substr(x = tempo_txt, start = 1, stop = 1) == "`"){ # name written between quotes or backquotes
                    tempo_txt <- base::substr(x = tempo_txt, start = 2, stop = base::nchar(x = tempo_txt, type = "chars", allowNA = FALSE, keepNA = NA) - 1)
                }
                tempo_arg_names[i2] <- tempo_txt
                if(base::length(x = tempo_g) == 3){
                    tempo_arg_rows[i2] <- tempo_g[3]
                }
            }else if(base::length(x = tempo_g) == 1){
                tempo_arg_rows[i2] <- tempo_g
            }
        }
        # value piped into the call (x |> f(y)): first argument, or value of the argument written with the placeholder _
        tempo_pipe <- base::which(x = parent_row == parent_row[qc_row[c1]] & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE) # children of the expression containing the call: piped value, |> and call, if the call is at the right of a pipe
        tempo_pipe <- tempo_pipe[base::order(pd$line1[tempo_pipe], pd$col1[tempo_pipe], na.last = TRUE, decreasing = FALSE, method = "auto")]
        if(base::length(x = tempo_pipe) == 3){
            if(pd$token[tempo_pipe[2]] == "PIPE" & tempo_pipe[3] == qc_row[c1]){
                bfs_parent[tempo_pipe[1]] <- qc_row[c1]
                tempo_log <- tempo_arg_rows %in% parent_row[pd$token == "PLACEHOLDER"]
                if(base::any(tempo_log, na.rm = TRUE)){
                    tempo_arg_rows[tempo_log] <- tempo_pipe[1]
                }else{
                    tempo_arg_names <- base::c("", tempo_arg_names, recursive = TRUE, use.names = FALSE)
                    tempo_arg_rows <- base::c(tempo_pipe[1], tempo_arg_rows, recursive = TRUE, use.names = FALSE)
                    tempo_nb <- tempo_nb + 1
                }
            }
        }
        # state of each argument when the call is in state 1: 4 for the arguments of quote(), expression() and alist(), and for all the arguments when the matching fails (error of R)
        tempo_state <- base::rep(x = 4L, times = tempo_nb)
        if(qc_kind[c1] %in% base::c("bquote", "substitute")){
            if(qc_kind[c1] == "bquote"){
                tempo_formals <- base::c("expr", "where", "splice")
            }else{
                tempo_formals <- base::c("expr", "env")
            }
            tempo_match <- base::rep(x = NA_character_, times = tempo_nb) # formal argument matched by each argument
            tempo_error <- FALSE
            for(i2 in base::seq_along(along.with = tempo_formals)){ # exact names
                tempo_log <- tempo_arg_names == tempo_formals[i2]
                tempo_error <- tempo_error | base::sum(tempo_log, na.rm = FALSE) > 1
                tempo_match[tempo_log] <- tempo_formals[i2]
            }
            tempo_exact <- tempo_match
            tempo_partial <- base::which(x = tempo_arg_names != "" & base::is.na(x = tempo_exact), arr.ind = FALSE, useNames = TRUE)
            for(i2 in base::seq_along(along.with = tempo_partial)){ # partial names
                tempo_cand <- tempo_formals[base::startsWith(x = tempo_formals, prefix = tempo_arg_names[tempo_partial[i2]]) & ! tempo_formals %in% tempo_exact]
                if(base::length(x = tempo_cand) == 1 & ! base::any(tempo_cand %in% tempo_match, na.rm = FALSE)){
                    tempo_match[tempo_partial[i2]] <- tempo_cand
                }else{
                    tempo_error <- TRUE # unused argument, argument matching several formal arguments, or formal argument matched by several arguments
                }
            }
            tempo_free <- tempo_formals[ ! tempo_formals %in% tempo_match]
            tempo_log <- tempo_arg_names == "" # arguments without name, matched by position
            if(base::sum(tempo_log, na.rm = FALSE) > base::length(x = tempo_free)){
                tempo_error <- TRUE # unused argument
            }else{
                tempo_match[tempo_log] <- tempo_free[base::seq_len(length.out = base::sum(tempo_log, na.rm = FALSE))]
            }
            if(tempo_error == FALSE){
                tempo_state[tempo_match %in% base::c("where", "splice", "env")] <- 1L
                if(qc_kind[c1] == "bquote"){
                    tempo_kids <- base::which(x = parent_row %in% tempo_arg_rows[tempo_match == "splice" & ! base::is.na(x = tempo_arg_rows)] & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE) # token of the value of the splice argument
                    tempo_log <- base::length(x = tempo_kids) == 1 & base::any(base::paste0(pd$token[tempo_kids], pd$text[tempo_kids], collapse = NULL, recycle0 = FALSE) %in% base::c("NUM_CONSTTRUE", "SYMBOLT"), na.rm = FALSE) # splice argument written TRUE or T
                    tempo_state[tempo_match == "expr"] <- base::ifelse(test = tempo_log, yes = 3L, no = 2L)
                }
            }
        }
        qa_call <- base::c(qa_call, base::rep(x = c1, times = tempo_nb), recursive = TRUE, use.names = FALSE)
        qa_row <- base::c(qa_row, tempo_arg_rows, recursive = TRUE, use.names = FALSE)
        qa_first <- base::c(qa_first, base::seq_len(length.out = tempo_nb) == 1, recursive = TRUE, use.names = FALSE)
        qa_state <- base::c(qa_state, tempo_state, recursive = TRUE, use.names = FALSE)
    }
    # states, from the top expression (the function definition) to the leaves of the parse tree
    formula_rows <- base::c(parent_row[pd$terminal == TRUE & pd$text == "~"], parent_row[parent_row[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "`~`"]], recursive = TRUE, use.names = FALSE) # expressions of the formulas (y ~ x, ~ x or `~`(y, x))
    code_state <- base::rep(x = NA_integer_, times = base::nrow(x = pd))
    tempo_rows <- base::which(x = base::is.na(x = bfs_parent) & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
    code_state[tempo_rows] <- 1L
    while(base::length(x = tempo_rows) != 0){
        tempo_kids <- base::which(x = bfs_parent %in% tempo_rows, arr.ind = FALSE, useNames = TRUE)
        code_state[tempo_kids] <- code_state[bfs_parent[tempo_kids]] # state of the parent
        code_state[tempo_kids[bfs_parent[tempo_kids] %in% formula_rows & code_state[bfs_parent[tempo_kids]] == 1L]] <- 4L # children of a formula in state 1
        tempo_calls <- base::which(x = qc_row %in% tempo_rows, arr.ind = FALSE, useNames = TRUE) # calls whose arguments are among the children
        for(c1 in base::seq_along(along.with = tempo_calls)){
            tempo_log <- qa_call == tempo_calls[c1] & ! base::is.na(x = qa_row)
            if(code_state[qc_row[tempo_calls[c1]]] == 1L & qc_kind[tempo_calls[c1]] %in% quote_fun_names){
                code_state[qa_row[tempo_log]] <- qa_state[tempo_log]
            }else if((code_state[qc_row[tempo_calls[c1]]] == 2L & qc_kind[tempo_calls[c1]] == ".") | (code_state[qc_row[tempo_calls[c1]]] == 3L & qc_kind[tempo_calls[c1]] %in% base::c(".", ".."))){
                code_state[qa_row[tempo_log & qa_first]] <- 1L # first argument of .() and ..() evaluated by bquote()
            }
        }
        tempo_rows <- tempo_kids
    }
    quoted_rows <- base::which(x = code_state > 1L, arr.ind = FALSE, useNames = TRUE) # rows of the code not checked
    # help topics: all the symbols located inside the subtree of the help call (? or ??) are names, whatever their position (?T, ?(T), ?((T)), etc.) and whatever the srcref presence (without srcrefs, ?T is deparsed as `?`(T) and ?(T) as `?`((T)), so that the topic is always inside the subtree of the ? call), as the symbols of the code not evaluated by R (above). Warning: the token ? must be compared through its text, because its token name is quoted ('?')
    name_context_rows <- quoted_rows
    special_rows <- base::which(x = pd$terminal == TRUE & pd$text %in% base::c("?", "`?`", "??", "`??`"), arr.ind = FALSE, useNames = TRUE)
    for(r1 in base::seq_along(along.with = special_rows)){
        r <- special_rows[r1]
        start_row <- parent_row[r]
        if(pd$token[r] == "SYMBOL_FUNCTION_CALL" & ! base::is.na(x = start_row)){ # deparsed `?` call: the parent expression only contains the function name, the help topic is inside the call expression, i.e., the parent of the parent. Warning: start_row can only be NA (top expression) or a positive row number, because the parent id 0 is absent from the pd$id column
            start_row <- parent_row[start_row]
        }
        if(base::is.na(x = start_row) | start_row == 0){
            next # nocov # start_row is never NA or 0 here: the checked code is always a function definition, so that every ? token has a parent inside the function body (the parent id 0 only exists for top-level expressions, which cannot occur in a function body)
        }
        subtree_rows <- start_row
        frontier_rows <- start_row
        while(base::length(x = frontier_rows) != 0){
            children_rows <- base::which(x = parent_row %in% frontier_rows, arr.ind = FALSE, useNames = TRUE)
            children_rows <- children_rows[ ! children_rows %in% subtree_rows]
            subtree_rows <- base::c(subtree_rows, children_rows, recursive = TRUE, use.names = TRUE)
            frontier_rows <- children_rows
        }
        name_context_rows <- base::c(name_context_rows, subtree_rows, recursive = TRUE, use.names = TRUE)
    }
    # end detection of the code not evaluated by R and of the help topics
    tf_val_pos <- base::which(x = pd$token[term_rows] == "SYMBOL" & pd$text[term_rows] %in% base::c("T", "F"), arr.ind = FALSE, useNames = TRUE)
    for(i1 in base::seq_along(along.with = tf_val_pos)){
        i <- tf_val_pos[i1]
        k <- term_rows[i]
        if(pd$text[k] %in% tf_names){ # symbol used as a name elsewhere in the code (formal name, loop variable, definition target): skipped, because the value usages refer to user-defined objects
            next
        }
        prev_txt <- base::ifelse(test = i == 1, yes = "", no = pd$text[term_rows[i - 1]])
        if(prev_txt %in% base::c("$", "@")){ # component or slot name: skipped, because these are names. Warning: compared through the text, because the token names of $ and @ are quoted ('$' and '@')
            next
        }
        if(k %in% name_context_rows){ # formula symbol, help topic or quoted code: skipped, because these are names
            next
        }
        # whole assigned value (x <- T, x = T or T -> x): skipped by user choice. Detection through the parse tree: the expression made of the symbol alone (not a qualified constant like base::T) is a child of an assignment expression. Warning: an operand of the assigned value (z <- T & y or y & T -> z) is reported. Warning: the definition targets (T <- x, T = x and y -> T) are already collected in tf_names above
        tempo_expr_row <- parent_row[k] # row of the expression made of the symbol
        tempo_assign_row <- parent_row[tempo_expr_row] # row of the enclosing expression (NA for a top level expression)
        if( ! base::is.na(x = tempo_assign_row) & base::sum(parent_row == tempo_expr_row, na.rm = TRUE) == 1){
            if(base::any(pd$token[base::which(x = parent_row == tempo_assign_row, arr.ind = FALSE, useNames = TRUE)] %in% base::c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"), na.rm = TRUE)){
                next
            }
        }
        findings_line <- base::c(findings_line, pd$line1[k], recursive = TRUE, use.names = TRUE)
        findings_symbol <- base::c(findings_symbol, pd$text[k], recursive = TRUE, use.names = TRUE)
        findings_code <- base::c(findings_code, base::trimws(x = code[pd$line1[k]], which = "left", whitespace = "[ \\t]"), recursive = TRUE, use.names = TRUE)
        findings_col1 <- base::c(findings_col1, pd$col1[k], recursive = TRUE, use.names = TRUE)
        findings_col2 <- base::c(findings_col2, pd$col2[k], recursive = TRUE, use.names = TRUE)
        findings_tr <- base::c(findings_tr, i, recursive = TRUE, use.names = TRUE) # position i of the finding in term_rows, for the qualified-constant detection below
    }
    # end scan of the T and F symbols used as values
    # modification of the recovered code: exact replacement of the reported tokens (T -> TRUE, F -> FALSE), from the last token to the first one so that the columns of the preceding tokens remain valid. Warning: findings_* are ordered by source position, because term_rows is sorted and the loop appends in order. Warning: the multi-byte safety of the substr()/substr<-() based replacement has been verified (the col1/col2 columns of utils::getParseData() count characters, not bytes, in R >= 4.0.0)
    if(base::length(x = findings_line) != 0){
        modified_code <- code
        for(i1 in base::seq_along(along.with = findings_line)){
            i <- base::length(x = findings_line) - i1 + 1 # from the last finding to the first one
            # exact token replacement at the parse-data columns: T -> TRUE and F -> FALSE. Warning: the substr() slices are rebuilt from the left part + replacement + right part, so that the columns of the other tokens of the same line remain valid
            tempo_line <- modified_code[findings_line[i]]
            # qualified constants (like base::T or pkg:::T): the :: or ::: operator cannot precede a reserved constant (base::TRUE is a syntax error), so the whole pkg:: or pkg::: prefix is removed as well, writing plain TRUE/FALSE (identical values, because TRUE and FALSE are reserved constants that cannot be masked). Warning: the SYMBOL token of a qualified constant is always immediately preceded, in term_rows order, by the NS_GET or NS_GET_INT token of the :: or ::: operator, itself immediately preceded by the SYMBOL_PACKAGE token of the package name (verified on the R parser output)
            tempo_qual_start <- NA_integer_ # base::NA_integer_ does not exist: :: cannot precede a reserved constant
            if(findings_tr[i] > 1){
                if(pd$token[term_rows[findings_tr[i] - 1]] %in% base::c("NS_GET", "NS_GET_INT") & findings_tr[i] > 2){
                    if(pd$token[term_rows[findings_tr[i] - 2]] == "SYMBOL_PACKAGE"){
                        tempo_qual_start <- pd$col1[term_rows[findings_tr[i] - 2]]
                    }
                }
            }
            if(base::is.na(x = tempo_qual_start)){
                tempo_start <- findings_col1[i]
            }else{
                tempo_start <- tempo_qual_start
            }
            tempo_rep <- base::ifelse(test = findings_symbol[i] == "T", yes = "TRUE", no = "FALSE")
            modified_code[findings_line[i]] <- base::paste0(
                base::substr(x = tempo_line, start = 1, stop = tempo_start - 1),
                tempo_rep,
                base::substr(x = tempo_line, start = findings_col2[i] + 1, stop = base::nchar(x = tempo_line, type = "chars", allowNA = FALSE, keepNA = NA)),
                collapse = NULL,
                recycle0 = FALSE
            )
        }
        # the modified code must parse, otherwise an internal error is returned (a modified code that does not parse would indicate a replacement bug)
        # nocov start
        # codecov inactivated because it is an internal control of the replacement, impossible to cover with argument values.
        if(base::inherits(x = base::try(expr = base::parse(text = base::paste0(modified_code, collapse = "\n", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), silent = TRUE, outFile = base::getOption(x = "try.outFile", default = base::stderr())), what = "try-error", which = FALSE)){
            tempo_cat <- base::paste0(
                intern_error_text_start,
                "THE CODE OF ",
                base::as.character(x = arg_user_setting$x),
                " MODIFIED BY THE T -> TRUE AND F -> FALSE REPLACEMENTS DOES NOT PARSE ANYMORE.\n\nTHIS IS AN INTERNAL BUG: PLEASE, DO NOT USE THE MODIFIED CODE (NOR THE EXPORTED FILE) AND REPORT IT.",
                intern_error_text_end,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        # nocov end
    }else{
        modified_code <- NULL
    }
    # end modification of the recovered code
    # message
    tempo_cat <- base::paste0(
        "AFTER RUNNING ",
        base::ifelse(
            test = base::is.null(x = package_name),
            yes = "",
            no = base::paste0(
                package_name,
                base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"),
                collapse = NULL,
                recycle0 = FALSE
            )
        ),
        function_name,
        ".",
        collapse = NULL,
        recycle0 = FALSE
    )
    if(base::length(x = findings_line) == 0){
        tempo_cat <- base::paste0(
            tempo_cat,
            "\nINSIDE ",
            base::as.character(x = arg_user_setting$x),
            ", EVERYTHING SEEMS CLEAN.",
            collapse = NULL,
            recycle0 = FALSE
        )
    }else{
        tempo_cat <- base::paste0(
            "\nINSIDE ",
            base::as.character(x = arg_user_setting$x),
            ", SOME SYMBOLS T OR F MUST BE WRITTEN TRUE OR FALSE:\n\n",
            "LINE\tSYMBOL\t\tCODE\n",
            base::paste0(base::paste0(findings_line, "\t", findings_symbol, "\t\t", findings_code, collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        if(export == TRUE){
            # the file is written BEFORE the message is printed, so that the message only claims an export that actually happened. Warning: the first line of the recovered code always starts by the function writing (fun <- function...), possibly indented
            export_code <- modified_code
            export_code[1] <- base::paste0(out_fun_name, "_safer <- ", export_code[1], collapse = NULL, recycle0 = FALSE)
            utils::write.table(x = export_code, file = out_file_path, row.names = FALSE, col.names = FALSE, append = FALSE, quote = FALSE, sep = "", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "") # write each line of the modified code, without any quoting, in a plain text R script
            # the export message is printed with on.exit(), after the message of the main code: it only claims an export that actually happened, because the file writing is performed just above and no on.exit() is registered when no modification is inserted
            base::on.exit(expr = base::message(base::paste0("MODIFIED CODE EXPORTED IN:\n", base::normalizePath(path = out_file_path, winslash = "/", mustWork = FALSE), "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }
    }
    base::message(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    if(base::length(x = findings_line) == 0){
        output <- NULL
    }else if(export == TRUE){
        output <- base::invisible(x = NULL) # the modified code has already been written in the exported file above
    }else{
        # warning: keep.source = TRUE and an explicit srcfile are required to keep the source references (attribute srcref) of the returned function, i.e., its comments and indentation when printed. With srcfile = NULL, or with keep.source = FALSE (default of Rscript), the returned function is printed deparsed: comments removed and code reindented
        output <- base::eval(expr = base::parse(text = base::paste0(modified_code, collapse = "\n", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = modified_code, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        # restore the environment of the submitted function, as curly_bracket_check() and lambda_check() do: without this, the modified function would keep the evaluation environment of true_false_check() as closure environment
        base::environment(fun = output) <- base::environment(fun = x)
    }
    if(base::is.null(x = output)){
        base::return(base::invisible(x = output)) # NULL returned invisibly when nothing is modified, or when export is TRUE (the invisibility is lost when assigned to output, hence invisible() here)
    }else{
        base::return(output)
    }
    #### end output

}
