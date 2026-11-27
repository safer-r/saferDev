#' @title Check and Add Curly Brackets Around Block Bodies
#' @description
#' Checks that the bodies of \code{if}, \code{else}, \code{for}, \code{while}, \code{repeat} and \code{function} (lambdas \code{\\(x)} included) are delimited by curly brackets (e.g., \code{if(x > 1)\{ x \}} and not \code{if(x > 1) x}). Prints a table of the missing curly brackets and returns a corrected version of the function.
#' @param x Function to check, written without quotes and brackets (e.g., \code{fun1} or \code{pkg::fun1}).
#' @param export Single logical value. If \code{TRUE}, the corrected function is written into the \code{<name>_safer.R} file of \code{out_path} instead of being returned (e.g., \code{fun1_safer.R} for \code{x = fun1} or \code{x = pkg::fun1}), as \code{fun1_safer <- function(...)}, which \code{base::source()} recreates. Nothing is written if nothing has to be corrected, but an error is returned if the file already exists, or if \code{x} is not a function name (e.g., \code{function(x)\{x\}} or \code{FUN_LIST$fun}).
#' @param out_path Single character string: path of an existing folder where the file is written. Required when \code{export} is \code{TRUE}; otherwise not used, but a non-\code{NULL} value must still be a single non-empty character string. \code{NULL} by default.
#' @inheritParams is_function_here
#' @returns
#' The corrected function, which keeps the environment of \code{x}, or \code{NULL}, invisibly, if nothing has to be corrected or if \code{export} is \code{TRUE}.
#'
#' A message is also printed: a table with one row per missing \code{\{ \}} (column 1: line number; column 2: construct; column 3: code of the line), or a sentence saying that everything seems clean.
#' @details
#' Comments are ignored. In \code{else if} chains, the \code{else} is not reported: the chained \code{if} is checked on its own.
#'
#' Bodies reduced to \code{.Primitive(...)}, as in \code{base::`+`}, are skipped, because they cannot be enclosed in curly brackets. Bodies made of \code{UseMethod(...)}, as in \code{base::mean}, are reported.
#'
#' Constructs written with backquotes, like \code{`if`(x > 1, x, 0)}, are not checked: use the standard writing. The constructs inside formulas, \code{quote()}, \code{bquote()}, \code{expression()}, \code{alist()} and \code{substitute()}, whose code R does not evaluate as written (e.g., \code{quote(if(a) b)}), are not checked either, except in the parts that R evaluates: \code{.()} in \code{bquote()} (\code{..()} too if \code{splice = TRUE} is written), and the \code{where} and \code{splice} arguments of \code{bquote()} and \code{env} of \code{substitute()}.
#'
#' Correction: a \code{\{} is added before each reported body and a \code{\}} after it, separated from the body by a space. The rest of the code is unchanged.
#'
#' Source references: line 1 is the line of the \code{function} keyword. Comments and indentation are kept only if \code{x} carries its source references (attribute \code{srcref}). Functions created with \code{options(keep.source = FALSE)} (default of Rscript) and functions of installed packages (e.g., \code{saferDev::colons_check}) have none: their deparsed code is analyzed, without comments and with line numbers that differ from the source file.
#' @seealso The code checkers of saferDev, in the recommended order of use: \code{\link{super_assign_check}}, \code{\link{lambda_check}}, \code{\link{colons_check}}, \code{\link{all_args_here}}, \code{\link{curly_bracket_check}}, \code{\link{true_false_check}}, \code{\link{for_loop_nb_check}} (see \code{vignette("saferDev")}).
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @examples
#' # Without source references ("Run examples" link, Rscript), line numbers and comments differ.
#' # See also https://safer-r.github.io/saferDev/articles/curly_bracket_check.html
#' fun1 <- function(x){ if(x > 1){ base::return(x) } }
#' saferDev::curly_bracket_check(x = fun1, export = FALSE, out_path = NULL, safer_check = TRUE,
#'     lib_path = NULL, error_text = "") # nothing to correct: NULL returned, invisibly
#' fun2 <- function(x){ if(x > 1) base::return(x) }
#' fun2_safer <- saferDev::curly_bracket_check(x = fun2, export = FALSE, out_path = NULL,
#'     safer_check = TRUE, lib_path = NULL, error_text = "") # missing { } reported
#' fun2_safer # corrected function
#' # Write the corrected function in the fun2_safer.R file of a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::curly_bracket_check(x = fun2, export = TRUE, out_path = tempo_dir, safer_check = TRUE,
#'     lib_path = NULL, error_text = "")
#' base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
#'
#' @export
curly_bracket_check <- function(
    x, 
    export = FALSE, 
    out_path = NULL, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # x = mean ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "curly_bracket_check" ; arg_user_setting = base::list(x = as.name(x = "mean"), export = FALSE, out_path = NULL, lib_path = NULL, safer_check = TRUE, error_text = " INSIDE A")
    # x = .base_op_check ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # internal function with function body made of a single .Primitive() call
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\curly_bracket_check.R") ; x = curly_bracket_check ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::curly_bracket_check does not return the same number of code lines

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
        "safer_check"
        # "out_path", # inactivated because can be NULL
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
        "x", 
        "safer_check", 
        "export", 
        "out_path", 
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
                "saferDev:::.base_op_check",
                "saferDev:::.find_construct_body"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = -1, envir = )
    .find_construct_body <- utils::getFromNamespace(x = ".find_construct_body", ns = "saferDev", pos = -1, envir = )

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
    if( ! base::is.null(x = out_path)){ # out_path can be NULL
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
    # x forwarded through the ... argument of another function (e.g., fun2 <- function(...){saferDev::curly_bracket_check(...)} called with x = fun1): base::match.call() writes x ..1 (or ..2, etc.) in arg_user_setting, instead of its writing in the call. This writing is recovered from the promise of x by base::substitute() (x is not reassigned before this line)
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
        # determination of the file name: name of the submitted function, as written by the user, with _safer added at the end
        # arg_user_setting$x is x as written in the call: a name (as.name(x = "test")), a call (like the parsed call "saferDev::test"), or a function object when the call holds the function itself (e.g., x = fun1 in the args argument of base::do.call()). The function object is tested first, because base::as.character() cannot convert it
        if(( ! base::is.function(x = arg_user_setting$x)) && (base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::")){
            tempo_fun_name <- base::as.character(x = arg_user_setting$x)[3] # form "package::function" or "package:::function"
        }else if(base::is.symbol(x = arg_user_setting$x)){
            tempo_fun_name <- base::as.character(x = arg_user_setting$x)[1]
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
        out_name <- base::paste0(tempo_fun_name, "_safer.R", collapse = NULL, recycle0 = FALSE)
        if(base::file.exists(base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE))){
            tempo_cat <- base::paste0(
                error_text_start,
                "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n",
                base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE),
                "\n\nPLEASE, RENAME OR REMOVE THIS FILE, OR CHANGE THE out_path ARGUMENT, AND RERUN ",
                package_function_name,
                ".",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
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
    # terminal tokens (comments excluded), sorted by source position
    term_rows <- base::which(x = pd$terminal == TRUE & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
    term_rows <- term_rows[base::order(pd$line1[term_rows], pd$col1[term_rows], na.last = TRUE, decreasing = FALSE, method = "auto")]
    # positions in term_rows of the terminal tokens of the checked constructs (keyword tokens). Warning: the lambda function \(x) gives the token made of a backslash between single quotes (and not the LAMBDA token) in the R versions checked so far
    # warning: the name of this token is built by pieces, as in lambda_check(), because a single string mixing single quotes and backslashes is misread by the in-quotes replacement of all_args_here() (INTERNAL ERROR 3 of .fun_args_pos())
    token_quote1 <- "'"
    token_bs <- "\\\\"
    token_quote2 <- "'"
    lambda_token_name <- base::paste0(token_quote1, token_bs, token_quote2, collapse = NULL, recycle0 = FALSE)
    kid_idx <- base::which(x = pd$token[term_rows] %in% base::c("IF", "FOR", "WHILE", "REPEAT", "FUNCTION", "LAMBDA", lambda_token_name), arr.ind = FALSE, useNames = TRUE)
    # code not evaluated by R (quoting rule): only the code that R evaluates is checked, the rest being code used as data (e.g., quote(if(a) b)). State of each row of the parse data, determined from the top of the checked function, which is evaluated: 1 for the evaluated code (checked), 2 for the code of the expr argument of an evaluated bquote() (3 if its splice argument is written TRUE or T), 4 for the code that R does not evaluate. The code in states 2, 3 and 4 is not checked:
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
    kid_idx <- kid_idx[ ! term_rows[kid_idx] %in% quoted_rows] # keyword tokens of the code not checked removed
    findings_line <- base::integer(length = 0)
    findings_kind <- base::character(length = 0)
    findings_code <- base::character(length = 0)
    mods_line <- base::integer(length = 0) # line of the construct body start (for the modified function: insertion of "{ " before) and of the body end (insertion of " }" after)
    mods_col <- base::integer(length = 0) # parse-data columns of the same positions (converted below into character positions of the code lines before substr(), since utils::getParseData() counts a TAB as a jump to the next multiple of 8)
    mods_kind <- base::character(length = 0) # "before" (insert "{ ") or "after" (insert " }")
    for(i1 in base::seq_along(along.with = kid_idx)){
        i <- kid_idx[i1]
        kid <- term_rows[i]
        if(pd$token[kid] != "REPEAT"){
            # search of the parentheses of the condition (if, for, while) or of the arguments (function), by scanning the terminal tokens located after the keyword in source order. Warning: the search cannot be done through the parse tree, because the forcond expr of for() owns the parentheses, and because the structure of lambda functions is different
            depth <- 0
            lp <- NULL
            rp <- NULL
            if(i < base::length(x = term_rows)){ # guard: no scan if the keyword token were the last terminal token (unreachable with parseable R code, but secured)
                for(j1 in base::seq_len(length.out = base::length(x = term_rows) - i)){ # j from i + 1 to the last terminal token
                    j <- i + j1
                    if(pd$token[term_rows[j]] == "'('"){
                        depth <- depth + 1
                        if(depth == 1){
                            lp <- term_rows[j]
                        }
                    }else if(pd$token[term_rows[j]] == "')'"){
                        depth <- depth - 1
                        if(depth == 0){
                            rp <- term_rows[j]
                            break
                        }
                    }
                }
            }
            if(base::is.null(x = lp) == TRUE || base::is.null(x = rp) == TRUE){
                # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without () after if, for, while and function cannot be parsed).
                tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "NO () FOUND AFTER THE KEYWORD TOKEN\ntoken: ", pd$token[kid], intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                # nocov end
            }
            body_start <- rp
        }else{
            body_start <- kid
        }
        body_start_pos <- base::which(x = term_rows == body_start, arr.ind = FALSE, useNames = TRUE)[1]
        if(body_start_pos == base::length(x = term_rows)){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without body after if, else, for, while, repeat and function cannot be parsed).
            tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "NO BODY FOUND AFTER THE KEYWORD TOKEN\ntoken: ", pd$token[kid], intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        ft_token <- pd$token[term_rows[body_start_pos + 1]] # first terminal token (comments excluded) of the body
        ft_text <- pd$text[term_rows[body_start_pos + 1]]
        # detection of bodies made of a single .Primitive() call: these bodies are skipped, because primitive functions cannot be delimited by curly brackets. The .Primitive() call must be the whole body: the terminal token following the ) of the .Primitive() call must belong to another expr (otherwise, the body contains more code, like in function(x) .Primitive('+') + 1, which must be reported)
        prim_log <- FALSE
        if((pd$token[kid] == "FUNCTION" || pd$token[kid] == "LAMBDA" || pd$token[kid] == lambda_token_name) && ft_token == "SYMBOL_FUNCTION_CALL" && ft_text == ".Primitive"){
            prim_call_close_pos <- body_start_pos + 1
            depth <- 0
            k <- body_start_pos + 1 # the scan includes the .Primitive token itself, so that the ( of the call is found
            while(k <= base::length(x = term_rows)){
                if(pd$token[term_rows[k]] == "'('"){
                    depth <- depth + 1
                }else if(pd$token[term_rows[k]] == "')'"){
                    depth <- depth - 1
                    if(depth == 0){
                        prim_call_close_pos <- k
                        break
                    }
                }
                k <- k + 1
            }
            if(depth != 0){ # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code with unbalanced parentheses cannot be parsed).
                tempo_cat <- base::paste0("INTERNAL ERROR 5 IN ", intern_error_text_start, "UNBALANCED PARENTHESES DETECTED AFTER .Primitive", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                # nocov end
            }else{
                # the whole body expr is the largest expr (or expr_or_assign_or_help, the token of a body written with =) starting at the first body terminal token. If its end is the ) of the .Primitive() call, the body is exactly the .Primitive() call (e.g., function(x) .Primitive('+')). Otherwise, the body contains more code (e.g., function(x) .Primitive('+') + 1 or function(x) .Primitive('+') = 1, which must be reported)
                body_term_pos1 <- pd$line1[term_rows[body_start_pos + 1]] * 1000000 + pd$col1[term_rows[body_start_pos + 1]]
                cand <- pd[base::which(x = pd$token %in% base::c("expr", "expr_or_assign_or_help") & pd$line1 * 1000000 + pd$col1 == body_term_pos1, arr.ind = FALSE, useNames = TRUE), ]
                body_end_pos <- cand[base::order(cand$line2 * 1000000 + cand$col2, na.last = TRUE, decreasing = TRUE, method = "auto"), ][1, "line2"] * 1000000 + cand[base::order(cand$line2 * 1000000 + cand$col2, na.last = TRUE, decreasing = TRUE, method = "auto"), ][1, "col2"]
                prim_log <- body_end_pos == pd$line1[term_rows[prim_call_close_pos]] * 1000000 + pd$col1[term_rows[prim_call_close_pos]]
            }
        }
        if(prim_log == FALSE){
            if(ft_token != "'{'"){
                findings_line <- base::c(findings_line, pd$line1[kid], recursive = TRUE, use.names = TRUE)
                findings_kind <- base::c(findings_kind, base::ifelse(test = pd$token[kid] %in% base::c("LAMBDA", lambda_token_name), yes = "function", no = pd$text[kid]), recursive = TRUE, use.names = TRUE)
                findings_code <- base::c(findings_code, base::trimws(x = code[pd$line1[kid]], which = "left", whitespace = "[ \\t]"), recursive = TRUE, use.names = TRUE)
                tempo_body <- .find_construct_body(pd = pd, kid = kid, body_start = body_start, body_start_pos = body_start_pos, term_rows = term_rows, else_branch = FALSE, else_tok = NULL, lib_path = lib_path, error_text = embed_error_text)
                mods_line <- base::c(mods_line, tempo_body$line1, pd$line2[tempo_body$row], recursive = TRUE, use.names = TRUE)
                mods_col <- base::c(mods_col, tempo_body$col1, pd$col2[tempo_body$row], recursive = TRUE, use.names = TRUE)
                mods_kind <- base::c(mods_kind, "before", "after", recursive = TRUE, use.names = TRUE)
            }
        }
        # else branch of if(), detected through the parse tree (the else of an if is a sibling token of this if). Warning: an else followed by an if() is not reported, because the chained if() is checked on its own
        if(pd$token[kid] == "IF"){
            else_kids <- base::which(x = pd$parent == pd$parent[kid] & pd$token == "ELSE", arr.ind = FALSE, useNames = TRUE)
            if(base::length(x = else_kids) != 0){
                else_tok <- else_kids[1]
                else_pos <- base::which(x = term_rows == else_tok, arr.ind = FALSE, useNames = TRUE)[1]
                if(else_pos == base::length(x = term_rows)){
                    # nocov start
                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without body after else cannot be parsed).
                    tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "NO BODY FOUND AFTER THE ELSE TOKEN", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    # nocov end
                }
                ft2_token <- pd$token[term_rows[else_pos + 1]] # first terminal token (comments excluded) of the else body
                if(ft2_token != "IF"){
                    if(ft2_token != "'{'"){
                        findings_line <- base::c(findings_line, pd$line1[else_tok], recursive = TRUE, use.names = TRUE)
                        findings_kind <- base::c(findings_kind, "else", recursive = TRUE, use.names = TRUE)
                        findings_code <- base::c(findings_code, base::trimws(x = code[pd$line1[else_tok]], which = "left", whitespace = "[ \\t]"), recursive = TRUE, use.names = TRUE)
                        tempo_body <- .find_construct_body(pd = pd, kid = kid, body_start = else_tok, body_start_pos = else_pos, term_rows = term_rows, else_branch = TRUE, else_tok = else_tok, lib_path = lib_path, error_text = embed_error_text)
                        mods_line <- base::c(mods_line, tempo_body$line1, pd$line2[tempo_body$row], recursive = TRUE, use.names = TRUE)
                        mods_col <- base::c(mods_col, tempo_body$col1, pd$col2[tempo_body$row], recursive = TRUE, use.names = TRUE)
                        mods_kind <- base::c(mods_kind, "before", "after", recursive = TRUE, use.names = TRUE)
                    }
                }
            }
        }
    }
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
            ", SOME CURLY BRACKETS {} ARE MISSING AFTER:\n\n", 
            "LINE\tCONSTRUCT\t\tCODE\n", 
            base::paste0(base::paste0(findings_line, "\t", findings_kind, "\t\t", findings_code, collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE),
            collapse = NULL, 
            recycle0 = FALSE
        )
    }
    base::message(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    # modified function
    output <- NULL # NULL if nothing to modify (the submitted function is already compliant), or if the modified function is exported
    if(base::length(x = findings_line) != 0){
        # build the modified code: insert "{ " before each construct body and " }" after, from the end of the code to the beginning (so that the positions of the not-yet-modified parts stay valid). The "after" modifications are applied before the "before" ones at the same position (the type key of the sort: 1 for "after", 0 for "before")
        mod_code <- code
        if(base::length(x = mods_line) != 0){
            mod_type_key <- base::ifelse(test = mods_kind == "after", yes = 1L, no = 0L)
            mod_ord <- base::order(mods_line, mods_col, mod_type_key, decreasing = TRUE, method = "radix", na.last = NA) # multi-key sort: no arithmetic key, to avoid integer overflow on long functions (line * 1e8 overflows R integers for line > 21)
            for(k1 in base::seq_along(along.with = mod_ord)){
                k <- mod_ord[k1]
                s <- mod_code[mods_line[k]]
                # conversion of the parse-data column into the character position used by substr(): utils::getParseData() counts a TAB as a jump to the next multiple of 8, and any other character as 1. The TABs of the submitted code are kept. The columns are computed on the submitted line (code), identical to s before the insertion point, since the insertions are performed from the end of the line to its beginning
                tempo_chars <- base::strsplit(x = code[mods_line[k]], split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
                tempo_cols <- base::integer(length = base::length(x = tempo_chars)) # parse-data column of each character of the line
                tempo_col <- 0L
                for(i2 in base::seq_along(along.with = tempo_chars)){
                    if(tempo_chars[i2] == "\t"){
                        tempo_col <- (tempo_col %/% 8L + 1L) * 8L # jump to the next multiple of 8
                    }else{
                        tempo_col <- tempo_col + 1L
                    }
                    tempo_cols[i2] <- tempo_col
                }
                char_pos <- base::match(x = mods_col[k], table = tempo_cols, nomatch = NA_integer_, incomparables = NULL)
                if(base::is.na(x = char_pos)){
                    # nocov start
                    # codecov inactivated because it is an internal control of code writing: the parse-data columns come from the parsing of these code lines, so that each of them is the column of a character of the line.
                    tempo_cat <- base::paste0("INTERNAL ERROR 8 IN ", intern_error_text_start, "PARSE-DATA COLUMN NOT FOUND IN THE CODE LINE\nline: ", mods_line[k], "\ncolumn: ", mods_col[k], intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    # nocov end
                }
                if(mods_kind[k] == "after"){
                    mod_code[mods_line[k]] <- base::paste0(base::substr(x = s, start = 1, stop = char_pos), " }", base::substr(x = s, start = char_pos + 1, stop = base::nchar(x = s, type = "chars", allowNA = FALSE, keepNA = NA)), collapse = NULL, recycle0 = FALSE)
                }else{
                    mod_code[mods_line[k]] <- base::paste0(base::substr(x = s, start = 1, stop = char_pos - 1), "{ ", base::substr(x = s, start = char_pos, stop = base::nchar(x = s, type = "chars", allowNA = FALSE, keepNA = NA)), collapse = NULL, recycle0 = FALSE)
                }
            }
        }
        mod_text <- base::paste0(mod_code, collapse = "\n", recycle0 = FALSE)
        # validation of the modified code: it must parse
        # warning: keep.source = TRUE and an explicit srcfile are required to keep the source references (attribute srcref) of the returned function, i.e., its comments and indentation when printed. With srcfile = NULL, or with keep.source = FALSE (default of Rscript), the returned function is printed deparsed: comments removed and code reindented
        parsed_mod <- base::try(expr = base::parse(text = mod_text, file = "", n = NULL, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = mod_text, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), silent = TRUE, outFile = base::getOption(x = "try.outFile", default = base::stderr()))
        parse_ok <- base::inherits(x = parsed_mod, what = "try-error", which = FALSE) == FALSE
        if(parse_ok == FALSE){
            # nocov start
            # codecov inactivated because the modifications above are driven by the parse tree of the submitted code, so the modified code always parses (this branch is unreachable unless the parser itself is inconsistent).
            tempo_cat <- base::paste0("INTERNAL ERROR 6 IN ", intern_error_text_start, "THE MODIFIED CODE CANNOT BE PARSED\n", intern_error_text_end, collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        # name of the modified function: the file name (out_name, "<FUNCTION_NAME>_safer.R") and the assignment name (tempo_fun_name, <FUNCTION_NAME>) have already been determined in the other checkings section when export == TRUE. When export == FALSE, no file is written, so no name is needed
        if(base::length(x = parsed_mod) == 1){
            mod_fun <- base::eval(expr = parsed_mod[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # evaluates the assignment fun <- function(...)... : returns the assigned function
        }else{
            # nocov start
            # codecov inactivated because it is an internal control of code writing: the recovered code of a function is exactly one expression (the function definition), its attributes, printed after its code, being refused above, so that the multi-expression branch cannot be reached with argument values. Several expressions are not evaluated: the last one would not be the function (e.g., the value of an attribute, whose code would be run)
            tempo_cat <- base::paste0("INTERNAL ERROR 7 IN ", intern_error_text_start, "THE MODIFIED CODE IS MADE OF SEVERAL EXPRESSIONS\n", intern_error_text_end, collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        # restore the environment of the submitted function: without this, the modified function would keep the evaluation environment of curly_bracket_check() as closure environment, which would break closures of the submitted function and could resolve names wrongly
        base::environment(fun = mod_fun) <- base::environment(fun = x)
        if(export == TRUE){
            # file content: assignment of the modified function, so that base::source()ing the file recreates it. The file name (out_name) and the assignment name (tempo_fun_name) have been determined in the other checkings section
            # the modified code lines (mod_text) are written, and not the deparsed function, so that the comments and the indentation of the submitted function are kept
            file_text <- base::paste0(tempo_fun_name, "_safer <- ", mod_text, "\n", collapse = NULL, recycle0 = FALSE)
            file_path <- base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE)
            base::cat(file_text, file = file_path, sep = "", fill = FALSE, labels = NULL, append = FALSE)
            # the export message is printed with on.exit(), after the message of the main code: it only claims an export that actually happened, because the file writing is performed just above and no on.exit() is registered when no modification is inserted
            base::on.exit(expr = base::message(base::paste0("MODIFIED CODE EXPORTED IN:\n", base::normalizePath(path = file_path, winslash = "/", mustWork = FALSE), "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
        }else{
            output <- mod_fun
        }
    }
    # end modified function
    #### end main code

    #### warning output
    #### end warning output

    #### output
    if(base::is.null(x = output)){
        base::return(value = base::invisible(x = NULL)) # NULL returned invisibly (the invisibility would be lost if assigned to an object first)
    }else{
        base::return(value = output)
    }
    #### end output
}
