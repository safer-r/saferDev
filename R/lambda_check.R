#' @title Check and Rewrite the \code{\\(x)} Lambda Functions
#' @description
#' Checks that the anonymous functions are written \code{function(x)} and not with the \code{\\(x)} shorthand (e.g., \code{function(x) x * 2} and not \code{\\(x) x * 2}). Prints a table of the reported lambdas and returns a corrected version of the function.
#' @param x Function to check, written without quotes and brackets (e.g., \code{fun1} or \code{pkg::fun1}).
#' @param export Single logical value. If \code{TRUE}, the corrected function is written into the \code{<name>_safer.R} file of \code{out_path} instead of being returned (e.g., \code{fun1_safer.R} for \code{x = fun1} or \code{x = pkg::fun1}), as \code{fun1_safer <- function(...)}, which \code{base::source()} recreates. Nothing is written if nothing has to be corrected, but an error is returned if the file already exists, or if \code{x} is not a function name (e.g., \code{function(x)\{x\}} or \code{FUN_LIST$fun}).
#' @param out_path Single character string: path of an existing folder where the file is written. Required when \code{export} is \code{TRUE}; otherwise not used, but a non-\code{NULL} value must still be a single non-empty character string. \code{NULL} by default.
#' @inheritParams is_function_here
#' @returns
#' The corrected function, which keeps the environment of \code{x}, or \code{NULL}, invisibly, if nothing has to be corrected or if \code{export} is \code{TRUE}.
#'
#' A message is also printed: a table with one row per reported lambda (column 1: line number; column 2: symbol; column 3: code of the line), or a sentence saying that everything seems clean.
#' @details
#' Source references: line 1 is the line of the \code{function} keyword. The \code{\\(x)} writing is only kept in a function that carries its source references (attribute \code{srcref}), e.g., parsed with \code{keep.source = TRUE} (\code{base::parse()}, \code{base::source()}). Functions created with \code{options(keep.source = FALSE)} (default of Rscript) and functions of installed packages (e.g., \code{saferDev::colons_check}) have none: their deparsed code shows \code{function(x)}, so that nothing can be reported.
#'
#' Reported: the lambdas written \code{\\(x)}, like \code{\\(x) x * 2}, \code{\\(x)\{ x \}} or \code{\\() base::mean(1:3)}, wherever they are written in the function. In the printed table, the symbol is \code{\\()} when the formals of the lambda span several lines.
#'
#' Not reported: the standard \code{function(x)} writing (an unbraced body like \code{function(x) x} is reported by \code{curly_bracket_check()}). A \code{\\(} inside a string, a comment or a backquoted name is ignored.
#'
#' Correction: the \code{\\} of each reported lambda is replaced by \code{function} (e.g., \code{\\(x) x * 2} becomes \code{function(x) x * 2}). The rest of the code, comments, indentation and lambda bodies included, is unchanged.
#' @seealso The code checkers of saferDev, in the recommended order of use: \code{\link{super_assign_check}}, \code{\link{lambda_check}}, \code{\link{colons_check}}, \code{\link{all_args_here}}, \code{\link{curly_bracket_check}}, \code{\link{true_false_check}}, \code{\link{for_loop_nb_check}} (see \code{vignette("saferDev")}).
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @examples
#' # See also https://safer-r.github.io/saferDev/articles/lambda_check.html
#' # The \\(x) writing is only kept in a function carrying its source references: hence
#' # keep.source = TRUE, and srcfile left empty (srcfile = NULL would remove them).
#' code1 <- "function(x){ base::sapply(X = 1:3, FUN = function(y){ y * 2 }) }"
#' fun1 <- base::eval(expr = base::parse(file = "", n = NULL, text = code1, prompt = "?",
#'     keep.source = TRUE, srcfile = , encoding = "unknown"), envir = , enclos = )
#' saferDev::lambda_check(x = fun1, export = FALSE, out_path = NULL, safer_check = TRUE,
#'     lib_path = NULL, error_text = "") # nothing to correct: NULL returned, invisibly
#' code2 <- "function(x){ base::sapply(X = 1:3, FUN = \\(y){ y * 2 }) }"
#' fun2 <- base::eval(expr = base::parse(file = "", n = NULL, text = code2, prompt = "?",
#'     keep.source = TRUE, srcfile = , encoding = "unknown"), envir = , enclos = )
#' fun2_safer <- saferDev::lambda_check(x = fun2, export = FALSE, out_path = NULL,
#'     safer_check = TRUE, lib_path = NULL, error_text = "") # lambda writing reported
#' fun2_safer # corrected function
#' # A backslash followed by ( in a string, a raw string, a comment or a backquoted name
#' # is not a lambda writing: not reported, not modified.
#' code3 <- base::paste0(
#'     "function(x){ y <- \"the text \\\\(x) is inside a string\" ; ",
#'     "z <- r\"(raw \\(x))\" ; # a \\(x) in a comment\n    `\\\\(w)` <- 1 ; x }",
#'     collapse = NULL, recycle0 = FALSE
#' )
#' fun3 <- base::eval(expr = base::parse(file = "", n = NULL, text = code3, prompt = "?",
#'     keep.source = TRUE, srcfile = , encoding = "unknown"), envir = , enclos = )
#' saferDev::lambda_check(x = fun3, export = FALSE, out_path = NULL, safer_check = TRUE,
#'     lib_path = NULL, error_text = "") # nothing to correct: NULL returned, invisibly
#' # Write the corrected function in the fun2_safer.R file of a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::lambda_check(x = fun2, export = TRUE, out_path = tempo_dir, safer_check = TRUE,
#'     lib_path = NULL, error_text = "")
#' base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
#'
#' @export
lambda_check <- function(
    x,
    export = FALSE,
    out_path = NULL,
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){
    # DEBUGGING
    # x = fun1 ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "lambda_check" ; arg_user_setting = base::list(x = as.name(x = "fun1"), export = FALSE, out_path = NULL, lib_path = NULL, safer_check = TRUE, error_text = " INSIDE A")
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\lambda_check.R") ; x = lambda_check ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::lambda_check does not return the same number of code lines

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
    tempo_arg <- base::c(
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
    tempo_arg <- base::c(
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
                "saferDev:::.base_op_check" # also in internal functions
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
    # x forwarded through the ... argument of another function (e.g., fun2 <- function(...){saferDev::lambda_check(...)} called with x = fun1): base::match.call() writes x ..1 (or ..2, etc.) in arg_user_setting, instead of its writing in the call. This writing is recovered from the promise of x by base::substitute() (x is not reassigned before this line)
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
    # warning: R only stores the parse data when the option keep.parse.data is TRUE, independently of the keep.source argument of base::parse(). The option can be FALSE when this function is called from a file loaded by base::sys.source() (which sets it to getOption("keep.parse.data.pkgs"), FALSE by default, for the whole loading), so that utils::getParseData() returns NULL and this would crash the function (issue found while validating the pre-check of all_args_here()). The option is set here and restored on exit
    oop <- base::options(keep.parse.data = TRUE)
    base::on.exit(expr = base::options(oop), add = TRUE, after = TRUE)
    code_text <- base::paste0(code, collapse = "\n", recycle0 = FALSE)
    pd <- base::tryCatch(
        expr = utils::getParseData(x = base::parse(file = "", n = NULL, text = code_text, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_text, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA), # warning: an explicit srcfile is required, because srcfile = NULL removes the source references of the parsed code, so that utils::getParseData() returns NULL (same writing as in curly_bracket_check(), true_false_check() and for_loop_nb_check())
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
    term_rows <- term_rows[base::order(pd$line1[term_rows], pd$col1[term_rows], method = "radix", na.last = TRUE, decreasing = FALSE)]
    # positions in term_rows of the terminal tokens of the reported lambda writings. Warning: the R versions checked so far always give the "'\\'" token (and not the LAMBDA token), but both are scanned: the LAMBDA token is reported in the R documentation of utils::getParseData(). A backslash written in a backquoted name (e.g., `\\`(x) or `\\(y)` <- 1) belongs to a SYMBOL or SYMBOL_FUNCTION_CALL token, and in a string to a STR_CONST token, not to the '\\' token, so that it is not scanned below. Warning: the "'\\'" token name (4 characters: quote, backslash, backslash, quote) is assembled below by base::paste0() from its 3 parts, so that no full "'\\'" string literal appears in the source code: the capture.output() form of such a literal triggers a known parsing bug of saferDev::all_args_here() (see dev/repro_all_args_here_internal_error_3.R and dev/lambda_token_diag.R)
    token_quote1 <- "'"
    token_bs <- "\\\\"
    token_quote2 <- "'"
    lambda_token_name <- base::paste0(token_quote1, token_bs, token_quote2, collapse = NULL, recycle0 = FALSE)
    kid_idx <- base::which(x = pd$token[term_rows] %in% base::c("LAMBDA", lambda_token_name), arr.ind = FALSE, useNames = TRUE)
    findings_line <- base::integer(length = 0)
    findings_symbol <- base::character(length = 0)
    findings_code <- base::character(length = 0)
    findings_col1 <- base::integer(length = 0)
    findings_col2 <- base::integer(length = 0)
    for(i1 in base::seq_along(along.with = kid_idx)){
        i <- kid_idx[i1]
        k <- term_rows[i]
        # symbol: the lambda writing itself, including its formals, recovered from the code line between the \ token and the matching ) of the formals. Warning: a single line is enough in the vast majority of the writings; the formals of a lambda written on several lines are not included in the symbol (the whole writing is still replaced correctly in the code, because only the \ token is replaced)
        tempo_depth <- 0
        tempo_formals_end <- NULL
        if(i < base::length(x = term_rows)){
            for(j1 in base::seq_len(length.out = base::length(x = term_rows) - i)){ # j from i + 1 to the last terminal token
                j <- i + j1
                if(pd$token[term_rows[j]] == "'('"){
                    tempo_depth <- tempo_depth + 1
                }else if(pd$token[term_rows[j]] == "')'"){
                    tempo_depth <- tempo_depth - 1
                    if(tempo_depth == 0){
                        tempo_formals_end <- term_rows[j]
                        break
                    }
                }
            }
        }
        tempo_symbol_end <- NULL # parse-data column of the ) closing the formals, if written on the line of the \ token
        if( ! base::is.null(x = tempo_formals_end) && pd$line2[tempo_formals_end] == pd$line1[k]){
            tempo_symbol_end <- pd$col2[tempo_formals_end]
        }
        # conversion of the parse-data columns into the character positions used by substr(): utils::getParseData() counts a TAB as a jump to the next multiple of 8, and any other character as 1. The TABs of the submitted code are kept
        tempo_chars <- base::strsplit(x = code[pd$line1[k]], split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
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
        tempo_pos <- base::match(x = base::c(pd$col1[k], pd$col2[k], tempo_symbol_end), table = tempo_cols, nomatch = NA_integer_, incomparables = NULL) # character positions of the start and of the end of the \ token, and of the ) closing the formals
        if(base::any(base::is.na(x = tempo_pos), na.rm = FALSE)){
            # nocov start
            # codecov inactivated because it is an internal control of code writing: the parse-data columns come from the parsing of these code lines, so that each of them is the column of a character of the line.
            tempo_cat <- base::paste0("INTERNAL ERROR IN ", intern_error_text_start, "PARSE-DATA COLUMN NOT FOUND IN THE CODE LINE\nline: ", pd$line1[k], intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        if(base::is.null(x = tempo_symbol_end)){
            findings_symbol <- base::c(findings_symbol, base::paste0(pd$text[k], "()", collapse = NULL, recycle0 = FALSE), recursive = TRUE, use.names = TRUE)
        }else{
            findings_symbol <- base::c(findings_symbol, base::substr(x = code[pd$line1[k]], start = tempo_pos[1], stop = tempo_pos[3]), recursive = TRUE, use.names = TRUE)
        }
        findings_line <- base::c(findings_line, pd$line1[k], recursive = TRUE, use.names = TRUE)
        findings_code <- base::c(findings_code, base::trimws(x = code[pd$line1[k]], which = "left", whitespace = "[ \\t]"), recursive = TRUE, use.names = TRUE)
        findings_col1 <- base::c(findings_col1, tempo_pos[1], recursive = TRUE, use.names = TRUE) # character position (not parse-data column)
        findings_col2 <- base::c(findings_col2, tempo_pos[2], recursive = TRUE, use.names = TRUE) # character position (not parse-data column)
    }
    # end loop over the reported lambda writings
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
    # modification of the recovered code: exact replacement of the reported tokens (\ -> function), from the last token to the first one so that the positions of the preceding tokens remain valid. Warning: findings_* are ordered by source position, because term_rows is sorted and the loop appends in order. Warning: findings_col1 and findings_col2 are character positions, converted above from the parse-data columns (TAB jumps). A multi-byte character counts 1 in both
    if(base::length(x = findings_line) != 0){
        modified_code <- code
        for(i1 in base::seq_along(along.with = findings_line)){
            i <- base::length(x = findings_line) - i1 + 1 # from the last finding to the first one
            # exact token replacement at the character positions of the token: \\ -> function. Warning: the substr() slices are rebuilt from the left part + replacement + right part, so that the positions of the other tokens of the same line remain valid. Warning: the ( of the formals is never part of the replaced token, so that the spaces between the \ and the ( are kept (the writing \( (x) becomes function (x), which is valid)
            tempo_line <- modified_code[findings_line[i]]
            modified_code[findings_line[i]] <- base::paste0(
                base::substr(x = tempo_line, start = 1, stop = findings_col1[i] - 1),
                "function",
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
                " MODIFIED BY THE \\", "() -> ", "function", "() REPLACEMENTS DOES NOT PARSE ANYMORE.\n\nTHIS IS AN INTERNAL BUG: PLEASE, DO NOT USE THE MODIFIED CODE (NOR THE EXPORTED FILE) AND REPORT IT.",
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
            ", SOME LAMBDA FUNCTIONS ARE NOT WRITTEN function():\n\n",
            "LINE\tSYMBOL\t\tCODE\n",
            base::paste0(base::paste0(findings_line, "\t", findings_symbol, "\t\t", findings_code, collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE),
            collapse = NULL,
            recycle0 = FALSE
        )
        if(export == TRUE){
            # the file is written BEFORE the message is printed, so that the message only claims an export that actually happened. Warning: the first line of the recovered code always starts by the function writing (fun <- function...), possibly indented
            export_code <- modified_code
            export_code[1] <- base::paste0(tempo_fun_name, "_safer <- ", export_code[1], collapse = NULL, recycle0 = FALSE)
            utils::write.table(x = export_code, file = base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE), row.names = FALSE, col.names = FALSE, append = FALSE, quote = FALSE, sep = "", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "") # write each line of the modified code, without any quoting, in a plain text R script
            # the export message is printed with on.exit(), after the message of the main code: it only claims an export that actually happened, because the file writing is performed just above and no on.exit() is registered when no modification is inserted
            base::on.exit(expr = base::message(base::paste0("MODIFIED CODE EXPORTED IN:\n", base::normalizePath(path = base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE), winslash = "/", mustWork = FALSE), "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
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
        # restore the environment of the submitted function: without this, the modified function would keep the evaluation environment of lambda_check() as closure environment, which would break closures of the submitted function and could resolve names wrongly
        base::environment(fun = output) <- base::environment(fun = x)
    }
    if(export == TRUE){
        base::return(base::invisible(x = output)) # NULL returned invisibly when export is TRUE
    }else if(base::is.null( x = output)){
        base::return(base::invisible(x = output)) # NULL if everything is clean, but invisible meaning that not shown but can be assigned
    }else{
        base::return(output) 
    }
    #### end output

}
