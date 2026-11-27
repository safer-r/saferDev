#' @title Check and Add Package Prefixes to Function Calls
#' @description
#' Checks that the functions called inside a function are preceded by their package (e.g., \code{base::mean()} and not \code{mean()}), and adds the missing \code{<package>::} prefixes before the basic R functions (functions of the base, stats, graphics, grDevices, utils, datasets and methods packages).
#' @param x Function to check, written without quotes and brackets (e.g., \code{fun1} or \code{pkg::fun1}).
#' @param export Single logical value. If \code{TRUE}, the corrected function is written into the \code{<name>_safer.R} file of \code{out_path} instead of being returned (e.g., \code{fun1_safer.R} for \code{x = fun1} or \code{x = pkg::fun1}), as \code{fun1_safer <- function(...)}, which \code{base::source()} recreates. Nothing is written if nothing has to be corrected, but an error is returned if the file already exists, or if \code{x} is not a function name (e.g., \code{function(x)\{x\}} or \code{FUN_LIST$fun}).
#' @param out_path Single character string: path of an existing folder where the file is written. Required when \code{export} is \code{TRUE}; otherwise not used, but a non-\code{NULL} value must still be a single non-empty character string. \code{NULL} by default.
#' @inheritParams is_function_here
#' @returns
#' The corrected function, which keeps the environment of \code{x}: \code{x} with the \code{<package>::} prefixes added before its calls of basic R functions.
#'
#' \code{NULL}, invisibly, if nothing has to be corrected, if \code{x} contains no function call, as a primitive function like \code{base::sum} (with a warning), or if \code{export} is \code{TRUE}.
#'
#' In addition, except when \code{x} contains no function call, a report is printed with \code{message()}: either a message saying that everything seems clean, or tables of the calls without prefix, one for the basic R functions and one for the other functions (column 1: line number; column 2: function name; column 3: code before the function name). The internal functions called (see the Details section) are then also listed.
#' @details
#' A call of a basic R function gets the prefix of its package (e.g., \code{mean()} becomes \code{base::mean()}) only if the function found by R when running \code{x}, from the environment of \code{x} (functions defined by the user, imported functions and attached packages), is this basic function. Otherwise, the name is masked (e.g., \code{filter()} with the dplyr package attached, or a \code{filter()} function defined in the global environment): the call is not modified and is reported with the other functions. Under \code{pkgload::load_all()} (\code{devtools::load_all()}), \code{system.file()}, \code{help()}, \code{library.dynam()} and \code{library.dynam.unload()} can thus be reported as masked by the pkgload shims. In code evaluated in another environment (e.g., \code{expr} of \code{with()}, or \code{.()} of \code{bquote()} with \code{where}), a function of that environment named like a basic function is not detected: its calls get the basic prefix. The stats, graphics, grDevices, utils, datasets and methods packages must be attached, as in a default R session, otherwise an error is returned.
#'
#' The calls of internal functions, written without \code{::} or \code{:::}, are not modified, nor reported as missing a prefix. The internal functions are the functions created inside \code{x} (names assigned a function definition or a \code{utils::getFromNamespace()} result), the arguments of \code{x} and of all the functions defined in \code{x}, anonymous ones included (e.g., \code{FUN} in \code{FUN(y)}), but not the arguments and local functions of those written in quoted code (e.g., \code{c} in \code{bquote(function(c) .(c(y)))}), and \code{x} itself when written as a name, like \code{fun1} or \code{pkg::fun1} (recursive calls). These names are recognized in the function that defines them (the whole code of \code{x} for its arguments, \code{x} itself and \code{<<-}): an argument \code{t} of a function defined in \code{x} does not prevent the check of \code{t()} outside this function. A name assigned another value is not an internal function: after \code{c <- 3}, \code{c(y)} calls \code{base::c()} and gets its prefix. A local name that holds a function obtained otherwise is not detected: after \code{rev <- base::get(x = "sort")}, \code{rev(y)} becomes \code{base::rev(y)}.
#'
#' The calls of the other functions are reported in the message and in a warning, but not modified, because their package cannot be determined reliably: add their prefixes manually.
#'
#' Not checked: function names written between quotes (like \code{"+"()}), preceded by \code{$} or \code{@}, or written without brackets (like \code{as.character} in \code{sapply(1:3, FUN = as.character)}), and \code{function}, \code{if}, \code{for}, \code{while}, \code{repeat} and \code{else}. Neither are the calls inside formulas, including the one-sided formulas used as lambdas (e.g., \code{log()} and \code{offset()} in \code{y ~ log(x) + offset(z)}, or \code{mean()} in \code{~ mean(.x)}), because modeling functions use their text, the calls inside \code{quote()}, \code{bquote()}, \code{expression()}, \code{alist()} and \code{substitute()}, whose arguments are code used as data (e.g., \code{paste()} in the plotmath label \code{expression(paste("Temp (", degree, "C)"))}), except the code that R evaluates (\code{.()} and spliced \code{..()} of \code{bquote()}, its \code{where} argument, and the \code{env} argument of \code{substitute()}), nor the function called by \code{.Internal()} (e.g., \code{toupper} in \code{.Internal(toupper(x))}). The other functions that use the code of their arguments get the corrected code (e.g., the default axis label of \code{ggplot2::aes(x = log(y))} becomes \code{base::log(y)}). Comments are not checked, but some unexpected writing could dupe \code{colons_check()}: please report it \href{https://github.com/safer-r/saferDev/issues}{here}.
#'
#' Names already preceded by \code{::} or \code{:::} are not checked (R CMD check reports the wrong ones, e.g., \code{base:::sum()} or \code{base::dev.off()}). The added prefixes are always \code{::}.
#'
#' Source references: line 1 is the line of the \code{function} keyword. Comments and indentation are kept only if \code{x} carries its source references (attribute \code{srcref}). Functions created with \code{options(keep.source = FALSE)} (default of Rscript) and functions of installed packages (e.g., \code{saferDev::colons_check}) have none: their deparsed code is analyzed, without comments and with line numbers that differ from the source file.
#'
#' If the corrected code does not parse (a writing that the analysis does not handle, e.g., a function name at the beginning of a line, after a \code{$} or \code{@} ending the previous line), \code{x} is returned unchanged with a warning, or an error is returned if \code{export} is \code{TRUE}.
#'
#' An error is returned if \code{x} contains \code{\\(x)} lambda functions (only detected with source references): run \code{lambda_check()} first.
#' @seealso The code checkers of saferDev, in the recommended order of use: \code{\link{super_assign_check}}, \code{\link{lambda_check}}, \code{\link{colons_check}}, \code{\link{all_args_here}}, \code{\link{curly_bracket_check}}, \code{\link{true_false_check}}, \code{\link{for_loop_nb_check}} (see \code{vignette("saferDev")}).
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @examples
#' # Without source references ("Run examples" link, Rscript), line numbers and comments differ.
#' # See also https://safer-r.github.io/saferDev/articles/colons_check.html
#' fun1 <- function(x){
#'     y <- mean(x) # mean() written without base::
#'     base::return(y)
#' }
#' fun1_safer <- saferDev::colons_check(x = fun1, export = FALSE, out_path = NULL,
#'     safer_check = TRUE, lib_path = NULL, error_text = "") # base:: added before mean()
#' fun1_safer # corrected function
#' # Write the corrected function in the fun1_safer.R file of a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(check = FALSE),
#'     fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::colons_check(x = fun1, export = TRUE, out_path = tempo_dir, safer_check = TRUE,
#'     lib_path = NULL, error_text = "")
#' base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE) # folder removed
#'
#' @export
colons_check <- function(
    x, 
    export = FALSE, 
    out_path = NULL, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # x = mean ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "mean" ; arg_user_setting = base::list(x = as.name(x = "mean"), export = FALSE, out_path = NULL, lib_path = NULL , safer_check = TRUE, error_text = " INSIDE A")
    # x = .expand_R_libs_env_var ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # library(saferGraph) ; x = close2 ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" 
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\get_message.R") ; x = get_message ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # library(saferDev) ; x = get_message ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: does not return the same number of code lines than the previsou example
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\colons_check.R") ; x = colons_check ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; x = test ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::get_message does not return the same number of code lines
    # function_name <- "colons_check" ; arg_user_setting = base::list(x = as.name(x = "test"), export = FALSE, out_path = NULL, lib_path = NULL , safer_check = TRUE , error_text = " INSIDE A")
    # FUN1 <- function(x, y){ifelse(base::is.null(x = warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE))} ; x = FUN1 ; export = FALSE ; out_path = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "colons_check" ; arg_user_setting = base::list(x = as.name(x = "FUN1"), export = FALSE, out_path = NULL, lib_path = NULL , safer_check = TRUE , error_text = " INSIDE A")

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
                "saferDev::arg_check", # also in internal functions
                "saferDev::lambda_check",
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check", # also in internal functions
                "saferDev:::.functions_detect",
                "saferDev:::.colons_check_message"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = -1, envir = )
    .functions_detect <- utils::getFromNamespace(x = ".functions_detect", ns = "saferDev", pos = -1, envir = )
    .colons_check_message <- utils::getFromNamespace(x = ".colons_check_message", ns = "saferDev", pos = -1, envir = )

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
    if( ! base::is.null(x = out_path)){ # for all arguments that can be NULL, write like this:
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
    ini_warning_length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE) # registered here so that warning.length is restored even if an error occurs before the warning-output on.exit() registrations below
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    # x forwarded through the ... argument of another function (e.g., fun2 <- function(...){saferDev::colons_check(...)} called with x = fun1): base::match.call() writes x ..1 (or ..2, etc.) in arg_user_setting, instead of its writing in the call. This writing is recovered from the promise of x by base::substitute() (x is not reassigned before this line)
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
        out_file_name <- base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE) # base::file.path(x = out_path, y = out_name, fsep = "/") equivalent, kept for OS consistency
        if(base::file.exists(out_file_name)){
            tempo_cat <- base::paste0(
                error_text_start,
                "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n",
                out_file_name,
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
    # reminder: the returned function is the code modified by adding the package attributions of the basic R functions
    # the message is printed below, after the modification logic, because the modification does not change the analysis results
    # (the positions in the table-like message are the ones of the initial analysis, performed by .functions_detect() on the submitted function)
    skipped_base <- base::c("function", "if", "for", "while", "repeat", "else", "in") # skipped function and elements. in must be here because for(j in (n - 1L):2L) breaks the code
    # pre-check of the lambda writings: lambda_check() must be run first, because the \\ writings are not managed by the text-based detection of colons_check(). Warning: out_path is not used here (export = FALSE: no file written)
    # name of the submitted function as written by the user, for the error message below (recovered before the modification of arg_user_setting$x): x written as a name, pkg::name, pkg:::name or other expression (e.g., FUN_LIST$fun), deparsed on one line, or "x" for a function object, which has no name (e.g., x = fun1 in the args argument of base::do.call())
    if(base::is.function(x = arg_user_setting$x)){
        arg_user_setting_x_ini <- "x"
    }else{
        arg_user_setting_x_ini <- base::paste0(base::trimws(x = base::deparse(expr = arg_user_setting$x, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), which = "left", whitespace = "[ \\t]"), collapse = " ", recycle0 = FALSE)
    }
    tempo_lambda <- base::suppressMessages(expr = saferDev::lambda_check(x = x, export = FALSE, out_path = base::tempdir(check = FALSE), safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text), classes = "message")
    if( ! base::is.null(x = tempo_lambda)){
        tempo_cat <- base::paste0(
            error_text_start,
            "SOME LAMBDA FUNCTIONS (WRITTEN \\(x)) ARE DETECTED IN THE CODE OF ",
            arg_user_setting_x_ini,
            ".\n\nPLEASE, RUN saferDev::lambda_check(",
            arg_user_setting_x_ini,
            ") FIRST,\nUSE THE CORRECTED FUNCTION RETURNED BY lambda_check() (OR EXPORT IT WITH export = TRUE),\nAND RERUN saferDev::colons_check(",
            arg_user_setting_x_ini,
            ").",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
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
    out <- .functions_detect(
        x = x, 
        skipped_base = skipped_base, 
        arg_user_setting2 = arg_user_setting, 
        lib_path = lib_path, 
        error_text = embed_error_text
    )
    if(base::is.null(x = out)){
        base::return(base::invisible(x = NULL)) # tested function empty of function: warning already returned by .functions_detect()
    }
    if( ! (base::all(base::typeof(x = out$fun_names) == "list", na.rm = TRUE) & base::all(base::typeof(x = out$fun_names_pos) == "list", na.rm = TRUE))){
        # nocov start
        # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
        tempo_cat <- base::paste0("INTERNAL ERROR 1 IN ", intern_error_text_start, "out$fun_names AND out$fun_names_pos MUST BE TYPE list\nout$fun_names:\n", out$fun_names, "\nout$fun_names_pos:\n", out$fun_names_pos, intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    # resolution of the names of basic objects called in x, as R does when running x (masking)
    # a name gets the prefix of a basic package only if the function found from the environment of x (user definitions, imports, attached packages) is the function of this basic package. Otherwise (name masked, e.g., by an attached package, or name of a basic object that is not a function), the name is processed like the names of the other functions: not modified, and reported in the OTHER section of the message and in the warning
    env_x <- base::environment(fun = x) # x is not a primitive function (.functions_detect() returns NULL for a primitive function), so that its environment is not NULL
    basic_scopes <- base::c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base") # basic packages, in the default base::search() order
    flagged_names <- base::unique(x = base::unlist(x = out$fun_names, recursive = TRUE, use.names = FALSE), incomparables = FALSE) # names of the functions called in x
    flagged_names <- flagged_names[flagged_names %in% out$all_basic_funs] # names of basic objects called in x
    flagged_package <- base::rep(x = "", times = base::length(x = flagged_names)) # package of each name ("" if the called function is not a basic function)
    masked_names <- base::character(length = 0) # names of basic functions masked by another function, for the warning
    for(i3 in base::seq_along(along.with = flagged_names)){
        resolved_fun <- base::tryCatch(
            expr = base::get0(x = flagged_names[i3], envir = env_x, mode = "function", inherits = TRUE, ifnotfound = NULL), # function found when running x (NULL if none)
            error = function(e){NULL}, # an object of this name cannot be evaluated (e.g., a promise of an enclosing function that returns an error): the function called when running x cannot be determined
            finally = 
        )
        basic_fun_log <- FALSE # becomes TRUE if a basic package contains a function of this name
        for(i4 in base::seq_along(along.with = basic_scopes)){
            basic_fun <- base::get0(x = flagged_names[i3], envir = base::as.environment(x = basic_scopes[i4]), mode = "function", inherits = FALSE, ifnotfound = NULL)
            if( ! base::is.null(x = basic_fun)){
                basic_fun_log <- TRUE
                if(flagged_package[i3] == "" & base::identical(x = resolved_fun, y = basic_fun, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){ # first basic package (base::search() order) holding the function found when running x (several basic packages can hold the same function, like base::plot and graphics::plot)
                    flagged_package[i3] <- base::sub(pattern = "^package:", replacement = "", x = basic_scopes[i4], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                }
            }
        }
        if(basic_fun_log == TRUE & flagged_package[i3] == "" & ! base::is.null(x = resolved_fun)){ # another function of the same name is found when running x
            masked_names <- base::c(masked_names, flagged_names[i3])
        }
    }
    basic_names_ok <- flagged_names[flagged_package != ""] # names of the basic functions that can be prefixed
    # end resolution of the names of basic objects called in x, as R does when running x (masking)
    # names neither modified nor reported: names of the code that R does not evaluate as written (formulas, quoted code, see .functions_detect()) and name of the function called by .Internal() (out$fun_names_skip), and calls written without :: or ::: of the internal functions, inside their scope (out$fun_names_internal: functions created inside x, formal arguments of x and of the functions defined inside x, and x itself)
    prefix_log <- base::mapply(FUN = function(x, y){base::substring(text = x, first = y - 2L, last = y - 1L) == "::"}, x = out$code[out$code_line_nb], y = out$fun_names_pos, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE) # names preceded by :: or :::
    internal_log <- base::mapply(FUN = function(x, y, z){x & ( ! y) & ( ! z)}, x = out$fun_names_internal, y = prefix_log, z = out$fun_names_skip, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE) # calls of internal functions
    internal_called <- base::unique(x = base::unlist(x = base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = internal_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE), recursive = TRUE, use.names = FALSE), incomparables = FALSE) # internal functions called, listed in the message
    if(base::length(x = internal_called) == 0){
        internal_called <- NULL
    }
    # basic function names in x
    # selection of basic functions
    basic_log <- base::mapply(FUN = function(x, y, z){x %in% basic_names_ok & ( ! y) & ( ! z)}, x = out$fun_names, y = internal_log, z = out$fun_names_skip, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE) #  are names basic functions used in x?
    tempo_log <- basic_log
    in_basic_fun <- base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    in_basic_fun_names_pos <-  base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    in_basic_fun_internal <-  base::mapply(FUN = function(x, y){x[y]}, x = internal_log, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE) # all FALSE: the calls of internal functions are not selected
    # end selection of basic functions
    if(base::length(x = base::unlist(x = in_basic_fun, recursive = TRUE, use.names = TRUE)) != 0){ # check if basic fun present
        # removal of string with empty function names
        tempo_log <- base::sapply(X = in_basic_fun, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # detection of string with empty function names
        in_basic_fun <- in_basic_fun[ ! tempo_log] # removal of empty string
        in_basic_fun_names_pos <- in_basic_fun_names_pos[ ! tempo_log]
        in_basic_fun_internal <- in_basic_fun_internal[ ! tempo_log]
        in_basic_code_line_nb <- out$code_line_nb[ ! tempo_log]
        if( ! (base::length(x = in_basic_fun) == base::length(x = in_basic_fun_names_pos) & base::length(x = in_basic_fun) == base::length(x = in_basic_code_line_nb))){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
            tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL\nin_basic_fun: ", base::length(x = in_basic_fun), "\nin_basic_fun_names_pos: ", base::length(x = in_basic_fun_names_pos), "\nin_basic_code_line_nb: ", base::length(x = in_basic_code_line_nb), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        # end removal of string with empty function names
        in_basic_fun_uni <- base::unlist(x = base::unique(x = in_basic_fun, incomparables = FALSE), recursive = TRUE, use.names = TRUE) #  names of unique basic functions used in x
        # end basic function names in x
        # analyse of :: before basic functions in x
        if(base::length(x = in_basic_fun_uni) > 0){
            tempo <- .colons_check_message(
                list_fun = in_basic_fun, 
                list_fun_pos = in_basic_fun_names_pos, 
                list_fun_internal = in_basic_fun_internal, 
                line_nb = in_basic_code_line_nb, 
                ini = out$code, 
                arg_user_setting2 = out$arg_user_setting, 
                text = "BASIC", 
                internal_fun_names = internal_called,
                lib_path = lib_path, 
                error_text = embed_error_text
            )
            log_basic <- tempo$colon_not_here
            cat_basic <- tempo$output.cat
            basic_col1 <- tempo$col1
            basic_col2 <- tempo$col2
            basic_col3 <- tempo$col3
        }else{
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values. Because empty function, etc., are already checked above
            log_basic <- FALSE
            cat_basic <- NULL
            basic_col1 <- base::integer(length = 0)
            basic_col2 <- base::character(length = 0)
            basic_col3 <- base::character(length = 0)
            # nocov end
        }
        # end analyse of :: before basic functions in x
    }else{
        log_basic <- FALSE
        cat_basic <- NULL
        basic_col1 <- base::integer(length = 0)
        basic_col2 <- base::character(length = 0)
        basic_col3 <- base::character(length = 0)
    }
    # other function names in x
    # selection of other functions
    tempo_log <- base::mapply(FUN = function(x, y){( ! x) & ( ! y)}, x = basic_log, y = out$fun_names_skip, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE) #  names of all the other functions used in x, including the masked names of basic functions, and the calls of internal functions (removed from the report and listed by .colons_check_message())
    in_other_fun <- base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    in_other_fun_names_pos <-  base::mapply(FUN = function(x, y){x[y]}, x = out$fun_names_pos, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    in_other_fun_internal <-  base::mapply(FUN = function(x, y){x[y]}, x = internal_log, y = tempo_log, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE) # calls of internal functions, removed from the report by .colons_check_message()
    # end selection of other functions
    if(base::length(x = base::unlist(x = in_other_fun, recursive = TRUE, use.names = TRUE)) != 0){ # check if other fun present
        # removal of string with empty function names
        tempo_log <- base::sapply(X = in_other_fun, FUN = function(x){base::length(x = x) == 0}, simplify = TRUE, USE.NAMES = TRUE) # detection of string with empty function names
        in_other_fun <- in_other_fun[ ! tempo_log] # removal of empty string
        in_other_fun_names_pos <- in_other_fun_names_pos[ ! tempo_log]
        in_other_fun_internal <- in_other_fun_internal[ ! tempo_log]
        in_other_code_line_nb <- out$code_line_nb[ ! tempo_log]
        if( ! (base::length(x = in_other_fun) == base::length(x = in_other_fun_names_pos) & base::length(x = in_other_fun) == base::length(x = in_other_code_line_nb))){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
            tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "LENGTHS SHOULD BE IDENTICAL\nin_other_fun: ", base::length(x = in_other_fun), "\nin_other_fun_names_pos: ", base::length(x = in_other_fun_names_pos), "\nin_other_code_line_nb: ", base::length(x = in_other_code_line_nb), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        # end removal of string with empty function names
        in_other_fun_uni <- base::unlist(x = base::unique(x = in_other_fun, incomparables = FALSE), recursive = TRUE, use.names = TRUE) #  names of unique other functions used in x
        # end other function names in x
        # analyse of :: before other functions in x
        if(base::length(x = in_other_fun_uni) > 0){
            tempo <- .colons_check_message(
                list_fun = in_other_fun, 
                list_fun_pos = in_other_fun_names_pos, 
                list_fun_internal = in_other_fun_internal, 
                line_nb = in_other_code_line_nb, 
                ini = out$code, 
                arg_user_setting2 = out$arg_user_setting, 
                text = "OTHER", 
                internal_fun_names = internal_called,
                lib_path = lib_path, 
                error_text = embed_error_text
            )
            log_other <- tempo$colon_not_here
            cat_other <- tempo$output.cat
            other_col1 <- tempo$col1
            other_col2 <- tempo$col2
            other_col3 <- tempo$col3
        }else{
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values. Because .colons_check_message() (with text = "OTHER") removes all the reported occurrences between quotes or preceded by $ (see the .noclean_functions() call inside), col1, col2 and col3 are empty when no clean other function is reported, even when in_other_fun_uni is not empty
            log_other <- FALSE
            cat_other <- NULL
            other_col1 <- base::integer(length = 0)
            other_col2 <- base::character(length = 0)
            other_col3 <- base::character(length = 0)
            # nocov end
        }
    }else{
        log_other <- FALSE
        cat_other <- NULL
        other_col1 <- base::integer(length = 0)
        other_col2 <- base::character(length = 0)
        other_col3 <- base::character(length = 0)
    }
    # end analyse of :: before basic functions in x
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
        ".\nINSIDE ", 
        base::as.character(x = arg_user_setting$x), 
        collapse = NULL, 
        recycle0 = FALSE
    )
    if(base::all( ! log_basic, na.rm = TRUE) & base::all( ! log_other, na.rm = TRUE)){ # log_basic TRUE means a problem, ! log_basic means all(FALSE) becomes TRUE. Idem for log_other
        tempo_cat <- base::paste0(tempo_cat, ", EVERYTHING SEEMS CLEAN.", collapse = NULL, recycle0 = FALSE)
    }else{
        tempo_cat <- base::paste0(
            base::ifelse(
                test = base::is.null(x = cat_basic), 
                yes = base::paste0(
                    tempo_cat, 
                    ", EVERYTHING SEEMS CLEAN",
                    base::ifelse(test = base::is.null(x = cat_other), yes =  ".", no =  " FOR R BASIC FUNCTIONS.\n\n"), 
                    collapse = NULL, 
                    recycle0 = FALSE
                ), 
                no = base::paste0(
                    cat_basic, 
                    base::ifelse(test = base::is.null(x = cat_other), yes =  "", no =  "\n\n"), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
            ), 
            cat_other, 
            collapse = NULL, 
            recycle0 = FALSE
        )
    }
    base::message(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    # warning if functions other than basic R functions need a package attribution (manual modification required)
    if(base::length(x = other_col1) > 0){
        warn_count <- warn_count + 1
        masked_reported <- masked_names[masked_names %in% other_col2] # masked names of basic functions among the reported other functions
        tempo_warn <- base::paste0(
            "(", warn_count, ") INSIDE ", base::as.character(x = arg_user_setting$x), ", SOME :: OR ::: ARE MISSING BEFORE FUNCTIONS THAT ARE NOT R BASIC FUNCTIONS:\n\n",
            "LINE\tFUN\t\tSTRING_BEFORE\n",
            base::paste0(base::paste0(other_col1, "\t", other_col2, "\t\t", other_col3, collapse = NULL, recycle0 = FALSE), collapse = "\n", recycle0 = FALSE),
            base::ifelse(
                test = base::length(x = masked_reported) > 0,
                yes = base::paste0("\n\nAMONG THEM, THESE NAMES OF R BASIC FUNCTIONS ARE MASKED: THE FUNCTION CALLED WHEN RUNNING ", base::as.character(x = arg_user_setting$x), " IS ANOTHER FUNCTION (ATTACHED PACKAGE, IMPORT OR USER DEFINITION):\n", base::paste0(masked_reported, collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE),
                no = ""
            ),
            "\n\nTHE PACKAGE ATTRIBUTIONS OF THESE FUNCTIONS CANNOT BE ADDED AUTOMATICALLY, BECAUSE THE CORRECT PACKAGE CANNOT BE DETERMINED RELIABLY.\nPLEASE, MODIFY THE CODE MANUALLY, WITH THE HELP OF THE MESSAGE PRINTED ABOVE.",
            collapse = NULL,
            recycle0 = FALSE
        )
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    # modification of the basic functions, only when some basic functions require a package attribution
    # the positions of the function names in out$code are recovered from the STRING_BEFORE column: the name starts just after the STRING_BEFORE string
    output <- NULL # NULL returned if no modification is done
    replaced_log <- FALSE # becomes TRUE as soon as at least one package attribution has been added
    if(base::length(x = basic_col1) > 0){
        # package of each flagged basic function, determined above by the resolution of the names as R does when running x
        package_of_flagged <- flagged_package[base::match(x = basic_col2, table = flagged_names, nomatch = NA_integer_, incomparables = NULL)] # package of each flagged function (NA or "" impossible since the flagged functions are in basic_names_ok)
        # the calls of the internal functions (a same name can be an internal function and a basic R function) and the names inside a formula or a quoting call or called by .Internal() are not flagged: they have been excluded from the selection of the basic functions above
        # modification of the code lines: insert <PACKAGE>:: just before the function name (always ::, also for the names starting by a dot, since all the basic functions are exported: R CMD check reports ::: before an exported function), from the end of each line to the beginning, so that the positions of the not yet modified occurrences remain valid
        # the strings of basic_col3 are the code before each function name in the analyzed code (out$code), i.e., the name of the i3-th flagged occurrence starts at the position base::nchar(x = basic_col3[i3]) + 1 of the line basic_col1[i3]
        # the package attributions are inserted in the code of the submitted function as printed (comments and indentation kept), and not in the analyzed code (out$code: comments removed and multiple spaces collapsed), so that the returned or exported function keeps the comments and the indentation of the submitted function
        # the line i of out$code is the line i of the printed code (same utils::capture.output() call as in .functions_detect(), which never removes lines), with the comments removed and the multiple spaces collapsed: the positions in out$code are converted below into positions in the printed code
        code_ini <- utils::capture.output(x, file = NULL, append = FALSE, type = NULL, split = FALSE) # printed code of the submitted function, not modified (used for the conversion of the positions)
        if(base::length(x = code_ini) != base::length(x = out$code)){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values, since the same utils::capture.output() call is used in .functions_detect()
            tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "THE PRINTED CODE AND THE ANALYZED CODE OF THE SUBMITTED FUNCTION SHOULD HAVE THE SAME NUMBER OF LINES\nPRINTED CODE: ", base::length(x = code_ini), "\nANALYZED CODE: ", base::length(x = out$code), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        code_mod <- code_ini # code to modify
        name_start_pos <- base::nchar(x = basic_col3, type = "chars", allowNA = FALSE, keepNA = NA) + 1L
        ord_flagged <- base::order(basic_col1, -name_start_pos, na.last = TRUE, decreasing = FALSE, method = "auto") # lines first, then reversed positions within a line
        for(i4 in base::seq_along(along.with = ord_flagged)){
            i3 <- ord_flagged[i4]
            line_i3 <- basic_col1[i3]
            pos_i3 <- name_start_pos[i3]
            fun_i3 <- basic_col2[i3]
            nchar_fun_i3 <- base::nchar(x = fun_i3, type = "chars", allowNA = FALSE, keepNA = NA)
            # security check: the name must be present at the expected position of the line in the analyzed code (out$code, never modified)
            if(pos_i3 + nchar_fun_i3 - 1L > base::nchar(x = out$code[line_i3], type = "chars", allowNA = FALSE, keepNA = NA)){
                # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values, since positions come from the same analyzed code
                next
                # nocov end
            }
            # conversion of the position in the analyzed line into the position in the printed line. The analyzed line is the printed line with: 1) each run of spaces collapsed into its first space, 2) the leading space removed, 3) the comment removed (end of the line) and 4) possibly a "(" added at the end. Steps 3 and 4 do not change the positions of the remaining characters
            chars_i3 <- base::strsplit(x = code_ini[line_i3], split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]] # characters of the printed line, not modified
            kept_pos_i3 <- base::which(x = ! (chars_i3 == " " & base::c(FALSE, chars_i3[-base::length(x = chars_i3)] == " ")), arr.ind = FALSE, useNames = TRUE) # positions of the characters kept by the collapsing of the multiple spaces (first space of each run kept)
            if(base::length(x = kept_pos_i3) > 0){
                if(chars_i3[kept_pos_i3[1]] == " "){
                    kept_pos_i3 <- kept_pos_i3[-1] # removal of the leading space
                }
            }
            if(pos_i3 + nchar_fun_i3 - 1L > base::length(x = kept_pos_i3)){
                # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values, since the analyzed line comes from the printed line
                next
                # nocov end
            }
            ini_pos_i3 <- kept_pos_i3[pos_i3] # position of the function name in the printed line (the printed line is modified from the end to the beginning, so that this position, computed on the not modified line, remains valid)
            if(base::substr(x = out$code[line_i3], start = pos_i3, stop = pos_i3 + nchar_fun_i3 - 1L) == fun_i3 & base::substr(x = code_mod[line_i3], start = ini_pos_i3, stop = ini_pos_i3 + nchar_fun_i3 - 1L) == fun_i3){
                if(base::is.na(x = package_of_flagged[i3]) | package_of_flagged[i3] == ""){
                    # nocov start
                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values, since the flagged basic functions are the ones of basic_names_ok, which all have a package
                    base::stop(base::paste0("\n\n================\n\n", intern_error_text_start, "THE PACKAGE OF THE BASIC FUNCTION ", fun_i3, " COULD NOT BE DETERMINED. PLEASE, REPORT THIS PROBLEM HERE: ", internal_error_report_link, ".", intern_error_text_end, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    # nocov end
                }
                code_mod[line_i3] <- base::paste0(
                    base::substr(x = code_mod[line_i3], start = 1, stop = ini_pos_i3 - 1L),
                    package_of_flagged[i3],
                    "::",
                    base::substr(x = code_mod[line_i3], start = ini_pos_i3, stop = base::nchar(x = code_mod[line_i3], type = "chars", allowNA = FALSE, keepNA = NA)),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                replaced_log <- TRUE
            }
        }
    }
    if(replaced_log == TRUE){
        # removal of the lines that appear when the submitted function has already been executed (R JIT compiler):
        # the deparse of a compiled function body starts with the "<bytecode: 0x...>" line, and can contain a "<environment: ...>" line
        # these lines are not R code and break the reconstruction below, so they are removed (the line numbers of the message table-like part, based on the initial analysis, are not affected because the removal is done after the modification)
        code_mod <- code_mod[ ! base::grepl(x = code_mod, pattern = "^\\s*<(bytecode|environment):", ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)]
        # reconstruction of the modified function
        # warning: keep.source = TRUE and an explicit srcfile are required to keep the source references (attribute srcref) of the returned function, i.e., its comments and indentation when printed. With srcfile = NULL, or with keep.source = FALSE (default of Rscript), the returned function is printed deparsed: comments removed and code reindented
        code_mod_text <- base::paste0(code_mod, collapse = "\n", recycle0 = FALSE)
        tempo_parse <- base::try(expr = base::parse(text = code_mod_text, file = "", n = NULL, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_mod_text, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), silent = TRUE, outFile = base::getOption(x = "try.outFile", default = base::stderr()))
        if(base::inherits(x = tempo_parse, what = "try-error", which = FALSE) | base::length(x = tempo_parse) != 1){
            # the modified code does not parse: rare writing that the analysis does not handle (e.g., a function name at the beginning of a line, after a $ or @ ending the previous line: x@\n    mean(1) becomes x@\n    base::mean(1)). See the Details section of the roxygen header
            if(export == TRUE){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE MODIFIED CODE DOES NOT PARSE. THE MODIFIED FUNCTION CANNOT BE EXPORTED NOR RETURNED.\nPLEASE, REPORT THIS PROBLEM HERE: ", 
                    internal_error_report_link, 
                    ".", 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                # the submitted function is returned unchanged, with a warning
                output <- x
                warn_count <- warn_count + 1
                tempo_warn <- base::paste0(
                    "(", warn_count, ") THE CODE OF ",
                    base::as.character(x = arg_user_setting$x),
                    " MODIFIED BY THE ADDITION OF THE PACKAGE ATTRIBUTIONS DOES NOT PARSE: THE SUBMITTED FUNCTION IS RETURNED UNCHANGED.\nPLEASE, ADD THE PACKAGE ATTRIBUTIONS MANUALLY, WITH THE HELP OF THE MESSAGE PRINTED ABOVE, AND REPORT THIS PROBLEM HERE: ",
                    internal_error_report_link,
                    ".",
                    collapse = NULL,
                    recycle0 = FALSE
                )
                warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            }
        }else{
            output_fun <- base::eval(expr = tempo_parse[[1]], envir = base::environment(fun = x), enclos = base::environment(fun = x)) # environment of the submitted function, so that the functions used inside the modified function still resolve the same way
            if(export == TRUE){
                # the file name is the name of the submitted function with _safer added at the end (already determined in the other checkings section)
                tempo_content <- base::paste0(
                    tempo_fun_name,
                    "_safer <- ",
                    base::paste0(code_mod, collapse = "\n", recycle0 = FALSE),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                base::writeLines(text = tempo_content, con = out_file_name, sep = "\n", useBytes = FALSE)
                # the message is printed with on.exit(), so that it is printed at the end of the function, after the messages issued earlier by the main code (all the message() calls of the main code are issued before this one)
                base::on.exit(expr = base::message(base::paste0("MODIFIED CODE EXPORTED IN:\n", base::normalizePath(path = base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE), winslash = "/", mustWork = FALSE), "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
            }else{
                output <- output_fun # the modified function is returned
            }
        }
    }
    # end modification of the basic functions
    #### end main code

    #### warning output
    # must be before return()
    if( ! base::is.null(x = warn)){
        base::on.exit(
            expr = base::warning(
                base::paste0(
                    base::sub(pattern = "^ERROR IN ", replacement = "FROM ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
                    warn,
                    collapse = NULL,
                    recycle0 = FALSE
                ), call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL
            ), add = TRUE, after = FALSE # after = FALSE: the warning is issued before the restoration of warning.length (on.exit() registered in the warning initiation section), so that it is not truncated
        )
    }
    #### end warning output

    #### output
    if(base::is.null(x = output)){
        base::return(base::invisible(x = NULL)) # nothing modified, or modified code exported (export = TRUE) instead of being returned
    }else{
        base::return(output) # modified function
    }
    #### end output
}

