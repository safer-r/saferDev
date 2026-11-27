#' @title Catch the Error, Warning or Message of an Instruction
#' @description
#' Evaluates an instruction written as a character string, and returns its error message, its last warning message or its standard messages (neither errors nor warnings), as a single character string.
#' @param data Single character string: the instruction to evaluate.
#' @param kind Single character string: the kind of message returned, either \code{"error"} (default), \code{"warning"} or \code{"message"} (standard messages).
#' @param header Single logical value. Start the returned message with a header (\code{"ERROR MESSAGE REPORTED:"}, \code{"WARNING MESSAGE REPORTED:"} or \code{"STANDARD MESSAGE REPORTED:"})? If \code{FALSE}, errors and warnings are returned as formatted by R (e.g., \code{"simpleWarning in log(-1): NaNs produced"} instead of \code{"In log(-1): NaNs produced"}).
#' @param print_no Single logical value. If \code{TRUE}, a \code{"NO ... REPORTED"} string is returned instead of \code{NULL} when nothing is caught (see the returned value).
#' @param text Single character string inserted at the end of the header, before the colon (e.g., \code{"ERROR MESSAGE REPORTED IN A:"} with \code{text = "IN A"}), and added to the \code{"NO ... REPORTED"} strings whatever \code{header}. \code{NULL} (default) for no text.
#' @param env Environment in which the instruction is evaluated (e.g., \code{.GlobalEnv} or \code{asNamespace("stats")}). If \code{NULL} (default), the environment from which \code{get_message()} is called.
#' @inheritParams is_function_here
#' @returns
#' A single character string: the error message (\code{kind = "error"}), the last warning message (\code{kind = "warning"}), or all the standard messages, separated by line breaks (\code{kind = "message"}).
#'
#' If no such message is caught: \code{NULL} or, with \code{print_no = TRUE}, \code{"NO (ERROR|WARNING|STANDARD) MESSAGE REPORTED"}. If the instruction stops with an error while \code{kind} is \code{"warning"} or \code{"message"}, the warnings and messages emitted before the error are not returned: \code{NULL} or, with \code{print_no = TRUE}, \code{"NO POTENTIAL (WARNING|STANDARD) MESSAGE BECAUSE OF ERROR MESSAGE REPORTED"}.
#' @details
#' The instruction is evaluated once, whatever \code{kind}. Its warnings and standard messages are caught and not displayed. Its printed outputs (e.g., by \code{print()} or \code{show()}) are not caught: use \code{printed <- utils::capture.output(msg <- get_message(data = "print(1) ; log(-1)", kind = "warning"))} to get them in \code{printed}. Text written directly to the standard error (e.g., by \code{cat(file = stderr())}) is not caught either: use \code{utils::capture.output(type = "message")} to get it.
#'
#' Plots are drawn on a NULL pdf device (no file created), and the graphic devices left open by the instruction are closed. A ggplot2 object returned by the instruction is printed, to catch the errors, warnings and messages emitted when the plot is built.
#' @seealso \code{\link{try}}.
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @examples
#' # See also https://safer-r.github.io/saferDev/articles/get_message.html
#'
#' # Error message returned by default. Here no error to report:
#' saferDev::get_message(data = "base::log(-1)", kind = "error", header = TRUE, print_no = FALSE,
#'     text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#'
#' # Warning message to report:
#' saferDev::get_message(data = "base::log(-1)", kind = "warning", header = TRUE, print_no = FALSE,
#'     text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#'
#' # No standard message to report:
#' saferDev::get_message(data = "base::log(-1)", kind = "message", header = TRUE, print_no = TRUE,
#'     text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#'
#' # No standard message returned, because wilcox.test() stops with an error:
#' saferDev::get_message(data = "wilcox.test()", kind = "message", header = TRUE, print_no = TRUE,
#'     text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#'
#' # Error message to report, with text added:
#' saferDev::get_message(data = "wilcox.test()", kind = "error", header = TRUE, print_no = TRUE,
#'     text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#'
#' # No error message to report, with text added:
#' saferDev::get_message(data = "sum(1)", kind = "error", header = TRUE, print_no = TRUE,
#'     text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#'
#' # A standard message is not reported with kind = "error":
#' saferDev::get_message(data = "message('ahah')", kind = "error", header = TRUE, print_no = TRUE,
#'     text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#'
#' # Standard message to report, with text added:
#' saferDev::get_message(data = "message('ahah')", kind = "message", header = TRUE, print_no = TRUE,
#'     text = "IN A", env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
#'
#' # Messages of a ggplot2 plot caught:
#' if(base::requireNamespace(package = "ggplot2", quietly = TRUE)){
#'     saferDev::get_message(data = "ggplot2::ggplot(data = data.frame(X = 1:10,
#'         stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()",
#'         kind = "message", header = TRUE, print_no = TRUE, text = "IN INSTRUCTION 1", env = NULL,
#'         safer_check = TRUE, lib_path = NULL, error_text = "")
#' }
#' @export
get_message <- function(
    data, 
    kind = "error", 
    header = TRUE, 
    print_no = FALSE, 
    text = NULL, 
    env = NULL, 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = "" 
){  
    # DEBUGGING
    # data = "base::log(-1)" ; kind = "warning" ; header = TRUE ; print_no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # function_name <- "get_message" ; arg_user_setting = base::list(x = as.name(x = "get_message"), kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "") ; arg_names <- c("data", "kind",  "header",  "print_no",  "text", "env", "safer_check", "lib_path", "error_text")
    # data = "sum(1)" ; kind = "warning" ; header = TRUE ; print_no = FALSE ; text = NULL ; env = NULL; safer_check = TRUE ; lib_path = NULL ; error_text = ""  
    # set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; data = 'gg_boxplot(data1 = obs1, y = "Time", categ = "Group1")' ; kind = "warning" ; header = TRUE ; print_no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = ""  
    # data = "message('ahah')" ; kind = "error" ; header = TRUE ; print_no = TRUE ; text = "IN A" ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = "" 
    # data = 'ggplot2::ggplot(data = data.frame(X = "a", stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()' ; kind = "message" ; header = TRUE ; print_no = FALSE ; text = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = "" 
    # data = 'ggplot2::ggplot(data = data.frame(X = "a", stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()' ; kind = "warning" ; header = TRUE ; print_no = FALSE ; text = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = "" 
    # data = "emmeans::emmeans(object = emm.rg, specs = contrast.var)" ; kind = "message" ; header = TRUE ; print_no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = "" 
    # data = sum("a") ; kind = "message" ; header = TRUE ; print_no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = "" 
    # data = NULL ; kind = "warning" ; header = TRUE ; print_no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = "" 
    # function_name <- "get_message" ; arg_user_setting = base::list(x = as.name(x = "get_message"), kind = "error", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "") ; arg_names <- c("data", "kind",  "header",  "print_no",  "text", "env", "safer_check", "lib_path", "error_text")
    # data = str4 ; kind = "error" ; header = TRUE ; print_no = TRUE ; text = "IN FUN1" ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # str7 <- "ggplot2::ggplot(data = data.frame(X = c(1:10, NA), stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()" ; data = str7 ; kind = "warning" ; header = TRUE ; print_no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # function_name <- "get_message" ; arg_user_setting = base::list(x = as.name(x = "get_message"), kind = "warning", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "") ; arg_names <- c("data", "kind",  "header",  "print_no",  "text", "env", "safer_check", "lib_path", "error_text")
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
    # data = 'saferDev::colons_check(x = test, safer_check = TRUE, lib_path = NULL, error_text = "")' ; kind = "message" ; header = TRUE ; print_no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # function_name <- "get_message" ; arg_user_setting = base::list(x = as.name(x = "get_message"), kind = "message", header = TRUE, print_no = FALSE, text = NULL, env = NULL, safer_check = TRUE, lib_path = NULL, error_text = "") ; arg_names <- c("data", "kind",  "header",  "print_no",  "text", "env", "safer_check", "lib_path", "error_text")
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
    # warning: personalized compared to backbone (the data argument is not evaluated below)
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names[-1], # first argument data removed because returns a bug if data is an unquote and uncorrect expression. Checked below
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names[-1]
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
    tempo_arg <-base::c(
        # "data", # inactivated because returns a bug if data is an unquote and uncorrect expression. Anyway: can be anything
        "kind", 
        "header", 
        "print_no",
        # "text",  # inactivated because can be null
        # "env",  # inactivated because can be null 
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
        # "data", # inactivated because returns a bug if data is an unquote and uncorrect expression. Anyway: can be anything
        "kind", 
        "header", 
        "print_no",
        "text",
        # "env", # inactivated because an environment can be empty (base::length() of an environment is its number of objects). Checked by arg_check() below
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
                "saferDev:::.base_op_check",
                "saferDev::arg_check"
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
    # tempo <- saferDev::arg_check(data = data, class = "character", typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # inactivated because returns a bug if data is an unquote and uncorrect expression. Anyway: can be anything
    tempo <- saferDev::arg_check(data = kind, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, options = base::c("error", "warning", "message"), double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = header, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = print_no, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = text)){
        tempo <- saferDev::arg_check(data = text, class = NULL, typeof = NULL, mode = "character", length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = TRUE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = env)){
        tempo <- saferDev::arg_check(data = env, class = "environment", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = TRUE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        # "data", # inactivated because returns a bug if data is an unquote and uncorrect expression. Anyway: can be anything
        # "kind" # already checked in arg_check section because it is an optional arg
        # "text" # inactivated because can be ""
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
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
    # warning: personalized compared to backbone. The instruction is drawn on the NULL pdf device opened in the main code: on exit, this device and those left open by the instruction are closed (instead of an internal error), and the initial current device is restored
    # no graphics::par() save and restore: the instruction does not draw on the devices of the user, and graphics::par() would reset their multi-panel layout (the next plot would start a new page)
    dev_list <- grDevices::dev.list() # devices open before running the function (NULL if none)
    dev_cur_ini <- grDevices::dev.cur() # current device before running the function (1, the null device, if none)
    base::on.exit(expr = {
        base::invisible(x = base::lapply(X = base::setdiff(x = grDevices::dev.list(), y = dev_list), FUN = function(x){grDevices::dev.off(which = x)})) # close the devices opened during the execution
        if(dev_cur_ini %in% grDevices::dev.list()){ # FALSE if no device was open, or if the instruction has closed the initial current device
            base::invisible(x = grDevices::dev.set(which = dev_cur_ini)) # dev.off() makes another device current
        }
    }, add = TRUE, after = TRUE)
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    output <- NULL
    grDevices::pdf(file = NULL, width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = , timestamp = , producer = , author = ) # send plots into a NULL file, no pdf file created. Device closed on exit (see the graphic device checking section)
    tempo_error <- base::try(expr = base::suppressMessages(expr = base::suppressWarnings(expr = base::eval(expr = base::parse(text = base::is.null(x = data), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = if(base::is.null(x = env)){base::parent.frame(n = 1)}else{env}, enclos = base::environment(fun = NULL)), classes = "warning"), classes = "message"), silent = TRUE, outFile = base::getOption(x = "try.outFile", default = base::stderr())) # deal first with data = NULL: return NULL
    if(base::all(tempo_error == TRUE, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE data ARGUMENT MUST BE A SINGLE CHARACTER STRING.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        tempo_error <- base::try(expr = base::suppressMessages(expr = base::suppressWarnings(expr = base::eval(expr = base::parse(text = base::is.character(x = data), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = if(base::is.null(x = env)){base::parent.frame(n = 1)}else{env}, enclos = base::environment(fun = NULL)), classes = "warning"), classes = "message"), silent = TRUE, outFile = base::getOption(x = "try.outFile", default = base::stderr())) # deal first with data = NULL: return NULL
        if(base::all(tempo_error == TRUE, na.rm = TRUE)){
            # single evaluation of the instruction: base::try() catches the error, and the calling handlers below record and muffle the warnings and the standard messages. These handlers are the closest to the instruction, so that outer handlers (knitr, testthat, base::suppressMessages()) cannot catch the warnings and messages first
            caught_env <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L) # environment receiving the caught warnings and messages: the handlers below modify it in place, so that no superassignment (<<-) is required
            caught_env$warn <- NULL
            caught_env$message <- NULL
            w_handler <- function(w){ # warning handler
                caught_env$warn <- base::as.character(x = w) # only the last warning is kept
                base::tryInvokeRestart(r = "muffleWarning") # tryInvokeRestart(): no error if the warning is signaled without muffle restart (e.g., by base::signalCondition()), as in base::suppressWarnings()
            }
            m_handler <- function(m){ # standard message handler
                caught_env$message <- base::c(caught_env$message, base::conditionMessage(c = m)) # all the standard messages are kept
                base::tryInvokeRestart(r = "muffleMessage")
            }
            tempo_error <- base::try(
                expr = base::withCallingHandlers(
                    expr = {
                        tempo_value <- base::eval(expr = base::parse(text = data, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = if(base::is.null(x = env)){base::parent.frame(n = 1)}else{env}, enclos = base::environment(fun = NULL)) # warning: keep this writing, which appears in the returned errors and warnings raised at the top level of the instruction (e.g., "In base::eval(expr = base::parse(text = data, file = "", n = NULL,  : "). Printing cannot be prevented, when data = "show(1)" for instance
                        if(base::any(base::class(x = tempo_value) %in% base::c("gg", "ggplot"), na.rm = TRUE)){ # %in% never returns NA
                            base::print(x = tempo_value) # a ggplot2 object emits its errors, warnings and messages when printed (here on the NULL pdf device)
                        }
                        NULL # the value of the instruction is not returned, so that only the try-error of base::try() means an error (an instruction that assigns the result of base::try() returns a try-error value without stopping)
                    },
                    warning = w_handler, # BEWARE: w_handler and m_handler are functions written without (), like in other functions with FUN argument
                    message = m_handler
                ),
                silent = TRUE,
                outFile = base::getOption(x = "try.outFile", default = base::stderr())
            )
        }else{
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE data ARGUMENT MUST BE A SINGLE CHARACTER STRING.", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if( ! base::all(base::class(x = tempo_error) == "try-error", na.rm = TRUE)){ # no error: tempo_error is the NULL returned by the expression of base::withCallingHandlers() above, not the try-error object of base::try() # no NA returned using base::class()
            tempo_error <- NULL
        }
        tempo_warn <- caught_env$warn # last warning, NULL if none
        tempo_message <- caught_env$message # all the standard messages, NULL if none
        if(kind == "error" & ! base::is.null(x = tempo_error)){ # 
            if(header == TRUE){
                tempo_error[1] <- base::gsub(x = tempo_error[1], pattern = "^Error i|^error i|^ERROR I", replacement = "I", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                output <- base::paste0("ERROR MESSAGE REPORTED", base::ifelse(test = base::is.null(x = text), yes = "", no = " "), text, ":\n", tempo_error[1], collapse = NULL, recycle0 = FALSE) #
            }else{
                output <- tempo_error[1]
            }
        }else if(kind == "error" & base::is.null(x = tempo_error) & print_no == TRUE){
            output <- base::paste0("NO ERROR MESSAGE REPORTED", base::ifelse(test = base::is.null(x = text), yes = "", no = " "), text, collapse = NULL, recycle0 = FALSE)
        }else if(kind != "error" & ( ! base::is.null(x = tempo_error)) & print_no == TRUE){
            output <- base::paste0("NO POTENTIAL ", base::ifelse(test = kind == "warning", yes = "WARNING", no = "STANDARD"), " MESSAGE BECAUSE OF ERROR MESSAGE REPORTED", base::ifelse(test = base::is.null(x = text), yes = "", no = " "), text, collapse = NULL, recycle0 = FALSE)
        }else if(base::is.null(x = tempo_error) & kind != "error"){ #is.null() TRUE means no error detected
            if(kind == "warning" & base::length(x = tempo_warn) > 0){ # to avoid base::character(0)
                # below can also be for ggplot2 
                if(header == TRUE){
                    if(base::any(base::grepl(x = tempo_warn, pattern = "^simpleWarning i|^Warning i", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), na.rm = TRUE)){
                        tempo_warn <- base::gsub(x = tempo_warn, pattern = "^simpleWarning i|^Warning i", replacement = "I", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                    }
                    output <- base::paste0("WARNING MESSAGE REPORTED", base::ifelse(test = base::is.null(x = text), yes = "", no = " "), text, ":\n", tempo_warn, collapse = NULL, recycle0 = FALSE) #
                }else{
                    output <- tempo_warn #
                }
            }else if(kind == "warning" & base::length(x = tempo_warn) == 0 & print_no == TRUE){
                output <- base::paste0("NO WARNING MESSAGE REPORTED", base::ifelse(test = base::is.null(x = text), yes = "", no = " "), text, collapse = NULL, recycle0 = FALSE)
            }else if(kind == "message" & base::length(x = tempo_message) > 0){ 
                tempo_message <- base::paste0(base::sub(pattern = "\n$", replacement = "", x = tempo_message, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), collapse = "\n", recycle0 = FALSE) # all the standard messages in a single string, one per line (final line break of each message removed)
                if(header == TRUE){
                    output <- base::paste0("STANDARD MESSAGE REPORTED", base::ifelse(test = base::is.null(x = text), yes = "", no = " "), text, ":\n", tempo_message, collapse = NULL, recycle0 = FALSE) #
                }else{
                    output <- tempo_message #
                }
            }else if(kind == "message" & base::length(x = tempo_message) == 0 & print_no == TRUE){
                output <- base::paste0("NO STANDARD MESSAGE REPORTED", base::ifelse(test = base::is.null(x = text), yes = "", no = " "), text, collapse = NULL, recycle0 = FALSE)
            } # no need else{} here because output is already NULL at first
        }
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output) # do not use base::message() because the idea is to reuse the message
    #### end output
    
}
