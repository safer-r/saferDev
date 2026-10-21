#' @title \code{for} Loop Number Check
#' @description
#' Check the sequence expression of every \code{for} loop of a function. If the number of iterations is not fixed by the writing itself (e.g., \code{1:length(x)}), a checking line is added before the loop, issuing a warning at execution if the sequence is empty, contains \code{NA} or infinite values, has a single value, or is decreasing. Return a table-like message of the problematic lines and a corrected version of the submitted function.
#' @param x A function name, written without quotes and brackets.
#' @param export Single logical value. Write the corrected function into a file instead of returning it? If \code{TRUE}, the file \code{<function name>_safer.R} is written in \code{out_path} (e.g., \code{fun_safer.R} for \code{x = fun} or \code{x = pkg::fun}). It contains \code{fun_safer <- function(...) ...}, so that \code{base::source()} recreates the corrected function. Nothing is written if nothing has to be corrected. An error is returned if the file already exists, or if \code{x} is not a function name (e.g., \code{function(x)\{x\}} or \code{FUN_LIST$fun}).
#' @param out_path Single character string indicating the path of the folder where to write the file when \code{export = TRUE}. \code{"."} (default) means the R working directory. Ignored if \code{export = FALSE}.
#' @param safer_check Single logical value. Perform some "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A table-like message is always printed, with one row per reported loop (column 1: line number; column 2: construct; column 3: code before the body), or a message saying that everything seems fine.
#'
#' If \code{export = FALSE}: the corrected function, or \code{NULL} if nothing has been corrected.
#'
#' If \code{export = TRUE}: \code{NULL}, returned invisibly. The corrected function is written in a file (see the \code{export} argument).
#' @details
#' Not reported: hard numbers, i.e., an integer-valued number, possibly negative, or two such numbers joined by \code{:} (e.g., \code{3}, \code{1:3}, \code{1e3}, \code{1L:3L}, \code{0:0}, \code{-3:-1}, \code{1:0}), and a single \code{seq_len(...)} or \code{seq_along(...)} call. These writings fully fix the iterations, and their control is left to the writer.
#'
#' Reported: everything else, e.g., \code{for(i in x)}, \code{1:n}, \code{1:length(x)}, \code{1:nrow(df)}, \code{1:(n - 1)}, \code{seq_along(x) - 1}, \code{1:2.5}, \code{1:NA} or \code{1:Inf}. Comments and strings are ignored.
#'
#' Why: with \code{x <- character()}, \code{for(i in 1:length(x))} runs twice, because \code{1:length(x)} is \code{c(1, 0)}. Use \code{base::seq_along(along.with = x)} instead.
#'
#' Correction: each reported loop becomes \code{tempo_for_seqN <- <SEQUENCE> ; <CHECKS> ; for(i in tempo_for_seqN)<BODY>}, N being the rank of the report. The checks warn if the sequence is empty, contains \code{NA} or infinite values, has a single value, has only negative values, or is decreasing. The loop thus runs on the checked sequence. Errors of the sequence itself (like \code{1:NA}) are not caught. If the loop is not a standalone statement (e.g., unbraced body of an \code{if}, or argument value), the whole is enclosed in curly brackets. The rest of the code is unchanged. The comments are kept in the exported file, but can be lost in the returned function. The returned function is evaluated in a new environment whose parent is the environment of the submitted function: check it before replacing the original function.
#'
#' Warning: a reported loop is left unchanged, and listed in the message, when:
#' \itemize{
#' \item the name \code{tempo_for_seqN} is already used in the function code (including comments and strings);
#' \item the loop is inside the sequence expression of another reported loop;
#' \item the loop must be enclosed in curly brackets and contains comments;
#' \item the loop must be enclosed in curly brackets and ends with another loop that must also be (e.g., \code{for(a in 1:n) for(b in 1:length(x)){...}}).
#' }
#'
#' Warning: only the writing is analyzed. In \code{n <- length(x) ; for(i in 1:n)}, \code{1:n} is reported as a variable, whatever \code{n}.
#'
#' Line numbers are indicative. They start at the \code{<- function} line (\code{#'} header lines not counted). Compiled functions, like \code{saferDev::colons_check}, have no comments: their line numbers differ from their source file.
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @examples
#' # Warning: these examples may not work well when using the "Run examples" link
#' # because of a particular environment. Please, copy-paste in a local environment.
#' # See also https://safer-r.github.io/saferDev/articles/for_loop_nb_check.html
#' fun1 <- function(x){ for(i in base::seq_along(along.with = x)){ base::print(x = i) } }
#' saferDev::for_loop_nb_check(fun1) # everything fine: NULL returned
#' fun2 <- function(x){ for(i in 1:length(x)){ base::print(x = i) } }
#' fun2_safer <- saferDev::for_loop_nb_check(fun2) # 1:length(x) reported
#' fun2_safer # corrected function
#' fun2_safer(x = base::character()) # warning: 1:length(x) is c(1, 0), a decreasing sequence
#' # Write the corrected function in the fun2_safer.R file of a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::for_loop_nb_check(x = fun2, export = TRUE, out_path = tempo_dir)
#'
#' @export
for_loop_nb_check <- function(
    x, 
    export = FALSE, 
    out_path = ".", 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # x = fun ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "for_loop_nb_check" ; arg_user_setting = base::list(x = as.name(x = "fun"), export = FALSE, out_path = ".", lib_path = NULL, safer_check = TRUE, error_text = " INSIDE A")
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\for_loop_nb_check.R") ; x = for_loop_nb_check ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::for_loop_nb_check does not return the same number of code lines

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
    if(base::any(x = tempo_log, na.rm = TRUE)) {
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
        "out_path", 
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
    # always executed: lib_path is always checked, whatever the value of safer_check
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
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
        }
    }else{
        lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
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

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )

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
    tempo <- saferDev::arg_check(data = out_path, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
    if(export == TRUE){
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
                "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXISTS:\n", 
                out_path, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        out_dir <- out_path # folder path after trailing / or \\ removal
        if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
            out_fun_name <- base::as.character(x = arg_user_setting$x)[3] # form "package::function" or "package:::function"
        }else if(base::is.symbol(x = arg_user_setting$x)){
            out_fun_name <- base::as.character(x = arg_user_setting$x)[1]
        }else{
            # multi-component written name (like FUN_LIST$fun), or inline definition (like function(x){x}), or other unexpected writing: cannot determine a file name reliably. FUN_LIST$fun would give fun_safer.R, but a same name can designate different functions depending on the object browsed by $: the written form is not reliable for the file name
            tempo_cat <- base::paste0(
                error_text_start,
                "THE x ARGUMENT MUST BE DESIGNATED BY A FUNCTION NAME (NOT BY AN INLINE FUNCTION DEFINITION, NOR BY A COMPONENT WRITING LIKE obj$fun) WHEN THE export ARGUMENT IS TRUE.\nHERE IT IS:\n",
                base::paste0(base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = "\n", recycle0 = FALSE),
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
                "\n\nPLEASE, RENAME OR REMOVE THIS FILE, OR CHANGE THE out_path ARGUMENT BEFORE RERUNNING ", 
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
    # recover the code of the checked function
    code <- utils::capture.output(x = x, file = NULL, append = FALSE, type = NULL, split = FALSE)
    # removal of the attribute lines printed for compiled functions (e.g., "<bytecode: 0x...>", "<environment: ...>", "<srcref: ...>"). Warning: the pattern cannot be simply a line starting by "<", because a valid R code line can start by "<" (binary operator continuation)
    code <- code[ ! base::grepl(x = code, pattern = "^<(bytecode|environment|srcref): ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)]
    pd <- utils::getParseData(x = base::parse(text = base::paste0(code, collapse = "\n", recycle0 = FALSE), keep.source = TRUE))
    # terminal tokens (comments excluded), sorted by source position
    term_rows <- base::which(x = pd$terminal == TRUE & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
    term_rows <- term_rows[base::order(x = pd$line1[term_rows], pd$col1[term_rows])]
    # positions in term_rows of the FOR keyword tokens
    for_idx <- base::which(x = pd$token[term_rows] == "FOR", arr.ind = FALSE, useNames = TRUE)
    findings_line <- base::integer(length = 0)
    findings_kind <- base::character(length = 0)
    findings_code <- base::character(length = 0)
    findings_in_line1 <- base::integer(length = 0)
    findings_in_col2 <- base::integer(length = 0)
    findings_rp_line2 <- base::integer(length = 0)
    findings_rp_col2 <- base::integer(length = 0)
    findings_body_line2 <- base::integer(length = 0)
    findings_body_col2 <- base::integer(length = 0)
    findings_braced <- base::logical(length = 0)
    findings_kid <- base::integer(length = 0)
    for(i in base::seq_along(along.with = for_idx)){ # seq_along() instead of 1:base::length() avoids iterating over c(1, 0) when for_idx is empty, which would be reported by this very function
        kid <- term_rows[for_idx[i]]
        # search of the parentheses of the condition by scanning the terminal tokens located after the FOR keyword in source order. Warning: the search cannot be done through the parse tree, because the forcond expr of for() owns the parentheses
        depth <- 0
        lp <- NULL
        rp <- NULL
        if(for_idx[i] < base::length(x = term_rows)){ # guard to avoid an infinite while() below if the keyword token were the last terminal token (unreachable with parseable R code, but secured)
            j <- for_idx[i] + 1
            while(j <= base::length(x = term_rows)){
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
                j <- j + 1
            }
        }
        if(base::is.null(x = lp) == TRUE || base::is.null(x = rp) == TRUE){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without () after for cannot be parsed).
            tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "NO () FOUND AFTER THE KEYWORD TOKEN\ntoken: ", pd$token[kid], intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        # positions of the ( and ) of the condition in term_rows, and search of the IN token between them
        lp_pos <- base::which(x = term_rows == lp, arr.ind = FALSE, useNames = TRUE)[1]
        rp_pos <- base::which(x = term_rows == rp, arr.ind = FALSE, useNames = TRUE)[1]
        in_pos <- NULL
        if(lp_pos + 1 <= rp_pos - 1){ # guard to avoid an infinite while() below (unreachable with parseable R code, because the loop variable is always written between the ( and the in, but secured)
            j <- lp_pos + 1
            while(j <= rp_pos - 1){
                if(pd$token[term_rows[j]] == "IN"){
                    in_pos <- j
                    break
                }
                j <- j + 1
            }
        }
        if(base::is.null(x = in_pos) == TRUE){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without in between the ( and the ) of a for() condition cannot be parsed).
            tempo_cat <- base::paste0("INTERNAL ERROR 5 IN ", intern_error_text_start, "NO in FOUND BETWEEN THE ( AND THE ) OF A for() CONDITION", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)            # nocov end
        }
        # rows of the IN token and of the loop variable token (the terminal token between the ( and the in)
        in_kid <- term_rows[in_pos]
        # span of the construct body: the expr child of the for expression that is after the forcond in source order. Warning: the parse tree is described by the pd$id and pd$parent columns, whose values must be converted into pd row numbers through a lookup vector, because the two numbering systems are different (the id numbering skips the non terminal rows)
        id2row <- base::seq_len(length.out = base::nrow(x = pd))
        base::names(x = id2row) <- base::as.character(x = pd$id)
        siblings <- base::which(x = pd$parent == pd$parent[kid], arr.ind = FALSE, useNames = TRUE)
        siblings <- siblings[pd$token[siblings] != "FOR"]
        if(base::length(x = siblings) != 2){ # forcond and body expr: exactly 2 children besides the FOR keyword itself
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (a for() construct with a different number of children cannot be parsed).
            tempo_cat <- base::paste0("INTERNAL ERROR 7 IN ", intern_error_text_start, "UNEXPECTED NUMBER OF CHILDREN FOR A for() CONSTRUCT IN THE PARSE TREE", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        siblings <- siblings[base::order(x = pd$line1[siblings], pd$col1[siblings])]
        body_row <- siblings[2]
        body_span <- base::c(pd$line1[body_row], pd$col1[body_row], pd$line2[body_row], pd$col2[body_row])
        # tokens of the sequence expression: between the IN token and the ) of the condition
        seq_pos1 <- in_pos + 1
        seq_pos2 <- rp_pos - 1
        if(seq_pos2 < seq_pos1){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code without sequence expression after in cannot be parsed).
            tempo_cat <- base::paste0("INTERNAL ERROR 6 IN ", intern_error_text_start, "NO SEQUENCE EXPRESSION FOUND AFTER THE in TOKEN", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
        seq_tokens <- term_rows[base::seq_len(length.out = seq_pos2 - seq_pos1 + 1) + seq_pos1 - 1]
        # 1) clean writings: seq_len() or seq_along() call, namespaced or not, whatever the arguments, but ONLY if the call is the whole sequence expression (e.g., seq_along(x) - 1 is NOT clean, because it can give c(0, 1))
        seq_clean <- FALSE
        tok1 <- pd$token[seq_tokens[1]]
        fun_name_pos <- NULL
        if(tok1 == "SYMBOL_FUNCTION_CALL" && pd$text[seq_tokens[1]] %in% base::c("seq_len", "seq_along")){
            fun_name_pos <- 1
        }else if(base::length(x = seq_tokens) >= 3 && tok1 == "SYMBOL_PACKAGE" && pd$token[seq_tokens[2]] %in% base::c("NS_GET", "NS_GET_INT") && pd$token[seq_tokens[3]] == "SYMBOL_FUNCTION_CALL" && pd$text[seq_tokens[3]] %in% base::c("seq_len", "seq_along")){
            fun_name_pos <- 3
        }
        if(base::is.null(x = fun_name_pos) == FALSE){
            # the call must occupy the whole sequence expression: its closing ) must be the last token of the sequence expression
            if(fun_name_pos + 1 > base::length(x = seq_tokens) || pd$token[seq_tokens[fun_name_pos + 1]] != "'('"){
                # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (a function call without () cannot be parsed).
                tempo_cat <- base::paste0("INTERNAL ERROR 3 IN ", intern_error_text_start, "NO ( FOUND AFTER THE seq_len OR seq_along FUNCTION NAME TOKEN", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                # nocov end
            }
            depth <- 0
            k <- fun_name_pos + 1 # the scan includes the ( token itself
            while(k <= base::length(x = seq_tokens)){
                if(pd$token[seq_tokens[k]] == "'('"){
                    depth <- depth + 1
                }else if(pd$token[seq_tokens[k]] == "')'"){
                    depth <- depth - 1
                    if(depth == 0){
                        break
                    }
                }
                k <- k + 1
            }
            if(depth != 0){ # nocov start
                # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (R code with unbalanced parentheses cannot be parsed).
                tempo_cat <- base::paste0("INTERNAL ERROR 4 IN ", intern_error_text_start, "UNBALANCED PARENTHESES DETECTED IN A seq_len OR seq_along CALL", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                # nocov end
            }else if(k == base::length(x = seq_tokens) && pd$token[seq_tokens[k]] == "')'"){
                seq_clean <- TRUE
            }
        }
        # 2) hard-number detection: STRICT structural writing only, i.e., [optional unary -] NUM_CONST [ ':' [optional unary -] NUM_CONST ], with nothing else. This rejects binary operators (e.g., 1:3 - 1, which gives c(-2, -1) at execution, and 2 - 1), chained colons (e.g., 1:3:1) and everything else
        if(seq_clean == FALSE){
            hard_log <- TRUE
            j <- 1
            tok_nb <- base::length(x = seq_tokens)
            # first operand: optional unary minus then NUM_CONST
            if(j <= tok_nb && pd$token[seq_tokens[j]] == "'-'"){
                j <- j + 1
            }
            if(j > tok_nb || pd$token[seq_tokens[j]] != "NUM_CONST"){ # j > tok_nb also covers the empty seq_tokens (unreachable because internal error 4 above, but secured)
                hard_log <- FALSE
            }else{
                j <- j + 1
                if(j <= tok_nb){
                    if(pd$token[seq_tokens[j]] != "':'"){
                        hard_log <- FALSE
                    }else{
                        j <- j + 1
                        if(j <= tok_nb && pd$token[seq_tokens[j]] == "'-'" && j < tok_nb){ # the optional unary minus must not be the last token (j < tok_nb because a NUM_CONST must follow)
                            j <- j + 1
                        }
                        if(j != tok_nb || pd$token[seq_tokens[j]] != "NUM_CONST"){ # j != tok_nb covers any missing or extra token (e.g., 1:3 - 1, 1:3:1, 1:): the short-circuit avoids the out of bounds indexing of the second condition
                            hard_log <- FALSE
                        }
                    }
                } # else: single NUM_CONST only: hard_log stays TRUE
            }
            # literal validation: every NUM_CONST must be a finite integer-valued number (2.5, 1e-3, NA, Inf, NaN, TRUE are not)
            if(hard_log == TRUE){
                for(j in base::seq_along(along.with = seq_tokens)){ # seq_along() instead of 1:base::length() avoids iterating over c(1, 0) when seq_tokens is empty, which would be reported by this very function
                    if(pd$token[seq_tokens[j]] == "NUM_CONST"){
                        val_txt <- pd$text[seq_tokens[j]]
                        val_txt <- base::gsub(x = val_txt, pattern = "_", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE) # digits separator (1_000) introduced in R 3.6.0
                        val_txt <- base::sub(x = val_txt, pattern = "L$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove the integer suffix (1L), because base::as.numeric() does not parse it
                        val <- base::suppressWarnings(expr = base::as.numeric(x = val_txt)) # parses integers, decimals, exponentials, hexadecimal literals (R >= 4.5.0 required by the package) and NA/NaN/Inf, the last ones rejected below
                        if(base::length(x = val) != 1 || base::is.na(x = val) == TRUE || base::is.finite(x = val) == FALSE || val != base::floor(x = val)){
                            hard_log <- FALSE
                            break
                        }
                    }
                }
            }
            if(hard_log == FALSE){
                findings_line <- base::c(findings_line, pd$line1[kid], recursive = TRUE, use.names = TRUE)
                findings_kind <- base::c(findings_kind, "for", recursive = TRUE, use.names = TRUE)
                findings_code <- base::c(findings_code, base::trimws(x = code[pd$line1[kid]], which = "left", whitespace = "[ \\t]"), recursive = TRUE, use.names = TRUE)
                # positions needed by the modification (below): span of the sequence expression (from the IN token to the ) of the condition), span of the construct body (to wrap the construct when needed), and enclosing-expression start (for the statement boundary detection). Warning: kid is the row of the FOR token, needed to recover the for keyword position after the filtering below
                findings_kid <- base::c(findings_kid, kid, recursive = TRUE, use.names = TRUE)
                findings_in_line1 <- base::c(findings_in_line1, pd$line1[in_kid], recursive = TRUE, use.names = TRUE)
                findings_in_col2 <- base::c(findings_in_col2, pd$col2[in_kid], recursive = TRUE, use.names = TRUE)
                findings_rp_line2 <- base::c(findings_rp_line2, pd$line2[rp], recursive = TRUE, use.names = TRUE)
                findings_rp_col2 <- base::c(findings_rp_col2, pd$col2[rp], recursive = TRUE, use.names = TRUE)
                findings_body_line2 <- base::c(findings_body_line2, body_span[3], recursive = TRUE, use.names = TRUE)
                findings_body_col2 <- base::c(findings_body_col2, body_span[4], recursive = TRUE, use.names = TRUE)
                # boundary detection (for the insertion below): the checking line can be inserted before the for keyword without wrapping only when the for construct is a statement of a braced block ({ or of the top level of the function body written without braces). Detection through the parse tree: the enclosing expr of the for expression (the parent of the parent of the FOR token) must start with a { terminal token. Any other enclosing writing (unbraced body of if/else/for/while/repeat, assignment value, function argument, operator operand, etc.) requires the wrap of the whole construct, because a statement insertion would otherwise break the binding of the enclosing writing
                for_expr_row <- id2row[base::as.character(x = pd$parent[kid])] # row of the expr containing the for construct
                enclosing_row <- id2row[base::as.character(x = pd$parent[for_expr_row])] # row of the enclosing expr
                if(base::is.na(x = enclosing_row)){ # the for construct is a top level expression of the recovered code (cannot happen inside a function body, but secured for a top level for())
                    # nocov start
                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (a for() construct at the top level of the recovered code cannot be produced by the parser from a function definition).
                    enclosing_line1 <- pd$line1[for_expr_row]
                    enclosing_col1 <- pd$col1[for_expr_row]
                    # nocov end
                }else{
                    enclosing_line1 <- pd$line1[enclosing_row]
                    enclosing_col1 <- pd$col1[enclosing_row]
                }
                first_tok_rows <- term_rows[pd$line1[term_rows] == enclosing_line1 & pd$col1[term_rows] == enclosing_col1]
                if(base::length(x = first_tok_rows) != 1){
                    # nocov start
                    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values (a parse tree node without exactly one terminal token at its start position cannot be produced by the R parser).
                    tempo_cat <- base::paste0("INTERNAL ERROR 8 IN ", intern_error_text_start, "NO OR SEVERAL TERMINAL TOKENS FOUND AT THE START POSITION OF THE ENCLOSING EXPRESSION OF A for() CONSTRUCT", intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    # nocov end
                }
                findings_braced <- base::c(findings_braced, pd$text[first_tok_rows] == "{", recursive = TRUE, use.names = TRUE)
            }
        }
    }
    # modification of the recovered code: insertion of a checking line before each reported for() construct (see the @details section), from the last construct to the first one so that the positions of the preceding constructs remain valid. Warning: findings_* are ordered by source position, because term_rows is sorted and the loop appends in order
    inserted_log <- NULL # one value per reported construct: TRUE if the checking line has been inserted, FALSE otherwise (NULL when no construct is reported)
    skipped_text <- base::character(length = 0)
    if(base::length(x = findings_line) != 0){
        modified_code <- code
        inserted_log <- base::rep(x = TRUE, times = base::length(x = findings_line))
        # column-shift events: each modification applied below changes the length of the concerned line, which shifts the character positions located after the modification point. Each event records the line number, the last column that is not shifted (in the coordinates used when the event is recorded) and the shift to apply to the positions located after it. The events are used to adjust the positions of the constructs processed later (the constructs are applied from the last one to the first one, see below)
        ev_line <- base::integer(length = 0)
        ev_col <- base::integer(length = 0)
        ev_shift <- base::double(length = 0)
        # tempo object names: specific names, refused if already present anywhere in the recovered code (including comments and strings), to avoid any masking
        tempo_names <- base::paste0("tempo_for_seq", base::seq_len(length.out = base::length(x = findings_line)), collapse = NULL, recycle0 = FALSE, sep = "")
        all_txt <- base::paste0(modified_code, collapse = "\n", recycle0 = FALSE)
        name_log <- base::vapply(X = tempo_names, FUN = function(x){base::grepl(x = all_txt, pattern = base::paste0("\\b", x, "\\b"), ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)}, FUN.VALUE = base::logical(length = 1), USE.NAMES = FALSE) # word boundary so that tempo_for_seq1 does not match tempo_for_seq10
        if(base::any(name_log, na.rm = TRUE)){
            name_idx <- base::which(x = name_log, arr.ind = FALSE, useNames = TRUE)
            for(k in base::seq_along(along.with = name_idx)){
                i <- name_idx[k]
                skipped_text <- base::c(skipped_text, base::paste0("line ", findings_line[i], ": the object name ", tempo_names[i], " is already used in the checked function code", collapse = NULL, recycle0 = FALSE), recursive = TRUE, use.names = TRUE)
            }
            # the constructs whose name is refused do not get any modification: remove their positions from all the findings vectors
            keep_log <- ! name_log
            findings_line <- findings_line[keep_log]
            findings_kind <- findings_kind[keep_log]
            findings_code <- findings_code[keep_log]
            findings_in_line1 <- findings_in_line1[keep_log]
            findings_in_col2 <- findings_in_col2[keep_log]
            findings_rp_line2 <- findings_rp_line2[keep_log]
            findings_rp_col2 <- findings_rp_col2[keep_log]
            findings_body_line2 <- findings_body_line2[keep_log]
            findings_body_col2 <- findings_body_col2[keep_log]
            findings_braced <- findings_braced[keep_log]
            findings_kid <- findings_kid[keep_log]
            tempo_names <- tempo_names[keep_log]
            inserted_log <- inserted_log[keep_log]
        }
        # guard: a reported for() construct must not be located inside the sequence expression of another reported for() construct (overlapping insertion spans). Detection: the span of the sequence expression of a construct contains the position of the for keyword of the other
        overlap_log <- base::rep(x = FALSE, times = base::length(x = findings_line))
        if(base::length(x = findings_line) > 1){
            findings_for_line1 <- pd$line1[findings_kid]
            findings_for_col1 <- pd$col1[findings_kid]
            for(i in base::seq_along(along.with = findings_line)){
                if(findings_in_line1[i] == findings_rp_line2[i]){ # sequence expression of construct i written on a single line
                    for(j in base::seq_along(along.with = findings_line)){
                        if(j == i){
                            next
                        }
                        if(findings_for_line1[j] == findings_in_line1[i] & findings_for_col1[j] >= findings_in_col2[i] + 1 & findings_for_col1[j] <= findings_rp_col2[i] - 1){
                            overlap_log[i] <- TRUE
                            break
                        }
                    }
                }
            }
        }
        if(base::any(overlap_log, na.rm = TRUE)){
            overlap_idx <- base::which(x = overlap_log, arr.ind = FALSE, useNames = TRUE)
            for(k in base::seq_along(along.with = overlap_idx)){
                i <- overlap_idx[k]
                skipped_text <- base::c(skipped_text, base::paste0("line ", pd$line1[findings_kid[i]], ": the for() construct is located inside the sequence expression of another reported for() construct", collapse = NULL, recycle0 = FALSE), recursive = TRUE, use.names = TRUE)
            }
            keep_log <- ! overlap_log
            findings_line <- findings_line[keep_log]
            findings_kind <- findings_kind[keep_log]
            findings_code <- findings_code[keep_log]
            findings_in_line1 <- findings_in_line1[keep_log]
            findings_in_col2 <- findings_in_col2[keep_log]
            findings_rp_line2 <- findings_rp_line2[keep_log]
            findings_rp_col2 <- findings_rp_col2[keep_log]
            findings_body_line2 <- findings_body_line2[keep_log]
            findings_body_col2 <- findings_body_col2[keep_log]
            findings_braced <- findings_braced[keep_log]
            findings_kid <- findings_kid[keep_log]
            tempo_names <- tempo_names[keep_log]
            inserted_log <- inserted_log[keep_log]
        }
        findings_for_line1 <- pd$line1[findings_kid] # line of the for keyword of each kept construct
        findings_for_col1 <- pd$col1[findings_kid] # column of the for keyword of each kept construct
        # rows of the COMMENT tokens, used to detect constructs that cannot be wrapped (see below)
        comment_rows <- base::which(x = pd$token == "COMMENT", arr.ind = FALSE, useNames = TRUE)
        # constructs whose body end is shared with a for() construct written after them (see the guard in the loop below): the wrap of the later construct (applied first, back-to-front order) moves the body-end token away from the position where the earlier construct would end its own body, which would corrupt the wrap extraction of the earlier construct. The earlier constructs are therefore skipped. Warning: only constructs that require the wrap are concerned (see the guard in the loop below), and the later construct must also require the wrap (a later construct enclosed in a braced block only receives an insertion before its for keyword, which never moves the body-end token: the shifted positions are handled by the shift events recorded in the loop)
        body_end_key <- base::paste0(findings_body_line2, ":", findings_body_col2, collapse = NULL, recycle0 = FALSE)
        shared_log <- base::rep(x = FALSE, times = base::length(x = findings_body_line2))
        if(base::length(x = findings_body_line2) > 1){
            for(m in base::seq_along(along.with = findings_body_line2)){
                same_end <- body_end_key == body_end_key[m]
                later_log <- (findings_for_line1 > findings_for_line1[m]) | (findings_for_line1 == findings_for_line1[m] & findings_for_col1 > findings_for_col1[m])
                if(base::any(x = same_end & later_log & findings_braced == FALSE, na.rm = TRUE)){
                    shared_log[m] <- TRUE
                }
            }
        }
        # apply back-to-front: highest (line, column) first, so that the positions of the not-yet-applied constructs remain valid. Per construct, the sequence expression replacement (at the IN position) comes after the insertion (at the for keyword position) in source order
        ord <- base::order(x = findings_for_line1, findings_for_col1, decreasing = TRUE)
        ord_len <- base::length(x = ord)
        for(k in base::seq_len(length.out = ord_len)){
            i <- ord[k]
            tempo_name <- tempo_names[i]
            # guards for constructs that cannot be modified (they remain unmodified and are listed in the skipped_text message below):
            # 1) a construct that requires the wrap (not enclosed in a braced block) cannot contain comments, because the wrap below collapses the construct on a single line, which would merge the comments with the code and comment out the rest of the line
            # 2) a construct that requires the wrap cannot share the end of its body with another reported for() construct written after it, because the wrap of that other construct (applied first, back-to-front order) moves the body-end token away from its position. Both guards must be applied BEFORE any modification of the construct (the sequence replacement of step 2 below), so that a skipped construct remains fully unmodified
            if(findings_braced[i] == FALSE){
                if(base::length(x = comment_rows) != 0){
                    comment_span <- comment_rows[pd$line1[comment_rows] > findings_for_line1[i] | (pd$line1[comment_rows] == findings_for_line1[i] & pd$col1[comment_rows] >= findings_for_col1[i])]
                    comment_span <- comment_span[pd$line1[comment_span] < findings_body_line2[i] | (pd$line1[comment_span] == findings_body_line2[i] & pd$col1[comment_span] <= findings_body_col2[i])]
                    if(base::length(x = comment_span) != 0){
                        skipped_text <- base::c(skipped_text, base::paste0("line ", findings_line[i], ": the for() construct contains comment(s) and cannot be wrapped in curly brackets on a single line", collapse = NULL, recycle0 = FALSE), recursive = TRUE, use.names = TRUE)
                        inserted_log[i] <- FALSE
                        next
                    }
                }
                if(shared_log[i] == TRUE){
                    skipped_text <- base::c(skipped_text, base::paste0("line ", findings_line[i], ": the end of the for() construct body is shared with a for() construct written after it, which prevents the wrap in curly brackets on a single line", collapse = NULL, recycle0 = FALSE), recursive = TRUE, use.names = TRUE)
                    inserted_log[i] <- FALSE
                    next
                }
            }
            # 1) one-line writing of the sequence expression, from the recovered code span. The span can cross several lines: the pieces are pasted with a space, trimmed
            if(findings_in_line1[i] == findings_rp_line2[i]){
                seq_txt <- base::substr(x = modified_code[findings_in_line1[i]], start = findings_in_col2[i] + 1, stop = findings_rp_col2[i] - 1)
            }else{
                seq_pieces <- base::c(
                    base::substr(x = modified_code[findings_in_line1[i]], start = findings_in_col2[i] + 1, stop = base::nchar(x = modified_code[findings_in_line1[i]], type = "chars", allowNA = FALSE, keepNA = NA)),
                    if(findings_rp_line2[i] - findings_in_line1[i] > 1){modified_code[(findings_in_line1[i] + 1):(findings_rp_line2[i] - 1)]}else{base::character(length = 0)}, # guard: a span of exactly 2 lines has no middle line, and the unguarded (in+1):(rp-1) writing would be a descending range in R (c(in+1, in)), pulling the for line into the reconstruction and corrupting the sequence text
                    base::substr(x = modified_code[findings_rp_line2[i]], start = 1, stop = findings_rp_col2[i] - 1)
                )
                seq_txt <- base::trimws(x = base::paste0(seq_pieces, collapse = " ", recycle0 = FALSE), which = "both", whitespace = "[ \t\r\n]")
            }
            seq_txt <- base::trimws(x = seq_txt, which = "both", whitespace = "[ \t\r\n]")
            # one-line deparse of the sequence expression, for a single-line insertion
            ok_parse <- FALSE
            seq_expr <- NULL
            res_parse <- base::try(expr = base::parse(text = seq_txt, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), silent = TRUE)
            if(base::inherits(x = res_parse, what = "try-error") == FALSE && base::length(x = res_parse) == 1){
                seq_expr <- res_parse[[1]]
                ok_parse <- TRUE
            }
            if(ok_parse == FALSE){
                skipped_text <- base::c(skipped_text, base::paste0("line ", findings_line[i], ": the sequence expression cannot be rewritten on a single line", collapse = NULL, recycle0 = FALSE), recursive = TRUE, use.names = TRUE)
                inserted_log[i] <- FALSE
                next
            }
            # 2) replace the sequence expression span by the tempo object name, keeping the surrounding spaces. The length changes of the modified lines are recorded as shift events, used to adjust the positions of the constructs processed later (the constructs are applied from the last one to the first one)
            len_in_before <- base::nchar(x = modified_code[findings_in_line1[i]], type = "chars", allowNA = FALSE, keepNA = NA)
            if(findings_in_line1[i] == findings_rp_line2[i]){
                tempo_line <- modified_code[findings_in_line1[i]]
                modified_code[findings_in_line1[i]] <- base::paste0(
                    base::substr(x = tempo_line, start = 1, stop = findings_in_col2[i]),
                    " ",
                    tempo_name,
                    base::substr(x = tempo_line, start = findings_rp_col2[i], stop = base::nchar(x = tempo_line, type = "chars", allowNA = FALSE, keepNA = NA)),
                    collapse = NULL,
                    recycle0 = FALSE
                )
            }else{
                tempo_line1 <- modified_code[findings_in_line1[i]]
                tempo_line2 <- modified_code[findings_rp_line2[i]]
                len_rp_before <- base::nchar(x = tempo_line2, type = "chars", allowNA = FALSE, keepNA = NA)
                modified_code[findings_in_line1[i]] <- base::paste0(base::substr(x = tempo_line1, start = 1, stop = findings_in_col2[i]), " ", tempo_name, collapse = NULL, recycle0 = FALSE)
                if(findings_rp_line2[i] - findings_in_line1[i] > 1){
                    modified_code[(findings_in_line1[i] + 1):(findings_rp_line2[i] - 1)] <- "" # empty lines keep the line numbering
                }
                modified_code[findings_rp_line2[i]] <- base::substr(x = tempo_line2, start = findings_rp_col2[i], stop = base::nchar(x = tempo_line2, type = "chars", allowNA = FALSE, keepNA = NA))
            }
            len_in_after <- base::nchar(x = modified_code[findings_in_line1[i]], type = "chars", allowNA = FALSE, keepNA = NA)
            ev_line <- base::c(ev_line, findings_in_line1[i], recursive = TRUE, use.names = TRUE)
            ev_col <- base::c(ev_col, findings_in_col2[i], recursive = TRUE, use.names = TRUE)
            ev_shift <- base::c(ev_shift, len_in_after - len_in_before, recursive = TRUE, use.names = TRUE)
            if(findings_in_line1[i] != findings_rp_line2[i]){
                len_rp_after <- base::nchar(x = modified_code[findings_rp_line2[i]], type = "chars", allowNA = FALSE, keepNA = NA)
                ev_line <- base::c(ev_line, findings_rp_line2[i], recursive = TRUE, use.names = TRUE)
                ev_col <- base::c(ev_col, findings_rp_col2[i] - 1, recursive = TRUE, use.names = TRUE)
                ev_shift <- base::c(ev_shift, len_rp_after - len_rp_before, recursive = TRUE, use.names = TRUE)
            }
            # 3) build the checking line: the sequence object is checked at execution, and then used as the for() controller. base::warning() only: the loop is still run (the errors of the sequence evaluation itself, like 1:NA or 1:Inf, are propagated as they are)
            check_txt <- base::paste0(
                "if(base::length(x = ", tempo_name, ") == 0){base::warning(\"FOR SEQUENCE EMPTY: 0 ITERATION\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::anyNA(x = ", tempo_name, ")){base::warning(\"FOR SEQUENCE WITH NA VALUES\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::length(x = ", tempo_name, ") == 1){base::warning(\"FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::is.atomic(x = ", tempo_name, ") && base::any(base::is.infinite(x = ", tempo_name, "), na.rm = TRUE)){base::warning(\"FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::is.atomic(x = ", tempo_name, ") && base::typeof(x = ", tempo_name, ") %in% base::c(\"double\", \"integer\") && base::all(x = ", tempo_name, " < 0, na.rm = FALSE)){base::warning(\"FOR SEQUENCE WITH ALL NEGATIVE VALUES\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::is.atomic(x = ", tempo_name, ") && base::typeof(x = ", tempo_name, ") %in% base::c(\"double\", \"integer\", \"character\", \"complex\") && base::length(x = ", tempo_name, ") >= 2 && ", tempo_name, "[1] > ", tempo_name, "[2]){base::warning(\"FOR SEQUENCE DECREASING\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            insert_txt <- base::paste0(tempo_name, " <- ", base::paste0(base::deparse(expr = seq_expr, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = " ", recycle0 = FALSE), " ; ", check_txt, " ; ", collapse = NULL, recycle0 = FALSE)
            # 4) insert the tempo assignment and the checking line before the for keyword, or wrap the whole modified construct in curly brackets when the enclosing expression of the construct is not a braced block (see the detection above): the insertion before the for keyword would otherwise break the binding of the enclosing writing (unbraced body of if/else/for/while/repeat, assignment value, function argument, operator operand, etc.). Warning: the positions of the for keyword and of the body end must be adjusted with the shift events recorded above, because the sequence replacement of this construct (step 2) and the modifications of the constructs processed before (back-to-front order) have shifted the columns located after their modification points
            for_col1_adj <- findings_for_col1[i] + base::sum(ev_shift[ev_line == findings_for_line1[i] & ev_col < findings_for_col1[i]], na.rm = FALSE)
            if(findings_braced[i] == TRUE){
                tempo_line <- modified_code[findings_for_line1[i]]
                len_for_before <- base::nchar(x = tempo_line, type = "chars", allowNA = FALSE, keepNA = NA)
                modified_code[findings_for_line1[i]] <- base::paste0(
                    base::substr(x = tempo_line, start = 1, stop = for_col1_adj - 1),
                    insert_txt,
                    base::substr(x = tempo_line, start = for_col1_adj, stop = base::nchar(x = tempo_line, type = "chars", allowNA = FALSE, keepNA = NA)),
                    collapse = NULL,
                    recycle0 = FALSE
                )
                ev_line <- base::c(ev_line, findings_for_line1[i], recursive = TRUE, use.names = TRUE)
                ev_col <- base::c(ev_col, for_col1_adj - 1, recursive = TRUE, use.names = TRUE)
                ev_shift <- base::c(ev_shift, base::nchar(x = modified_code[findings_for_line1[i]], type = "chars", allowNA = FALSE, keepNA = NA) - len_for_before, recursive = TRUE, use.names = TRUE)
            }else{
                # wrap the whole construct in curly brackets: prefix + "{ " + modified construct + " }"
                body_col2_adj <- findings_body_col2[i] + base::sum(ev_shift[ev_line == findings_body_line2[i] & ev_col < findings_body_col2[i]], na.rm = FALSE)
                tempo_line1 <- modified_code[findings_for_line1[i]]
                prefix_txt <- base::substr(x = tempo_line1, start = 1, stop = for_col1_adj - 1)
                len_for_before <- base::nchar(x = tempo_line1, type = "chars", allowNA = FALSE, keepNA = NA)
                if(findings_for_line1[i] == findings_body_line2[i]){
                    # single-line construct: the positions on this line have been shifted by the sequence replacement of step 2 (and by the constructs processed before, when inside this construct)
                    tempo_line2 <- tempo_line1
                    construct_txt <- base::substr(x = tempo_line2, start = for_col1_adj, stop = body_col2_adj)
                    suffix_txt <- base::substr(x = tempo_line2, start = body_col2_adj + 1, stop = base::nchar(x = tempo_line2, type = "chars", allowNA = FALSE, keepNA = NA))
                    wrapped_txt <- base::paste0("{ ", insert_txt, construct_txt, " }", collapse = NULL, recycle0 = FALSE)
                    modified_code[findings_for_line1[i]] <- base::paste0(prefix_txt, wrapped_txt, suffix_txt, collapse = NULL, recycle0 = FALSE)
                }else{
                    # multi-line construct: the construct is collapsed on a single line, which keeps the line numbering (the middle lines are emptied, and the prefix and suffix remain on their lines)
                    tempo_line2 <- modified_code[findings_body_line2[i]]
                    len_body_before <- base::nchar(x = tempo_line2, type = "chars", allowNA = FALSE, keepNA = NA)
                    construct_txt <- base::c(
                        base::substr(x = tempo_line1, start = for_col1_adj, stop = base::nchar(x = tempo_line1, type = "chars", allowNA = FALSE, keepNA = NA)),
                        if(findings_body_line2[i] - findings_for_line1[i] > 1){modified_code[(findings_for_line1[i] + 1):(findings_body_line2[i] - 1)]}else{base::character(length = 0)}, # guard: a construct written on exactly 2 lines has no middle line, and the unguarded (for+1):(body-1) writing would be a descending range in R (c(for+1, for)), pulling the for line into the wrap and producing a non-parseable modified code
                        base::substr(x = tempo_line2, start = 1, stop = body_col2_adj)
                    )
                    suffix_txt <- base::substr(x = tempo_line2, start = body_col2_adj + 1, stop = base::nchar(x = tempo_line2, type = "chars", allowNA = FALSE, keepNA = NA))
                    construct_txt <- base::paste0(construct_txt, collapse = " ", recycle0 = FALSE)
                    wrapped_txt <- base::paste0("{ ", insert_txt, construct_txt, " }", collapse = NULL, recycle0 = FALSE)
                    modified_code[findings_for_line1[i]] <- base::paste0(prefix_txt, wrapped_txt, collapse = NULL, recycle0 = FALSE)
                    if(findings_body_line2[i] - findings_for_line1[i] > 1){
                        modified_code[(findings_for_line1[i] + 1):(findings_body_line2[i] - 1)] <- "" # empty lines keep the line numbering
                    }
                    modified_code[findings_body_line2[i]] <- suffix_txt
                }
                # shift events of the wrap: one for the line of the for keyword (always modified), one for the line of the body end (modified only when different from the for keyword line)
                len_for_after <- base::nchar(x = modified_code[findings_for_line1[i]], type = "chars", allowNA = FALSE, keepNA = NA)
                ev_line <- base::c(ev_line, findings_for_line1[i], recursive = TRUE, use.names = TRUE)
                ev_col <- base::c(ev_col, for_col1_adj - 1, recursive = TRUE, use.names = TRUE)
                ev_shift <- base::c(ev_shift, len_for_after - len_for_before, recursive = TRUE, use.names = TRUE)
                if(findings_body_line2[i] != findings_for_line1[i]){
                    len_body_after <- base::nchar(x = modified_code[findings_body_line2[i]], type = "chars", allowNA = FALSE, keepNA = NA)
                    ev_line <- base::c(ev_line, findings_body_line2[i], recursive = TRUE, use.names = TRUE)
                    ev_col <- base::c(ev_col, body_col2_adj, recursive = TRUE, use.names = TRUE)
                    ev_shift <- base::c(ev_shift, len_body_after - len_body_before, recursive = TRUE, use.names = TRUE)
                }
            }
            inserted_log[i] <- TRUE
        }
        # the modified code must parse, otherwise an internal error is returned (a modified code that does not parse would indicate a replacement bug)
        # nocov start
        # codecov inactivated because it is an internal control of the replacement, impossible to cover with argument values.
        if(base::inherits(x = base::try(expr = base::parse(text = base::paste0(modified_code, collapse = "\n", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), silent = TRUE), what = "try-error")){
            tempo_cat <- base::paste0(
                intern_error_text_start, 
                "THE CODE OF ", 
                base::as.character(x = arg_user_setting$x), 
                " MODIFIED BY THE INSERTION OF THE for() SEQUENCE CHECKING LINES DOES NOT PARSE ANYMORE.\n\nTHIS IS AN INTERNAL BUG: PLEASE, DO NOT USE THE MODIFIED CODE (NOR THE EXPORTED FILE) AND REPORT IT.", 
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
    # modification of arg_user_setting$x for clean messages
    if(base::as.character(x = arg_user_setting$x)[1] == "::" | base::as.character(x = arg_user_setting$x)[1] == ":::"){
        arg_user_setting$x <- base::paste0(
            base::as.character(x = arg_user_setting$x)[2], 
            base::as.character(x = arg_user_setting$x)[1], 
            base::as.character(x = arg_user_setting$x)[3], 
            "()",
            collapse = NULL, 
            recycle0 = FALSE
        )
    }else{
        arg_user_setting$x <- base::paste0(
            arg_user_setting$x, 
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
            ", SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:\n\n", 
            "LINE\tCONSTRUCT\t\tCODE\n", 
            base::paste0(base::paste0(findings_line, "\t", findings_kind, "\t\t", findings_code), collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        if(base::length(x = skipped_text) != 0){
            tempo_cat <- base::paste0(
                tempo_cat, 
                "\n\nCHECKING LINES NOT INSERTED FOR THE FOLLOWING for() CONSTRUCTS (THEY REMAIN UNMODIFIED):\n", 
                base::paste0(skipped_text, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
        }
        if(base::any(inserted_log, na.rm = TRUE)){
            if(export == TRUE){
                # the file is written BEFORE the message is printed, so that the message only claims an export that actually happened. Warning: the first line of the recovered code always starts by the function writing (fun <- function...), possibly indented
                export_code <- modified_code
                export_code[1] <- base::paste0(out_fun_name, "_safer <- ", export_code[1], collapse = NULL, recycle0 = FALSE)
                utils::write.table(x = export_code, file = out_file_path, row.names = FALSE, col.names = FALSE, append = FALSE, quote = FALSE, sep = "", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "") # write each line of the modified code, without any quoting, in a plain text R script
                # the export message is printed with on.exit(), after the message of the main code: it only claims an export that actually happened, because the file writing is performed just above and no on.exit() is registered when no modification is inserted
                base::on.exit(expr = base::message(base::paste0("MODIFIED CODE EXPORTED IN:\n", out_file_path, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
            }else{
                tempo_cat <- base::paste0(
                    tempo_cat, 
                    "\n\nMODIFIED FUNCTION RETURNED BY return().", 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
            }
        }
    }
    base::message(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    if(base::length(x = findings_line) == 0){
        output <- NULL
    }else if(base::any(inserted_log, na.rm = TRUE) == FALSE){
        output <- NULL # no checking line inserted: the reported for() constructs remain unmodified (see the returned message)
    }else if(export == TRUE){
        output <- base::invisible(x = NULL) # the modified code has already been written in the exported file above
    }else{
        output <- base::eval(expr = base::parse(text = base::paste0(modified_code, collapse = "\n", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown")[[1]], envir = base::new.env(parent = base::environment(fun = x)), enclos = base::globalenv())
    }
    if(export == TRUE){
        base::return(base::invisible(x = output)) # NULL returned invisibly when export is TRUE (the invisibility is lost when assigned to output, hence invisible() here)
    }else{
        base::return(output) # NULL if everything is clean or if no checking line has been inserted
    }
    #### end output

    #### end of function
}
