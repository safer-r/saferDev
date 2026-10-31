#' @title True False Check
#' @description
#' Verify that the \code{TRUE} and \code{FALSE} logical constants are always written in full when used as values (argument values, conditions, comparisons, indexing) and never abbreviated as \code{T} or \code{F}, which can be overwritten by the user. Return table message of the line with problems and a corrected version of the submitted function.
#' @param x A function name, written without quotes and brackets.
#' @param export Single logical value. Write the corrected function into a file instead of returning it? If \code{TRUE}, the file \code{<function name>_safer.R} is written in \code{out_path} (e.g., \code{fun_safer.R} for \code{x = fun} or \code{x = pkg::fun}). It contains \code{fun_safer <- function(...) ...}, so that \code{base::source()} recreates the corrected function. Nothing is written if nothing has to be corrected. An error is returned if the file already exists, or if \code{x} is not a function name (e.g., \code{function(x)\{x\}} or \code{FUN_LIST$fun}).
#' @param out_path Single character string indicating the path of the folder where to write the file when \code{export = TRUE}. \code{"."} (default) means the R working directory. Ignored if \code{export = FALSE}.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) required functions and related packages effectively present in local R libraries and 2) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A table-like message is always printed, with one row per reported symbol (column 1: line number; column 2: symbol; column 3: code before the symbol), or a message saying that everything seems fine.
#'
#' If \code{export = FALSE}: the corrected function, or \code{NULL} if nothing has to be corrected.
#'
#' If \code{export = TRUE}: \code{NULL}, returned invisibly. The corrected function is written in a file (see the \code{export} argument).
#' @details
#' Warning: \code{true_false_check()} only checks the \code{T} and \code{F} symbols used as values: the package association of the function calls is not checked (run \code{saferDev::colons_check()} separately for that). Function names written without brackets (e.g., \code{FUN = as.character}) or used as function names (e.g., \code{T()}) are not reported.
#'
#' Reported: \code{T} or \code{F} used as a value, e.g., \code{f(log = T)}, \code{f(T)}, \code{if(T)}, \code{while(T)}, \code{x == T} or \code{x[T]}.
#'
#' Not reported: \code{T} or \code{F} used as a name or assigned, e.g., \code{T <- 1}, \code{x <- T}, \code{function(T)}, \code{f(T = 1)}, \code{df$T}, \code{for(T in 1:3)}, \code{T()}, \code{?T} or \code{y ~ T}. Strings, comments and backquoted names (\code{`T`}) are ignored. If \code{T} or \code{F} is defined anywhere in the function (argument, loop variable or assignment), none of its uses is reported.
#'
#' Correction: each reported \code{T} is replaced by \code{TRUE} and each \code{F} by \code{FALSE} (\code{base::T} also becomes \code{TRUE}). The rest of the code is unchanged. The comments and the indentation of the submitted function are kept, in the returned function and in the exported file. Warning: comments are only available when the submitted function carries its source references (attribute srcref). Otherwise (e.g., compiled package functions, or functions created with \code{options(keep.source = FALSE)}, default of Rscript), the code is the deparsed one: no comments and standard indentation. The returned function is evaluated in a new environment whose parent is the environment of the submitted function: check it before replacing the original function.
#'
#' Line numbers are indicative. They start at the \code{<- function} line (\code{#'} header lines not counted). Compiled functions, like \code{saferDev::colons_check}, have no comments: their line numbers differ from their source file.
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @examples
#' # Warning: these examples may not work well when using the "Run examples" link
#' # because of a particular environment. Please, copy-paste in a local environment.
#' # See also https://safer-r.github.io/saferDev/articles/true_false_check.html
#' fun1 <- function(x){ if(x == TRUE){ base::return(x) } }
#' saferDev::true_false_check(fun1) # everything fine: NULL returned
#' fun2 <- function(x){ if(x == T){ base::return(x) } }
#' fun2_safer <- saferDev::true_false_check(fun2) # T reported
#' fun2_safer # corrected function
#' # Write the corrected function in the fun2_safer.R file of a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferDev::true_false_check(x = fun2, export = TRUE, out_path = tempo_dir)
#'
#' @export
true_false_check <- function(
    x,
    export = FALSE,
    out_path = ".",
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){
    # DEBUGGING
    # x = fun1 ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A"
    # function_name <- "true_false_check" ; arg_user_setting = base::list(x = as.name(x = "fun1"), export = FALSE, out_path = ".", lib_path = NULL , safer_check = TRUE, error_text = " INSIDE A")
    # x = .base_op_check ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # internal function with function body made of a single .Primitive() call
    # source("C:\\Users\\gmillot\\Documents\\Git_projects\\safer-r\\saferDev\\R\\true_false_check.R") ; x = true_false_check ; export = FALSE ; out_path = "." ; lib_path = NULL ; safer_check = TRUE ; error_text = " INSIDE A" # Warning: x = saferDev::true_false_check does not return the same number of code lines

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
        "out_path",
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
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
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
        # determination of the file name: name of the submitted function, as written by the user, with _safer added at the end
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
    # recover the code of the checked function
    code <- utils::capture.output(x, file = NULL, append = FALSE, type = NULL, split = FALSE)
    # removal of the attribute lines printed for compiled functions (e.g., "<bytecode: 0x...>", "<environment: ...>", "<srcref: ...>"). Warning: the pattern cannot be simply a line starting by "<", because a valid R code line can start by "<" (binary operator continuation)
    code <- code[ ! base::grepl(x = code, pattern = "^<(bytecode|environment|srcref): ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)]
    code_text <- base::paste0(code, collapse = "\n", recycle0 = FALSE)
    pd <- utils::getParseData(x = base::parse(file = "", n = NULL, text = code_text, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = code_text, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA) # warning: an explicit srcfile is required, because srcfile = NULL removes the source references of the parsed code, so that utils::getParseData() returns NULL
    # terminal tokens (comments excluded), sorted by source position
    term_rows <- base::which(x = pd$terminal == TRUE & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
    term_rows <- term_rows[base::order(pd$line1[term_rows], pd$col1[term_rows], na.last = TRUE, decreasing = FALSE, method = "auto")]
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
    # detection of the formula symbols and help topics: all the symbols located inside the subtree of the formula expression (~) or of the help call (? or ??) are names, whatever their position (y ~ T, T ~ y, y ~ x + T, ?T, ?(T), ?((T)), etc.) and whatever the srcref presence (without srcrefs, ?T is deparsed as `?`(T) and ?(T) as `?`((T)), so that the topic is always inside the subtree of the ? call). Warning: the tokens ~ and ? must be compared through their text, because their token names are quoted ('~' and '?'). Warning: the parse tree is described by the pd$id and pd$parent columns, whose values must be converted into pd row numbers through a lookup vector, because the two numbering systems are different (the id numbering skips the non terminal rows)
    id2row <- base::seq_len(length.out = base::nrow(x = pd))
    base::names(x = id2row) <- base::as.character(x = pd$id)
    parent_row <- id2row[base::as.character(x = pd$parent)] # row number of the parent of each parse tree node. Warning: NA for the top expressions (parent id 0) and for the root node
    name_context_rows <- base::integer(length = 0)
    special_rows <- base::which(x = pd$terminal == TRUE & pd$text %in% base::c("~", "?", "`?`", "??", "`??`"), arr.ind = FALSE, useNames = TRUE)
    for(r1 in base::seq_along(along.with = special_rows)){
        r <- special_rows[r1]
        start_row <- parent_row[r]
        if(pd$token[r] == "SYMBOL_FUNCTION_CALL" & ! base::is.na(x = start_row)){ # deparsed `?` call: the parent expression only contains the `?` symbol itself, the help topic is inside the call expression, i.e., the parent of the parent. Warning: start_row can only be NA (top expression) or a positive row number, because the parent id 0 is absent from the pd$id column
            start_row <- parent_row[start_row]
        }
        if(base::is.na(x = start_row) | start_row == 0){
            next # nocov # start_row is never NA or 0 here: the checked code is always a function definition, so that every ? or ~ token has a parent inside the function body (the parent id 0 only exists for top-level expressions, which cannot occur in a function body)
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
    # end detection of the formula symbols and help topics
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
        if(k %in% name_context_rows){ # formula symbol or help topic: skipped, because these are names
            next
        }
        prev_tok <- base::ifelse(test = i == 1, yes = "", no = pd$token[term_rows[i - 1]])
        next_tok <- base::ifelse(test = i == term_nb, yes = "", no = pd$token[term_rows[i + 1]])
        if(prev_tok %in% base::c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN") | next_tok %in% base::c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")){ # assignment value (x <- T or T -> x): skipped by user choice, because the symbol refers to a user-defined object. Warning: the definition targets (T <- x, T = x and y -> T) are already collected in tf_names above
            next
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
        output <- base::eval(expr = base::parse(text = base::paste0(modified_code, collapse = "\n", recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = modified_code, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::new.env(hash = TRUE, parent = base::environment(fun = x), size = 29L), enclos = base::globalenv())
    }
    if(export == TRUE){
        base::return(base::invisible(x = output)) # NULL returned invisibly when export is TRUE (the invisibility is lost when assigned to output, hence invisible() here)
    }else{
        base::return(output) # NULL if everything is clean
    }
    #### end output

}
