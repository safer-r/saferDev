#' @title Function for All Arguments Here
#' @description
#' Insert the missing arguments detected by \code{all_args_here()} into the code of the submitted function and return the modified function.
#' @param x Function to modify. Function object.
#' @param out List returned by \code{.functions_detect()}: \code{$code}, \code{$fun_names}, \code{$fun_names_pos}, \code{$code_line_nb}, \code{$internal_fun_names} and \code{$arg_user_setting}. The \code{$code} compartment is the normalized code read by \code{all_args_here()} and must have the same line indexing as the \code{$LINE_NB} column of the \code{output} data frame.
#' @param output Data frame returned by the main code of \code{all_args_here()}: the \code{$LINE_NB}, \code{$FUN_POS}, \code{$FUN_ARGS} and \code{$STATUS} columns are used to apply the corrections.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A list:
#' \itemize{
#'   \item \code{fun}: the modified function object, or \code{NULL} if no correction has been applied.
#'   \item \code{fun_txt}: the modified code as a single string (\code{NULL} if no correction has been applied).
#'   \item \code{inserted}: character vector of the applied corrections ("line <n>: <status>" for each).
#'   \item \code{left}: character vector of the corrections that could not be applied (with line number and status), for the final message of \code{all_args_here()}.
#' }
#' @details
#' Warnings:
#' \itemize{
#'   \item requires \code{saferDev::arg_check}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also \code{saferDev::arg_check} when checking for the presence of \code{.all_args_insert}.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @author Haiding Wang
#' @author Yushi Han
#' @keywords internal
.all_args_insert <- function(
    x,
    out,
    output,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .all_args_insert(x = FUN1, out = out, output = output, lib_path = NULL, error_text = " INSIDE P1::F1") # FUN1 being the checked function, out the .functions_detect() output and output the data frame built by all_args_here()

    # DEBUGGING
    # FUN1 <- function(x, y){missing_arg1 <- base::sum(1:2, na.rm) ; missing_arg2 <- base::paste0("a", "b")} ; x = FUN1 ; out = .functions_detect(x = FUN1, arg_user_setting = base::list(x = base::as.name(x = "FUN1")), skipped_base = base::c("function", "if", "for", "while", "repeat", "else"), lib_path = NULL, error_text = "") ; output = NULL ; lib_path = NULL ; error_text = ""
    # function_name <- ".all_args_insert" ; arg_user_setting = base::list(x = base::as.name(x = "FUN1"), out = out, output = output, lib_path = NULL, error_text = "") ; arg_names <- c("x", "out", "output", "lib_path", "error_text")

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
    # evaluation of arguments settings
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
    # end evaluation of arguments settings
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
    # personalized for internal functions: lib_path and error_text are mandatory; safer_check does not exist in internal functions
    mandat_args <- base::c("lib_path", "error_text")
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
        "x",
        "out",
        "output",
        "lib_path",
        "error_text"
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
        "out",
        "output"
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
        # "x", # inactivated because can be empty (function with no argument)
        # "out", # inactivated because can be empty
        # "output", # inactivated because can be empty (no function detected in the checked function)
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
    # not used here
    ######## end safer_check argument checking

    ######## check of lib_path
    # check already done in the main safer function
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    # saferDev::arg_check is required here
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    # check already done in the main safer function
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
    tempo <- saferDev::arg_check(data = x, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = out, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = output, class = "data.frame", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
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
    # not used here (no character argument that cannot contain "")
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
    # coherence between output and out$code: output must only contain rows whose LINE_NB indexes out$code
    if(base::nrow(x = output) > 0){
        if(base::any(base::is.na(x = output$LINE_NB), na.rm = TRUE) | base::any(base::is.na(x = output$FUN_POS), na.rm = TRUE) | base::any( ! output$LINE_NB %in% base::seq_along(along.with = out$code), na.rm = TRUE) | base::any(output$FUN_POS < 1, na.rm = TRUE)){
            # nocov start
            # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
            tempo_cat <- base::paste0("INTERNAL ERROR 1 IN ", intern_error_text_start, "THE output ARGUMENT MUST ONLY CONTAIN ROWS WHOSE LINE_NB INDEXES out$code AND WHOSE FUN_POS IS A POSITIVE POSITION.\nHERE IT IS:\n", base::paste0(base::paste0("LINE_NB: ", output$LINE_NB, " ; FUN_POS: ", output$FUN_POS), collapse = "\n", recycle0 = FALSE), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            # nocov end
        }
    }
    # end coherence between output and out$code: output must only contain rows whose LINE_NB indexes out$code

    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # two parallel versions of the code lines are used:
    # 1) code_lines (out$code): the normalized lines read by all_args_here() (runs of spaces collapsed into a single space, leading spaces removed and comments blanked by .functions_detect()). These are the exact lines that FUN_POS indexes (the FUN_ARGS of the output table is sliced from these very lines in all_args_here()), so all the validations below are performed on them
    # 2) orig_lines: the same lines as printed by the function object (same utils::capture.output() as in .functions_detect(), hence the same line indexing), with the comments and the indentation of the submitted function. Each correction is also inserted into these lines, and the modified function is rebuilt from them, so that the comments and the indentation are preserved
    # Warning: the comments and the original indentation only exist if the submitted function carries its source references (srcref attribute, i.e., keep.source = TRUE when the function was created). Otherwise, the printed function is the deparsed code (no comments, standard indentation)
    code_lines <- out$code
    orig_lines <- utils::capture.output(x, file = NULL, append = FALSE, type = NULL, split = FALSE)
    if(base::length(x = orig_lines) != base::length(x = code_lines)){
        # nocov start
        # codecov inactivated because it is an internal control of code writing: out is built by .functions_detect() from the same function x, using the same utils::capture.output() call, which does not remove any line
        tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "THE PRINTED CODE OF x AND out$code MUST HAVE THE SAME NUMBER OF LINES.\nHERE IT IS:\nPRINTED CODE OF x: ", base::length(x = orig_lines), "\nout$code: ", base::length(x = code_lines), intern_error_text_end, collapse = NULL, recycle0 = FALSE) ; base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    # rows to apply: only rows where the correction is a complete call proposal (the STATUS proposals that are not "GOOD", not "HAS TO BE FULLY WRITTEN" messages, and not empty)
    keep <- base::nzchar(x = output$STATUS, keepNA = FALSE) & (output$STATUS != "GOOD") & ! base::grepl(x = output$STATUS, pattern = "HAS TO BE FULLY WRITTEN", fixed = TRUE) & ( ! base::is.na(x = output$MISSING_ARGS)) & base::nzchar(x = output$MISSING_ARGS, keepNA = FALSE)
    # IMPORTANT: rows with MISSING_ARGS non empty but STATUS being a message (HAS TO BE FULLY WRITTEN) are excluded by the grepl above
    rows <- output[keep, , drop = FALSE]
    if(base::nrow(x = rows) == 0){
        output_list <- base::list(fun = NULL, fun_txt = NULL, inserted = base::character(length = 0L), left = base::character(length = 0L))
        base::return(output_list)
    }
    # apply back-to-front: highest LINE_NB first, then highest FUN_POS, so that replacing a span never invalidates the position of a not-yet-applied row
    # on a single line, several proposals can be applied when their spans do not overlap (e.g. sum(1:2) + paste0("a", "b")),
    # but the proposals of nested calls overlap (e.g. paste0(base::mean(1:2)) with corrections inside mean() and inside paste0()):
    # only the innermost one (the highest FUN_POS) is applied, because the outer proposal embeds the ORIGINAL inner text and would revert the inner correction if applied after.
    # The outer proposals of the same line are therefore routed to the left vector, so that the user can apply them manually after checking.
    ord <- base::order(rows$LINE_NB, rows$FUN_POS, decreasing = TRUE)
    inserted <- base::character(length = 0L)
    left <- base::character(length = 0L)
    applied_starts <- base::list() # FUN_POS of the already applied corrections, by line number (as character key), to detect overlapping spans on the same line
    for(i2 in ord){
        status_i2 <- rows$STATUS[i2]
        pos_i2 <- rows$FUN_POS[i2]
        line_i2 <- rows$LINE_NB[i2]
        # validation 1: the STATUS must parse as R code. Of note, an empty argument value (like "envir = )" or "new = ,") is accepted: R parses it as a "missing" argument, meaning that the default value is used (safer-r rule for args with no default value, or with a default value that cannot be copied)
        status_parsed <- base::tryCatch(
            expr = {
                base::parse(file = "", n = NULL, text = status_i2, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown")
            },
            error = function(e){NULL}
        )
        ok_parse <- ( ! base::is.null(x = status_parsed))
        if( ! ok_parse){
            left <- base::c(left, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
            next
        }
        # validation 2: FUN_ARGS must be a complete one-line call: parse it (a multi-line call is truncated at the line end and does not parse, and a call containing a "(" string still parses)
        fun_args_i2 <- rows$FUN_ARGS[i2]
        fun_args_len <- base::nchar(x = fun_args_i2, type = "chars", allowNA = FALSE, keepNA = NA)
        if(fun_args_len == 0 | ! base::tryCatch(
            expr = {
                base::parse(file = "", n = NULL, text = fun_args_i2, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown")
                TRUE
            },
            error = function(e){FALSE}
        )){
            left <- base::c(left, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
            next
        }
        # validation 3: the span must not overlap an already applied correction on the same line (the rows are processed in decreasing FUN_POS, so an already applied span starts at or after pos_i2):
        # this can only happen for nested or overlapping calls, and only the innermost correction is applied (the outer proposal embeds the ORIGINAL inner text and would revert the inner correction if applied after)
        # this check is run before the FUN_ARGS position check below, because once the innermost correction is applied on the line, the outer FUN_ARGS text no longer matches the line, which would report a misleading position error
        key_i2 <- base::as.character(x = line_i2)
        if( ! base::is.null(x = applied_starts[[key_i2]])){
            if(base::any(applied_starts[[key_i2]] <= pos_i2 + fun_args_len - 1, na.rm = TRUE)){
                left <- base::c(left, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
                next
            }
        }
        # validation 4: FUN_ARGS must sit exactly at FUN_POS in the out$code line (substr() returns "" when the span goes beyond the end of the line, which fails the comparison)
        at <- base::substr(x = code_lines[line_i2], start = pos_i2, stop = pos_i2 + fun_args_len - 1)
        if(base::is.na(x = at) | at != fun_args_i2){
            left <- base::c(left, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
            next
        }
        # validation 5: the STATUS must not contain an argument name written both positionally (bare symbol without written name) and as a named argument (e.g. "sum(1:2, na.rm, na.rm = FALSE)"):
        # such a proposal, although parsable, produces a function that fails at call time (the positional bare symbol is evaluated as a value in the caller environment), so it is routed to the left vector for manual correction
        # status_parsed is already available from validation 1 (the STATUS is guaranteed to be parsable here)
        if(base::length(x = status_parsed) == 1 & base::is.call(x = status_parsed[[1]]) & base::length(x = status_parsed[[1]]) > 1){
            status_args <- status_parsed[[1]][-1]
            status_arg_names <- base::names(x = status_args)
            if(base::is.null(x = status_arg_names)){
                status_arg_names <- base::rep(x = "", times = base::length(x = status_args))
            }
            pos_sym_names <- base::character(length = 0L)
            for(k in base::seq_along(along.with = status_args)){
                if(status_arg_names[k] == "" & base::is.symbol(x = status_args[[k]])){
                    pos_sym_names <- base::c(pos_sym_names, base::as.character(x = status_args[[k]]))
                }
            }
            named_names <- status_arg_names[status_arg_names != ""]
            if(base::length(x = pos_sym_names) > 0 & base::length(x = named_names) > 0){
                if(base::any(pos_sym_names %in% named_names, na.rm = TRUE)){
                    left <- base::c(left, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
                    next
                }
            }
        }
        # validation 6: position of the FUN_ARGS span in the original line (orig_lines), where the correction is also inserted to preserve the comments and the indentation
        # .functions_detect() collapses the runs of spaces into a single space (gsub(" +", " ")), then removes the remaining leading space (sub("^ +", "")), and the comment blanking only truncates the end of the line
        # thus, the k-th character of the normalized line is the k-th kept character of the original line, a kept character being any character except a space preceded by a space, and except a leading space
        orig_chars <- base::strsplit(x = orig_lines[line_i2], split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
        orig_space_log <- orig_chars == " "
        orig_kept_log <- ! (orig_space_log & base::c(FALSE, orig_space_log)[base::seq_along(along.with = orig_space_log)])
        if(base::length(x = orig_chars) > 0){
            if(orig_space_log[1] == TRUE){
                orig_kept_log[1] <- FALSE
            }
        }
        orig_kept_pos <- base::which(x = orig_kept_log, arr.ind = FALSE, useNames = TRUE)
        orig_start <- orig_kept_pos[pos_i2] # NA if beyond the kept characters
        orig_stop <- orig_kept_pos[pos_i2 + fun_args_len - 1] # NA if beyond the kept characters
        orig_span <- NA
        if( ! (base::is.na(x = orig_start) | base::is.na(x = orig_stop))){
            orig_span <- base::substr(x = orig_lines[line_i2], start = orig_start, stop = orig_stop)
        }
        if(base::is.na(x = orig_span)){ # the span cannot be located in the original line (out$code not built from x)
            left <- base::c(left, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
            next
        }
        if(base::gsub(pattern = " +", replacement = " ", x = orig_span, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) != fun_args_i2){ # the original span, once normalized, must be FUN_ARGS
            left <- base::c(left, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
            next
        }
        # validation 7: the STATUS is built from the normalized FUN_ARGS: if the original span contains runs of spaces inside string literals (e.g., "a  b"), these strings are collapsed in the STATUS (e.g., "a b"), which would silently modify the values of the code
        # in that case, the correction is routed to the left vector for manual correction
        if(orig_span != fun_args_i2){
            str_orig <- base::tryCatch(
                expr = {
                    tempo_parse_data <- utils::getParseData(x = base::parse(file = "", n = NULL, text = orig_span, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = orig_span, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA) # keep.source = TRUE and a srcfile object required by utils::getParseData(). Warning: srcfile = NULL (instead of a missing srcfile) prevents the creation of the source references by base::parse()
                    tempo_parse_data$text[tempo_parse_data$token == "STR_CONST"]
                },
                error = function(e){NA} # nocov : defensive net, orig_span only differs from the parsable FUN_ARGS by runs of spaces
            )
            str_status <- utils::getParseData(x = base::parse(file = "", n = NULL, text = status_i2, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = status_i2, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA) # the STATUS is guaranteed to be parsable here (validation 1)
            str_status <- str_status$text[str_status$token == "STR_CONST"]
            if(base::any(base::is.na(x = str_orig), na.rm = FALSE) | base::any( ! str_orig %in% str_status, na.rm = FALSE)){
                left <- base::c(left, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
                next
            }
        }
        # apply on the original line (orig_lines): the prefix of the line before the corrected span is untouched
        nch_orig_line <- base::nchar(x = orig_lines[line_i2], type = "chars", allowNA = FALSE, keepNA = NA)
        orig_lines[line_i2] <- base::paste0(
            base::substr(x = orig_lines[line_i2], start = 1, stop = orig_start - 1),
            status_i2,
            base::substr(x = orig_lines[line_i2], start = orig_stop + 1, stop = nch_orig_line),
            collapse = NULL,
            recycle0 = FALSE
        )
        # apply on the out$code line: the prefix of the line before the corrected span is untouched, so the positions of the remaining rows on this same line (all with smaller FUN_POS, since rows are applied in decreasing order) stay valid, in code_lines and in orig_lines
        nch_line <- base::nchar(x = code_lines[line_i2], type = "chars", allowNA = FALSE, keepNA = NA)
        code_lines[line_i2] <- base::paste0(
            base::substr(x = code_lines[line_i2], start = 1, stop = pos_i2 - 1),
            status_i2,
            base::substr(x = code_lines[line_i2], start = pos_i2 + fun_args_len, stop = nch_line),
            collapse = NULL,
            recycle0 = FALSE
        )
        if(base::is.null(x = applied_starts[[key_i2]])){
            applied_starts[[key_i2]] <- pos_i2
        }else{
            applied_starts[[key_i2]] <- base::c(applied_starts[[key_i2]], pos_i2)
        }
        inserted <- base::c(inserted, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
    }
    if(base::length(x = inserted) > 0){
        # rebuild the corrected function from orig_lines (comments and indentation preserved): orig_lines comes from capture.output of the function object, so the lines reassemble into a parsable function (the 1st line is the "function(...)" header, whatever the print style: "function (x) " or "function(a, b){" for srcref functions)
        # the "<bytecode: ...>" and "<environment: ...>" lines can be printed by capture.output() when the submitted function is byte-compiled (e.g., a package function) or when its closure environment is not the global environment (e.g., inside testthat::test_that()): they are not R code and are removed, otherwise the final parse guard fails. They are always the last lines of the printed function, so the code lines before them are untouched
        orig_lines_out <- orig_lines[ ! base::grepl(x = orig_lines, pattern = "^<(environment|bytecode):", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)]
        fun_txt <- base::paste(orig_lines_out, collapse = "\n", recycle0 = FALSE, sep = " ")
        # final guard: never hand back code that does not parse
        ok_full <- base::tryCatch(
            expr = {
                base::parse(file = "", n = NULL, text = fun_txt, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown")
                TRUE
            },
            error = function(e){FALSE} # nocov : defensive net, impossible to trigger with argument values, see the comment inside the if() block below
        )
        if( ! ok_full){
            # nocov start
            # codecov inactivated because it is a defensive net: each applied STATUS and FUN_ARGS is individually validated as parsable, and a parsable span is replaced by another parsable span at the same position, so the reassembled code cannot fail to parse without a bug in the validations above.
            left <- base::c(left, inserted, "corrected code does not parse: all corrections discarded")
            inserted <- base::character(length = 0L)
            output_list <- base::list(fun = NULL, fun_txt = NULL, inserted = inserted, left = left)
            base::return(output_list)
            # nocov end
        }
        # return the modified function as an R function object
        # keep.source = TRUE (instead of the keep.source option, FALSE in Rscript sessions) and a srcfile object (srcfile = NULL prevents the creation of the source references by base::parse()), so that the returned function keeps its source references, i.e., prints with its comments and indentation
        fun <- base::eval(expr = base::parse(file = "", n = NULL, text = fun_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = fun_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        output_list <- base::list(fun = fun, fun_txt = fun_txt, inserted = inserted, left = left)
    }else{
        output_list <- base::list(fun = NULL, fun_txt = NULL, inserted = inserted, left = left)
    }
    base::return(output_list)
    #### end output

    #### end of function
}


