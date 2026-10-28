#' @title Function for All Arguments Here
#' @description
#' Insert the missing arguments detected by \code{all_args_here()} into the printed code of the submitted function, at the positions given by the R parser, so that the line breaks, the indentation and the comments are kept, and return the modified function.
#' @param x Function to modify. Function object.
#' @param out List returned by \code{.functions_detect()}: \code{$code}, \code{$fun_names}, \code{$fun_names_pos}, \code{$code_line_nb}, \code{$internal_fun_names} and \code{$arg_user_setting}. The \code{$code} compartment is the normalized code read by \code{all_args_here()} and must have the same line indexing as the \code{$LINE_NB} column of the \code{output} data frame.
#' @param output Data frame returned by the main code of \code{all_args_here()}: the \code{$LINE_NB}, \code{$FUN_NAME}, \code{$FUN_POS}, \code{$DEF_ARGS}, \code{$MISSING_ARGS} and \code{$STATUS} columns are used to apply the corrections (\code{$FUN_ARGS} is not used).
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A list:
#' \itemize{
#'   \item \code{fun}: the modified function object, or \code{NULL} if no correction has been applied.
#'   \item \code{fun_txt}: the modified code as a single string (\code{NULL} if no correction has been applied).
#'   \item \code{inserted}: character vector of the applied corrections ("line <n>: <status>" for each).
#'   \item \code{left}: character vector of the corrections that could not be applied ("line <n>: <status>", followed by a "REASON:" line explaining why the correction has not been applied), for the final message of \code{all_args_here()}.
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
    # 1) code_lines (out$code): the normalized lines read by all_args_here() (runs of spaces collapsed into a single space, leading spaces removed and comments blanked by .functions_detect()). FUN_POS indexes these lines, and is used to locate each call in orig_lines
    # 2) orig_lines: the same lines as printed by the function object (same utils::capture.output() as in .functions_detect(), hence the same line indexing), with the comments, the line breaks and the indentation of the submitted function. The modified function is rebuilt from them
    # The corrections are not made by replacing the calls by the STATUS proposals (built from the normalized code, where a call written on several lines is joined into a single line), but by inserting the missing arguments into orig_lines, at the positions given by the R parser (utils::getParseData()): a missing argument whose value is written without name gets its name inserted before this value, and the other missing arguments are inserted after the last argument of the call. Thus, the line breaks, the indentation, the comments and the strings of the submitted function are kept, including inside the corrected calls
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
    # reasons written below each correction of the left vector, in the final message of all_args_here(), so that the user knows why the correction has not been applied and what to do. Of note, an empty argument value (like "envir = )") is never a reason
    left_reason <- base::c(
        unreadable = "the MISSING_ARGS or DEF_ARGS column of the call cannot be read. Apply the proposal manually.",
        not_located = "the call cannot be located in the code of the function. Apply the proposal manually.",
        not_parsable = "the corrected call does not parse as R code. Apply the proposal manually.",
        pos_and_named = "an argument is written without its name (positionally) and is also proposed with its name, e.g., sum(x, na.rm) proposed as sum(x, na.rm, na.rm = FALSE), which would fail at call time. Check the positional value and write it with its name.",
        differs = "the corrected call does not match the proposal of the STATUS column. Apply the proposal manually."
    )
    # parse data of the printed code of x: the "<bytecode: ...>" and "<environment: ...>" lines (printed after the code, e.g., for a byte-compiled function) are not R code and are blanked, to keep the line indexing
    orig_attr_log <- base::grepl(x = orig_lines, pattern = "^<(environment|bytecode):", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    orig_parse_lines <- orig_lines
    orig_parse_lines[orig_attr_log] <- ""
    pd <- base::tryCatch(
        expr = {
            utils::getParseData(x = base::parse(file = "", n = NULL, text = orig_parse_lines, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = orig_parse_lines, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA) # keep.source = TRUE and a srcfile object required by utils::getParseData()
        },
        error = function(e){NULL} # nocov : defensive net, the printed code of a function always parses
    )
    # conversion of a column of the parse data into a character position of the line: the parse data columns are display columns, where a tab moves to the next multiple of 8. end = TRUE for the last column of a token (col2), FALSE for the first one (col1). NA if not found
    col_to_char <- function(line_txt, col, end){
        chars <- base::strsplit(x = line_txt, split = "", fixed = TRUE, perl = FALSE, useBytes = FALSE)[[1]]
        end_col <- base::integer(length = base::length(x = chars))
        cur <- 0
        for(k in base::seq_along(along.with = chars)){
            if(chars[k] == "\t"){
                cur <- (cur %/% 8 + 1) * 8
            }else{
                cur <- cur + 1
            }
            end_col[k] <- cur
        }
        start_col <- base::c(0, end_col)[base::seq_along(along.with = chars)] + 1
        if(end == TRUE){
            tempo_pos <- base::which(x = end_col == col, arr.ind = FALSE, useNames = FALSE)
        }else{
            tempo_pos <- base::which(x = start_col == col, arr.ind = FALSE, useNames = FALSE)
        }
        if(base::length(x = tempo_pos) != 1){
            base::return(NA_integer_) # nocov : defensive net, a column of the parse data always matches a character of its line
        }
        base::return(tempo_pos)
    }
    # arguments of the call whose expression has the id call_id in the parse data pdata: list of arguments, each being a list with named (logical), name, start and end (line1, col1, line2, col2 of the argument in the parse data), and value_col1 (col1 of the value of a named argument, NA if no value written, e.g., "envir = ")
    # NULL if the call has an empty argument (e.g., "f(a, )"), which is not managed
    call_args_get <- function(pdata, call_id){
        child <- pdata[pdata$parent == call_id & pdata$token != "COMMENT", , drop = FALSE]
        child <- child[base::order(child$line1, child$col1, na.last = TRUE, decreasing = FALSE, method = "radix"), , drop = FALSE]
        open_pos <- base::which(x = child$token == "'('", arr.ind = FALSE, useNames = FALSE)[1]
        close_pos <- base::nrow(x = child)
        args <- base::list()
        if(close_pos - open_pos <= 1){ # no argument
            base::return(base::list(args = args, open = child[open_pos, , drop = FALSE], close = child[close_pos, , drop = FALSE]))
        }
        inside <- child[(open_pos + 1):(close_pos - 1), , drop = FALSE]
        seg_nb <- base::cumsum(x = inside$token == "','")
        for(k in base::seq_len(length.out = base::max(seg_nb, na.rm = FALSE) + 1)){ # k - 1 is the number of commas before the argument
            seg <- inside[seg_nb == k - 1 & inside$token != "','", , drop = FALSE]
            if(base::nrow(x = seg) == 0){
                base::return(NULL)
            }
            named <- base::nrow(x = seg) >= 2 & seg$token[1] %in% base::c("SYMBOL_SUB", "STR_CONST", "NULL_CONST") & seg$token[base::min(2, base::nrow(x = seg), na.rm = FALSE)] == "EQ_SUB"
            args[[base::length(x = args) + 1]] <- base::list(
                named = named,
                name = base::ifelse(test = named, yes = seg$text[1], no = ""),
                line1 = seg$line1[1],
                col1 = seg$col1[1],
                line2 = seg$line2[base::nrow(x = seg)],
                col2 = seg$col2[base::nrow(x = seg)],
                value_col1 = base::ifelse(test = named & base::nrow(x = seg) >= 3, yes = seg$col1[base::min(3, base::nrow(x = seg), na.rm = FALSE)], no = NA)
            )
        }
        base::return(base::list(args = args, open = child[open_pos, , drop = FALSE], close = child[close_pos, , drop = FALSE]))
    }
    # text of the code between the positions (line1, col1) and (line2, col2) of the parse data pdata, without spaces and comments: concatenation of the terminal tokens
    span_text <- function(pdata, line1, col1, line2, col2){
        tempo_log <- pdata$terminal == TRUE & pdata$token != "COMMENT" & (pdata$line1 > line1 | (pdata$line1 == line1 & pdata$col1 >= col1)) & (pdata$line2 < line2 | (pdata$line2 == line2 & pdata$col2 <= col2))
        tempo_pdata <- pdata[tempo_log, , drop = FALSE]
        tempo_pdata <- tempo_pdata[base::order(tempo_pdata$line1, tempo_pdata$col1, na.last = TRUE, decreasing = FALSE, method = "radix"), , drop = FALSE]
        base::return(base::paste0(tempo_pdata$text, collapse = "", recycle0 = FALSE))
    }
    # insertion of texts into lines, at character positions (each text is inserted before the character of the position). The insertions are made from the end of the code, so that each insertion does not shift the positions of the following ones
    insert_txt <- function(lines, ins_line, ins_char, ins_text){
        tempo_ord <- base::order(ins_line, ins_char, na.last = TRUE, decreasing = TRUE, method = "radix")
        for(k in base::seq_along(along.with = tempo_ord)){
            k2 <- tempo_ord[k]
            nch <- base::nchar(x = lines[ins_line[k2]], type = "chars", allowNA = FALSE, keepNA = NA)
            lines[ins_line[k2]] <- base::paste0(
                base::substr(x = lines[ins_line[k2]], start = 1, stop = ins_char[k2] - 1),
                ins_text[k2],
                base::substr(x = lines[ins_line[k2]], start = ins_char[k2], stop = nch),
                collapse = NULL,
                recycle0 = FALSE
            )
        }
        base::return(lines)
    }
    # call made of the function name f and of the arguments of a call, with the unnamed arguments first (in their order) and then the named arguments ordered by name, deparsed without spaces: used to compare two calls whatever the order of the named arguments and the writing of the spaces (the proposals of STATUS are built from the normalized code, where the runs of spaces are collapsed, including inside strings)
    call_canonical <- function(tempo_call){
        tempo_args <- base::as.list(x = tempo_call)[-1]
        tempo_names <- base::names(x = tempo_args)
        if(base::is.null(x = tempo_names)){
            tempo_names <- base::rep(x = "", times = base::length(x = tempo_args))
        }
        tempo_ord <- base::c(base::which(x = tempo_names == "", arr.ind = FALSE, useNames = FALSE), base::which(x = tempo_names != "", arr.ind = FALSE, useNames = FALSE)[base::order(tempo_names[tempo_names != ""], na.last = TRUE, decreasing = FALSE, method = "radix")])
        tempo_call2 <- base::as.call(x = base::c(base::list(base::as.name(x = "f")), tempo_args[tempo_ord]))
        tempo_txt <- base::paste0(base::deparse(expr = tempo_call2, width.cutoff = 500L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = "", recycle0 = FALSE)
        base::return(base::gsub(pattern = "\\s", replacement = "", x = tempo_txt, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE))
    }
    inserted <- base::character(length = 0L)
    left <- base::character(length = 0L)
    all_ins_line <- base::integer(length = 0L) # insertions of all the applied corrections
    all_ins_char <- base::integer(length = 0L)
    all_ins_text <- base::character(length = 0L)
    for(i2 in base::seq_len(length.out = base::nrow(x = rows))){
        status_i2 <- rows$STATUS[i2]
        pos_i2 <- rows$FUN_POS[i2]
        line_i2 <- rows$LINE_NB[i2]
        left_i2 <- base::paste0("line ", line_i2, ": ", status_i2, "\n    REASON: ", collapse = NULL, recycle0 = FALSE)
        # reading of the missing arguments (MISSING_ARGS, e.g., "x = , na.rm = FALSE") and of the arguments of the called function (DEF_ARGS, e.g., "x, ..., na.rm = FALSE"), using the parser
        miss_txt <- base::paste0("f(", rows$MISSING_ARGS[i2], ")", collapse = NULL, recycle0 = FALSE)
        miss_pd <- base::tryCatch(
            expr = {
                utils::getParseData(x = base::parse(file = "", n = NULL, text = miss_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = miss_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown"), includeText = NA)
            },
            error = function(e){NULL}
        )
        def_names <- base::tryCatch(
            expr = {
                base::names(x = base::str2lang(s = base::paste0("function(", rows$DEF_ARGS[i2], ") NULL", collapse = NULL, recycle0 = FALSE))[[2]])
            },
            error = function(e){NULL}
        )
        miss <- NULL
        if( ! base::is.null(x = miss_pd)){
            miss_fun_expr_id <- miss_pd$parent[miss_pd$token == "SYMBOL_FUNCTION_CALL" & miss_pd$line1 == 1 & miss_pd$col1 == 1][1] # expression of the function name f
            miss_call_id <- miss_pd$parent[miss_pd$id == miss_fun_expr_id] # expression of the call
            miss <- call_args_get(pdata = miss_pd, call_id = miss_call_id)$args
        }
        if(base::is.null(x = miss) | base::is.null(x = def_names) | base::length(x = miss) == 0){
            left <- base::c(left, base::paste0(left_i2, left_reason[["unreadable"]], collapse = NULL, recycle0 = FALSE))
            next
        }
        if( ! base::all(base::vapply(X = miss, FUN = function(y){y$named}, FUN.VALUE = base::logical(length = 1L), USE.NAMES = FALSE), na.rm = FALSE)){
            left <- base::c(left, base::paste0(left_i2, left_reason[["unreadable"]], collapse = NULL, recycle0 = FALSE))
            next
        }
        # arguments that can take a value written without name (positional matching): those before ... in the called function
        before_dots <- def_names
        if("..." %in% def_names){
            before_dots <- def_names[base::seq_len(length.out = base::match(x = "...", table = def_names, nomatch = 0L, incomparables = NULL) - 1)]
        }
        # location of the call in the parse data of orig_lines: the function name token at the position FUN_POS of the normalized line, i.e., at the FUN_POS-th kept character of the original line
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
        orig_start <- base::which(x = orig_kept_log, arr.ind = FALSE, useNames = TRUE)[pos_i2] # NA if beyond the kept characters
        fun_tok <- NULL
        if(( ! base::is.null(x = pd)) & ! base::is.na(x = orig_start)){
            fun_tok <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$line1 == line_i2 & pd$text == rows$FUN_NAME[i2], , drop = FALSE]
            if(base::nrow(x = fun_tok) > 0){
                tempo_char <- base::vapply(X = fun_tok$col1, FUN = function(y){col_to_char(line_txt = orig_lines[line_i2], col = y, end = FALSE)}, FUN.VALUE = base::integer(length = 1L), USE.NAMES = FALSE)
                fun_tok <- fun_tok[ ! base::is.na(x = tempo_char) & tempo_char == orig_start, , drop = FALSE]
            }
        }
        if(base::is.null(x = fun_tok)){
            left <- base::c(left, base::paste0(left_i2, left_reason[["not_located"]], collapse = NULL, recycle0 = FALSE))
            next
        }
        if(base::nrow(x = fun_tok) != 1){
            left <- base::c(left, base::paste0(left_i2, left_reason[["not_located"]], collapse = NULL, recycle0 = FALSE))
            next
        }
        fun_expr_id <- fun_tok$parent[1] # expression of the function name (e.g., base::sum)
        call_id <- pd$parent[pd$id == fun_expr_id] # expression of the call
        call_args <- call_args_get(pdata = pd, call_id = call_id)
        if(base::is.null(x = call_args)){
            left <- base::c(left, base::paste0(left_i2, left_reason[["not_located"]], collapse = NULL, recycle0 = FALSE))
            next
        }
        unnamed <- call_args$args[ ! base::vapply(X = call_args$args, FUN = function(y){y$named}, FUN.VALUE = base::logical(length = 1L), USE.NAMES = FALSE)]
        # insertions: a missing argument whose value is the next value written without name (positional matching, as in all_args_here()) gets its name inserted before this value. The other missing arguments are inserted after the last argument of the call (before the closing bracket if the call has no argument), so that a comment following the last argument stays after the insertion
        ins_line <- base::integer(length = 0L)
        ins_char <- base::integer(length = 0L)
        ins_text <- base::character(length = 0L)
        added <- base::character(length = 0L)
        unnamed_pos <- 1
        for(i3 in base::seq_along(along.with = miss)){
            miss_value <- ""
            if( ! base::is.na(x = miss[[i3]]$value_col1)){
                miss_value <- base::substr(x = miss_txt, start = col_to_char(line_txt = miss_txt, col = miss[[i3]]$value_col1, end = FALSE), stop = col_to_char(line_txt = miss_txt, col = miss[[i3]]$col2, end = TRUE))
            }
            positional <- FALSE
            if(miss[[i3]]$name %in% before_dots & unnamed_pos <= base::length(x = unnamed)){
                tempo_unnamed <- unnamed[[unnamed_pos]]
                positional <- base::gsub(pattern = "\\s", replacement = "", x = miss_value, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE) == span_text(pdata = pd, line1 = tempo_unnamed$line1, col1 = tempo_unnamed$col1, line2 = tempo_unnamed$line2, col2 = tempo_unnamed$col2)
            }
            if(positional == TRUE){
                ins_line <- base::c(ins_line, tempo_unnamed$line1)
                ins_char <- base::c(ins_char, col_to_char(line_txt = orig_lines[tempo_unnamed$line1], col = tempo_unnamed$col1, end = FALSE))
                ins_text <- base::c(ins_text, base::paste0(miss[[i3]]$name, " = ", collapse = NULL, recycle0 = FALSE))
                unnamed_pos <- unnamed_pos + 1
            }else{
                added <- base::c(added, base::paste0(miss[[i3]]$name, " = ", miss_value, collapse = NULL, recycle0 = FALSE))
            }
        }
        if(base::length(x = added) > 0){
            if(base::length(x = call_args$args) == 0){
                ins_line <- base::c(ins_line, call_args$close$line1)
                ins_char <- base::c(ins_char, col_to_char(line_txt = orig_lines[call_args$close$line1], col = call_args$close$col1, end = FALSE))
                ins_text <- base::c(ins_text, base::paste0(added, collapse = ", ", recycle0 = FALSE))
            }else{
                tempo_last <- call_args$args[[base::length(x = call_args$args)]]
                ins_line <- base::c(ins_line, tempo_last$line2)
                ins_char <- base::c(ins_char, col_to_char(line_txt = orig_lines[tempo_last$line2], col = tempo_last$col2, end = TRUE) + 1)
                ins_text <- base::c(ins_text, base::paste0(", ", base::paste0(added, collapse = ", ", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE))
            }
        }
        if(base::any(base::is.na(x = ins_char), na.rm = FALSE)){
            # nocov start
            # codecov inactivated because it is a defensive net: a column of the parse data always matches a character of its line
            left <- base::c(left, base::paste0(left_i2, left_reason[["not_located"]], collapse = NULL, recycle0 = FALSE))
            next
            # nocov end
        }
        # validation 1: the corrected call (this correction only, the nested calls being kept as written) must parse
        call_line1 <- pd$line1[pd$id == call_id]
        call_line2 <- pd$line2[pd$id == call_id]
        call_char1 <- col_to_char(line_txt = orig_lines[call_line1], col = pd$col1[pd$id == call_id], end = FALSE)
        call_char2 <- col_to_char(line_txt = orig_lines[call_line2], col = pd$col2[pd$id == call_id], end = TRUE)
        tempo_lines <- insert_txt(lines = orig_lines, ins_line = ins_line, ins_char = ins_char, ins_text = ins_text)
        call_char2 <- call_char2 + base::sum(base::nchar(x = ins_text[ins_line == call_line2], type = "chars", allowNA = FALSE, keepNA = NA), na.rm = FALSE) # all the insertions are inside the call, thus before its last character
        tempo_lines <- tempo_lines[call_line1:call_line2]
        tempo_lines[base::length(x = tempo_lines)] <- base::substr(x = tempo_lines[base::length(x = tempo_lines)], start = 1, stop = call_char2)
        tempo_lines[1] <- base::substr(x = tempo_lines[1], start = call_char1, stop = base::nchar(x = tempo_lines[1], type = "chars", allowNA = FALSE, keepNA = NA))
        new_call <- base::tryCatch(
            expr = {
                base::str2lang(s = base::paste0(tempo_lines, collapse = "\n", recycle0 = FALSE))
            },
            error = function(e){NULL} # nocov : defensive net, the insertions of "name = " before a value and of ", name = value" after the last argument cannot break a parsable call
        )
        if(base::is.null(x = new_call)){
            # nocov start
            left <- base::c(left, base::paste0(left_i2, left_reason[["not_parsable"]], collapse = NULL, recycle0 = FALSE))
            next
            # nocov end
        }
        # validation 2: the corrected call must not contain an argument name written both positionally (bare symbol without written name) and as a named argument (e.g. "sum(1:2, na.rm, na.rm = FALSE)"):
        # such a correction, although parsable, produces a function that fails at call time (the positional bare symbol is evaluated as a value in the caller environment), so it is routed to the left vector for manual correction
        new_args <- base::as.list(x = new_call)[-1]
        new_arg_names <- base::names(x = new_args)
        if(base::is.null(x = new_arg_names)){
            new_arg_names <- base::rep(x = "", times = base::length(x = new_args))
        }
        pos_sym_names <- base::character(length = 0L)
        for(k in base::seq_along(along.with = new_args)){
            if(new_arg_names[k] == "" & base::is.symbol(x = new_args[[k]])){
                pos_sym_names <- base::c(pos_sym_names, base::as.character(x = new_args[[k]]))
            }
        }
        if(base::any(pos_sym_names %in% new_arg_names[new_arg_names != ""], na.rm = FALSE)){
            left <- base::c(left, base::paste0(left_i2, left_reason[["pos_and_named"]], collapse = NULL, recycle0 = FALSE))
            next
        }
        # validation 3: the corrected call must be the STATUS proposal, whatever the order of the named arguments, the spaces, the line breaks and the comments. If the STATUS does not parse (e.g., call written on several lines with several statements inside {}, joined into a single line in the STATUS), the corrected call must at least contain all the missing argument names
        status_call <- base::tryCatch(
            expr = {
                base::str2lang(s = status_i2)
            },
            error = function(e){NULL}
        )
        if(base::is.call(x = status_call)){
            same_log <- call_canonical(tempo_call = new_call) == call_canonical(tempo_call = status_call)
        }else{
            same_log <- base::all(base::vapply(X = miss, FUN = function(y){y$name}, FUN.VALUE = base::character(length = 1L), USE.NAMES = FALSE) %in% new_arg_names, na.rm = FALSE)
        }
        if( ! same_log){
            left <- base::c(left, base::paste0(left_i2, left_reason[["differs"]], collapse = NULL, recycle0 = FALSE))
            next
        }
        all_ins_line <- base::c(all_ins_line, ins_line)
        all_ins_char <- base::c(all_ins_char, ins_char)
        all_ins_text <- base::c(all_ins_text, ins_text)
        inserted <- base::c(inserted, base::paste0("line ", line_i2, ": ", status_i2, collapse = NULL, recycle0 = FALSE))
    }
    # application of all the insertions into orig_lines: the insertions of the different calls (e.g., nested calls) never overlap, since they are insertions of text without removal
    orig_lines <- insert_txt(lines = orig_lines, ins_line = all_ins_line, ins_char = all_ins_char, ins_text = all_ins_text)
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
            # codecov inactivated because it is a defensive net: each corrected call is individually validated as parsable, and the insertions of the different calls never overlap, so the reassembled code cannot fail to parse without a bug in the validations above.
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


