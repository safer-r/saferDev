#' @title arg_test
#' @description
#' Test combinations of argument values of a function.
#' @param fun Single character string indicating the name of the function tested (without brackets).
#' @param arg Vector of character strings of arguments of fun. At least arguments that do not have default values must be present in this vector.
#' @param val List with number of compartments equal to the length of arg, each compartment containing values of the corresponding argument in arg. Each different value must be in a list or in a vector. For instance, argument 3 in arg is a logical argument (values accepted TRUE, FALSE, NA). Thus, compartment 3 of val can be either list(TRUE, FALSE, NA), or c(TRUE, FALSE, NA). NULL value alone must be written list(NULL).
#' @param expect_error List of exactly the same structure as val argument, but containing FALSE or TRUE, depending on whether error is expected (TRUE) or not (FALSE) for each corresponding value of val. A message is returned depending on discrepancies between the expected and observed errors. See the examples below. BEWARE: not always possible to write the expected errors for all the combination of argument values. Ignored if NULL.
#' @param parall Single logical value. Force parallelization ?
#' @param thread_nb Single numeric integer indicating the number of threads to use if ever parallelization is required. If NULL, all the available threads will be used. Ignored if parall is FALSE.
#' @param print_count Single interger value. Print a working progress message every print_count during loops. BEWARE: can increase substentially the time to complete the process if using a small integer value, like 10 for instance. Use Inf if no loop message desired.
#' @param plot_fun Single logical value. Plot the plotting function tested for each test? Ignored if the tested function is not a graphic function.
#' @param export Single logical value. Export the results into a .RData file and into a .tsv file? If FALSE, return a list into the console (see below). BEWARE: will be automatically set to TRUE if parall is TRUE. This means that when using parallelization, the results are systematically exported, not returned into the console.
#' @param res_path Single character string indicating the absolute pathway of the folder where the txt results and pdfs, containing all the plots, will be saved (by default into the working directory). Several txt and pdf, one per thread, if parallelization. Ignored if export is FALSE. Must be specified if parall is TRUE or if export is TRUE.
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) correct lib_path argument value 2) required functions and related packages effectively present in local R lybraries and 3) R classical operators (like "<-") not overwritten by another package because of the R scope. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R package are not installed in the default directories because of lack of admin rights.  More precisely, lib_path is passed through the new argument of .libPaths() so that the new library paths are unique(c(new, .Library.site, .Library)). Warning: .libPaths() is restored to the initial paths, after function execution. Ignored if NULL (default) or if the safer_check argument is FALSE: only the pathways specified by the current .libPaths() are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>.". If NULL, converted into "".
#' @returns
#' One or several pdf if a plotting function is tested and if the plot_fun argument is TRUE. 
#' 
#' And then, if export is FALSE a list containing:
#' 
#' - $fun: the tested function.
#' 
#' - $ini: the initial input values.
#' 
#' - $data: a data frame of all the combination tested, containing the following columns:
#' 
#'      # the different values tested, named by arguments
#'      - $kind: a vector of character strings indicating the kind of test result: either "ERROR", or "WARNING", or "OK".
#'      - $problem: a logical vector indicating if error or not.
#'      
#'      - $expected.error: optional logical vector indicating the expected error specified in the expect_error argument.
#'      - $message: either NULL if $kind is always "OK", or the messages.
#'      
#' - $sys.info: system and packages info.
#' 
#' If export is TRUE: 
#' 
#' - the same list object into a .RData file.
#' 
#' - also the $data data frame into a .tsv file.
#' 
#' - if expect_error is non NULL and if any discrepancy, the $data data frame into a .tsv file but containing only the rows with discrepancies between expected and observed errors.
#' @details 
#' Limited to 43 arguments with at least 2 values each. The total number of arguments tested can be more if the additional arguments have a single value. The limit is due to nested "for" loops (https://stat.ethz.ch/pipermail/r-help/2008-March/157341.html), but this limitation is away from the number of tests performed that would be 2^43.
#' @seealso \code{\link{arg_check}}.
#' @author \href{gael.millot@pasteur.fr}{Gael Millot}
#' @author \href{yushi.han2000@gmail.com}{Yushi Han}
#' @author \href{wanghaiding442@gmail.com}{Haiding Wang}
#' @examples
#' arg_test(fun = "unique", arg = c("x", "incomparables"), 
#' val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)))
#' 
#' arg_test(fun = "unique", arg = c("x", "incomparables"), 
#' val = list(x = list(1:10, c(1,1,2,8), NA), 
#' incomparable = c(TRUE, FALSE, NA)), expect_error = list(x = list(FALSE, FALSE, TRUE), 
#' incomparable = c(FALSE, FALSE, TRUE)))
#' 
#' arg_test(fun = "unique", arg = c("x", "incomparables"), 
#' val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)), 
#' expect_error = list(x = c(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE)), 
#' export = TRUE, res_path = getwd())
#' 
#' \dontrun{ # Example that return an error
#' arg_test(fun = "unique", arg = c("x", "incomparables"), val = list(A = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA))))
#' arg_test(fun = "round", arg = c("data", "dec.nb", "after.lead.zero"), val = list(L1 = list(c(1, 1.0002256, 1.23568), "a", NA), L2 = list(2, c(1,3), NA), L3 = c(TRUE, FALSE, NA)))
#' }
#' 
#' arg_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 12:13, NA, (1:10)^2), 
#' y = list(1:10, NA, NA)),  expect_error = list(x = list(FALSE, TRUE, TRUE, FALSE), 
#' y = list(FALSE, TRUE, TRUE)), parall = FALSE, thread_nb = NULL, plot_fun = TRUE, 
#' res_path = ".", lib_path = NULL)
#' 
#' arg_test(fun = "plot", arg = c("x", "y"), val = list(x = list(1:10, 12:13, NA, (1:10)^2), 
#' y = list(1:10, NA, NA)), parall = FALSE, thread_nb = 4, 
#' plot_fun = TRUE, res_path = ".", 
#' lib_path = NULL)
#' @importFrom lubridate seconds_to_period
#' @importFrom qpdf pdf_combine
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterSplit
#' @importFrom parallel clusterApply
#' @importFrom parallel stopCluster
#' @export
arg_test <- function(
    fun, 
    arg, 
    val, 
    expect_error = NULL, 
    parall = FALSE, 
    thread_nb = NULL, 
    print_count = 10, 
    plot_fun = FALSE, 
    export = FALSE, 
    res_path = ".", 
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # fun = "unique" ; arg = "x" ; val = base::list(x = base::list(1:3, mean)) ; expect_error = base::list(x = base::list(TRUE, TRUE)) ; parall = FALSE ; thread_nb = NULL ; plot_fun = FALSE ; export = FALSE ; res_path = "C:\\Users\\gmillot\\Desktop\\" ; lib_path = NULL ; print_count = 1; safer_check = TRUE ; error_text = "" # for function debugging
# fun = "unique" ; arg = c("x", "incomparables") ; val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)) ; expect_error = list(x = c(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE)) ; parall = FALSE ; thread_nb = NULL ; plot_fun = FALSE ; export = FALSE ; res_path = "C:\\Users\\gmillot\\Desktop\\" ; lib_path = NULL ; print_count = 1; safer_check = TRUE ; error_text = "" # for function debugging
# function_name <- "arg_test" ; arg_user_setting = base::list(x = as.name(x = "arg_test"), fun = "unique", arg = c("x", "incomparables"), val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)), expect_error = list(x = c(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE)), parall = FALSE, thread_nb = NULL, plot_fun = FALSE, export = FALSE, res_path = "C:\\Users\\gmillot\\Desktop\\", lib_path = NULL, print_count = 1, safer_check = TRUE, error_text = "") ; arg_names <- c("x", "fun",  "arg",  "val", "expect_error", "parall", "thread_nb", "print_count", "plot_fun", "export", "res_path", "safer_check", "lib_path", "error_text") ; ini <- arg_user_setting ; arg_user_setting_eval <- list(fun = "unique", arg = c("x", "incomparables"), val = list(x = list(1:10, c(1,1,2,8), NA), incomparable = c(TRUE, FALSE, NA)), expect_error = list(x = c(FALSE, FALSE, TRUE), incomparable = c(FALSE, FALSE, TRUE)), export = TRUE, res_path = getwd())

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
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
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

    ######## arg with no default values
    mandat_args <- base::c(
        "fun", 
        "arg", 
        "val"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "fun", 
        "arg", 
        "val", 
        # "expect_error", # inactivated because can be NULL
        "parall", 
        # "thread_nb", # inactivated because can be NULL
        "print_count", 
        "plot_fun", 
        "export",
        "res_path", 
        "safer_check" 
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
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
        "fun", 
        "arg", 
        "val", 
        "expect_error", 
        "parall", 
        "thread_nb",
        "print_count", 
        "plot_fun", 
        "export",
        "res_path", 
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
                base::paste0(tempo_arg_user_setting_eval[tempo_log], collapse = "\n", recycle0 = FALSE), 
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
                base::paste0(arg_user_setting_names[tempo_log], collapse = "\n", recycle0 = FALSE), 
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
    if(safer_check == TRUE){
        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
            if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
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
                ini_lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base:::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base:::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "lubridate::seconds_to_period", 
                "qpdf::pdf_combine",
                "parallel::detectCores",
                "parallel::makeCluster",
                "parallel::clusterSplit",
                "parallel::clusterApply",
                "parallel::stopCluster",
                "saferDev:::.base_op_check", 
                "saferDev::arg_check",
                "saferDev::get_message"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
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
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = fun, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = arg, class = "vector", typeof = "character", mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = val, class = "list", typeof = NULL, mode = "list", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = expect_error)){
        tempo <- saferDev::arg_check(data = expect_error, class = "list", typeof = NULL, mode = "list", length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = parall, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(parall == TRUE){
        if( ! base::is.null(x = thread_nb)){
            tempo <- saferDev::arg_check(data = thread_nb, class = "numeric", typeof = "double", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = FALSE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
        }
    }
    tempo <- saferDev::arg_check(data = print_count, class = "vector", typeof = "integer", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = plot_fun, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = export, class = "vector", typeof = NULL, mode = "logical", length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(parall == TRUE | export == TRUE){
        tempo <- saferDev::arg_check(data = res_path, class = "vector", typeof = NULL, mode = "character", length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        "fun", 
        "arg",  
        "res_path"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
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
    warn <- NULL
    warn_count <- 0
    ######## end warning initiation

    ######## graphic device checking
    # check the number of graphic devices on exit
    dev_list <- grDevices::dev.list() 
    base::on.exit(
        expr = if(base::length(x = dev_list) != base::length(x = grDevices::dev.list())){
            tempo_cat <- base::paste0(
                "INTERNAL ERROR IN THE BACKBONE PART OF ", 
                intern_error_text_start, 
                "SOME GRAPHIC DEVICES WERE OPENED BY ", 
                function_name, 
                " BUT NOT CLOSED BEFORE END OF EXECUTION.\n\nIF IT IS EXPECTED, JUST REMOVE THE CODE DISPLAYING THIS MESSAGE INSIDE ", 
                function_name, 
                ".\n\nOTHERWISE, THE PROBLEM COMES FROM OPENED GRAPHIC DEVICES BEFORE RUNNING ", 
                function_name, 
                " (n = ", 
                base::length(x = dev_list), 
                ") AND AFTER (n = ", 
                base::length(x = grDevices::dev.list()), 
                ").", 
                intern_error_text_end, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }, 
        add = TRUE, 
        after = TRUE
    )
    # end check the number of graphic devices on exit
    # restore the graphic parameters on exit
    if(base::length(x = grDevices::dev.list()) > 0){
        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = "warning") # to recover the present graphical parameters
        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = "warning"), add = TRUE, after = TRUE)
    }
    # end restore the graphic parameters on exit
    ######## end graphic device checking

    ######## other checkings
    if( ! base::is.null(x = thread_nb)){
        if(parall == TRUE & thread_nb < 1){
            tempo_cat <- base::paste0(
                error_text_start, 
                "thread_nb PARAMETER MUST EQUAL OR GREATER THAN 1.\nHERE IT IS: ", 
                thread_nb, 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(base::grepl(x = fun, pattern = "()$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){ # remove ()
        fun <- base::sub(x = fun, pattern = "()$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    }
    if( ! base::exists(x = fun, where = -1, envir = , frame = , mode = "any", inherits = TRUE)){ # inherits TRUE because the function can be somewhere in the scope 
        tempo_cat <- base::paste0(
            error_text_start, 
            "CHARACTER STRING IN fun ARGUMENT DOES NOT EXIST IN THE R ENVIRONMENTS:\n", 
            base::paste0(fun, collapse = "\n", recycle0 = FALSE),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else if( ! base::all(base::class(x = base::get(x = fun, pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = TRUE)) == "function", na.rm = TRUE)){ # here no env = base::sys.nframe(), inherit = FALSE for base::get() because fun is a function in the classical scope
        tempo_cat <- base::paste0(
             error_text_start, 
            "fun ARGUMENT IS NOT CLASS \"function\" BUT:\n", 
            base::paste0(base::class(x = base::get(x = fun, pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = TRUE)), collapse = "\n", recycle0 = FALSE), 
            "\nCHECK IF ANY CREATED OBJECT WOULD HAVE THE NAME OF THE TESTED FUNCTION.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::length(x = arg) == 0L){
        tempo_cat <- base::paste0(
             error_text_start, 
            "arg ARGUMENT CANNOT BE LENGTH 0.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    for(i2 in 1:base::length(x = val)){ # base::length(x = val) must be aequal to nb of arguments
        tempo1 <- saferDev::arg_check(data = val[[i2]], class = "vector", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        tempo2 <- saferDev::arg_check(data = val[[i2]], class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if(tempo1$problem == TRUE & tempo2$problem == TRUE){
            tempo_cat <- base::paste0(
                error_text_start, 
                "COMPARTMENT ", 
                i2, 
                " OF val ARGUMENT MUST BE A VECTOR OR A LIST.", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if(tempo1$problem == FALSE){ # vector split into list compartments
            val[[i2]] <- base::split(x = val[[i2]], f = 1:base::length(x = val[[i2]]), drop = FALSE) # convert a vector into list, with each value of the vector in a compartment
        }
    }
    if(base::length(x = arg) != base::length(x = val)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "LENGTH OF arg ARGUMENT MUST BE IDENTICAL TO LENGTH OF val ARGUMENT.\nHERE IT IS\n", 
            base::length(x = arg), 
            "\nVERSUS\n", 
            base::length(x = val), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    args <- base::names(x = base::formals(fun = base::get(x = fun, pos = -1L, envir = base::as.environment(-1), mode = "any", inherits = TRUE), envir = base::parent.frame(n = 1))) # here no env = base::sys.nframe(), inherit = FALSE for base::get() because fun is a function in the classical scope
    if( ! base::all(arg %in% args, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "SOME OF THE STRINGS IN arg ARE NOT ARGUMENTS OF fun.\nfun ARGUMENTS:\n", 
            base::paste0(args, collapse = " ", recycle0 = FALSE), 
            "\nPROBLEMATIC STRINGS IN arg:\n", 
            base::paste0(arg[ ! arg %in% args], collapse = " ", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::sum(base::sapply(X = val, FUN = base::length, simplify = TRUE, USE.NAMES = TRUE) > 1, na.rm = TRUE) > 43){
        tempo_cat <- base::paste0(
            error_text_start, 
            "CANNOT TEST MORE THAN 43 ARGUMENTS IF THEY ALL HAVE AT LEAST 2 VALUES EACH.\nHERE THE NUMBER IS:\n", 
            base::sum(base::sapply(X = val, FUN = base::length, simplify = TRUE, USE.NAMES = TRUE) > 1, na.rm = TRUE),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if( ! base::is.null(x = expect_error)){
        if(base::length(x = val) != base::length(x = expect_error)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "LENGTH OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF expect_error ARGUMENT.\nHERE IT IS\n", 
                base::length(x = val), 
                "\nVERSUS\n", 
                base::length(x = expect_error), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        for(i3 in 1:base::length(x = expect_error)){
            tempo1 <- saferDev::arg_check(data = expect_error[[i3]], class = "vector",  typeof = NULL, mode = "logical", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            tempo2 <- saferDev::arg_check(data = expect_error[[i3]], class = "list",  typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
            if(tempo1$problem == TRUE & tempo2$problem == TRUE){
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "COMPARTMENT ", 
                    i3, 
                    " OF expect_error ARGUMENT MUST BE TRUE OR FALSE.",
                collapse = NULL, 
                recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if(tempo1$problem == FALSE){ # vector split into list compartments
                expect_error[[i3]] <- base::split(x = expect_error[[i3]], f = 1:base::length(x = expect_error[[i3]]), drop = FALSE) # convert a vector into list, with each value of the vector in a compartment
            }
        }
        for(i2 in 1:base::length(x = expect_error)){
            if(base::all(base::class(x = expect_error[[i2]]) == "list", na.rm = FALSE)){
                if( ! base::all(base::class(x = val[[i2]]) == "list", na.rm = FALSE)){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "expect_error ARGUMENT MUST BE A LIST OF EXACTLY THE SAME STRUCTURE AS val ARGUMENT.\nHERE COMPARTMENT ", 
                        i2, 
                        " OF expect_error IS CLASS ", 
                        base::paste0(base::class(x = expect_error[[i2]]), collapse = " ", recycle0 = FALSE), 
                        "\nAND COMPARTMENT ", 
                        i2, 
                        " OF val IS CLASS ", 
                        base::paste0(base::class(x = val[[i2]]), collapse = " ", recycle0 = FALSE), 
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else if(base::length(x = val[[i2]]) != base::length(x = expect_error[[i2]])){
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "LENGTH OF COMPARTMENT ", 
                        i2, 
                        " OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF COMPARTMENT ", 
                        i2, 
                        " OF expect_error ARGUMENT.\nHERE IT IS\n", 
                        base::length(x = val[[i2]]), 
                        "\nVERSUS\n", 
                        base::length(x = expect_error[[i2]]), 
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }
            }
        }
    }
    if(parall == TRUE & export == FALSE){
        export <- TRUE
        warn_count <- warn_count + 1
        tempo_warn <- "export ARGUMENT CONVERTED TO TRUE BECAUSE thread_nb ARGUMENT IS NOT NULL."
        warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
    }
    if(parall == TRUE | export == TRUE){
        if( ! base::all(base::dir.exists(paths = res_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and res_path == NA
            tempo_cat <- base::paste0(
                error_text_start, 
                "DIRECTORY PATH INDICATED IN THE res_path ARGUMENT DOES NOT EXISTS:\n", 
                base::paste0(res_path, collapse = "\n", recycle0 = FALSE),
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if( ! base::is.null(x = lib_path)){
        if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA
            tempo_cat <- base::paste0(
                error_text_start, 
                "DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\n", 
                base::paste0(lib_path, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # declaration of special plot functions
    sp_plot_fun <- base::c("gg_scatter", "gg_bar", "gg_boxplot")
    # end declaration of special plot functions

    # new environment
    env.name <- base::paste0("env", base::as.numeric(x = base::Sys.time()), collapse = NULL, recycle0 = FALSE)
    if(base::exists(x = env.name, where = -1, envir = , frame = , mode = "any", inherits = FALSE)){ # verify if still ok when this function is inside a function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ENVIRONMENT env.name ALREADY EXISTS.\nPLEASE RERUN ONCE.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }else{
        base::assign(x = env.name, value = base::new.env(hash = TRUE, parent = base::parent.frame(n = 1), size = 29L), pos = -1, envir = , inherits = FALSE, immediate = TRUE)
        base::assign(x = "data", value = data,  pos = -1, envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), inherits = FALSE, immediate = TRUE) # data assigned in a new envir for test
    }
    # end new environment

    ini <- base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L)) # initial parameters (specific of arg_test())
    base::cat("\ntest JOB IGNITION\n", file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
    ini.date <- base::Sys.time()
    ini.time <- base::as.numeric(x = ini.date) # time of process begin, converted into seconds
    if(export == TRUE){
        res_path <- base::paste0(res_path, "/arg_test_res_", base::trunc(x = ini.time), collapse = NULL, recycle0 = FALSE)
        if(base::dir.exists(paths = res_path)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "FOLDER ALREADY EXISTS:\n", 
                res_path, 
                "\nPLEASE RERUN ONCE.", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            base::dir.create(path = res_path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
        }
    }
    total.comp.nb <- base::prod(base::sapply(X = val, FUN = "length", simplify = TRUE, USE.NAMES = TRUE), na.rm = FALSE)
    base::cat(base::paste0("\nTOTAL NUMBER OF TESTS: ", total.comp.nb, "\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)

    # creation of the txt instruction that includes several loops
    loop.string <- NULL
    end.loop.string <- NULL
    fun.args <- NULL
    fun.args2 <- NULL
    error.values <- NULL
    arg.values <- "base::list("
    for(i1 in 1:base::length(x = arg)){
        if(parall == FALSE){
            if(base::length(x = val[[i1]]) > 1){ # loop only if more than one value in base::length(x = val[[i1]])
                loop.string <- base::paste0(loop.string, "for(i", i1, " in 1:", base::length(x = val[[i1]]), "){", collapse = NULL, recycle0 = FALSE)
                end.loop.string <- base::paste0(end.loop.string, "}", collapse = NULL, recycle0 = FALSE)
            }
        }else{
            loop.string <- "for(i in x){"
            end.loop.string <- "}"
        }
        fun.args <- base::paste0(
            fun.args, 
            base::ifelse(test = i1 == 1L, yes = "", no = ", "), 
            arg[i1], 
            " = val[[", 
            i1, 
            "]][[", 
            if(parall == FALSE){
                if(base::length(x = val[[i1]]) > 1){
                    base::paste0("i", i1, collapse = NULL, recycle0 = FALSE)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                base::paste0("i.list[[", i1, "]][i]", collapse = NULL, recycle0 = FALSE)
            }, 
            "]]",
            collapse = NULL, 
            recycle0 = FALSE
        )
        fun.args2 <- base::paste0(
            fun.args2, 
            base::ifelse(test = i1 == 1L, yes = "", no = ", "), 
            arg[i1], 
            " = val[[", 
            i1, 
            "]][[', ", 
            if(parall == FALSE){
                if(base::length(x = val[[i1]]) > 1){
                    base::paste0("i", i1, collapse = NULL, recycle0 = FALSE)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                base::paste0("i.list[[", i1, "]][i]", collapse = NULL, recycle0 = FALSE)
            }, 
            ", ']]", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        arg.values <- base::paste0(
            arg.values, 
            "val[[", i1, "]][[", 
            if(parall == FALSE){
                if(base::length(x = val[[i1]]) > 1){
                    base::paste0("i", i1, collapse = NULL, recycle0 = FALSE)
                }else{
                    "1" # a unique element in val[[i1]]
                }
            }else{
                base::paste0("i.list[[", i1, "]][i]", collapse = NULL, recycle0 = FALSE)
            }, 
            "]]", 
            base::ifelse(test = i1 == base::length(x = arg), yes = "", no = ", "), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        error.values <- base::paste0(
            error.values, 
            base::ifelse(test = i1 == 1L, yes = "", no = " | "), 
            "expect_error[[", i1, "]][[", 
            if(parall == FALSE){
                if(base::length(x = expect_error[[i1]]) > 1){
                    base::paste0("i", i1, collapse = NULL, recycle0 = FALSE)
                }else{
                    "1" # a unique element in expect_error[[i1]]
                }
            }else{
                base::paste0("i.list[[", i1, "]][i]", collapse = NULL, recycle0 = FALSE)
            }, 
            "]]", 
            collapse = NULL, 
            recycle0 = FALSE
        )
    }
    arg.values <- base::paste0(arg.values, ")", collapse = NULL, recycle0 = FALSE)
    fun.test <- base::paste0(fun, "(", fun.args, ")", collapse = NULL, recycle0 = FALSE)
    fun.test2 <- base::paste0("base::paste0('", fun, "(", fun.args2, ")')", collapse = NULL, recycle0 = FALSE)
    # plot title for special plot functions
    if(plot_fun == TRUE){
        plot.kind <- "classic"
        if(fun %in% sp_plot_fun){
            plot.kind <- "special"
            if(base::any(arg %in% "title", na.rm = TRUE)){ # this is for the special functions
                tempo.match <- base::regmatches(x = fun.test, m = base::regexpr(text = fun.test, pattern = "title = .+[,)]", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), invert = FALSE)
                tempo.match <- base::substring(text = tempo.match , first =  1, last =  base::nchar(x = tempo.match, type = "chars", allowNA = FALSE, keepNA = NA) - 1)
                fun.test <- base::sub(x = fun.test, pattern = tempo.match, replacement = base::paste0(tempo.match, "\ntempo.title", collapse = NULL, recycle0 = FALSE), ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
            }else{
                fun.test <- base::sub(x = fun.test, pattern = ")$", replacement = ", title = tempo.title)", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
            }
        }
    }
    # end plot title for special plot functions
    kind <- base::character(length = 0L)
    problem <- base::logical(length = 0L)
    expected.error <- base::logical(length = 0L)
    res <- base::character(length = 0L)
    count <- 0
    print_count_loop <- 0
    plot.count <- 0
    if(base::length(x = arg) == 1L){
        data <- base::data.frame(row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    }else{ # base::length(x = arg) == 0L already tested above
        data <- base::data.frame(base::t(x = base::vector(mode = "character", length =  base::length(x = arg))), row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)[-1, ] # -1 to remove the single row created and to have an empty data frame with base::length(x = arg) columns
    }
    code <- base::paste0(
        loop.string, '
            count <- count + 1
            print_count_loop <- print_count_loop + 1
            arg.values.print <- base::eval(expr = base::parse(text = arg.values, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) # recover the list of the i1 compartment
            for(j3 in 1:base::length(x = arg.values.print)){ # WARNING: do not use i1, i2 etc., here because already in loop.string
                tempo.capt <- utils::capture.output(tempo.error <- saferDev::get_message(
                    data =  base::paste0("base::paste0(arg.values.print[[", j3, "]], collapse = NULL, recycle0 = FALSE)", collapse = NULL, recycle0 = FALSE), 
                    kind = "error", 
                    header = FALSE, 
                    print_no = FALSE, 
                    text = NULL, 
                    env = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), 
                    lib_path = lib_path, 
                    safer_check = FALSE, 
                    error_text = embed_error_text
                ), file = NULL, append = FALSE, type = base::c("output", "message"), split = FALSE) # collapsing arg.values sometimes does not work (with function for instance)
                if( ! base::is.null(x = tempo.error)){
                    arg.values.print[[j3]] <- base::paste0("SPECIAL VALUE OF CLASS ", base::class(x = arg.values.print[[j3]]), " AND TYPE ", base::typeof(x = arg.values.print[[j3]]), collapse = NULL, recycle0 = FALSE)
                }
            }
            data <- base::rbind(data, base::as.character(x = base::sapply(X = arg.values.print, FUN = function(x){base::paste0(x, collapse = " ", recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE)), stringsAsFactors = FALSE, deparse.level = 1) # each colum is a test
            tempo.capt <- utils::capture.output(tempo.try.error <- saferDev::get_message(
                data = base::eval(expr = base::parse(text = fun.test2, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), 
                kind = "error", 
                header = FALSE, 
                print_no = FALSE, 
                text = NULL, 
                env = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), 
                lib_path = lib_path, 
                safer_check = FALSE, 
                error_text = embed_error_text
            ), file = NULL, append = FALSE, type = base::c("output", "message"), split = FALSE) # data argument needs a character string but base::eval(expr = base::parse(text = fun.test2, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) provides it (base::eval base::parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)
            tempo.capt <- utils::capture.output(tempo.try.warning <- saferDev::get_message(
                data = base::eval(expr = base::parse(text = fun.test2, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), 
                kind = "warning", 
                header = FALSE, 
                print_no = FALSE, 
                text = NULL, 
                env = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), 
                lib_path = lib_path, 
                safer_check = FALSE, 
                error_text = embed_error_text
            ), file = NULL, append = FALSE, type = base::c("output", "message"), split = FALSE) # data argument needs a character string but base::eval(expr = base::parse(text = fun.test2, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) provides it (base::eval base::parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)
            if( ! base::is.null(x = expect_error)){
                expected.error <- base::c(expected.error, base::eval(expr = base::parse(text = error.values, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)))
            }
            if( ! base::is.null(x = tempo.try.error)){
                kind <- base::c(kind, "ERROR")
                problem <- base::c(problem, TRUE)
                res <- base::c(res, tempo.try.error)
            }else{
                if( ! base::is.null(x = tempo.try.warning)){
                    kind <- base::c(kind, "WARNING")
                    problem <- base::c(problem, FALSE)
                    res <- base::c(res, tempo.try.warning)
                }else{
                    kind <- base::c(kind, "OK")
                    problem <- base::c(problem, FALSE)
                    res <- base::c(res, "")
                }
                if(plot_fun == TRUE){
                    base::invisible(x = grDevices::dev.set(which = window.nb))
                    plot.count <- plot.count + 1
                    tempo.title <- base::paste0("test_", base::sprintf(fmt = base::paste0("%0", base::nchar(x = total.comp.nb, type = "chars", allowNA = FALSE, keepNA = NA), "d", collapse = NULL, recycle0 = FALSE), base::ifelse(test = parall == FALSE, yes = count, no = x[count])), collapse = NULL, recycle0 = FALSE)
                    if(plot.kind == "classic"){ # not ggplot. So title has to be added in a classical way
                        # graphics::par(ann=FALSE, xaxt="n", yaxt="n", mar = base::rep(x = 1, times = 4), bty = "n", xpd = NA) # old
                        graphics::par(bty = "n", xpd = NA, no.readonly = FALSE) # new
                        base::eval(expr = base::parse(text = fun.test, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
                        # base::plot(1, 1, type = "n") # no display with type = "n"
                        x.left.dev.region <- (graphics::par("usr", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / (graphics::par("plt", no.readonly = FALSE)[2] - graphics::par("plt", no.readonly = FALSE)[1])) * graphics::par("plt", no.readonly = FALSE)[1] - ((graphics::par("usr", no.readonly = FALSE)[2] - graphics::par("usr", no.readonly = FALSE)[1]) / ((graphics::par("omd", no.readonly = FALSE)[2] - graphics::par("omd", no.readonly = FALSE)[1]) * (graphics::par("plt", no.readonly = FALSE)[2] - graphics::par("plt", no.readonly = FALSE)[1]))) * graphics::par("omd", no.readonly = FALSE)[1])
                        y.top.dev.region <- (graphics::par("usr", no.readonly = FALSE)[4] + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / (graphics::par("plt", no.readonly = FALSE)[4] - graphics::par("plt", no.readonly = FALSE)[3])) * (1 - graphics::par("plt", no.readonly = FALSE)[4]) + ((graphics::par("usr", no.readonly = FALSE)[4] - graphics::par("usr", no.readonly = FALSE)[3]) / ((graphics::par("omd", no.readonly = FALSE)[4] - graphics::par("omd", no.readonly = FALSE)[3]) * (graphics::par("plt", no.readonly = FALSE)[4] - graphics::par("plt", no.readonly = FALSE)[3]))) * (1 - graphics::par("omd", no.readonly = FALSE)[4]))
                        graphics::text(x = x.left.dev.region, y = y.top.dev.region, labels = tempo.title, adj=base::c(0, 1), cex = 1.5)
                    }else if(plot.kind == "special"){ # ggplot. title has been added above
                        base::eval(expr = base::parse(text = fun.test, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
                    }else{
                        tempo_cat <- base::paste0("INTERNAL ERROR 1 IN ", intern_error_text_start, "CODE HAS TO BE MODIFIED.", intern_error_text_end, collapse = NULL, recycle0 = FALSE)
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }
                }
            }
            if(print_count_loop == print_count){
                print_count_loop <- 0
                tempo.time <- base::as.numeric(x = base::Sys.time())
                tempo.lapse <- base::round(x = lubridate::seconds_to_period(x = tempo.time - ini.time), digits = 0)
                final.loop <- (tempo.time - ini.time) / count * base::ifelse(test = parall == FALSE, yes = total.comp.nb, no = base::length(x = x)) # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining
                final.exp <- base::as.POSIXct(origin = ini.date,x = final.loop, tz = "")
                base::cat(base::paste0(base::ifelse(test = parall == FALSE, yes = "\n", no = base::paste0("\nIN PROCESS ", process.id, " | ", collapse = NULL, recycle0 = FALSE)), "LOOP ", base::format(x = count, big.mark=","), " / ", base::format(x = base::ifelse(test = parall == FALSE, yes = total.comp.nb, no = base::length(x = x)), big.mark=","), " | TIME SPENT: ", tempo.lapse, " | EXPECTED END: ", final.exp, collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
            }
            if(count == base::ifelse(test = parall == FALSE, yes = total.comp.nb, no = base::length(x = x))){
                tempo.time <- base::as.numeric(x = base::Sys.time())
                tempo.lapse <- base::round(x = lubridate::seconds_to_period(x = tempo.time - ini.time), digits = 0)
                base::cat(base::paste0(base::ifelse(test = parall == FALSE, yes = "\nLOOP PROCESS ENDED | ", no = base::paste0("\nPROCESS ", process.id, " ENDED | ", collapse = NULL, recycle0 = FALSE)), "LOOP ", base::format(x = count, big.mark=","), " / ", base::format(x = base::ifelse(test = parall == FALSE, yes = total.comp.nb, no = base::length(x = x)), big.mark=","), " | TIME SPENT: ", tempo.lapse, "\n\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
            }
        ', 
        end.loop.string,
        collapse = NULL, 
        recycle0 = FALSE
    )
    # end creation of the txt instruction that includes several loops

    if(parall == TRUE){
        # list of i numbers that will be split
        i.list <- base::vector(mode = "list", length =  base::length(x = val)) # positions to split in parallel jobs
        for(i2 in 1:base::length(x = arg)){
            if(i2 == 1L){
                tempo.divisor <- total.comp.nb / base::length(x = val[[i2]])
                i.list[[i2]] <- base::rep(x = 1:base::length(x = val[[i2]]), each = base::as.integer(x = tempo.divisor))
                tempo.multi <- base::length(x = val[[i2]])
            }else{
                tempo.divisor <- tempo.divisor / base::length(x = val[[i2]])
                i.list[[i2]] <- base::rep(x = base::rep(x = 1:base::length(x = val[[i2]]), each = base::as.integer(x = tempo.divisor)), time = base::as.integer(x = tempo.multi))
                tempo.multi <- tempo.multi * base::length(x = val[[i2]])
            }
        }
        # end list of i numbers that will be split

        tempo_cat <- base::paste0("PARALLELIZATION INITIATED AT: ", ini.date, collapse = NULL, recycle0 = FALSE)
        base::cat(base::paste0("\n", tempo_cat, "\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
        tempo_thread_nb = parallel::detectCores(all.tests = FALSE, logical = TRUE) # detect the number of threads
        if(base::is.null(x = thread_nb)){
            thread_nb <- tempo_thread_nb
        }else if(tempo_thread_nb < thread_nb){
            thread_nb <- tempo_thread_nb
        }
        tempo_cat <- base::paste0("NUMBER OF THREADS USED: ", thread_nb, collapse = NULL, recycle0 = FALSE)
        base::cat(base::paste0("\n    ", tempo_cat, "\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
        Clust <- parallel::makeCluster(spec = thread_nb, type = , outfile = base::paste0(res_path, "/test_parall_log.txt", collapse = NULL, recycle0 = FALSE)) # outfile to print or cat during parallelization (only possible in a file, outfile = "" do not work on windows)
        tempo_cat <- base::paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:", collapse = NULL, recycle0 = FALSE)
        base::cat(base::paste0("\n    ", tempo_cat, "\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
        cluster.list <- parallel::clusterSplit(cl = Clust, seq =  1:total.comp.nb) # split according to the number of cluster
        utils::str(object = cluster.list) # using base::print(utils::str()) add a NULL below the result
        base::cat("\n", file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
        paral.output.list <- parallel::clusterApply( # paral.output.list is a list made of thread_nb compartments, each made of n / thread_nb (mat theo column number) compartment. Each compartment receive the corresponding results of this function
            cl = Clust,
            x = cluster.list,
            ini = ini, 
            thread_nb = thread_nb, 
            print_count = print_count, 
            total.comp.nb = total.comp.nb, 
            sp_plot_fun = sp_plot_fun,
            i.list = i.list, 
            fun.tested = fun,
            arg.values = arg.values,
            fun.test = fun.test,
            fun.test2 = fun.test2,
            kind = kind,
            problem = problem,
            res = res,
            count = count,
            plot.count = plot.count,
            data = data,
            code = code,
            plot_fun = plot_fun, 
            res_path = res_path, 
            lib_path = lib_path, 
            error_text_start = error_text_start,
            embed_error_text = embed_error_text,
            intern_error_text_start = intern_error_text_start, 
            intern_error_text_end = intern_error_text_end, 
            fun = function(
        x, 
        ini, 
        thread_nb, 
        print_count, 
        total.comp.nb, 
        sp_plot_fun, 
        i.list, 
        fun.tested, 
        arg.values, 
        fun.test, 
        fun.test2, 
        kind, 
        problem, 
        res, 
        count, 
        plot.count, 
        data, 
        code, 
        plot_fun, 
        res_path, 
        lib_path,
        error_text_start,
        embed_error_text,
        intern_error_text_start, 
        intern_error_text_end
            ){
                # check again: very important because another R
                process.id <- base::Sys.getpid()
                base::cat(base::paste0("\nPROCESS ID ", process.id, " -> TESTS ", x[1], " TO ", x[base::length(x = x)], "\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
                saferDev:::.pack_and_function_check(
                    fun = base::c(
                        "lubridate::seconds_to_period"
                    ),
                    lib_path = lib_path,
                    error_text = embed_error_text
                )
                # end check again: very important because another R
                # plot management
                if(plot_fun == TRUE){
                    grDevices::pdf(file = base::paste0(res_path, "/plots_from_test_", x[1], base::ifelse(test = base::length(x = x) == 1L, yes = ".pdf", no = base::paste0("-", x[base::length(x = x)], ".pdf", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = )
                }else{
                    grDevices::pdf(file = NULL, width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = ) # send plots into a NULL file, no pdf file created
                }
                window.nb <- grDevices::dev.cur()
                base::invisible(x = grDevices::dev.set(which = window.nb))
                # end plot management
                # new environment
                ini.date <- base::Sys.time()
                ini.time <- base::as.numeric(x = ini.date) # time of process begin, converted into 
                env.name <- base::paste0("env", ini.time, collapse = NULL, recycle0 = FALSE)
                if(base::exists(x = env.name, where = -1, envir = , frame = , mode = "any", inherits = FALSE)){ # verify if still ok when arg_test() is inside a function
                    tempo_cat <- base::paste0(
                        error_text_start, 
                        "ENVIRONMENT env.name ALREADY EXISTS.\nPLEASE RERUN ONCE.", 
                        collapse = NULL, 
                        recycle0 = FALSE
                    )
                    base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                }else{
                    base::assign(x = env.name, value = base::new.env(hash = TRUE, parent = base::parent.frame(n = 1), size = 29L), pos = -1, envir = , inherits = FALSE, immediate = TRUE)
                    base::assign(x = "val", value = val,  pos = -1, envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), inherits = FALSE, immediate = TRUE) # var replaced by val
                }
                # end new environment
                print_count_loop <- 0
                base::suppressMessages(expr = base::suppressWarnings(expr = base::eval(expr = base::parse(text = code, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning"), classes = "message")
                base::"colnames<-"(data, arg)
                if( ! base::is.null(x = expect_error)){
                    data <- base::data.frame(data, kind = kind, problem = problem, expected.error = expected.error, message = res, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
                }else{
                    data <- base::data.frame(data, kind = kind, problem = problem, message = res, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
                }
                base::row.names(x = data) <- base::paste0("arg_test_", base::sprintf(fmt = base::paste0("%0", base::nchar(x = total.comp.nb, type = "chars", allowNA = FALSE, keepNA = NA), "d", collapse = NULL, recycle0 = FALSE), x), collapse = NULL, recycle0 = FALSE)
                sys.info <- utils::sessionInfo(package = NULL)
                sys.info$loadedOnly <- sys.info$loadedOnly[base::order(base::names(x = sys.info$loadedOnly), na.last = TRUE, decreasing = FALSE, method = )] # sort the packages
                base::invisible(x = grDevices::dev.off(which = window.nb))
                base::rm(env.name, list = base::character(length = 0L), pos = -1, envir = , inherits = FALSE) # optional, because should disappear at the end of the function execution
                # output
                output <- base::list(fun = fun, ini = ini, data = data, sys.info = sys.info)
                base::save(list = output, file = base::paste0(res_path, "/arg_test_", x[1], base::ifelse(test = base::length(x = x) == 1L, yes = ".RData", no = base::paste0("-", x[base::length(x = x)], ".RData", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), ascii = FALSE, version = NULL, envir = base::parent.frame(n = 1), compress = FALSE, compression_level = , eval.promises = TRUE, precheck = TRUE)
                if(plot_fun == TRUE & plot.count == 0L){
                    warn_count <- warn_count + 1
                    tempo_warn <- base::paste0("(", warn_count,") IN PROCESS ", process.id, ": NO PDF PLOT BECAUSE ONLY ERRORS REPORTED.", collapse = NULL, recycle0 = FALSE)
                    warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
                    base::file.remove(base::paste0(res_path, "/plots_from_arg_test_", x[1], base::ifelse(test = base::length(x = x) == 1L, yes = ".pdf", no = base::paste0("-", x[base::length(x = x)], ".pdf", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE))
                }
                table.out <- base::as.matrix(x = data)
                # table.out[table.out == ""] <- " " # does not work # because otherwise utils::read.table() converts "" into NA
                table.out <- base::gsub(x = table.out, pattern = "\n", replacement = " ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                utils::write.table(x = table.out, file = base::paste0(res_path, "/table_from_arg_test_", x[1], base::ifelse(test = base::length(x = x) == 1L, yes = ".tsv", no = base::paste0("-", x[base::length(x = x)], ".tsv", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "")
            }
        )
        parallel::stopCluster(cl = Clust)

        # files assembly
        if(base::length(x = cluster.list) > 1){
            for(i2 in 1:base::length(x = cluster.list)){
                tempo.file <- base::paste0(res_path, "/table_from_arg_test_", base::min(cluster.list[[i2]], na.rm = TRUE), base::ifelse(test = base::length(x = cluster.list[[i2]]) == 1L, yes = ".tsv", no = base::paste0("-", base::max(cluster.list[[i2]], na.rm = TRUE), ".tsv", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE) # txt file
                tempo <- utils::read.table(file = tempo.file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", row.names = 1, comment.char = "", colClasses = "character", quote = "\"'", dec = ".", numerals = base::c("allow.loss", "warn.loss", "no.loss"), col.names = , as.is = , tryLogical = TRUE, na.strings = "NA", nrows = -1, skip = 0, check.names = TRUE, fill = , strip.white = FALSE, blank.lines.skip = TRUE, allowEscapes = FALSE, flush = FALSE, fileEncoding = "", encoding = "unknown", text = , skipNul = FALSE) # row.names = 1 (1st column) because now utils::read.table adds a NA in the header if the header starts by a tabulation, comment.char = "" because colors with #, colClasses = "character" otherwise convert "" (from NULL) into NA
                if(base::file.exists(base::paste0(res_path, "/plots_from_arg_test_", base::min(cluster.list[[i2]], na.rm = TRUE), base::ifelse(test = base::length(x = cluster.list[[i2]]) == 1L, yes = ".pdf", no = base::paste0("-", base::max(cluster.list[[i2]], na.rm = TRUE), ".pdf", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE))){
                    tempo.pdf <- base::paste0(res_path, "/plots_from_arg_test_", base::min(cluster.list[[i2]], na.rm = TRUE), base::ifelse(test = base::length(x = cluster.list[[i2]]) == 1L, yes = ".pdf", no = base::paste0("-", base::max(cluster.list[[i2]], na.rm = TRUE), ".pdf", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE) # pdf file
                }else{
                    tempo.pdf <- NULL
                }
                tempo.rdata <- base::paste0(res_path, "/arg_test_", base::min(cluster.list[[i2]], na.rm = TRUE), base::ifelse(test = base::length(x = cluster.list[[i2]]) == 1L, yes = ".RData", no = base::paste0("-", base::max(cluster.list[[i2]], na.rm = TRUE), ".RData", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE) # RData file
                if(i2 == 1L){
                    final.file <- tempo
                    final.pdf <- tempo.pdf
                    # new env for RData combining
                    env.name <- base::paste0("env", ini.time, collapse = NULL, recycle0 = FALSE)
                    if(base::exists(x = env.name, where = -1, envir = , frame = , mode = "any", inherits = FALSE)){ # verify if still ok when this function is inside a function
                        tempo_cat <- base::paste0(
                            error_text_start, 
                            "ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE.",
                            collapse = NULL, 
                            recycle0 = FALSE
                        )
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                        # end new env for RData combining
                    }else{
                        base::assign(x = env.name, value = base::new.env(hash = TRUE, parent = base::parent.frame(n = 1), size = 29L), pos = -1, envir = , inherits = FALSE, immediate = TRUE)
                        base::load(file = tempo.rdata, envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), verbose = FALSE)
                        tempo.rdata1 <- tempo.rdata
                        base::assign(x = "final.output", value = base::get(x = "output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE), envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1, inherits = FALSE, immediate = TRUE)
                    }
                }else{
                    final.file <- base::rbind(final.file, tempo, stringsAsFactors = TRUE, deparse.level = 1)
                    final.pdf <- base::c(final.pdf, tempo.pdf)
                    base::load(file = tempo.rdata, envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), verbose = FALSE)
                    if( ! base::identical(x = base::get(x = "final.output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)[base::c("R.version", "locale", "platform")], y = base::get(x = "output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)[base::c("R.version", "locale", "platform")], num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                        tempo_cat <- base::paste0(
                            error_text_start, 
                            "DIFFERENCE BETWEEN OUTPUTS WHILE THEY SHOULD BE IDENTICAL.\nPLEASE CHECK\n", 
                            tempo.rdata1, 
                            "\n", 
                            tempo.rdata, 
                            collapse = NULL, 
                            recycle0 = FALSE
                        )
                        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
                    }else{
                        # add the differences in RData $sysinfo into final.output
                        tempo.base1 <- base::sort(x = base::get(x = "final.output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)$sys.info$basePkgs, decreasing = FALSE)
                        tempo.base2 <- base::sort(x = base::get(x = "output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)$sys.info$basePkgs, decreasing = FALSE)
                        tempo.other1 <- base::names(x = base::get(x = "final.output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)$sys.info$otherPkgs)
                        tempo.other2 <- base::names(x = base::get(x = "output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)$sys.info$otherPkgs)
                        tempo.loaded1 <- base::names(x = base::get(x = "final.output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)$sys.info$loadedOnly)
                        tempo.loaded2 <- base::names(x = base::get(x = "output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)$sys.info$loadedOnly)
                        base::assign(
                            x = "final.output", 
                            value = {
                                x <- base::get(x = "final.output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)
                                y <- base::get(x = "output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE)
                                x$sys.info$basePkgs <- base::sort(x = base::unique(x = tempo.base1, incomparables =  tempo.base2), decreasing = FALSE)
                                if( ! base::all(tempo.other2 %in% tempo.other1, na.rm = TRUE)){
                                    x$sys.info$otherPkgs <- base::c(x$sys.info$otherPkgs, y$sys.info$otherPkgs[ ! (tempo.other2 %in% tempo.other1)])
                                    x$sys.info$otherPkgs <- x$sys.info$otherPkgs[base::order(base::names(x = x$sys.info$otherPkgs), na.last = TRUE, decreasing = FALSE, method = )]
                                }
                                if( ! base::all(tempo.loaded2 %in% tempo.loaded1, na.rm = TRUE)){
                                    x$sys.info$loadedOnly <- base::c(x$sys.info$loadedOnly, y$sys.info$loadedOnly[ ! (tempo.loaded2 %in% tempo.loaded1)])
                                    x$sys.info$loadedOnly <- x$sys.info$loadedOnly[base::order(base::names(x = x$sys.info$loadedOnly), na.last = TRUE, decreasing = FALSE, method = )]
                                }
                                x
                            }, 
                            pos = -1, 
                            envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE),
                            inherits = FALSE, 
                            immediate = TRUE
                        )
                        # add the differences in RData $sysinfo into final.output
                    }
                }
                base::file.remove(base::c(tempo.file, tempo.rdata))
            }
            # combine pdf and save
            if( ! base::is.null(x = final.pdf)){
                qpdf::pdf_combine(
                    input = final.pdf,
                    output = base::paste0(res_path, "/plots_from_arg_test_1-", total.comp.nb, ".pdf", collapse = NULL, recycle0 = FALSE),
                    password = ""
                )
                base::file.remove(final.pdf)
            }
            # end combine pdf and save
            # save RData
            base::assign(x = "output", value = base::c(base::get(x = "final.output", envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1L, mode = "any", inherits = TRUE), data = base::list(final.file)), envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1, inherits = FALSE, immediate = TRUE)
            base::save(list = output, file = base::paste0(res_path, "/arg_test_1-", total.comp.nb, ".RData", collapse = NULL, recycle0 = FALSE), envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), ascii = FALSE, version = NULL, compress = FALSE, compression_level = , eval.promises = TRUE, precheck = TRUE)
            base::rm(env.name, list = base::character(length = 0L), pos = -1, envir = , inherits = FALSE) # optional, because should disappear at the end of the function execution
            # end save RData
            # save txt
            utils::write.table(x = final.file, file = base::paste0(res_path, "/table_from_arg_test_1-", total.comp.nb, ".tsv", collapse = NULL, recycle0 = FALSE), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "")
            # end save txt
            if( ! base::is.null(x = expect_error)){
                final.file <- final.file[ ! final.file$problem == final.file$expected.error, ]
                if(base::nrow(x = final.file) == 0L){
                    base::cat(base::paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
                }else{
                    base::cat(base::paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE discrepancy_table_from_arg_test_1-", total.comp.nb, ".tsv FILE)\n\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
                    utils::write.table(x = final.file, file = base::paste0(res_path, "/discrepancy_table_from_arg_test_1-", total.comp.nb, ".tsv", collapse = NULL, recycle0 = FALSE), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "")
                }
            }
        }else{
            tempo_cat <- base::paste0("INTERNAL ERROR 2 IN ", intern_error_text_start, "WEIRD THAT base::length(cluster.list) IN NOT > 1.", intern_error_text_end, collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        # end files assembly
    }else{
        # plot management
        if(plot_fun == TRUE){
            grDevices::pdf(file = base::paste0(res_path, "/plots_from_arg_test_1", base::ifelse(test = total.comp.nb == 1L, yes = ".pdf", no = base::paste0("-", total.comp.nb, ".pdf", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = )
        }else{
            grDevices::pdf(file = NULL, width = , height = , onefile = , family = , title = , fonts = , version = , paper = , encoding = , bg = , fg = , pointsize = , pagecentre = , colormodel = , useDingbats = , useKerning = , fillOddEven = , compress = ) # send plots into a NULL file, no pdf file created
        }
        window.nb <- grDevices::dev.cur()
        base::invisible(x = grDevices::dev.set(which = window.nb))
        # end plot management
        # new environment
        env.name <- base::paste0("env", ini.time, collapse = NULL, recycle0 = FALSE)
        if(base::exists(x = env.name, where = -1, envir = , frame = , mode = "any", inherits = FALSE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                "ENVIRONMENT env.name ALREADY EXISTS.\nPLEASE RERUN ONCE.", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", base::ifelse(test = base::is.null(x = warn), yes = "", no = base::paste0("IN ADDITION\nWARNING", base::ifelse(test = warn_count > 1, yes = "S", no = ""), ":\n\n", warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            base::assign(x = env.name, value = base::new.env(hash = TRUE, parent = base::parent.frame(n = 1), size = 29L), pos = -1, envir = , inherits = FALSE, immediate = TRUE)
            base::assign(x = "val", value = val, envir = base::get(x = env.name, pos = , envir = base::sys.nframe(), mode = "any", inherits = TRUE), pos = -1, inherits = FALSE, immediate = TRUE) # var replaced by val
        }
        # end new environment
        base::suppressMessages(expr = base::suppressWarnings(expr = base::eval(expr = base::parse(text = code, file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)), classes = "warning"), classes = "message")
        base::"colnames<-"(data, arg)
        expect.data <- base::data.frame(row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
        if( ! base::is.null(x = expect_error)){
            data <- base::data.frame(data, kind = kind, problem = problem, expected.error = expected.error, message = res, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
        }else{
            data <- base::data.frame(data, kind = kind, problem = problem, message = res, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
        }
        base::row.names(x = data) <- base::paste0("arg_test_", base::sprintf(fmt = base::paste0("%0", base::nchar(x = total.comp.nb, type = "chars", allowNA = FALSE, keepNA = NA), "d", collapse = NULL, recycle0 = FALSE), 1:total.comp.nb), collapse = NULL, recycle0 = FALSE)
        sys.info <- utils::sessionInfo(package = NULL)
        sys.info$loadedOnly <- sys.info$loadedOnly[base::order(base::names(x = sys.info$loadedOnly), na.last = TRUE, decreasing = FALSE, method = )] # sort the packages
        base::invisible(x = grDevices::dev.off(which = window.nb))
        base::rm(env.name, list = base::character(length = 0L), pos = -1, envir = , inherits = FALSE) # optional, because should disappear at the end of the function execution
        if(plot_fun == TRUE & plot.count == 0L){
            warn_count <- warn_count + 1
            tempo_warn <- base::paste0("(", warn_count,") NO PDF PLOT BECAUSE ONLY ERRORS REPORTED.", collapse = NULL, recycle0 = FALSE)
            warn <- base::paste0(base::ifelse(test = base::is.null(x = warn), yes = tempo_warn, no = base::paste0(warn, "\n\n", tempo_warn, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
            base::file.remove(base::paste0(res_path, "/plots_from_arg_test_1", base::ifelse(test = total.comp.nb == 1L, yes = ".pdf", no = base::paste0("-", total.comp.nb, ".pdf", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE))
        }
        # output
        output <- base::list(fun = fun, ini = ini, data = data, sys.info = sys.info)
        if( ! base::is.null(x = expect_error)){
            expect.data <- output$data[ ! output$data$problem == output$data$expected.error, ]
            if(base::nrow(x = expect.data) == 0L){
                base::cat(base::paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
            }else{
                base::cat(base::paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE ", if(export == TRUE){base::paste0("discrepancy_table_from_arg_test_1", base::ifelse(test = total.comp.nb == 1L, yes = "", no = base::paste0("-", total.comp.nb, collapse = NULL, recycle0 = FALSE)), ".tsv FILE", collapse = NULL, recycle0 = FALSE)}else{"$data RESULT"}, ")\n\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
                if(export == TRUE){
                    expect.data <- base::as.matrix(x = expect.data)
                    expect.data <- base::gsub(x = expect.data, pattern = "\n", replacement = "  ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
                    utils::write.table(x = expect.data, file = base::paste0(res_path, "/discrepancy_table_from_arg_test_1", base::ifelse(test = total.comp.nb == 1L, yes = ".tsv", no = base::paste0("-", total.comp.nb, ".tsv", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "")
                }
            }
        }
        if(export == TRUE){
            base::save(output, list = NULL, file = base::paste0(res_path, "/arg_test_1", base::ifelse(test = total.comp.nb == 1L, yes = ".RData", no = base::paste0("-", total.comp.nb, ".RData", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), ascii = FALSE, version = NULL, envir = , compress = FALSE, compression_level = , eval.promises = TRUE, precheck = TRUE)
            table.out <- base::as.matrix(x = output$data)
            table.out <- base::gsub(x = table.out, pattern = "\n", replacement = "  ", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
            utils::write.table(x = table.out, file = base::paste0(res_path, "/table_from_arg_test_1", base::ifelse(test = total.comp.nb == 1L, yes = ".tsv", no = base::paste0("-", total.comp.nb, ".tsv", collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE), row.names = TRUE, col.names = NA, append = FALSE, quote = FALSE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = base::c("escape", "double"), fileEncoding = "")
        }
        # end output
    }
    end.date <- base::Sys.time()
    end.time <- base::as.numeric(x = end.date)
    total.lapse <- base::round(x = lubridate::seconds_to_period(x = end.time - ini.time), digits = 0)
    base::cat(base::paste0("test JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", total.lapse, "\n\n\n", collapse = NULL, recycle0 = FALSE), file = , sep =  , fill = FALSE, labels = NULL, append = FALSE)
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
            ), add = TRUE, after = TRUE
        )
    }
    base::on.exit(expr = base::options(warning.length = ini_warning_length), add = TRUE, after = TRUE)
    #### end warning output

    #### output
    if(parall == FALSE & export == FALSE){
        base::return(output)
    }
    #### end output

}

