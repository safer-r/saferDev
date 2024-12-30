#' @title arg_check
#' @description
#' Check expected values of an argument of functions: class, type, mode, length, restricted values panel, kind of numeric values in addition to the distinction between 'integer' and 'double' (proportion only? Inf values authorized? negative values authorized? Integers of type 'double'?)
#' @param data Object to test.
#' @param class Single character string. Either one of the class() result or "vector" or "ggplot2" (i.e., objects of class c("gg", "ggplot")) or NULL. See the warning section below.
#' @param typeof Single character string. Either one of the typeof() result or NULL.
#' @param mode Single character string. Either one of the mode() result (for non-vector object) or NULL.
#' @param length Single numeric value indicating the length of the object. Not considered if NULL.
#' @param prop Single logical value. Are the numeric values between 0 and 1 (proportion)? If TRUE, can be used alone, without considering class, etc.
#' @param double_as_integer_allowed Single logical value. If TRUE, no error is reported in the cheking message if argument is set to typeof == "integer" or class == "integer", while the reality is typeof == "double" or class == "numeric" but the numbers strictly have zero as modulo (remainder of a division). This means that i <- 1, which is typeof(i) -> "double" is considered as integer with double_as_integer_allowed = TRUE. WARNING: data mod 1 == 0L but not isTRUE(all.equal(data mod 1, 0)) is used here because the argument checks for integers stored as double (does not check for decimal numbers that are approximate integers).
#' @param options Vector of character strings or integers indicating all the possible option values for the data argument, or NULL. Numbers of type "double" are accepted if they have a 0 modulo.
#' @param all_options_in_data Single logical value. If TRUE, all of the options must be present at least once in the data argument, and nothing else. If FALSE, some or all of the options must be present in the data argument, and nothing else. Ignored if options base::is.null.
#' @param na_contain Single logical value. Can the data argument contain NA?
#' @param neg_values Single logical value. Are negative numeric values authorized? Warning: the default setting is TRUE, meaning that, in that case, no check is performed for the presence of negative values. The neg_values argument is activated only when set to FALSE. In addition, (1) neg_values = FALSE can only be used when class, typeof or mode arguments are not NULL, otherwise return an error message, (2) the presence of negative values is not checked with neg_values = FALSE if the tested object is a factor and the following checking message is returned "OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS A FACTOR".
#' @param inf_values Single logical value. Are infinite numeric values authorized (Inf or -Inf)? Identical remarks as for the neg_values argument.
#' @param print Single logical value. Print the message if $problem is TRUE? Warning: set by default to FALSE, which facilitates the control of the checking message output when using arg_check() inside functions. See the example section.
#' @param data_name Single character string indicating the name of the object to test. If NULL, use what is assigned to the data argument for the returned message.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful to overcome R execution using system with non admin rights for R package installation in the default directories. Ignored if NULL (default): only the pathways specified by .libPaths() are used for package calling. Specify the right path if the function returns a package path error.
#' @param safer_check Single logical value. Perform some "safer" checks? If TRUE, checkings are performed before main code running (see https://github.com/safer-r): 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: error_text = "INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>".
#' @returns 
#' A list containing:
#' 
#'- §problem: logical. Is there any problem detected?
#' 
#'- §text: message indicating the details of the problem, or the absence of problem.
#' 
#' - §object.name: value of the data_name argument (i.e., name of the checked object if provided, NULL otherwise).
#' @details
#' - If options == NULL, then at least class or type or mode or length argument must be non-null.
#'  
#' - If options is non-null, then class, type and mode must be NULL, and length can be NULL or specified.
#'  
#' - The function tests what is written in its arguments, even if what is written is incoherent. For instance, arg_check(data = factor(1), class = "factor", mode = "character") will return a problem, whatever the object tested in the data argument, because no object can be class "factor" and mode "character" (factors are class "factor" and mode "numeric"). Of note, length of object of class "environment" is always 0.
#'  
#' - If the tested object base::is.null, then the function will always return a checking problem.
#'  
#' - Argument "class" with value "vector" means that the object is tested for class(data) returning only "numeric", "integer", "character", "logical", "complex" or "expression". Please, use another value of class (e.g., class = "call" or class = "list") for other types and class of objects
#'  
#' - Since R >= 4.0.0, class(matrix()) returns "matrix" "array", and not "matrix" alone as before. However, use argument class = "matrix" to check for matrix object (of class "matrix" "array" in R >= 4.0.0) and use argument class = "array" to check for array object (of class "array" in R >= 4.0.0).
#' @seealso \code{\link{match.arg}} et \code{\link{arg_test}}.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' test <- matrix(1:3)
#' \dontrun{ # Example that return an error
#' arg_check(data = test, print = TRUE, class = "vector", mode = "numeric")  # commented because this example returns an error
#' }
#' arg_check(data = test, print = TRUE, class = "matrix", mode = "numeric")
#' arg_check(data = test, print = TRUE, class = "matrix", mode = "numeric", error_text = "BY saferDev::arg_check()")
#' @export
arg_check <- function(
    data, 
    class = NULL, 
    typeof = NULL, 
    mode = NULL, 
    length = NULL, 
    prop = FALSE, 
    double_as_integer_allowed = FALSE, 
    options = NULL, 
    all_options_in_data = FALSE, 
    na_contain = FALSE, 
    neg_values = TRUE, 
    inf_values = TRUE, 
    print = FALSE, 
    data_name = NULL, 
    lib_path = NULL, # required because of saferDev:::.base_op_check()
    safer_check = TRUE,
    error_text = ""
){
    # DEBUGGING
    # data = mean ; class = NULL ; typeof = NULL ; mode = NULL ; length = NULL ; prop = FALSE ; double_as_integer_allowed = FALSE ; options = "a" ; all_options_in_data = FALSE ; na_contain = FALSE ; neg_values = TRUE ; inf_values = TRUE ; print = TRUE ; data_name = NULL ; lib_path = NULL ; safer_check = TRUE ; error_text = ""

    #### package name
    package_name <- "saferDev" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

     #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user)
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation
    # basic error text start
    error_text_start <- base::paste0(
        "ERROR IN ", 
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name, 
        collapse = NULL, 
        recycle0 = FALSE
    )
    # end basic error text start
    # check of the error_text argument
    if( ! (base::all(base::typeof(x = error_text) == "character", na.rm = TRUE) & base::length(x = error_text) == 1)){ # no need to test is.null(error_text) because typeof(x = NULL) == "character" returns FALSE
        tempo_cat <- base::paste0(
            error_text_start, 
            "\nTHE error_text ARGUMENT MUST BE A SINGLE CHARACTER STRING (CAN BE \"\").\nHERE IT IS:\n", 
            base::paste0(error_text, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    # end check of the error_text argument
    # basic error text start updated
    error_text_start <- base::paste0(
        error_text_start, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    # end basic error text start updated
    # internal error text
    intern_error_text_start <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    # end internal error text
    #### end error_text initiation

    #### environment checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if( ! base::is.null(x = lib_path)){
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = FALSE)){ # no na.rm = TRUE with typeof
            tempo_cat <- base::paste0(
                error_text_start, 
                "DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                base::paste0(lib_path, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA
            tempo_cat <- base::paste0(
                error_text_start, 
                "DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\n", 
                base::paste0(lib_path, collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }else{
            base:::.libPaths(new = base::sub(x = lib_path, pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base:::.libPaths(new = ) add path to default path. BEWARE: base:::.libPaths() does not support / at the end of a submitted path. Thus check and replace last / or \\ in path
            lib_path <- base:::.libPaths(new = , include.site = TRUE)
        }
    }else{
        lib_path <- base:::.libPaths(new = , include.site = TRUE) # base:::.libPaths(new = lib_path) # or base:::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    ######## end check of lib_path

    ######## safer_check argument checking
    if( ! (base::all(safer_check %in% base::c(TRUE, FALSE), na.rm = FALSE) & base::length(x = safer_check) == 1 & base::all(base::is.logical(x = safer_check), na.rm = TRUE))){
        tempo_cat <- base::paste0(
            error_text_start, 
            "safer_check ARGUMENT MUST BE EITHER TRUE OR FALSE.\nHER IT IS:\n", 
            base::paste0(safer_check, collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end safer_check argument checking

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
            fun = base::c(
                "saferDev:::.base_op_check"
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), 
            internal_error_report_link = internal_error_report_link
        )
    }
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "data"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = FALSE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values
    
    ######## argument checking with arg_check()
    ######## end argument checking with arg_check()

    ######## management of NA arguments
    if(base::length(x = arg_user_setting) != 0){
        tempo_log <- base::suppressWarnings(expr = base::sapply(X = base::lapply(X = arg_user_setting, FUN = function(x){base::is.na(x = x)}), FUN = function(x){base::any(x = x, na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE), classes = "warning") & base::lapply(X = arg_user_setting, FUN = function(x){base::length(x = x)}) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA because base::is.na() used here
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT JUST BE NA:", 
                base::paste0(arg_names[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    ######## management of NULL arguments
    tempo_arg <- base::c(
        "prop", 
        "double_as_integer_allowed", 
        "all_options_in_data", 
        "na_contain",
        "neg_values",
        "inf_values",
        "print",
        # "lib_path", # because can be NULL
        "safer_check",
        "error_text"
    )
    tempo_log <- base::sapply( X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            "\nCANNOT BE NULL", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## management of "" in arguments of mode character
    tempo_arg <-base::c(
        "class",
        "typeof",
        "mode",
        "data_name",
        "lib_path"
        # "error_text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  # for character argument that can also be NULL, if NULL -> considered as character
    if(base::any(tempo_log, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT MODE \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
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
    
    #### end argument primary checking
    
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

    # management of special classes
    basic.class <- base::c(
        "NULL", # because base::class(NULL) is "NULL". The NULL aspect will be dealt later
        "logical", 
        "integer", 
        "numeric", 
        # "complex", 
        "character"
        # "matrix", 
        # "array", 
        # "data.frame", 
        # "list", 
        # "factor", 
        # "table", 
        # "expression", 
        # "name", 
        # "symbol", 
        # "function", 
        # "uneval", 
        # "environment", 
        # "ggplot2", 
        # "ggplot_built", 
        # "call"
    )
    tempo.arg.base <-base::c( # no base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) used with arg_check() to be sure to deal with the correct environment
        "class", 
        "typeof", 
        "mode", 
        "length", 
        "prop", 
        "double_as_integer_allowed", 
        "options", 
        "all_options_in_data", 
        "na_contain", 
        "neg_values", 
        "inf_values", 
        "print", 
        "data_name",
        "safer_check",
        "error_text"
    )
    tempo.class <-base::list( # no base::get() used to be sure to deal with the correct environment
        base::class(class), 
        base::class(typeof), 
        base::class(mode), 
        base::class(length), 
        base::class(prop), 
        base::class(double_as_integer_allowed), 
        base::class(options), 
        base::class(all_options_in_data), 
        base::class(na_contain), 
        base::class(neg_values), 
        base::class(inf_values), 
        base::class(print), 
        base::class(data_name),
        base::class(safer_check),
        base::class(error_text)
    )
    tempo <- ! base::sapply(base::lapply(tempo.class, FUN = "%in%", basic.class), FUN = base::all)
    if(base::any(tempo)){
        tempo.cat1 <- tempo.arg.base[tempo]
        tempo.cat2 <- base::sapply(tempo.class[tempo], FUN = base::paste0, collapse = " ")
        tempo.sep <- base::sapply(base::mapply(" ", base::max(base::nchar(tempo.cat1)) - base::nchar(tempo.cat1) + 3, FUN = base::rep, SIMPLIFY = FALSE), FUN = base::paste0, collapse = "")
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nANY ARGUMENT EXCEPT data MUST HAVE A BASIC CLASS.\nPROBLEMATIC ARGUMENT", 
            base::ifelse(base::length(tempo.cat1) > 1, "S", ""), 
            " AND ASSOCIATED CLASS", 
            base::ifelse(base::length(tempo.cat1) > 1, "ES ARE", " IS"), 
            ":\n", 
            base::paste0(tempo.cat1, tempo.sep, tempo.cat2, collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of special classes
    
    if( ! base::is.null(data_name)){
        if( ! (base::length(data_name) == 1L & base::all(base::class(data_name) == "character"))){ # base::all() without na.rm -> ok because base::class(NA) is "logical"
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\ndata_name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT ", 
                base::paste(data_name, collapse = " ")
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if(base::is.null(options) & base::is.null(class) & base::is.null(typeof) & base::is.null(mode) &  prop == FALSE & base::is.null(length)){
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nAT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop)."
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::is.null(options) & ( ! base::is.null(class) | ! base::is.null(typeof) | ! base::is.null(mode) | prop == TRUE)){
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nTHE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED."
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(neg_values) == "logical") & base::length(neg_values) == 1L)){ # base::all() without na.rm -> ok because base::class(NA) is "logical" 
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nTHE neg_values ARGUMENT MUST BE TRUE OR FALSE ONLY"
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(neg_values == FALSE & base::is.null(class) & base::is.null(typeof) & base::is.null(mode)){
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nTHE neg_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL"
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(inf_values) == "logical") & base::length(inf_values) == 1L)){ # base::all() without na.rm -> ok because base::class(NA) is "logical" 
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nTHE inf_values ARGUMENT MUST BE TRUE OR FALSE ONLY"
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(inf_values == FALSE & base::is.null(class) & base::is.null(typeof) & base::is.null(mode)){
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nTHE inf_values ARGUMENT CANNOT BE SWITCHED FROM TRUE (DEFAULT VALUE) TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL"
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::is.null(class)){ # may add "formula" and "Date" as in https://renenyffenegger.ch/notes/development/languages/R/functions/class
        if( ! base::all(class %in% base::c("vector", "logical", "integer", "numeric", "complex", "character", "matrix", "array", "data.frame", "list", "factor", "table", "expression", "name", "symbol", "function", "uneval", "environment", "ggplot2", "ggplot_built", "call") & base::length(class) == 1L)){ # length == 1L here because of base::class(base::matrix()) since R4.0.0  # base::all() without na.rm -> ok because class cannot be NA (tested above)
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\nclass ARGUMENT MUST BE ONE OF THESE VALUE:\n\"vector\"\n\"logical\"\n\"integer\"\n\"numeric\"\n\"complex\"\n\"character\"\n\"matrix\"\n\"array\"\n\"data.frame\"\n\"list\"\n\"factor\"\n\"table\"\n\"expression\"\n\"name\"\n\"symbol\"\n\"function\"\n\"environment\"\n\"ggplot2\"\n\"ggplot_built\"\n\"call\""
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg_values == FALSE & ! base::any(class %in% base::c("vector", "numeric", "integer", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\nclass ARGUMENT CANNOT BE OTHER THAN\n\"vector\"\n\"numeric\"\n\"integer\"\n\"matrix\"\n\"array\"\n\"data.frame\"\n\"table\"\nIF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE"
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf_values == FALSE & ! base::any(class %in% base::c("vector", "numeric", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\nclass ARGUMENT CANNOT BE OTHER THAN\n\"vector\"\n\"numeric\"\n\"matrix\"\n\"array\"\n\"data.frame\"\n\"table\"\nIF ABSENCE OF INFINITE VALUE IS CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE\n\"integer\" IS NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE NOT INTEGERS"
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(typeof)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (base::all(typeof %in% base::c("logical", "integer", "double", "complex", "character", "list", "expression", "symbol", "closure", "special", "builtin", "environment", "S4", "language")) & base::length(typeof) == 1L)){ # "language" is the type of object of class "call" # base::all() without na.rm -> ok because typeof cannot be NA (tested above)
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\ntypeof ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\"\n\"integer\"\n\"double\"\n\"complex\"\n\"character\"\n\"list\"\n\"expression\"\n\"name\"\n\"symbol\"\n\"closure\"\n\"special\"\n\"builtin\"\n\"environment\"\n\"S4\"\n\"language\""
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg_values == FALSE & ! typeof %in% base::c("double", "integer")){
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\ntypeof ARGUMENT CANNOT BE OTHER THAN \"double\" OR \"integer\" IF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE"
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf_values == FALSE & typeof != "double"){
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\ntypeof ARGUMENT CANNOT BE OTHER THAN \"double\" IF ABSENCE OF INFINITE VALUE ARE CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE\n\"integer\" IS NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE NOT INTEGERS"
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(mode)){ # all the types are here: https://renenyffenegger.ch/notes/development/languages/R/functions/typeof
        if( ! (base::all(mode %in% base::c("logical", "numeric", "complex", "character", "list", "expression", "name", "symbol", "function", "environment", "S4", "call")) & base::length(mode) == 1L)){ # base::all() without na.rm -> ok because mode cannot be NA (tested above)
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\nmode ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\"\n\"numeric\"\n\"complex\"\n\"character\"\n\"list\"\n\"expression\"\n\"name\"\n\"symbol\"\n\"function\"\n\"environment\"\n\"S4\"\n\"call\""
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(neg_values == FALSE & mode != "numeric"){
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\nmode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF ABSENCE OF NEGATIVE VALUES IS CONTROLED BY SWITCHING THE neg_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE"
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
        if(inf_values == FALSE & mode != "numeric"){
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\nmode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF ABSENCE OF INFINITE VALUE ARE CONTROLED BY SWITCHING THE inf_values ARGUMENT FROM TRUE (DEFAULT VALUE) TO FALSE\nOTHER VALUES NOT ALLOWED BECAUSE OBJECTS WITH INFINITE VALUES ARE ONLY \"numeric\""
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! base::is.null(length)){
        if( ! (base::is.numeric(length) & base::length(length) == 1L & base::all( ! base::grepl(length, pattern = "\\.")))){ # base::is.na() already arg_checked for length
            tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\nlength ARGUMENT MUST BE A SINGLE INTEGER VALUE"
            )
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    if( ! (base::is.logical(prop) & base::length(prop) == 1L)){ # base::is.na() already checked for prop
        tempo.cat <- base::paste0(
                "ERROR IN ", 
                function_name, 
                base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                "\nprop ARGUMENT MUST BE TRUE OR FALSE ONLY"
            )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else if(prop == TRUE){
        if( ! base::is.null(class)){
            if( ! base::any(class %in% base::c("vector", "numeric", "matrix", "array", "data.frame", "table"))){ # no need of na.rm = TRUE for base::any() because %in% does not output NA
                tempo.cat <- base::paste0(
                    "ERROR IN ", 
                    function_name, 
                    base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                    base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                    "\nclass ARGUMENT CANNOT BE OTHER THAN\nNULL\n\"vector\"\n\"numeric\"\n\"matrix\"\n\"array\"\n\"data.frame\"\n\"table\"\nIF prop ARGUMENT IS TRUE."
                ) # not integer because prop
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
        if( ! base::is.null(mode)){
            if(mode != "numeric"){
                tempo.cat <- base::paste0(
                    "ERROR IN ", 
                    function_name, 
                    base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                    base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                    "\nmode ARGUMENT CANNOT BE OTHER THAN NULL OR \"numeric\" IF prop ARGUMENT IS TRUE."
                )
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
        if( ! base::is.null(typeof)){
            if(typeof != "double"){
                tempo.cat <- base::paste0(
                    "ERROR IN ", 
                    function_name, 
                    base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
                    base::ifelse(test = error_text == "", yes = ".", no = error_text), 
                    "\ntypeof ARGUMENT CANNOT BE OTHER THAN NULL OR \"double\" IF prop ARGUMENT IS TRUE"
                )
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
            }
        }
    }
    if( ! (base::all(base::class(double_as_integer_allowed) == "logical") & base::length(double_as_integer_allowed) == 1L)){ # base::all() without na.rm -> ok because base::class() never returns NA
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nTHE double_as_integer_allowed ARGUMENT MUST BE TRUE OR FALSE ONLY: ", 
            base::paste(double_as_integer_allowed, collapse = " ")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::is.logical(all_options_in_data) & base::length(all_options_in_data) == 1L)){
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nall_options_in_data ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY): ", 
            base::paste(all_options_in_data, collapse = " ")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(na_contain) == "logical") & base::length(na_contain) == 1L)){ # base::all() without na.rm -> ok because base::class() never returns NA
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nTHE na_contain ARGUMENT MUST BE TRUE OR FALSE ONLY: ", 
            base::paste(na_contain, collapse = " ")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::all(base::class(print) == "logical") & base::length(print) == 1L)){ # base::all() without na.rm -> ok because base::class() never returns NA
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            function_name, 
            base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), 
            base::ifelse(test = error_text == "", yes = ".", no = error_text), 
            "\nTHE print ARGUMENT MUST BE TRUE OR FALSE ONLY: ", 
            base::paste(print, collapse = " ")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # data_name and error_text tested at the beginning
    # end management of special classes
    ######## end other checkings
    
    #### end second round of checking and data preparation

    #### main code
    if(base::is.null(data_name)){
        data_name <- base::deparse(base::substitute(data))
    }
    problem <- FALSE
    text <- base::paste0(
        "NO PROBLEM DETECTED FOR THE ", 
        data_name, 
        " OBJECT",
        base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))
    )
    if(( ! base::is.null(options)) & (base::all(base::typeof(data) == "character") | base::all(base::typeof(data) == "integer") | base::all(base::typeof(data) == "double"))){ # base::all() without na.rm -> ok because base::typeof() never returns NA
        test.log <- TRUE
        if(base::all(base::typeof(data) == "double")){
            if( ! base::all(data %% 1 == 0L, na.rm = TRUE)){
                problem <- TRUE
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\nTHE ", data_name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
                test.log <- FALSE
            }
        }
        if(test.log == TRUE){
            text <- ""
            if( ! base::all(data %in% options)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                problem <- TRUE
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\nTHE ", data_name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nTHE PROBLEMATIC ELEMENTS OF ", data_name, " ARE: ", base::paste(base::unique(data[ ! (data %in% options)]), collapse = " "))
            }
            if(all_options_in_data == TRUE){
                if( ! base::all(options %in% data)){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    problem <- TRUE
                    text <- base::paste0(
                        base::ifelse(text == "", "", base::paste0(text, "\n")), 
                        base::ifelse(test = error_text == "", yes = "ERROR", no = base::paste0("ERROR IN ", error_text)), 
                        "\nTHE ", 
                        data_name, 
                        " OBJECT MUST BE MADE OF ALL THESE OPTIONS: ", 
                        base::paste(options, collapse = " "), 
                        "\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: ",  
                        base::paste(base::unique(options[ ! (options %in% data)]), collapse = " ")
                    )
                }
            }
            if( ! base::is.null(length)){
                if(base::length(data) != length){
                    problem <- TRUE
                    text <- base::paste0(
                        base::ifelse(text == "", "", base::paste0(text, "\n")), 
                        base::ifelse(test = error_text == "", yes = "ERROR", no = base::paste0("ERROR IN ", error_text)), 
                        "\nTHE LENGTH OF ", 
                        data_name, 
                        " MUST BE ", 
                        length, 
                        " AND NOT ", 
                        base::length(data)
                    )
                }
            }
            if(text == ""){
                text <- base::paste0(
                    "NO PROBLEM DETECTED FOR THE ", 
                    data_name, 
                    " OBJECT",
                    base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))
                )
            }
        }
    }else if( ! base::is.null(options)){
        problem <- TRUE
        text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\nTHE ", data_name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", base::paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
    }
    arg.names <- base::c("class", "typeof", "mode", "length")
    if( ! base::is.null(class)){
        if(class == "matrix"){ # because of base::class(base::matrix()) since R4.0.0
            class <- base::c("matrix", "array")
        }else if(class == "factor" & base::all(base::class(data) %in% base::c("factor", "ordered"))){ # to deal with ordered factors # base::all() without na.rm -> ok because base::class(NA) is "logical"
            class <- base::c("factor", "ordered")
        }
    }
    if(base::is.null(options)){
        for(i2 in 1:base::length(arg.names)){
            if( ! base::is.null(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){
                # script to execute
                tempo.script <- '
                    problem <- TRUE ;
                    if(base::identical(text, base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT", base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))))){
                        text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\nTHE ", data_name, " OBJECT MUST BE ") ;
                    }else{
                        text <- base::paste0(text, " AND ") ; 
                    }
                    text <- base::paste0(text, base::toupper(arg.names[i2]), " ", if(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("matrix", "array"))){"matrix"}else if(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("factor", "ordered"))){"factor"}else{base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE)})
                ' # no need of na.rm = TRUE for base::all() because %in% does not output NA
                # end script to execute
                if(base::typeof(data) == "double" & double_as_integer_allowed == TRUE & ((arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")) | (arg.names[i2] == "typeof" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "integer")))){ # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names # base::typeof(data) == "double" means no factor allowed
                    if( ! base::all(data %% 1 == 0L, na.rm = TRUE)){ # to check integers (use %%, meaning the remaining of a division): see the precedent line. base::isTRUE(base::all.equal(data%%1, base::rep(0, base::length(data)))) not used because we strictly need zero as a result. Warning: na.rm = TRUE required here for base::all()
                        base::eval(base::parse(text = tempo.script)) # execute tempo.script
                    }
                }else if( ! base::any(base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) %in% base::c("vector", "ggplot2"))) & ! base::all(base::eval(base::parse(text = base::paste0(arg.names[i2], "(data)"))) %in% base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE))){ # test the four base::c("class", "typeof", "mode", "length") arguments with their corresponding function. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::any() because get base::get(arg.names) does not contain NA
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "vector") & ! (base::all(base::class(data) %in% "numeric") | base::all(base::class(data) %in% "integer") | base::all(base::class(data) %in% "character") | base::all(base::class(data) %in% "logical") | base::all(base::class(data) %in% "complex") | base::all(base::class(data) %in% "expression"))){ # test class == "vector". base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) check is user as used the argument class = "vector". If TRUE and base::length(data) > 1, the class "numeric" "integer" "character" "logical" "complex" "expression" should be returned. No need of na.rm = TRUE for base::all() because %in% does not output NA # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names. Other classes "list", "name", "symbol", "function", "environment", "S4", "call" return a list if length of data > 1
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }else if(arg.names[i2] == "class" & base::all(base::get(arg.names[i2], envir = base::sys.nframe(), inherits = FALSE) == "ggplot2") & ! base::all(base::class(data) %in% base::c("gg", "ggplot"))){ # test ggplot object # no need of na.rm = TRUE for base::all() because == does not output NA if no NA in left of ==, which is the case for arg.names # no need of na.rm = TRUE for base::all() because %in% does not output NA
                    base::eval(base::parse(text = tempo.script)) # execute tempo.script
                }
            }
        }
    }
    if(prop == TRUE & base::all(base::typeof(data) == "double")){ # base::all() without na.rm -> ok because base::typeof(NA) is "logical"
        if(base::is.null(data) | base::any(data < 0 | data > 1, na.rm = TRUE)){ # works if data base::is.null # Warning: na.rm = TRUE required here for base::any() # base::typeof(data) == "double" means no factor allowed
            problem <- TRUE
            if(base::identical(text, base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT", base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))))){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\n")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
        }
    }else if(prop == TRUE){
        problem <- TRUE
        if(base::identical(text, base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT", base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))))){
            text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\n")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
    }
    if(base::all(base::class(data) %in% "expression")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        data <- base::as.character(data) # to evaluate the presence of NA
    }
    if(na_contain == FALSE & (base::mode(data) %in% base::c("logical", "numeric", "complex", "character", "list"))){ # before it was ! (base::class(data) %in% base::c("function", "environment"))
        if(base::any(base::is.na(data)) == TRUE){ # not on the same line because when data is class envir or function , do not like that # normally no NA with base::is.na()
            problem <- TRUE
            if(base::identical(text, base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT", base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))))){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\n")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data_name, " OBJECT CONTAINS NA WHILE NOT AUTHORIZED")
        }
    }
    if(neg_values == FALSE & base::all(base::mode(data) %in% "numeric") & ! base::any(base::class(data) %in% "factor")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        if(base::any(data < 0, na.rm = TRUE)){ # Warning: na.rm = TRUE required here for base::any()
            problem <- TRUE
            if(base::identical(text, base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT", base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))))){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\n")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES")
        }
    }else if(neg_values == FALSE){
        problem <- TRUE
        if(base::identical(text, base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT", base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))))){
            text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\n")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS ", base::ifelse(base::any(base::class(data) %in% "factor"), "A FACTOR", "NOT EVEN MODE NUMERIC")) # no need of na.rm = TRUE
    }
    if(inf_values == FALSE & base::all(base::typeof(data) %in% "double") & ! base::any(base::class(data) %in% "factor")){ # no need of na.rm = TRUE for base::all() because %in% does not output NA
        if(base::any(base::is.infinite(data), na.rm = TRUE)){ # Warning: na.rm = TRUE required here for base::any()
            problem <- TRUE
            if(base::identical(text, base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT", base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))))){
                text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\n")
            }else{
                text <- base::paste0(text, " AND ")
            }
            text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE MADE OF NON INFINITE NUMERIC VALUES")
        }
    }else if(inf_values == FALSE){
        problem <- TRUE
        if(base::identical(text, base::paste0("NO PROBLEM DETECTED FOR THE ", data_name, " OBJECT", base::ifelse(test = error_text == "", yes = ".", no = base::paste0(" ", error_text, collapse = NULL, recycle0 = FALSE))))){
            text <- base::paste0("ERROR", base::ifelse(test = error_text == "", yes = "", no = base::paste0(" ", error_text)), "\n")
        }else{
            text <- base::paste0(text, " AND ")
        }
        text <- base::paste0(text, "THE ", data_name, " OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS ", base::ifelse(base::any(base::class(data) %in% "factor"), "A FACTOR", "NOT EVEN TYPE DOUBLE")) # no need of na.rm = TRUE
    }
    if(print == TRUE & problem == TRUE){
        base::cat(base::paste0("\n\n================\n\n", text, "\n\n================\n\n"))
    }
    #### end main code
    
    #### output
    output <- base::list(problem = problem, text = text, object.name = data_name)
    base::return(output)
    # end output

    #### warning output
    #### end warning output
}
