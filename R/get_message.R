#' @title get_message
#' @description
#' Evaluate an instruction written between "" and return the first of the error, or warning or standard (non error non warning) messages if ever exist.
#' 
#' Using argument print.no = FALSE, return NULL if no message, which is convenient in some cases.
#' @param data Single character string to evaluate.
#' @param kind Single character string. Either "error" to get error messages, or "warning" to get warning messages, or "message" to get non error and non warning messages.
#' @param header Single logical value. Add a header in the returned message?
#' @param print.no Single logical value. Print a message saying that no message reported?
#' @param text Single character string added to the output message (even if no message exists and print.no is TRUE). Inactivated if the header argument is FALSE. Write NULL if not required.
#' @param env The name of an existing environment. Write NULL if not required.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns The message or NULL if no message and print.no is FALSE.
#' @details 
#' WARNINGS
#' 
#' Only the first standard/error/warning message is returned.
#' 
#' Always use the env argument when get_message() is used inside functions.
#' 
#' The function does not prevent printing if print() is used inside the instruction tested. To prevent that, use tempo <- utils::capture.output(error <- get_message(data = "arg_check(data = 'a', class = mean, neg_values = FALSE, print = TRUE)")). The return of get_message() is assigned into error and the printed messages are captured by utils::capture.output() and assigned into tempo. See the examples.
#' @seealso \code{\link{try}}.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "warning", 
#' print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "message", 
#' print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "wilcox.test()", kind = "message", print.no = TRUE, text = "IN A")

#' get_message(data = "wilcox.test()", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "sum(1)", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "message('ahah')", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "message('ahah')", kind = "message", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "ggplot2::ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), 
#' mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()", kind = "message", print.no = TRUE, 
#' text = "IN INSTRUCTION 1")
#' 
#' set.seed(1) ; 
#' obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), 
#' Group1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; 
#' get_message(data = 'gg_boxplot(data = obs1, y = "Time", categ = "Group1")', 
#' kind = "message", print.no = TRUE, text = "IN INSTRUCTION 1")
#' 
#' @importFrom ggplot2 ggplot_build
#' @importFrom saferDev arg_check
#' @export
get_message <- function(
        data, 
        kind = "error", 
        header = TRUE, 
        print.no = FALSE, 
        text = NULL, 
        env = NULL,
        safer_check = TRUE
){  
    # DEBUGGING
    # data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE # for function debugging
    # data = "sum(1)" ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL; safer_check = TRUE  # for function debugging
    # set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; data = 'gg_boxplot(data1 = obs1, y = "Time", categ = "Group1")' ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE  # for function debugging
    # data = "message('ahah')" ; kind = "error" ; header = TRUE ; print.no = TRUE ; text = "IN A" ; env = NULL ; safer_check = TRUE 
    # data = 'ggplot2::ggplot(data = data.frame(X = "a", stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()' ; kind = "message" ; header = TRUE ; print.no = FALSE ; text = NULL ; safer_check = TRUE # for function debugging
    # data = 'ggplot2::ggplot(data = data.frame(X = "a", stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()' ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; safer_check = TRUE # for function debugging
    # data = "emmeans::emmeans(object = emm.rg, specs = contrast.var)" ; kind = "message" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL ; safer_check = TRUE# for function debugging

    #### package name
    package_name <- "saferDev"# write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in a internal error message. Write NULL if no link to propose or no internal error message
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

    #### critical operator checking
    if( ! (base::all(safer_check %in% base::c(TRUE, FALSE), na.rm = FALSE) & base::length(x = safer_check) == 1 & base::all(base::is.logical(x = safer_check), na.rm = TRUE))){
        tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nsafer_check ARGUMENT MUST BE EITHER TRUE OR FALSE. HER IT IS:\n", base::paste0(safer_check, collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
            external_function_name = function_name, 
            external_package_name = package_name
        )
    }
    #### end critical operator checking

    #### package checking

    ######## check of lib_path
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        saferDev:::.pack_and_function_check(
            fun = base::c(
                "saferDev::arg_check",
                "ggplot2::ggplot_build"
            ),
            lib_path = NULL, # NULL if the function does not have any lib_path argument
            external_function_name = function_name,
            external_package_name = package_name
        )
    }
    ######## end check of the required functions from the required packages

    #### end package checking

    #### argument primary checking

    ######## arg with no default values
    mandat_args <- base::c(
        "data"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = FALSE)){
        tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\nFOLLOWING ARGUMENT", base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat_args[tempo], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end arg with no default values

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = data, class = "vector", typeof = "character", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = kind, class = NULL, typeof = NULL, mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = base::c("error", "warning", "message"), all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = header, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = print.no, class = "vector", typeof = "logical", mode = NULL, length = 1, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = text)){
        tempo <- saferDev::arg_check(data = text, class = "character", typeof = NULL, mode = "character", length = 1, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    if( ! base::is.null(x = env)){
        tempo <- saferDev::arg_check(data = env, class = "environment", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = TRUE, options = NULL, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, fun_name = function_name, pack_name = package_name, safer_check = FALSE) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL)) #
    }
    # lib_path already checked above
    # safer_check already checked above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text_check[argum_check], sep = " ", collapse = "\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of NA arguments
    if(base::length(x = arg_user_setting) != 0){
        tempo_log <- base::suppressWarnings(expr = base::sapply(X = base::lapply(X = arg_user_setting, FUN = function(x){base::is.na(x = x)}), FUN = function(x){base::any(x = x, na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE), classes = "warning") & base::lapply(X = arg_user_setting, FUN = function(x){base::length(x = x)}) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo_log, na.rm = FALSE)){ # normally no NA because base::is.na() used here
            tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(arg_names[tempo_log], collapse = "\n", recycle0 = FALSE), collapse = NULL, recycle0 = FALSE)
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
        }
    }
    ######## end management of NA arguments

    ######## management of NULL arguments
    tempo_arg <- base::c(
        "data", 
        "kind", 
        "header", 
        "print.no",
        # "text",  # inactivated because can be null
        # "env",  # inactivated because can be null
        "safer_check"
    )
    tempo_log <- base::sapply( X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = FALSE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),"\nCANNOT BE NULL", collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }
    ######## end management of NULL arguments

    ######## management of "" in arguments of mode character
    tempo_arg <-base::c(
        "data", 
        "kind"
        # "text" # inactivated because can be ""
    )
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  # for character argument that can also be NULL, if NULL -> considered as character
    if(base::any(tempo_log, na.rm = TRUE)){
        tempo_cat <- base::paste0("INTERNAL ERROR IN THE BACKBONE PART OF THE ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), " NOT MODE \"character\":\n", base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, collapse = NULL, recycle0 = FALSE)), collapse = NULL, recycle0 = FALSE)
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in base::stop() to be able to add several messages between ==
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = FALSE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0("ERROR IN ", function_name, base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(" OF THE ", package_name, " PACKAGE", collapse = NULL, recycle0 = FALSE)), "\n", base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),"\nCANNOT CONTAIN \"\"", collapse = NULL, recycle0 = FALSE)
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

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation
    
    #### main code
    grDevices::pdf(file = NULL) # send plots into a NULL file, no pdf file created
    window.nb <- grDevices::dev.cur()
    base::invisible(grDevices::dev.set(window.nb))
    # last warning cannot be used because suppressWarnings() does not modify last.warning present in the base evironment (created at first warning in a new R session), or warnings() # to reset the warning history : unlockBinding("last.warning", baseenv()) ; assign("last.warning", NULL, envir = baseenv())
    output <- NULL
    tempo.error <- base::try(base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = data), envir = if(base::is.null(env)){base::parent.frame()}else{env}))), silent = TRUE) # get error message, not warning or messages
    if(base::any(base::class(tempo.error) %in% base::c("gg", "ggplot"))){ # %in% never returns NA
        tempo.error <- base::try(base::suppressMessages(base::suppressWarnings(ggplot2::ggplot_build(tempo.error))), silent = TRUE)[1]
    }
    if(base::exists("tempo.error", inherits = FALSE) == TRUE){ # inherits = FALSE avoid the portee lexical and thus the declared word
        if( ! base::all(base::class(tempo.error) == "try-error")){ # deal with NULL and S4 objects. Old code:  ! (base::all(base::class(tempo.error) == "try-error") & base::any(base::grepl(x = tempo.error, pattern = "^Error|^error|^ERROR"))) but problem with S4 objects. Old code : if((base::length(tempo.error) > 0 & ! base::any(base::grepl(x = tempo.error, pattern = "^Error|^error|^ERROR"))) | (base::length(tempo.error) == 0) ){ but problem when tempo.error is a list but added this did not work: | ! base::all(base::class(tempo.error) == "character") # no NA returned using base::class()
            tempo.error <- NULL
        }
    }else{
        tempo.error <- NULL
    }
    if(kind == "error" & ! base::is.null(tempo.error)){ # 
        if(header == TRUE){
            tempo.error[1] <- base::gsub(x = tempo.error[1], pattern = "^Error i|^error i|^ERROR I", replacement = "I")
            output <- base::paste0("ERROR MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text, ":\n", tempo.error[1]) #
        }else{
            output <- tempo.error[1] #
        }
    }else if(kind == "error" & base::is.null(tempo.error) & print.no == TRUE){
        output <- base::paste0("NO ERROR MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text)
    }else if(kind != "error" & ( ! base::is.null(tempo.error)) & print.no == TRUE){
        output <- base::paste0("NO POTENTIAL ", base::ifelse(kind == "warning", "WARNING", "STANDARD (NON ERROR AND NON WARNING)"), " MESSAGE BECAUSE OF ERROR MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text)
    }else if(base::is.null(tempo.error)){
        fun.warning.capture <- function(expr){
            # from demo(error.catching) typed in the R console, coming from ?tryCatch
            # see also http://mazamascience.com/WorkingWithData/?p=912
            # return a character string or NULL
            # expr <- wilcox.test.default(c(1, 1, 3), c(1, 2, 4), paired = TRUE)
            W <- NULL
            w.handler <- function(w){ # warning handler
                W <<- w # send to the above env, i.e., the inside of the fun.warning.capture function
                base::invokeRestart("muffleWarning") # here w.handler() muffles all the warnings. See http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings to muffle specific warnings and print others
            }
            output <- base::list(
                value = base::suppressMessages(base::withCallingHandlers(base::tryCatch(expr, error = function(e){e}), warning = w.handler)), # BEWARE: w.handler is a function written without (), like in other functions with FUN argument
                warning = W # processed by w.handler()
            )
            base::return(if(base::is.null(output$warning)){NULL}else{base::as.character(output$warning)})
        }
        tempo.warn <- fun.warning.capture(base::eval(base::parse(text = data), envir = if(base::is.null(env)){base::parent.frame()}else{env}))
        # warn.options.ini <- options()$warn ; options(warn = 1) ; tempo.warn <- utils::capture.output({tempo <- suppressMessages(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))}, type = "message") ; options(warn = warn.options.ini) # this recover warnings not messages and not errors but does not work in all enviroments
        tempo.message <- utils::capture.output({
            tempo <- base::suppressMessages(base::suppressWarnings(base::eval(base::parse(text = data), envir = if(base::is.null(env)){base::parent.frame()}else{env})))
            if(base::any(base::class(tempo) %in% base::c("gg", "ggplot"))){ # %in% never returns NA
                tempo <- ggplot2::ggplot_build(tempo)
            }else{
                tempo <- base::suppressWarnings(base::eval(base::parse(text = data), envir = if(base::is.null(env)){base::parent.frame()}else{env}))
            }
        }, type = "message") # recover messages not warnings and not errors
        if(kind == "warning" & ! base::is.null(tempo.warn)){
            if(base::length(tempo.warn) > 0){ # to avoid base::character(0)
                if( ! base::any(base::sapply(tempo.warn, FUN = "grepl", pattern = "() FUNCTION:$"), na.rm = TRUE)){
                    tempo.warn <- base::paste(base::unique(tempo.warn), collapse = "\n") # if FALSE, means that the tested data is a special function. If TRUE, means that the data is a standard function. In that case, the output of utils::capture.output() is two strings per warning messages: if several warning messages -> identical first string, which is removed in next messages by base::unique()
                }else{
                    tempo.warn <- base::paste(tempo.warn, collapse = "\n")
                }
                if(header == TRUE){
                    if(base::any(base::grepl(x = tempo.warn[[1]], pattern = "^simpleWarning i"), na.rm = TRUE)){
                        tempo.warn[[1]] <- base::gsub(x = tempo.warn[[1]], pattern = "^Warning i", replacement = "I")
                    }
                    if(base::any(base::grepl(x = tempo.warn[[1]], pattern = "^Warning i"), na.rm = TRUE)){
                        tempo.warn[[1]] <- base::gsub(x = tempo.warn[[1]], pattern = "^Warning i", replacement = "I")
                    }
                    output <- base::paste0("WARNING MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text, ":\n", tempo.warn) #
                }else{
                    output <- tempo.warn #
                }
            }else{
                if(print.no == TRUE){
                    output <- base::paste0("NO WARNING MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text)
                } # no need else{} here because output is already NULL at first
            }
        }else if(kind == "warning" & base::is.null(tempo.warn) & print.no == TRUE){
            output <- base::paste0("NO WARNING MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text)
        }else if(kind == "message" & base::exists("tempo.message", inherits = FALSE) == TRUE){ # inherits = FALSE avoid the portee lexical and thus the declared word
            if(base::length(tempo.message) > 0){ # if something is returned by capture.ouptput() (only in this env) with a length more than 1
                if(header == TRUE){
                    output <- base::paste0("STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text, ":\n", tempo.message) #
                }else{
                    output <- tempo.message #
                }
            }else{
                if(print.no == TRUE){
                    output <- base::paste0("NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text)
                } # no need else{} here because output is already NULL at first
            }
        }else if(kind == "message" & base::exists("tempo.message", inherits = FALSE) == FALSE & print.no == TRUE){
            output <- base::paste0("NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text)
        } # no need else{} here because output is already NULL at first
    }
    base::invisible(grDevices::dev.off(window.nb)) # end send plots into a NULL file
    #### end main code

    #### output
    base::return(output) # do not use base::cat() because the idea is to reuse the message
    #### end output
    
}
