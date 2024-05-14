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
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns The message or NULL if no message and print.no is FALSE.
#' @details 
#' WARNINGS
#' 
#' Only the first message is returned.
#' 
#' Always use the env argument when get_message() is used inside functions.
#' 
#' The function does not prevent printing if print() is used inside the instruction tested. To prevent that, use tempo <- utils::capture.output(error <- get_message(data = "arg_check(data = 'a', class = mean, neg.values = FALSE, print = TRUE)")). The return of get_message() is assigned into error and the printed messages are captured by utils::capture.output() and assigned into tempo. See the examples.
#' @seealso \code{\link{try}} .
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
#' get_message(data = "wilcox.test()", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "sum(1)", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "message('ahah')", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "message('ahah')", kind = "message", print.no = TRUE, text = "IN A")
#' 
#' get_message(data = "ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), 
#' mapping = aes(x = X)) + geom_histogram()", kind = "message", print.no = TRUE, 
#' text = "IN FUNCTION 1")
#' 
#' set.seed(1) ; 
#' obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), 
#' Group1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; 
#' get_message(data = 'gg_boxplot(data = obs1, y = "Time", categ = "Group1")', 
#' kind = "message", print.no = TRUE, text = "IN FUNCTION 1")
#' 
#' @importFrom ggplot2 ggplot_build
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
    # package name
    package.name <- "saferDev"
    # end package name
    # function name
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            external.function.name = function.name,
            external.package.name = package.name)
    }
    # end critical operator checking
    # package checking
    # check of lib.path
    # end check of lib.path
    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "ggplot2::ggplot_build"
        ),
        lib.path = NULL,
        external.function.name = function.name,
        external.package.name = package.name
        )
    }
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "data"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- arg_check(data = data, class = "vector", typeof = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- arg_check(data = kind, options = base::c("error", "warning", "message"), length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- arg_check(data = header, class = "vector", typeof = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    tempo <- arg_check(data = print.no, class = "vector", typeof = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(text)){
        tempo <- arg_check(data = text, class = "character", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }
    if( ! base::is.null(env)){
        tempo <- arg_check(data = env, class = "environment", fun.name = function.name, safer_check = FALSE) ; base::eval(ee) #
    }
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because base::is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "data", 
        "kind", 
        "header", 
        "print.no",
        # "text",  # inactivated because can be null
        # "env",  # inactivated because can be null
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, envir = base::sys.nframe(), inherits = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    # end other checkings
    # end second round of checking and data preparation

    
    # main code
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
        output <- base::paste0("NO ", base::ifelse(kind == "warning", "WARNING", "STANDARD (NON ERROR AND NON WARNING)"), " MESSAGE BECAUSE OF ERROR MESSAGE REPORTED", base::ifelse(base::is.null(text), "", " "), text)
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
    # end main code
    # output
    # warning output
    # end warning output
    base::return(output) # do not use base::cat() because the idea is to reuse the message
    # end output
}