######## fun_get_message() #### return error/warning/other messages of an expression (that can be exported)

# todo list check OK
# Check r_debugging_tools-v1.4.R 
# Check fun_test() 20201107 (see cute_checks.docx) 
# example sheet 
# check all and any OK
# -> clear to go Apollo
# -> transferred into the cute package

#' @title fun_get_message
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
#' @returns The message or NULL if no message and print.no is FALSE.
#' @details 
#' REQUIRED PACKAGES
#' 
#' None
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' 
#' 
#' WARNINGS
#' 
#' Only the first message is returned.
#' 
#' Always use the env argument when fun_get_message() is used inside functions.
#' 
#' The function does not prevent printing if print() is used inside the instruction tested. To prevent that, use tempo <- capture.output(error <- fun_get_message(data = "fun_check(data = 'a', class = mean, neg.values = FALSE, print = TRUE)")). The return of fun_get_message() is assigned into error and the printed messages are captured by capture.output() and assigned into tempo. See the examples.
#' 
#' 
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' fun_pack()
#'
#' @examples
#' fun_get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' fun_get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "warning", print.no = TRUE, text = "IN A")
#' 
#' fun_get_message(data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)", kind = "message", print.no = TRUE, text = "IN A")
#' 
#' fun_get_message(data = "wilcox.test()", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' fun_get_message(data = "sum(1)", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' fun_get_message(data = "message('ahah')", kind = "error", print.no = TRUE, text = "IN A")
#' 
#' fun_get_message(data = "message('ahah')", kind = "message", print.no = TRUE, text = "IN A")
#' 
#' fun_get_message(data = "ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), mapping = aes(x = X)) + geom_histogram()", kind = "message", print.no = TRUE, text = "IN FUNCTION 1")
#' 
#' set.seed(1) ; 
#' obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), 
#' Group1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; 
#' fun_get_message(data = 'fun_gg_boxplot(data = obs1, y = "Time", categ = "Group1")', 
#' kind = "message", print.no = TRUE, text = "IN FUNCTION 1")
#' 
#' @importFrom ggplot2 ggplot_build
#' @importFrom grDevices dev.cur
#' @importFrom grDevices dev.off
#' @importFrom grDevices dev.set
#' @importFrom grDevices pdf
#' @importFrom utils capture.output
#' @importFrom utils find
#' @export
fun_get_message <- function(
        data, 
        kind = "error", 
        header = TRUE, 
        print.no = FALSE, 
        text = NULL, 
        env = NULL
){  
    # DEBUGGING
    # data = "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL # for function debugging
    # data = "sum(1)" ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL  # for function debugging
    # set.seed(1) ; obs1 <- data.frame(Time = c(rnorm(10), rnorm(10) + 2), Group1 = rep(c("G", "H"), each = 10), stringsAsFactors = TRUE) ; data = 'fun_gg_boxplot(data1 = obs1, y = "Time", categ = "Group1")' ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL  # for function debugging
    # data = "message('ahah')" ; kind = "error" ; header = TRUE ; print.no = TRUE ; text = "IN A" ; env = NULL 
    # data = 'ggplot2::ggplot(data = data.frame(X = "a", stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()' ; kind = "message" ; header = TRUE ; print.no = FALSE ; text = NULL # for function debugging
    # data = 'ggplot2::ggplot(data = data.frame(X = "a", stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()' ; kind = "warning" ; header = TRUE ; print.no = FALSE ; text = NULL # for function debugging
    # data = "emmeans::emmeans(object = emm.rg, specs = contrast.var)" ; kind = "message" ; header = TRUE ; print.no = FALSE ; text = NULL ; env = NULL # for function debugging

    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # package checking
    # check of lib.path
    if( ! is.null(lib.path)){
        if( ! all(typeof(lib.path) == "character")){ # no na.rm = TRUE with typeof
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }else if( ! all(dir.exists(lib.path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib.path == NA
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
        }
    }
    # end check of lib.path
    # cuteDev required function checking
    req.function <- c(
        "fun_check"
    )
    tempo <- NULL
    for(i1 in req.function){
        if(length(find(i1, mode = "function")) == 0L){
            tempo <- c(tempo, i1)
        }
    }
    if( ! is.null(tempo)){
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFUNCTION", ifelse(length(tempo) > 1, "S ", ""), " FROM THE cuteDev PACKAGE", ifelse(length(tempo) > 1, " ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", paste0(tempo, collapse = "()\n"), "()")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end cutedev required function checking
    # check of other required packages
    # end check of other required packages
    # end package checking
    # check of the required function from the required packages
    # end check of the required function from the required packages
    
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "data"
    )
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, collapse = ") | missing("), ")")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = data, class = "vector", typeof = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = kind, options = c("error", "warning", "message"), length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = header, class = "vector", typeof = "logical", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = print.no, class = "vector", typeof = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(text)){
        tempo <- fun_check(data = text, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    }
    if( ! is.null(env)){
        tempo <- fun_check(data = env, class = "environment", fun.name = function.name) ; eval(ee) #
    }
    if( ! is.null(arg.check)){
        if(any(arg.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with fun_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, envir = sys.nframe(), inherits = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, envir = sys.nframe(), inherits = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-c(
        "data", 
        "kind", 
        "header", 
        "print.no"
        # "text",  # inactivated because can be null
        # "env"  # inactivated because can be null
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, envir = sys.nframe(), inherits = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    # end warning initiation
    # other checkings
    # end other checkings
    # reserved words to avoid bugs (names of dataframe columns used in this function)
    # no need to use reserved words to avoid bugs, because it is local, and  exists("tempo.warning", inherits = FALSE), never use the scope
    # end reserved words to avoid bugs (used in this function)
    # end second round of checking and data preparation
    
    # main code
    pdf(file = NULL) # send plots into a NULL file, no pdf file created
    window.nb <- dev.cur()
    invisible(dev.set(window.nb))
    # last warning cannot be used because suppressWarnings() does not modify last.warning present in the base evironment (created at first warning in a new R session), or warnings() # to reset the warning history : unlockBinding("last.warning", baseenv()) ; assign("last.warning", NULL, envir = baseenv())
    output <- NULL
    tempo.error <- try(suppressMessages(suppressWarnings(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))), silent = TRUE) # get error message, not warning or messages
    if(any(class(tempo.error) %in% c("gg", "ggplot"))){ # %in% never returns NA
        tempo.error <- try(suppressMessages(suppressWarnings(ggplot2::ggplot_build(tempo.error))), silent = TRUE)[1]
    }
    if(exists("tempo.error", inherits = FALSE) == TRUE){ # inherits = FALSE avoid the portee lexical and thus the declared word
        if( ! all(class(tempo.error) == "try-error")){ # deal with NULL and S4 objects. Old code:  ! (all(class(tempo.error) == "try-error") & any(grepl(x = tempo.error, pattern = "^Error|^error|^ERROR"))) but problem with S4 objects. Old code : if((length(tempo.error) > 0 & ! any(grepl(x = tempo.error, pattern = "^Error|^error|^ERROR"))) | (length(tempo.error) == 0) ){ but problem when tempo.error is a list but added this did not work: | ! all(class(tempo.error) == "character") # no NA returned using class()
            tempo.error <- NULL
        }
    }else{
        tempo.error <- NULL
    }
    if(kind == "error" & ! is.null(tempo.error)){ # 
        if(header == TRUE){
            tempo.error[1] <- gsub(x = tempo.error[1], pattern = "^Error i|^error i|^ERROR I", replacement = "I")
            output <- paste0("ERROR MESSAGE REPORTED", ifelse(is.null(text), "", " "), text, ":\n", tempo.error[1]) #
        }else{
            output <- tempo.error[1] #
        }
    }else if(kind == "error" & is.null(tempo.error) & print.no == TRUE){
        output <- paste0("NO ERROR MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
    }else if(kind != "error" & ( ! is.null(tempo.error)) & print.no == TRUE){
        output <- paste0("NO ", ifelse(kind == "warning", "WARNING", "STANDARD (NON ERROR AND NON WARNING)"), " MESSAGE BECAUSE OF ERROR MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
    }else if(is.null(tempo.error)){
        fun.warning.capture <- function(expr){
            # from demo(error.catching) typed in the R console, coming from ?tryCatch
            # see also http://mazamascience.com/WorkingWithData/?p=912
            # return a character string or NULL
            # expr <- wilcox.test.default(c(1, 1, 3), c(1, 2, 4), paired = TRUE)
            W <- NULL
            w.handler <- function(w){ # warning handler
                W <<- w # send to the above env, i.e., the inside of the fun.warning.capture function
                invokeRestart("muffleWarning") # here w.handler() muffles all the warnings. See http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings to muffle specific warnings and print others
            }
            output <- list(
                value = suppressMessages(withCallingHandlers(tryCatch(expr, error = function(e){e}), warning = w.handler)), # BEWARE: w.handler is a function written without (), like in other functions with FUN argument
                warning = W # processed by w.handler()
            )
            return(if(is.null(output$warning)){NULL}else{as.character(output$warning)})
        }
        tempo.warn <- fun.warning.capture(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))
        # warn.options.ini <- options()$warn ; options(warn = 1) ; tempo.warn <- utils::capture.output({tempo <- suppressMessages(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))}, type = "message") ; options(warn = warn.options.ini) # this recover warnings not messages and not errors but does not work in all enviroments
        tempo.message <- capture.output({
            tempo <- suppressMessages(suppressWarnings(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env})))
            if(any(class(tempo) %in% c("gg", "ggplot"))){ # %in% never returns NA
                tempo <- ggplot2::ggplot_build(tempo)
            }else{
                tempo <- suppressWarnings(eval(parse(text = data), envir = if(is.null(env)){parent.frame()}else{env}))
            }
        }, type = "message") # recover messages not warnings and not errors
        if(kind == "warning" & ! is.null(tempo.warn)){
            if(length(tempo.warn) > 0){ # to avoid character(0)
                if( ! any(sapply(tempo.warn, FUN = "grepl", pattern = "() FUNCTION:$"), na.rm = TRUE)){
                    tempo.warn <- paste(unique(tempo.warn), collapse = "\n") # if FALSE, means that the tested data is a special function. If TRUE, means that the data is a standard function. In that case, the output of capture.output() is two strings per warning messages: if several warning messages -> identical first string, which is removed in next messages by unique()
                }else{
                    tempo.warn <- paste(tempo.warn, collapse = "\n")
                }
                if(header == TRUE){
                    if(any(grepl(x = tempo.warn[[1]], pattern = "^simpleWarning i"), na.rm = TRUE)){
                        tempo.warn[[1]] <- gsub(x = tempo.warn[[1]], pattern = "^Warning i", replacement = "I")
                    }
                    if(any(grepl(x = tempo.warn[[1]], pattern = "^Warning i"), na.rm = TRUE)){
                        tempo.warn[[1]] <- gsub(x = tempo.warn[[1]], pattern = "^Warning i", replacement = "I")
                    }
                    output <- paste0("WARNING MESSAGE REPORTED", ifelse(is.null(text), "", " "), text, ":\n", tempo.warn) #
                }else{
                    output <- tempo.warn #
                }
            }else{
                if(print.no == TRUE){
                    output <- paste0("NO WARNING MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
                } # no need else{} here because output is already NULL at first
            }
        }else if(kind == "warning" & is.null(tempo.warn) & print.no == TRUE){
            output <- paste0("NO WARNING MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
        }else if(kind == "message" & exists("tempo.message", inherits = FALSE) == TRUE){ # inherits = FALSE avoid the portee lexical and thus the declared word
            if(length(tempo.message) > 0){ # if something is returned by capture.ouptput() (only in this env) with a length more than 1
                if(header == TRUE){
                    output <- paste0("STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", ifelse(is.null(text), "", " "), text, ":\n", tempo.message) #
                }else{
                    output <- tempo.message #
                }
            }else{
                if(print.no == TRUE){
                    output <- paste0("NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
                } # no need else{} here because output is already NULL at first
            }
        }else if(kind == "message" & exists("tempo.message", inherits = FALSE) == FALSE & print.no == TRUE){
            output <- paste0("NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", ifelse(is.null(text), "", " "), text)
        } # no need else{} here because output is already NULL at first
    invisible(dev.off(window.nb)) # end send plots into a NULL file
    # output
    # warning output
    # end warning output
    return(output) # do not use cat() because the idea is to reuse the message
    # end output
    # end main code
}



