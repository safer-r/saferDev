\name{get_message}
\alias{get_message}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_message(data, kind = "error", header = TRUE, print.no = FALSE, text = NULL, env = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{kind}{
%%     ~~Describe \code{kind} here~~
}
  \item{header}{
%%     ~~Describe \code{header} here~~
}
  \item{print.no}{
%%     ~~Describe \code{print.no} here~~
}
  \item{text}{
%%     ~~Describe \code{text} here~~
}
  \item{env}{
%%     ~~Describe \code{env} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, kind = "error", header = TRUE, print.no = FALSE, 
    text = NULL, env = NULL) 
{
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    req.function <- c("check")
    tempo <- NULL
    for (i1 in req.function) {
        if (length(find(i1, mode = "function")) == 0L) {
            tempo <- c(tempo, i1)
        }
    }
    if (!is.null(tempo)) {
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFUNCTION", 
            ifelse(length(tempo) > 1, "S ", ""), " FROM THE cuteDev PACKAGE", 
            ifelse(length(tempo) > 1, " ARE", " IS"), " MISSING IN THE R ENVIRONMENT:\n", 
            paste0(tempo, collapse = "()\n"), "()")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    pack(req.package = c("ggplot2"), load = TRUE, lib.path = NULL)
    mandat.args <- c("data")
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, 
        collapse = ") | missing("), ")")))
    if (any(tempo)) {
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", 
            ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, 
                collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    arg.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(arg.check <- c(arg.check, tempo$problem), 
        text.check <- c(text.check, tempo$text), checked.arg.names <- c(checked.arg.names, 
            tempo$object.name))
    tempo <- check(data = data, class = "vector", typeof = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- check(data = kind, options = c("error", "warning", 
        "message"), length = 1, fun.name = function.name)
    eval(ee)
    tempo <- check(data = header, class = "vector", typeof = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- check(data = print.no, class = "vector", typeof = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(text)) {
        tempo <- check(data = text, class = "character", length = 1, 
            fun.name = function.name)
        eval(ee)
    }
    if (!is.null(env)) {
        tempo <- check(data = env, class = "environment", fun.name = function.name)
        eval(ee)
    }
    if (!is.null(arg.check)) {
        if (any(arg.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (!(all(class(arg.user.setting) == "list", na.rm = TRUE) & 
        length(arg.user.setting) == 0)) {
        tempo.arg <- names(arg.user.setting)
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, 
            FUN = get, envir = sys.nframe(), inherits = FALSE), 
            FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, 
            FUN = get, envir = sys.nframe(), inherits = FALSE), 
            FUN = length) == 1L
        if (any(tempo.log) == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", 
                ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", 
                  "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], 
                  collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    tempo.arg <- c("data", "kind", "header", "print.no")
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, envir = sys.nframe(), 
        inherits = FALSE), FUN = is.null)
    if (any(tempo.log) == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", 
            ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", 
                "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], 
                collapse = "\n"), "\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    pdf(file = NULL)
    window.nb <- dev.cur()
    invisible(dev.set(window.nb))
    output <- NULL
    tempo.error <- try(suppressMessages(suppressWarnings(eval(parse(text = data), 
        envir = if (is.null(env)) {
            parent.frame()
        }
        else {
            env
        }))), silent = TRUE)
    if (any(class(tempo.error) \%in\% c("gg", "ggplot"))) {
        tempo.error <- try(suppressMessages(suppressWarnings(ggplot2::ggplot_build(tempo.error))), 
            silent = TRUE)[1]
    }
    if (exists("tempo.error", inherits = FALSE) == TRUE) {
        if (!all(class(tempo.error) == "try-error")) {
            tempo.error <- NULL
        }
    }
    else {
        tempo.error <- NULL
    }
    if (kind == "error" & !is.null(tempo.error)) {
        if (header == TRUE) {
            tempo.error[1] <- gsub(x = tempo.error[1], pattern = "^Error i|^error i|^ERROR I", 
                replacement = "I")
            output <- paste0("ERROR MESSAGE REPORTED", ifelse(is.null(text), 
                "", " "), text, ":\n", tempo.error[1])
        }
        else {
            output <- tempo.error[1]
        }
    }
    else if (kind == "error" & is.null(tempo.error) & print.no == 
        TRUE) {
        output <- paste0("NO ERROR MESSAGE REPORTED", ifelse(is.null(text), 
            "", " "), text)
    }
    else if (kind != "error" & (!is.null(tempo.error)) & print.no == 
        TRUE) {
        output <- paste0("NO ", ifelse(kind == "warning", "WARNING", 
            "STANDARD (NON ERROR AND NON WARNING)"), " MESSAGE BECAUSE OF ERROR MESSAGE REPORTED", 
            ifelse(is.null(text), "", " "), text)
    }
    else if (is.null(tempo.error)) {
        fun.warning.capture <- function(expr) {
            W <- NULL
            w.handler <- function(w) {
                W <<- w
                invokeRestart("muffleWarning")
            }
            output <- list(value = suppressMessages(withCallingHandlers(tryCatch(expr, 
                error = function(e) {
                  e
                }), warning = w.handler)), warning = W)
            return(if (is.null(output$warning)) {
                NULL
            } else {
                as.character(output$warning)
            })
        }
        tempo.warn <- fun.warning.capture(eval(parse(text = data), 
            envir = if (is.null(env)) {
                parent.frame()
            }
            else {
                env
            }))
        tempo.message <- capture.output({
            tempo <- suppressMessages(suppressWarnings(eval(parse(text = data), 
                envir = if (is.null(env)) {
                  parent.frame()
                }
                else {
                  env
                })))
            if (any(class(tempo) \%in\% c("gg", "ggplot"))) {
                tempo <- ggplot2::ggplot_build(tempo)
            }
            else {
                tempo <- suppressWarnings(eval(parse(text = data), 
                  envir = if (is.null(env)) {
                    parent.frame()
                  }
                  else {
                    env
                  }))
            }
        }, type = "message")
        if (kind == "warning" & !is.null(tempo.warn)) {
            if (length(tempo.warn) > 0) {
                if (!any(sapply(tempo.warn, FUN = "grepl", pattern = "() FUNCTION:$"), 
                  na.rm = TRUE)) {
                  tempo.warn <- paste(unique(tempo.warn), collapse = "\n")
                }
                else {
                  tempo.warn <- paste(tempo.warn, collapse = "\n")
                }
                if (header == TRUE) {
                  if (any(grepl(x = tempo.warn[[1]], pattern = "^simpleWarning i"), 
                    na.rm = TRUE)) {
                    tempo.warn[[1]] <- gsub(x = tempo.warn[[1]], 
                      pattern = "^Warning i", replacement = "I")
                  }
                  if (any(grepl(x = tempo.warn[[1]], pattern = "^Warning i"), 
                    na.rm = TRUE)) {
                    tempo.warn[[1]] <- gsub(x = tempo.warn[[1]], 
                      pattern = "^Warning i", replacement = "I")
                  }
                  output <- paste0("WARNING MESSAGE REPORTED", 
                    ifelse(is.null(text), "", " "), text, ":\n", 
                    tempo.warn)
                }
                else {
                  output <- tempo.warn
                }
            }
            else {
                if (print.no == TRUE) {
                  output <- paste0("NO WARNING MESSAGE REPORTED", 
                    ifelse(is.null(text), "", " "), text)
                }
            }
        }
        else if (kind == "warning" & is.null(tempo.warn) & print.no == 
            TRUE) {
            output <- paste0("NO WARNING MESSAGE REPORTED", ifelse(is.null(text), 
                "", " "), text)
        }
        else if (kind == "message" & exists("tempo.message", 
            inherits = FALSE) == TRUE) {
            if (length(tempo.message) > 0) {
                if (header == TRUE) {
                  output <- paste0("STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", 
                    ifelse(is.null(text), "", " "), text, ":\n", 
                    tempo.message)
                }
                else {
                  output <- tempo.message
                }
            }
            else {
                if (print.no == TRUE) {
                  output <- paste0("NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", 
                    ifelse(is.null(text), "", " "), text)
                }
            }
        }
        else if (kind == "message" & exists("tempo.message", 
            inherits = FALSE) == FALSE & print.no == TRUE) {
            output <- paste0("NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED", 
                ifelse(is.null(text), "", " "), text)
        }
    }
    invisible(dev.off(window.nb))
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
