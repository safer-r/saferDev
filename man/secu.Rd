\name{secu}
\alias{secu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
secu(pos = 1, name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pos}{
%%     ~~Describe \code{pos} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
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
function (pos = 1, name = NULL) 
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
    arg.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(arg.check <- c(arg.check, tempo$problem), 
        text.check <- c(text.check, tempo$text), checked.arg.names <- c(checked.arg.names, 
            tempo$object.name))
    tempo <- check(data = pos, class = "vector", typeof = "integer", 
        double.as.integer.allowed = TRUE, length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(name)) {
        tempo <- check(data = name, class = "vector", typeof = "character", 
            fun.name = function.name)
        eval(ee)
    }
    if (!is.null(arg.check)) {
        if (any(arg.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], 
                collapse = "\n"), "\n\n================\n\n"), 
                call. = FALSE)
        }
    }
    if (!(all(class(arg.user.setting) == "list", na.rm = TRUE) & 
        length(arg.user.setting) == 0)) {
        tempo.arg <- names(arg.user.setting)
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
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
    tempo.arg <- c("pos")
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), 
        inherit = FALSE), FUN = is.null)
    if (any(tempo.log) == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", 
            ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", 
                "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], 
                collapse = "\n"), "\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    tempo.name <- rev(as.character(unlist(sys.calls())))
    tempo.frame <- rev(sys.frames())
    if (any(sapply(tempo.frame, FUN = environmentName) \%in\% "R_GlobalEnv")) {
        global.pos <- which(sapply(tempo.frame, FUN = environmentName) \%in\% 
            "R_GlobalEnv")
        tempo.name <- tempo.name[-c(global.pos:length(tempo.frame))]
        tempo.frame <- tempo.frame[-c(global.pos:length(tempo.frame))]
    }
    match.list <- vector("list", length = length(tempo.name) + 
        length(search()))
    ls.names <- c(tempo.name, search())
    ls.input <- c(tempo.frame, as.list(search()))
    names(match.list) <- ls.names
    match.list <- match.list[-c(1:(pos + 1))]
    ls.tested <- ls.input[[pos + 1]]
    ls.input <- ls.input[-c(1:(pos + 1))]
    for (i1 in 1:length(match.list)) {
        if (any(ls(name = ls.input[[i1]], all.names = TRUE) \%in\% 
            ls(name = ls.tested, all.names = TRUE))) {
            match.list[i1] <- list(ls(name = ls.input[[i1]], 
                all.names = TRUE)[ls(name = ls.input[[i1]], all.names = TRUE) \%in\% 
                ls(name = ls.tested, all.names = TRUE)])
        }
    }
    if (!all(sapply(match.list, FUN = is.null), na.rm = TRUE)) {
        output <- paste0("SOME VARIABLES ", ifelse(is.null(name), 
            "OF THE CHECKED ENVIRONMENT", paste0("OF ", name)), 
            " ARE ALSO PRESENT IN :\n", paste0(names(match.list[!sapply(match.list, 
                FUN = is.null)]), ": ", sapply(match.list[!sapply(match.list, 
                FUN = is.null)], FUN = paste0, collapse = " "), 
                collapse = "\n"), "\n")
    }
    else {
        output <- NULL
    }
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
