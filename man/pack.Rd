\name{pack}
\alias{pack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pack(req.package, load = FALSE, lib.path = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{req.package}{
%%     ~~Describe \code{req.package} here~~
}
  \item{load}{
%%     ~~Describe \code{load} here~~
}
  \item{lib.path}{
%%     ~~Describe \code{lib.path} here~~
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
function (req.package, load = FALSE, lib.path = NULL) 
{
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    if (!is.null(lib.path)) {
        if (!all(typeof(lib.path) == "character")) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else if (!all(dir.exists(lib.path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
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
    mandat.args <- c("req.package")
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
    tempo <- check(data = req.package, class = "vector", mode = "character", 
        fun.name = function.name)
    eval(ee)
    tempo <- check(data = load, class = "vector", mode = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(lib.path)) {
        tempo <- check(data = lib.path, class = "vector", mode = "character", 
            fun.name = function.name)
        eval(ee)
        if (tempo$problem == FALSE) {
            if (!all(dir.exists(lib.path), na.rm = TRUE)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", 
                  paste(lib.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                arg.check <- c(arg.check, TRUE)
            }
        }
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
    tempo.arg <- c("req.package", "load")
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
    if (is.null(lib.path)) {
        lib.path <- .libPaths()
    }
    else {
        .libPaths(new = sub(x = lib.path, pattern = "/$|\\\\$", 
            replacement = ""))
        lib.path <- .libPaths()
    }
    tempo <- NULL
    for (i1 in 1:length(req.package)) {
        if (!req.package[i1] \%in\% rownames(utils::installed.packages(lib.loc = lib.path))) {
            tempo <- c(tempo, req.package[i1])
        }
    }
    if (!is.null(tempo)) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": PACKAGE", 
            ifelse(length(tempo) == 1L, paste0("\n\n", tempo, 
                "\n\n"), paste0("S\n", paste(tempo, collapse = "\n"), 
                "\n")), "MUST BE INSTALLED IN", ifelse(length(lib.path) == 
                1L, "", " ONE OF THESE FOLDERS"), ":\n", paste(lib.path, 
                collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    else if (load == TRUE) {
        for (i2 in 1:length(req.package)) {
            suppressMessages(suppressWarnings(suppressPackageStartupMessages(library(req.package[i2], 
                lib.loc = lib.path, quietly = TRUE, character.only = TRUE))))
        }
    }
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
