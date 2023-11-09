\name{python_pack}
\alias{python_pack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
python_pack(req.package, python.exec.path = NULL, lib.path = NULL, R.lib.path = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{req.package}{
%%     ~~Describe \code{req.package} here~~
}
  \item{python.exec.path}{
%%     ~~Describe \code{python.exec.path} here~~
}
  \item{lib.path}{
%%     ~~Describe \code{lib.path} here~~
}
  \item{R.lib.path}{
%%     ~~Describe \code{R.lib.path} here~~
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
function (req.package, python.exec.path = NULL, lib.path = NULL, 
    R.lib.path = NULL) 
{
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    if (!is.null(R.lib.path)) {
        if (!all(typeof(R.lib.path) == "character")) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE R.lib.path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n", 
                paste(R.lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        else if (!all(dir.exists(R.lib.path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE R.lib.path ARGUMENT DOES NOT EXISTS:\n", 
                paste(R.lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    req.function <- c("check", "pack")
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
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, 
        collapse = "),missing("), "))")))
    if (any(tempo)) {
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", 
            ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), 
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
    tempo <- check(data = req.package, class = "character", fun.name = function.name)
    eval(ee)
    if (!is.null(python.exec.path)) {
        tempo <- check(data = python.exec.path, class = "character", 
            length = 1, fun.name = function.name)
        eval(ee)
        if (tempo$problem == FALSE) {
            if (!all(file.exists(python.exec.path), na.rm = TRUE)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": FILE PATH INDICATED IN THE python.exec.path ARGUMENT DOES NOT EXISTS:\n", 
                  paste(python.exec.path, collapse = "\n"))
                text.check <- c(text.check, tempo.cat)
                arg.check <- c(arg.check, TRUE)
            }
        }
    }
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
    if (!is.null(R.lib.path)) {
        tempo <- check(data = R.lib.path, class = "character", 
            fun.name = function.name)
        eval(ee)
        if (tempo$problem == FALSE) {
            if (!all(dir.exists(R.lib.path), na.rm = TRUE)) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": DIRECTORY PATH INDICATED IN THE R.lib.path ARGUMENT DOES NOT EXISTS:\n", 
                  paste(R.lib.path, collapse = "\n"))
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
    pack(req.package = "reticulate", lib.path = R.lib.path)
    if (is.null(python.exec.path)) {
        python.exec.path <- reticulate::py_run_string("\nimport sys ;\npath_lib = sys.path\n")
        python.exec.path <- python.exec.path$path_lib
    }
    if (is.null(lib.path)) {
        lib.path <- reticulate::py_run_string("\nimport sys ;\npath_lib = sys.path\n")
        lib.path <- lib.path$path_lib
    }
    reticulate::use_python(Sys.which(python.exec.path), required = TRUE)
    for (i1 in 1:length(req.package)) {
        tempo.try <- vector("list", length = length(lib.path))
        for (i2 in 1:length(lib.path)) {
            tempo.try[[i2]] <- suppressWarnings(try(reticulate::import_from_path(req.package[i1], 
                path = lib.path[i2]), silent = TRUE))
            tempo.try[[i2]] <- suppressWarnings(try(reticulate::import_from_path(req.package[i1], 
                path = lib.path[i2]), silent = TRUE))
        }
        if (all(sapply(tempo.try, FUN = grepl, pattern = "[Ee]rror"), 
            na.rm = TRUE)) {
            print(tempo.try)
            tempo.cat <- paste0("ERROR IN ", function.name, ": PACKAGE ", 
                req.package[i1], " MUST BE INSTALLED IN THE MENTIONNED DIRECTORY:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
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
