\name{report}
\alias{report}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
report(data, output = "results.txt", path, overwrite = FALSE, rownames.kept = FALSE, vector.cat = FALSE, noquote = TRUE, sep = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{output}{
%%     ~~Describe \code{output} here~~
}
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{overwrite}{
%%     ~~Describe \code{overwrite} here~~
}
  \item{rownames.kept}{
%%     ~~Describe \code{rownames.kept} here~~
}
  \item{vector.cat}{
%%     ~~Describe \code{vector.cat} here~~
}
  \item{noquote}{
%%     ~~Describe \code{noquote} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
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
function (data, output = "results.txt", path, overwrite = FALSE, 
    rownames.kept = FALSE, vector.cat = FALSE, noquote = TRUE, 
    sep = 2) 
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
    mandat.args <- c("data", "path")
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
    tempo <- check(data = output, class = "character", length = 1, 
        fun.name = function.name)
    eval(ee)
    if (tempo$problem == FALSE & output == "") {
        tempo.cat <- paste0("ERROR IN ", function.name, ": output ARGUMENT AS \"\" DOES NOT CORRESPOND TO A VALID FILE NAME")
        text.check <- c(text.check, tempo.cat)
        arg.check <- c(arg.check, TRUE)
    }
    tempo <- check(data = path, class = "vector", mode = "character", 
        fun.name = function.name)
    eval(ee)
    if (tempo$problem == FALSE) {
        if (!all(dir.exists(path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": path ARGUMENT DOES NOT CORRESPOND TO EXISTING DIRECTORY\n", 
                paste(path, collapse = "\n"))
            text.check <- c(text.check, tempo.cat)
            arg.check <- c(arg.check, TRUE)
        }
    }
    tempo <- check(data = overwrite, class = "logical", length = 1, 
        fun.name = function.name)
    eval(ee)
    tempo <- check(data = rownames.kept, class = "logical", length = 1, 
        fun.name = function.name)
    eval(ee)
    tempo <- check(data = vector.cat, class = "logical", length = 1, 
        fun.name = function.name)
    eval(ee)
    tempo <- check(data = noquote, class = "logical", length = 1, 
        fun.name = function.name)
    eval(ee)
    tempo <- check(data = sep, class = "vector", typeof = "integer", 
        length = 1, double.as.integer.allowed = TRUE, fun.name = function.name)
    eval(ee)
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
    tempo.arg <- c("data", "output", "path", "overwrite", "rownames.kept", 
        "vector.cat", "noquote", "sep")
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
    if (!is.null(data)) {
        if (all(class(data) == "data.frame") | all(class(data) == 
            "table") | all(class(data) \%in\% c("matrix", "array"))) {
            if (rownames.kept == FALSE & all(class(data) == "data.frame") & 
                nrow(data) != 0 & nrow(data) <= 4) {
                rownames.output.tables <- ""
                length.rows <- nrow(data)
                for (i in 1:length.rows) {
                  rownames.output.tables <- c(rownames.output.tables, 
                    paste0(rownames.output.tables[i], " ", collapse = ""))
                }
                row.names(data) <- rownames.output.tables[1:length.rows]
            }
            else if (rownames.kept == FALSE & (all(class(data) == 
                "table") | all(class(data) \%in\% c("matrix", "array")))) {
                rownames(data) <- rep("", nrow(data))
            }
            if (noquote == TRUE) {
                utils::capture.output(noquote(data), file = paste0(path, 
                  "/", output), append = !overwrite)
            }
            else {
                utils::capture.output(data, file = paste0(path, 
                  "/", output), append = !overwrite)
            }
        }
        else if (is.vector(data) & all(class(data) != "list") & 
            (length(data) == 1L | vector.cat == TRUE)) {
            if (noquote == TRUE) {
                cat(noquote(data), file = paste0(path, "/", output), 
                  append = !overwrite)
            }
            else {
                cat(data, file = paste0(path, "/", output), append = !overwrite)
            }
        }
        else if (all(base::mode(data) == "character")) {
            if (noquote == TRUE) {
                utils::capture.output(noquote(data), file = paste0(path, 
                  "/", output), append = !overwrite)
            }
            else {
                utils::capture.output(data, file = paste0(path, 
                  "/", output), append = !overwrite)
            }
        }
        else {
            utils::capture.output(data, file = paste0(path, "/", 
                output), append = !overwrite)
        }
        sep.final <- paste0(rep("\n", sep), collapse = "")
        write(sep.final, file = paste0(path, "/", output), append = TRUE)
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
