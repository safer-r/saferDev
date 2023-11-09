\name{test}
\alias{test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
test(fun, arg, val, expect.error = NULL, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib.path = NULL, cute.path = "C:\\Users\\yhan\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
%%     ~~Describe \code{fun} here~~
}
  \item{arg}{
%%     ~~Describe \code{arg} here~~
}
  \item{val}{
%%     ~~Describe \code{val} here~~
}
  \item{expect.error}{
%%     ~~Describe \code{expect.error} here~~
}
  \item{parall}{
%%     ~~Describe \code{parall} here~~
}
  \item{thread.nb}{
%%     ~~Describe \code{thread.nb} here~~
}
  \item{print.count}{
%%     ~~Describe \code{print.count} here~~
}
  \item{plot.fun}{
%%     ~~Describe \code{plot.fun} here~~
}
  \item{export}{
%%     ~~Describe \code{export} here~~
}
  \item{res.path}{
%%     ~~Describe \code{res.path} here~~
}
  \item{lib.path}{
%%     ~~Describe \code{lib.path} here~~
}
  \item{cute.path}{
%%     ~~Describe \code{cute.path} here~~
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
function (fun, arg, val, expect.error = NULL, parall = FALSE, 
    thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, 
    res.path = NULL, lib.path = NULL, cute.path = "C:\\Users\\yhan\\Documents\\Git_projects\\cute_little_R_functions\\cute_little_R_functions.R") 
{
    ini <- match.call(expand.dots = FALSE)
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
    req.function <- c("fun_check", "fun_get_message", "fun_pack")
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
    mandat.args <- c("fun", "arg", "val")
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
    tempo <- fun_check(data = fun, class = "vector", mode = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- fun_check(data = arg, class = "vector", mode = "character", 
        fun.name = function.name)
    eval(ee)
    tempo <- fun_check(data = val, class = "list", na.contain = TRUE, 
        fun.name = function.name)
    eval(ee)
    if (!is.null(expect.error)) {
        tempo <- fun_check(data = expect.error, class = "list", 
            fun.name = function.name)
        eval(ee)
    }
    tempo <- fun_check(data = parall, class = "vector", mode = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (parall == TRUE) {
        if (!is.null(thread.nb)) {
            tempo <- fun_check(data = thread.nb, typeof = "integer", 
                double.as.integer.allowed = TRUE, neg.values = FALSE, 
                length = 1, fun.name = function.name)
            eval(ee)
            if (tempo$problem == FALSE & thread.nb < 1) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": thread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: ", 
                  thread.nb)
                text.check <- c(text.check, tempo.cat)
                arg.check <- c(arg.check, TRUE)
            }
        }
    }
    tempo <- fun_check(data = print.count, class = "vector", 
        typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
        neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- fun_check(data = plot.fun, class = "vector", mode = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- fun_check(data = export, class = "vector", mode = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(res.path)) {
        tempo <- fun_check(data = res.path, class = "vector", 
            mode = "character", fun.name = function.name)
        eval(ee)
    }
    if (!is.null(lib.path)) {
        tempo <- fun_check(data = lib.path, class = "vector", 
            mode = "character", fun.name = function.name)
        eval(ee)
    }
    tempo <- fun_check(data = cute.path, class = "vector", typeof = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(arg.check)) {
        if (any(arg.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], 
                collapse = "\n"), "\n\n================\n\n"), 
                call. = FALSE)
        }
    }
    env.name <- paste0("env", as.numeric(Sys.time()))
    if (exists(env.name, where = -1)) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    else {
        assign(env.name, new.env())
        assign("data", data, envir = get(env.name, env = sys.nframe(), 
            inherit = FALSE))
    }
    if (!(all(class(arg.user.setting) == "list", na.rm = TRUE) & 
        length(arg.user.setting) == 0)) {
        tempo.arg <- names(arg.user.setting)
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = length) == 1L
        if (any(tempo.log, na.rm = TRUE) == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", 
                ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", 
                  "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], 
                  collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    tempo.arg <- c("fun", "arg", "val", "parall", "print.count", 
        "plot.fun", "export", "cute.path")
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
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    if (grepl(x = fun, pattern = "()$")) {
        fun <- sub(x = fun, pattern = "()$", replacement = "")
    }
    if (!exists(fun)) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": CHARACTER STRING IN fun ARGUMENT DOES NOT EXIST IN THE R WORKING ENVIRONMENT: ", 
            paste(fun, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    else if (!all(base::class(get(fun)) == "function")) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": fun ARGUMENT IS NOT CLASS \"function\" BUT: ", 
            paste(base::class(get(fun)), collapse = "\n"), "\nCHECK IF ANY CREATED OBJECT WOULD HAVE THE NAME OF THE TESTED FUNCTION")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (tempo$problem == FALSE & base::length(arg) == 0L) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": arg ARGUMENT CANNOT BE LENGTH 0")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    for (i2 in 1:base::length(val)) {
        tempo1 <- fun_check(data = val[[i2]], class = "vector", 
            na.contain = TRUE, fun.name = function.name)
        tempo2 <- fun_check(data = val[[i2]], class = "list", 
            na.contain = TRUE, fun.name = function.name)
        if (tempo1$problem == TRUE & tempo2$problem == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": COMPARTMENT ", 
                i2, " OF val ARGUMENT MUST BE A VECTOR OR A LIST")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        else if (tempo1$problem == FALSE) {
            val[[i2]] <- split(x = val[[i2]], f = 1:base::length(val[[i2]]))
        }
    }
    if (base::length(arg) != base::length(val)) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": LENGTH OF arg ARGUMENT MUST BE IDENTICAL TO LENGTH OF val ARGUMENT:\nHERE IT IS: ", 
            base::length(arg), " VERSUS ", base::length(val))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    args <- names(formals(get(fun)))
    if (!all(arg \%in\% args)) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": SOME OF THE STRINGS IN arg ARE NOT ARGUMENTS OF fun\nfun ARGUMENTS: ", 
            paste(args, collapse = " "), "\nPROBLEMATIC STRINGS IN arg: ", 
            paste(arg[!arg \%in\% args], collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (sum(sapply(val, FUN = length) > 1) > 43) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": CANNOT TEST MORE THAN 43 ARGUMENTS IF THEY ALL HAVE AT LEAST 2 VALUES EACH\nHERE THE NUMBER IS: ", 
            sum(sapply(val, FUN = length) > 1))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (!is.null(expect.error)) {
        if (base::length(val) != base::length(expect.error)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": LENGTH OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF expect.error ARGUMENT:\nHERE IT IS: ", 
                base::length(val), " VERSUS ", base::length(expect.error))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        for (i3 in 1:base::length(expect.error)) {
            tempo1 <- fun_check(data = expect.error[[i3]], class = "vector", 
                mode = "logical", fun.name = function.name)
            tempo2 <- fun_check(data = expect.error[[i3]], class = "list", 
                fun.name = function.name)
            if (tempo1$problem == TRUE & tempo2$problem == TRUE) {
                tempo.cat <- paste0("ERROR IN ", function.name, 
                  ": COMPARTMENT ", i3, " OF expect.error ARGUMENT MUST BE TRUE OR FALSE")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n", ifelse(is.null(warn), 
                    "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                      1, "S", ""), ":\n\n", warn))), call. = FALSE)
            }
            else if (tempo1$problem == FALSE) {
                expect.error[[i3]] <- split(x = expect.error[[i3]], 
                  f = 1:base::length(expect.error[[i3]]))
            }
        }
        for (i2 in 1:length(expect.error)) {
            if (all(class(expect.error[[i2]]) == "list")) {
                if (!all(class(val[[i2]]) == "list")) {
                  tempo.cat <- paste0("ERROR IN ", function.name, 
                    ": expect.error ARGUMENT MUST BE A LIST OF EXACTLY THE SAME STRUCTURE AS val ARGUMENT.\nHERE COMPARTMENT ", 
                    i2, " OF expect.error IS CLASS ", paste(class(expect.error[[i2]]), 
                      collapse = " "), "\nAND COMPARTMENT ", 
                    i2, " OF val IS CLASS ", paste(class(val[[i2]]), 
                      collapse = " "))
                  stop(paste0("\n\n================\n\n", tempo.cat, 
                    "\n\n================\n\n", ifelse(is.null(warn), 
                      "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                        1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
                else if (base::length(val[[i2]]) != base::length(expect.error[[i2]])) {
                  tempo.cat <- paste0("ERROR IN ", function.name, 
                    ": LENGTH OF COMPARTMENT ", i2, " OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF COMPARTMENT ", 
                    i2, " OF expect.error ARGUMENT:\nHERE IT IS: ", 
                    base::length(val[[i2]]), " VERSUS ", base::length(expect.error[[i2]]))
                  stop(paste0("\n\n================\n\n", tempo.cat, 
                    "\n\n================\n\n", ifelse(is.null(warn), 
                      "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                        1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
            }
        }
    }
    if (!is.null(res.path)) {
        if (!all(dir.exists(res.path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE res.path ARGUMENT DOES NOT EXISTS:\n", 
                paste(res.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    if (parall == TRUE & is.null(res.path)) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": res.path ARGUMENT MUST BE SPECIFIED IF parall ARGUMENT IS TRUE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (is.null(res.path) & export == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": res.path ARGUMENT MUST BE SPECIFIED IF export ARGUMENT TRUE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", 
            ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", 
                ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), 
            call. = FALSE)
    }
    if (parall == TRUE & export == FALSE) {
        export <- TRUE
        tempo.cat <- paste0("WARNING FROM ", function.name, ": export ARGUMENT CONVERTED TO TRUE BECAUSE thread.nb ARGUMENT IS NOT NULL")
        warning(paste0("\n", tempo.cat, "\n"), call. = FALSE)
    }
    if (!is.null(lib.path)) {
        if (!all(dir.exists(lib.path), na.rm = TRUE)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": DIRECTORY PATH INDICATED IN THE lib.path ARGUMENT DOES NOT EXISTS:\n", 
                paste(lib.path, collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    if (parall == TRUE) {
        if (grepl(x = cute.path, pattern = "^http")) {
            tempo.error1 <- any(grepl(x = fun_get_message(data = "source(cute.path)", 
                kind = "error", header = FALSE, env = get(env.name, 
                  env = sys.nframe(), inherit = FALSE)), pattern = "^[Ee]rror"), 
                na.rm = TRUE)
            tempo.error2 <- FALSE
        }
        else {
            tempo.error1 <- FALSE
            tempo.error2 <- !file.exists(cute.path)
        }
        if (tempo.error1 | tempo.error2) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": ", 
                ifelse(grepl(x = cute.path, pattern = "^http"), 
                  "URL", "FILE"), " PATH INDICATED IN THE cute.path PARAMETER DOES NOT EXISTS:\n", 
                cute.path)
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
    }
    sp.plot.fun <- c("fun_gg_scatter", "fun_gg_bar", "fun_gg_boxplot")
    ini.warning.length <- base::options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    cat("\ntest JOB IGNITION\n")
    ini.date <- Sys.time()
    ini.time <- as.numeric(ini.date)
    if (export == TRUE) {
        res.path <- paste0(res.path, "/test_res_", trunc(ini.time))
        if (dir.exists(res.path)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": FOLDER ALREADY EXISTS\n", 
                res.path, "\nPLEASE RERUN ONCE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        else {
            dir.create(res.path)
        }
    }
    total.comp.nb <- prod(sapply(val, FUN = "length"))
    cat(paste0("\nTHE TOTAL NUMBER OF TESTS IS: ", total.comp.nb, 
        "\n"))
    loop.string <- NULL
    end.loop.string <- NULL
    fun.args <- NULL
    fun.args2 <- NULL
    error.values <- NULL
    arg.values <- "list("
    for (i1 in 1:base::length(arg)) {
        if (parall == FALSE) {
            if (base::length(val[[i1]]) > 1) {
                loop.string <- paste0(loop.string, "for(i", i1, 
                  " in 1:", base::length(val[[i1]]), "){")
                end.loop.string <- paste0(end.loop.string, "}")
            }
        }
        else {
            loop.string <- "for(i in x){"
            end.loop.string <- "}"
        }
        fun.args <- paste0(fun.args, ifelse(i1 == 1L, "", ", "), 
            arg[i1], " = val[[", i1, "]][[", if (parall == FALSE) {
                if (base::length(val[[i1]]) > 1) {
                  paste0("i", i1)
                }
                else {
                  "1"
                }
            }
            else {
                paste0("i.list[[", i1, "]][i]")
            }, "]]")
        fun.args2 <- paste0(fun.args2, ifelse(i1 == 1L, "", ", "), 
            arg[i1], " = val[[", i1, "]][[', ", if (parall == 
                FALSE) {
                if (base::length(val[[i1]]) > 1) {
                  paste0("i", i1)
                }
                else {
                  "1"
                }
            }
            else {
                paste0("i.list[[", i1, "]][i]")
            }, ", ']]")
        arg.values <- paste0(arg.values, "val[[", i1, "]][[", 
            if (parall == FALSE) {
                if (base::length(val[[i1]]) > 1) {
                  paste0("i", i1)
                }
                else {
                  "1"
                }
            }
            else {
                paste0("i.list[[", i1, "]][i]")
            }, "]]", ifelse(i1 == base::length(arg), "", ", "))
        error.values <- paste0(error.values, ifelse(i1 == 1L, 
            "", " | "), "expect.error[[", i1, "]][[", if (parall == 
            FALSE) {
            if (base::length(expect.error[[i1]]) > 1) {
                paste0("i", i1)
            }
            else {
                "1"
            }
        }
        else {
            paste0("i.list[[", i1, "]][i]")
        }, "]]")
    }
    arg.values <- paste0(arg.values, ")")
    fun.test <- paste0(fun, "(", fun.args, ")")
    fun.test2 <- paste0("paste0('", fun, "(", fun.args2, ")')")
    if (plot.fun == TRUE) {
        plot.kind <- "classic"
        if (fun \%in\% sp.plot.fun) {
            plot.kind <- "special"
            if (any(arg \%in\% "title")) {
                tempo.match <- regmatches(x = fun.test, m = regexpr(text = fun.test, 
                  pattern = "title = .+[,)]"))
                tempo.match <- substring(tempo.match, 1, nchar(tempo.match) - 
                  1)
                fun.test <- sub(x = fun.test, pattern = tempo.match, 
                  replacement = paste0(tempo.match, "\ntempo.title"))
            }
            else {
                fun.test <- sub(x = fun.test, pattern = ")$", 
                  replacement = ", title = tempo.title)")
            }
        }
    }
    kind <- character()
    problem <- logical()
    expected.error <- logical()
    res <- character()
    count <- 0
    print.count.loop <- 0
    plot.count <- 0
    if (base::length(arg) == 1L) {
        data <- data.frame()
    }
    else {
        data <- data.frame(t(vector("character", base::length(arg))), 
            stringsAsFactors = FALSE)[-1, ]
    }
    code <- paste(loop.string, "\ncount <- count + 1\nprint.count.loop <- print.count.loop + 1\narg.values.print <- eval(parse(text = arg.values)) # recover the list of the i1 compartment\nfor(j3 in 1:base::length(arg.values.print)){ # WARNING: do not use i1, i2 etc., here because already in loop.string\ntempo.capt <- capture.output(tempo.error <- fun_get_message(data =  paste0(\"paste(arg.values.print[[\", j3, \"]])\"), kind = \"error\", header = FALSE, print.no = FALSE, env = get(env.name, env = sys.nframe(), inherit = FALSE))) # collapsing arg.values sometimes does not work (with function for instance)\nif( ! is.null(tempo.error)){\narg.values.print[[j3]] <- paste0(\"SPECIAL VALUE OF CLASS \", base::class(arg.values.print[[j3]]), \" AND TYPE \", base::typeof(arg.values.print[[j3]]))\n}\n}\ndata <- rbind(data, as.character(sapply(arg.values.print, FUN = \"paste\", collapse = \" \")), stringsAsFactors = FALSE) # each colum is a test\ntempo.capt <- capture.output(tempo.try.error <- fun_get_message(data = eval(parse(text = fun.test2)), kind = \"error\", header = FALSE, print.no = FALSE, env = get(env.name, env = sys.nframe(), inherit = FALSE))) # data argument needs a character string but eval(parse(text = fun.test2)) provides it (eval parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)\ntempo.capt <- capture.output(tempo.try.warning <- fun_get_message(data = eval(parse(text = fun.test2)), kind = \"warning\", header = FALSE, env = get(env.name, env = sys.nframe(), inherit = FALSE), print.no = FALSE)) # data argument needs a character string but eval(parse(text = fun.test2)) provides it (eval parse replace the i1, i2, etc., by the correct values, meaning that only val is required in the env.name environment)\nif( ! is.null(expect.error)){\nexpected.error <- c(expected.error, eval(parse(text = error.values)))\n}\nif( ! is.null(tempo.try.error)){\nkind <- c(kind, \"ERROR\")\nproblem <- c(problem, TRUE)\nres <- c(res, tempo.try.error)\n}else{\nif( ! is.null(tempo.try.warning)){\nkind <- c(kind, \"WARNING\")\nproblem <- c(problem, FALSE)\nres <- c(res, tempo.try.warning)\n}else{\nkind <- c(kind, \"OK\")\nproblem <- c(problem, FALSE)\nres <- c(res, \"\")\n}\nif(plot.fun == TRUE){\ninvisible(dev.set(window.nb))\nplot.count <- plot.count + 1\ntempo.title <- paste0(\"test_\", sprintf(paste0(\"\%0\", nchar(total.comp.nb), \"d\"), ifelse(parall == FALSE, count, x[count])))\nif(plot.kind == \"classic\"){\neval(parse(text = fun.test))\ntempo <- fun_post_plot(corner.text = tempo.title)\n}else if(plot.kind == \"special\"){\neval(parse(text = fun.test))\n}else{\ntempo.cat <- paste0(\"INTERNAL CODE ERROR 1 IN \", function.name, \": CODE HAS TO BE MODIFIED\")\nstop(paste0(\"\n\n================\n\n\", tempo.cat, \"\n\n================\n\n\"), call. = FALSE) # == in stop() to be able to add several messages between ==\n}\n}\n}\nif(print.count.loop == print.count){\nprint.count.loop <- 0\ntempo.time <- as.numeric(Sys.time())\ntempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))\nfinal.loop <- (tempo.time - ini.time) / count * ifelse(parall == FALSE, total.comp.nb, base::length(x)) # expected duration in seconds # intra nb.compar loop lapse: time lapse / cycles done * cycles remaining\nfinal.exp <- as.POSIXct(final.loop, origin = ini.date)\ncat(paste0(ifelse(parall == FALSE, \"\n\", paste0(\"\nIN PROCESS \", process.id, \" | \")), \"LOOP \", format(count, big.mark=\",\"), \" / \", format(ifelse(parall == FALSE, total.comp.nb, base::length(x)), big.mark=\",\"), \" | TIME SPENT: \", tempo.lapse, \" | EXPECTED END: \", final.exp))\n}\nif(count == ifelse(parall == FALSE, total.comp.nb, base::length(x))){\ntempo.time <- as.numeric(Sys.time())\ntempo.lapse <- round(lubridate::seconds_to_period(tempo.time - ini.time))\ncat(paste0(ifelse(parall == FALSE, \"\nLOOP PROCESS ENDED | \", paste0(\"\nPROCESS \", process.id, \" ENDED | \")), \"LOOP \", format(count, big.mark=\",\"), \" / \", format(ifelse(parall == FALSE, total.comp.nb, base::length(x)), big.mark=\",\"), \" | TIME SPENT: \", tempo.lapse, \"\n\n\"))\n}\n", 
        end.loop.string)
    if (parall == TRUE) {
        i.list <- vector("list", base::length(val))
        for (i2 in 1:base::length(arg)) {
            if (i2 == 1L) {
                tempo.divisor <- total.comp.nb/base::length(val[[i2]])
                i.list[[i2]] <- rep(1:base::length(val[[i2]]), 
                  each = as.integer(tempo.divisor))
                tempo.multi <- base::length(val[[i2]])
            }
            else {
                tempo.divisor <- tempo.divisor/base::length(val[[i2]])
                i.list[[i2]] <- rep(rep(1:base::length(val[[i2]]), 
                  each = as.integer(tempo.divisor)), time = as.integer(tempo.multi))
                tempo.multi <- tempo.multi * base::length(val[[i2]])
            }
        }
        tempo.cat <- paste0("PARALLELIZATION INITIATED AT: ", 
            ini.date)
        cat(paste0("\n", tempo.cat, "\n"))
        tempo.thread.nb = parallel::detectCores(all.tests = FALSE, 
            logical = TRUE)
        if (tempo.thread.nb < thread.nb) {
            thread.nb <- tempo.thread.nb
        }
        tempo.cat <- paste0("NUMBER OF THREADS USED: ", thread.nb)
        cat(paste0("\n    ", tempo.cat, "\n"))
        Clust <- parallel::makeCluster(thread.nb, outfile = paste0(res.path, 
            "/test_parall_log.txt"))
        tempo.cat <- paste0("SPLIT OF TEST NUMBERS IN PARALLELISATION:")
        cat(paste0("\n    ", tempo.cat, "\n"))
        cluster.list <- parallel::clusterSplit(Clust, 1:total.comp.nb)
        str(cluster.list)
        cat("\n")
        paral.output.list <- parallel::clusterApply(cl = Clust, 
            x = cluster.list, function.name = function.name, 
            ini = ini, thread.nb = thread.nb, print.count = print.count, 
            total.comp.nb = total.comp.nb, sp.plot.fun = sp.plot.fun, 
            i.list = i.list, fun.tested = fun, arg.values = arg.values, 
            fun.test = fun.test, fun.test2 = fun.test2, kind = kind, 
            problem = problem, res = res, count = count, plot.count = plot.count, 
            data = data, code = code, plot.fun = plot.fun, res.path = res.path, 
            lib.path = lib.path, cute.path = cute.path, fun = function(x, 
                function.name, ini, thread.nb, print.count, total.comp.nb, 
                sp.plot.fun, i.list, fun.tested, arg.values, 
                fun.test, fun.test2, kind, problem, res, count, 
                plot.count, data, code, plot.fun, res.path, lib.path, 
                cute.path) {
                process.id <- Sys.getpid()
                cat(paste0("\nPROCESS ID ", process.id, " -> TESTS ", 
                  x[1], " TO ", x[base::length(x)], "\n"))
                source(cute.path, local = .GlobalEnv)
                fun_pack(req.package = "lubridate", lib.path = lib.path, 
                  load = TRUE)
                if (plot.fun == TRUE) {
                  pdf(file = paste0(res.path, "/plots_from_test_", 
                    x[1], ifelse(base::length(x) == 1L, ".pdf", 
                      paste0("-", x[base::length(x)], ".pdf"))))
                }
                else {
                  pdf(file = NULL)
                }
                window.nb <- dev.cur()
                invisible(dev.set(window.nb))
                ini.date <- Sys.time()
                ini.time <- as.numeric(ini.date)
                env.name <- paste0("env", ini.time)
                if (exists(env.name, where = -1)) {
                  tempo.cat <- paste0("ERROR IN ", function.name, 
                    ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
                  stop(paste0("\n\n================\n\n", tempo.cat, 
                    "\n\n================\n\n", ifelse(is.null(warn), 
                      "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                        1, "S", ""), ":\n\n", warn))), call. = FALSE)
                }
                else {
                  assign(env.name, new.env())
                  assign("val", val, envir = get(env.name, env = sys.nframe(), 
                    inherit = FALSE))
                }
                print.count.loop <- 0
                suppressMessages(suppressWarnings(eval(parse(text = code))))
                colnames(data) <- arg
                if (!is.null(expect.error)) {
                  data <- data.frame(data, kind = kind, problem = problem, 
                    expected.error = expected.error, message = res, 
                    stringsAsFactors = FALSE)
                }
                else {
                  data <- data.frame(data, kind = kind, problem = problem, 
                    message = res, stringsAsFactors = FALSE)
                }
                row.names(data) <- paste0("test_", sprintf(paste0("\%0", 
                  nchar(total.comp.nb), "d"), x))
                sys.info <- sessionInfo()
                sys.info$loadedOnly <- sys.info$loadedOnly[order(names(sys.info$loadedOnly))]
                invisible(dev.off(window.nb))
                rm(env.name)
                output <- list(fun = fun, ini = ini, data = data, 
                  sys.info = sys.info)
                save(output, file = paste0(res.path, "/test_", 
                  x[1], ifelse(base::length(x) == 1L, ".RData", 
                    paste0("-", x[base::length(x)], ".RData"))))
                if (plot.fun == TRUE & plot.count == 0L) {
                  warn.count <- warn.count + 1
                  tempo.warn <- paste0("(", warn.count, ") IN PROCESS ", 
                    process.id, ": NO PDF PLOT BECAUSE ONLY ERRORS REPORTED")
                  warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                    paste0(warn, "\n\n", tempo.warn)))
                  file.remove(paste0(res.path, "/plots_from_test_", 
                    x[1], ifelse(base::length(x) == 1L, ".pdf", 
                      paste0("-", x[base::length(x)], ".pdf"))))
                }
                table.out <- as.matrix(data)
                table.out <- gsub(table.out, pattern = "\n", 
                  replacement = " ")
                write.table(table.out, file = paste0(res.path, 
                  "/table_from_test_", x[1], ifelse(base::length(x) == 
                    1L, ".tsv", paste0("-", x[base::length(x)], 
                    ".tsv"))), row.names = TRUE, col.names = NA, 
                  append = FALSE, quote = FALSE, sep = "\t", 
                  eol = "\n", na = "")
            })
        parallel::stopCluster(Clust)
        if (base::length(cluster.list) > 1) {
            for (i2 in 1:base::length(cluster.list)) {
                tempo.file <- paste0(res.path, "/table_from_test_", 
                  min(cluster.list[[i2]], na.rm = TRUE), ifelse(base::length(cluster.list[[i2]]) == 
                    1L, ".tsv", paste0("-", max(cluster.list[[i2]], 
                    na.rm = TRUE), ".tsv")))
                tempo <- read.table(file = tempo.file, header = TRUE, 
                  stringsAsFactors = FALSE, sep = "\t", row.names = 1, 
                  comment.char = "", colClasses = "character")
                if (file.exists(paste0(res.path, "/plots_from_test_", 
                  min(cluster.list[[i2]], na.rm = TRUE), ifelse(base::length(cluster.list[[i2]]) == 
                    1L, ".pdf", paste0("-", max(cluster.list[[i2]], 
                    na.rm = TRUE), ".pdf"))))) {
                  tempo.pdf <- paste0(res.path, "/plots_from_test_", 
                    min(cluster.list[[i2]], na.rm = TRUE), ifelse(base::length(cluster.list[[i2]]) == 
                      1L, ".pdf", paste0("-", max(cluster.list[[i2]], 
                      na.rm = TRUE), ".pdf")))
                }
                else {
                  tempo.pdf <- NULL
                }
                tempo.rdata <- paste0(res.path, "/test_", min(cluster.list[[i2]], 
                  na.rm = TRUE), ifelse(base::length(cluster.list[[i2]]) == 
                  1L, ".RData", paste0("-", max(cluster.list[[i2]], 
                  na.rm = TRUE), ".RData")))
                if (i2 == 1L) {
                  final.file <- tempo
                  final.pdf <- tempo.pdf
                  env.name <- paste0("env", ini.time)
                  if (exists(env.name, where = -1)) {
                    tempo.cat <- paste0("ERROR IN ", function.name, 
                      ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
                    stop(paste0("\n\n================\n\n", tempo.cat, 
                      "\n\n================\n\n", ifelse(is.null(warn), 
                        "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                          1, "S", ""), ":\n\n", warn))), call. = FALSE)
                  }
                  else {
                    assign(env.name, new.env())
                    load(tempo.rdata, envir = get(env.name))
                    tempo.rdata1 <- tempo.rdata
                    assign("final.output", get("output", envir = get(env.name)), 
                      envir = get(env.name))
                  }
                }
                else {
                  final.file <- rbind(final.file, tempo, stringsAsFactors = TRUE)
                  final.pdf <- c(final.pdf, tempo.pdf)
                  load(tempo.rdata, envir = get(env.name))
                  if (!identical(get("final.output", envir = get(env.name))[c("R.version", 
                    "locale", "platform")], get("output", envir = get(env.name))[c("R.version", 
                    "locale", "platform")])) {
                    tempo.cat <- paste0("ERROR IN ", function.name, 
                      ": DIFFERENCE BETWEEN OUTPUTS WHILE THEY SHOULD BE IDENTICAL\nPLEASE CHECK\n", 
                      tempo.rdata1, "\n", tempo.rdata)
                    stop(paste0("\n\n================\n\n", tempo.cat, 
                      "\n\n================\n\n", ifelse(is.null(warn), 
                        "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                          1, "S", ""), ":\n\n", warn))), call. = FALSE)
                  }
                  else {
                    tempo.base1 <- sort(get("final.output", envir = get(env.name))$sys.info$basePkgs)
                    tempo.base2 <- sort(get("output", envir = get(env.name))$sys.info$basePkgs)
                    tempo.other1 <- names(get("final.output", 
                      envir = get(env.name))$sys.info$otherPkgs)
                    tempo.other2 <- names(get("output", envir = get(env.name))$sys.info$otherPkgs)
                    tempo.loaded1 <- names(get("final.output", 
                      envir = get(env.name))$sys.info$loadedOnly)
                    tempo.loaded2 <- names(get("output", envir = get(env.name))$sys.info$loadedOnly)
                    assign("final.output", {
                      x <- get("final.output", envir = get(env.name))
                      y <- get("output", envir = get(env.name))
                      x$sys.info$basePkgs <- sort(unique(tempo.base1, 
                        tempo.base2))
                      if (!all(tempo.other2 \%in\% tempo.other1)) {
                        x$sys.info$otherPkgs <- c(x$sys.info$otherPkgs, 
                          y$sys.info$otherPkgs[!(tempo.other2 \%in\% 
                            tempo.other1)])
                        x$sys.info$otherPkgs <- x$sys.info$otherPkgs[order(names(x$sys.info$otherPkgs))]
                      }
                      if (!all(tempo.loaded2 \%in\% tempo.loaded1)) {
                        x$sys.info$loadedOnly <- c(x$sys.info$loadedOnly, 
                          y$sys.info$loadedOnly[!(tempo.loaded2 \%in\% 
                            tempo.loaded1)])
                        x$sys.info$loadedOnly <- x$sys.info$loadedOnly[order(names(x$sys.info$loadedOnly))]
                      }
                      x
                    }, envir = get(env.name))
                  }
                }
                file.remove(c(tempo.file, tempo.rdata))
            }
            if (!is.null(final.pdf)) {
                pdftools::pdf_combine(input = final.pdf, output = paste0(res.path, 
                  "/plots_from_test_1-", total.comp.nb, ".pdf"))
                file.remove(final.pdf)
            }
            assign("output", c(get("final.output", envir = get(env.name)), 
                data = list(final.file)), envir = get(env.name))
            save(output, file = paste0(res.path, "/test__1-", 
                total.comp.nb, ".RData"), envir = get(env.name))
            rm(env.name)
            write.table(final.file, file = paste0(res.path, "/table_from_test_1-", 
                total.comp.nb, ".tsv"), row.names = TRUE, col.names = NA, 
                append = FALSE, quote = FALSE, sep = "\t", eol = "\n", 
                na = "")
            if (!is.null(expect.error)) {
                final.file <- final.file[!final.file$problem == 
                  final.file$expected.error, ]
                if (nrow(final.file) == 0L) {
                  cat(paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n"))
                }
                else {
                  cat(paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE discrepancy_table_from_test_1-", 
                    total.comp.nb, ".tsv FILE)\n\n"))
                  write.table(final.file, file = paste0(res.path, 
                    "/discrepancy_table_from_test_1-", total.comp.nb, 
                    ".tsv"), row.names = TRUE, col.names = NA, 
                    append = FALSE, quote = FALSE, sep = "\t", 
                    eol = "\n", na = "")
                }
            }
        }
    }
    else {
        if (plot.fun == TRUE) {
            pdf(file = paste0(res.path, "/plots_from_test_1", 
                ifelse(total.comp.nb == 1L, ".pdf", paste0("-", 
                  total.comp.nb, ".pdf"))))
        }
        else {
            pdf(file = NULL)
        }
        window.nb <- dev.cur()
        invisible(dev.set(window.nb))
        env.name <- paste0("env", ini.time)
        if (exists(env.name, where = -1)) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        else {
            assign(env.name, new.env())
            assign("val", val, envir = get(env.name, env = sys.nframe(), 
                inherit = FALSE))
        }
        suppressMessages(suppressWarnings(eval(parse(text = code))))
        colnames(data) <- arg
        expect.data <- data.frame()
        if (!is.null(expect.error)) {
            data <- data.frame(data, kind = kind, problem = problem, 
                expected.error = expected.error, message = res, 
                stringsAsFactors = FALSE)
        }
        else {
            data <- data.frame(data, kind = kind, problem = problem, 
                message = res, stringsAsFactors = FALSE)
        }
        row.names(data) <- paste0("test_", sprintf(paste0("\%0", 
            nchar(total.comp.nb), "d"), 1:total.comp.nb))
        sys.info <- sessionInfo()
        sys.info$loadedOnly <- sys.info$loadedOnly[order(names(sys.info$loadedOnly))]
        invisible(dev.off(window.nb))
        rm(env.name)
        if (!is.null(warn)) {
            on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
                warn), call. = FALSE))
        }
        on.exit(exp = options(warning.length = ini.warning.length), 
            add = TRUE)
        output <- list(fun = fun, ini = ini, data = data, sys.info = sys.info)
        if (plot.fun == TRUE & plot.count == 0L) {
            warn.count <- warn.count + 1
            tempo.warn <- paste0("(", warn.count, ") NO PDF PLOT BECAUSE ONLY ERRORS REPORTED")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                paste0(warn, "\n\n", tempo.warn)))
            file.remove(paste0(res.path, "/plots_from_test_1", 
                ifelse(total.comp.nb == 1L, ".pdf", paste0("-", 
                  total.comp.nb, ".pdf"))))
        }
        if (!is.null(expect.error)) {
            expect.data <- output$data[!output$data$problem == 
                output$data$expected.error, ]
            if (nrow(expect.data) == 0L) {
                cat(paste0("NO DISCREPANCY BETWEEN EXPECTED AND OBSERVED ERRORS\n\n"))
            }
            else {
                cat(paste0("DISCREPANCIES BETWEEN EXPECTED AND OBSERVED ERRORS (SEE THE ", 
                  if (export == TRUE) {
                    paste0("discrepancy_table_from_test_1", ifelse(total.comp.nb == 
                      1L, "", paste0("-", total.comp.nb)), ".tsv FILE")
                  }
                  else {
                    "$data RESULT"
                  }, ")\n\n"))
                if (export == TRUE) {
                  expect.data <- as.matrix(expect.data)
                  expect.data <- gsub(expect.data, pattern = "\n", 
                    replacement = "  ")
                  write.table(expect.data, file = paste0(res.path, 
                    "/discrepancy_table_from_test_1", ifelse(total.comp.nb == 
                      1L, ".tsv", paste0("-", total.comp.nb, 
                      ".tsv"))), row.names = TRUE, col.names = NA, 
                    append = FALSE, quote = FALSE, sep = "\t", 
                    eol = "\n", na = "")
                }
            }
        }
        if (!is.null(warn)) {
            base::options(warning.length = 8170)
            on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
                warn), call. = FALSE))
        }
        on.exit(exp = base::options(warning.length = ini.warning.length), 
            add = TRUE)
        if (export == TRUE) {
            save(output, file = paste0(res.path, "/test_1", ifelse(total.comp.nb == 
                1L, ".RData", paste0("-", total.comp.nb, ".RData"))))
            table.out <- as.matrix(output$data)
            table.out <- gsub(table.out, pattern = "\n", replacement = "  ")
            write.table(table.out, file = paste0(res.path, "/table_from_test_1", 
                ifelse(total.comp.nb == 1L, ".tsv", paste0("-", 
                  total.comp.nb, ".tsv"))), row.names = TRUE, 
                col.names = NA, append = FALSE, quote = FALSE, 
                sep = "\t", eol = "\n", na = "")
        }
        else {
            return(output)
        }
    }
    end.date <- Sys.time()
    end.time <- as.numeric(end.date)
    total.lapse <- round(lubridate::seconds_to_period(end.time - 
        ini.time))
    cat(paste0("test JOB END\n\nTIME: ", end.date, "\n\nTOTAL TIME LAPSE: ", 
        total.lapse, "\n\n\n"))
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
