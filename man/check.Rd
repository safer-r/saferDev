\name{check}
\alias{check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
check(data, class = NULL, typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double.as.integer.allowed = FALSE, options = NULL, all.options.in.data = FALSE, na.contain = FALSE, neg.values = TRUE, inf.values = TRUE, print = FALSE, data.name = NULL, fun.name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{class}{
%%     ~~Describe \code{class} here~~
}
  \item{typeof}{
%%     ~~Describe \code{typeof} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
}
  \item{length}{
%%     ~~Describe \code{length} here~~
}
  \item{prop}{
%%     ~~Describe \code{prop} here~~
}
  \item{double.as.integer.allowed}{
%%     ~~Describe \code{double.as.integer.allowed} here~~
}
  \item{options}{
%%     ~~Describe \code{options} here~~
}
  \item{all.options.in.data}{
%%     ~~Describe \code{all.options.in.data} here~~
}
  \item{na.contain}{
%%     ~~Describe \code{na.contain} here~~
}
  \item{neg.values}{
%%     ~~Describe \code{neg.values} here~~
}
  \item{inf.values}{
%%     ~~Describe \code{inf.values} here~~
}
  \item{print}{
%%     ~~Describe \code{print} here~~
}
  \item{data.name}{
%%     ~~Describe \code{data.name} here~~
}
  \item{fun.name}{
%%     ~~Describe \code{fun.name} here~~
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
function (data, class = NULL, typeof = NULL, mode = NULL, length = NULL, 
    prop = FALSE, double.as.integer.allowed = FALSE, options = NULL, 
    all.options.in.data = FALSE, na.contain = FALSE, neg.values = TRUE, 
    inf.values = TRUE, print = FALSE, data.name = NULL, fun.name = NULL) 
{
    if (!is.null(fun.name)) {
        if (all(base::class(fun.name) == "character")) {
            if (base::length(fun.name) != 1) {
                tempo.cat <- paste0("ERROR IN check(): THE fun.name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1: ", 
                  paste(fun.name, collapse = " "))
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
            else if (any(is.na(fun.name))) {
                tempo.cat <- paste0("ERROR IN check(): NO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT IS fun.name")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
        }
        else {
            tempo.cat <- paste0("ERROR IN check(): THE fun.name ARGUMENT MUST BE A CHARACTER VECTOR OF LENGTH 1")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    mandat.args <- c("data")
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, 
        collapse = "), missing("), "))")))
    if (any(tempo)) {
        tempo.cat <- paste0("ERROR IN ", fun.name, "\nFOLLOWING ARGUMENT", 
            ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args[tempo], 
                collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    basic.class <- c("NULL", "logical", "integer", "numeric", 
        "character")
    tempo.arg.base <- c("class", "typeof", "mode", "length", 
        "prop", "double.as.integer.allowed", "options", "all.options.in.data", 
        "na.contain", "neg.values", "inf.values", "print", "data.name", 
        "fun.name")
    tempo.class <- list(base::class(class), base::class(typeof), 
        base::class(mode), base::class(length), base::class(prop), 
        base::class(double.as.integer.allowed), base::class(options), 
        base::class(all.options.in.data), base::class(na.contain), 
        base::class(neg.values), base::class(inf.values), base::class(print), 
        base::class(data.name), base::class(fun.name))
    tempo <- !sapply(lapply(tempo.class, FUN = "\%in\%", basic.class), 
        FUN = all)
    if (any(tempo)) {
        tempo.cat1 <- tempo.arg.base[tempo]
        tempo.cat2 <- sapply(tempo.class[tempo], FUN = paste0, 
            collapse = " ")
        tempo.sep <- sapply(mapply(" ", max(nchar(tempo.cat1)) - 
            nchar(tempo.cat1) + 3, FUN = rep, SIMPLIFY = FALSE), 
            FUN = paste0, collapse = "")
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": ANY ARGUMENT EXCEPT data MUST HAVE A BASIC CLASS\nPROBLEMATIC ARGUMENT", 
            ifelse(base::length(tempo.cat1) > 1, "S", ""), " AND ASSOCIATED CLASS", 
            ifelse(base::length(tempo.cat1) > 1, "ES ARE", " IS"), 
            ":\n", paste0(tempo.cat1, tempo.sep, tempo.cat2, 
                collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (any(is.na(data.name)) | any(is.na(class)) | any(is.na(typeof)) | 
        any(is.na(mode)) | any(is.na(length)) | any(is.na(prop)) | 
        any(is.na(double.as.integer.allowed)) | any(is.na(all.options.in.data)) | 
        any(is.na(na.contain)) | any(is.na(neg.values)) | any(is.na(inf.values)) | 
        any(is.na(print)) | any(is.na(fun.name))) {
        tempo <- c("data.name", "class", "typeof", "mode", "length", 
            "prop", "double.as.integer.allowed", "all.options.in.data", 
            "na.contain", "neg.values", "inf.values", "print", 
            "fun.name")[c(any(is.na(data.name)), any(is.na(class)), 
            any(is.na(typeof)), any(is.na(mode)), any(is.na(length)), 
            any(is.na(prop)), any(is.na(double.as.integer.allowed)), 
            any(is.na(all.options.in.data)), any(is.na(na.contain)), 
            any(is.na(neg.values)), any(is.na(inf.values)), any(is.na(print)), 
            any(is.na(fun.name)))]
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": NO ARGUMENT EXCEPT data AND options CAN HAVE NA VALUES\nPROBLEMATIC ARGUMENT", 
            ifelse(length(tempo) > 1, "S ARE", " IS"), ":\n", 
            paste(tempo, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    tempo.arg <- c("prop", "double.as.integer.allowed", "all.options.in.data", 
        "na.contain", "neg.values", "inf.values", "print")
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, envir = sys.nframe(), 
        inherits = FALSE), FUN = is.null)
    if (any(tempo.log) == TRUE) {
        tempo.cat <- paste0("ERROR IN fun.check():\n", ifelse(sum(tempo.log, 
            na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", paste0(tempo.arg[tempo.log], 
                collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!is.null(data.name)) {
        if (!(base::length(data.name) == 1L & all(base::class(data.name) == 
            "character"))) {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": data.name ARGUMENT MUST BE A SINGLE CHARACTER ELEMENT AND NOT ", 
                paste(data.name, collapse = " "))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (is.null(options) & is.null(class) & is.null(typeof) & 
        is.null(mode) & prop == FALSE & is.null(length)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": AT LEAST ONE OF THE options, class, typeof, mode, prop, OR length ARGUMENT MUST BE SPECIFIED (I.E, TRUE FOR prop)")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!is.null(options) & (!is.null(class) | !is.null(typeof) | 
        !is.null(mode) | prop == TRUE)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": THE class, typeof, mode ARGUMENTS MUST BE NULL, AND prop FALSE, IF THE options ARGUMENT IS SPECIFIED\nTHE options ARGUMENT MUST BE NULL IF THE class AND/OR typeof AND/OR mode AND/OR prop ARGUMENT IS SPECIFIED")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!(all(base::class(neg.values) == "logical") & base::length(neg.values) == 
        1L)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": THE neg.values ARGUMENT MUST BE TRUE OR FALSE ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (neg.values == FALSE & is.null(class) & is.null(typeof) & 
        is.null(mode)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": THE neg.values ARGUMENT CANNOT BE SWITCHED TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!(all(base::class(inf.values) == "logical") & base::length(inf.values) == 
        1L)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": THE inf.values ARGUMENT MUST BE TRUE OR FALSE ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (inf.values == FALSE & is.null(class) & is.null(typeof) & 
        is.null(mode)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": THE inf.values ARGUMENT CANNOT BE SWITCHED TO FALSE IF class, typeof AND mode ARGUMENTS ARE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!is.null(class)) {
        if (!all(class \%in\% c("vector", "logical", "integer", 
            "numeric", "complex", "character", "matrix", "array", 
            "data.frame", "list", "factor", "table", "expression", 
            "name", "symbol", "function", "uneval", "environment", 
            "ggplot2", "ggplot_built", "call") & base::length(class) == 
            1L)) {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": class ARGUMENT MUST BE ONE OF THESE VALUE:\n\"vector\", \"logical\", \"integer\", \"numeric\", \"complex\", \"character\", \"matrix\", \"array\", \"data.frame\", \"list\", \"factor\", \"table\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"ggplot2\", \"ggplot_built\", \"call\"")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (neg.values == FALSE & !any(class \%in\% c("vector", 
            "numeric", "integer", "matrix", "array", "data.frame", 
            "table"))) {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"integer\", \"matrix\", \"array\", \"data.frame\", \"table\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (inf.values == FALSE & !any(class \%in\% c("vector", 
            "numeric", "matrix", "array", "data.frame", "table"))) {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF inf.values ARGUMENT IS SWITCHED TO FALSE. \"integer IS NOT ALLOWED BECAUSE IFINITE VALUES ARE NOT INTEGERS\"")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (!is.null(typeof)) {
        if (!(all(typeof \%in\% c("logical", "integer", "double", 
            "complex", "character", "list", "expression", "symbol", 
            "closure", "special", "builtin", "environment", "S4", 
            "language")) & base::length(typeof) == 1L)) {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"integer\", \"double\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"closure\", \"special\", \"builtin\", \"environment\", \"S4\", \"language\"")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (neg.values == FALSE & !typeof \%in\% c("double", "integer")) {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN \"double\" OR \"integer\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (inf.values == FALSE & typeof != "double") {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN \"double\" IF inf.values ARGUMENT IS SWITCHED TO FALSE. \"integer IS NOT ALLOWED BECAUSE IFINITE VALUES ARE NOT INTEGERS\"")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (!is.null(mode)) {
        if (!(all(mode \%in\% c("logical", "numeric", "complex", 
            "character", "list", "expression", "name", "symbol", 
            "function", "environment", "S4", "call")) & base::length(mode) == 
            1L)) {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": mode ARGUMENT MUST BE ONE OF THESE VALUE:\n\"logical\", \"numeric\", \"complex\", \"character\", \"list\", \"expression\", \"name\", \"symbol\", \"function\", \"environment\", \"S4\", \"call\"")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (neg.values == FALSE & mode != "numeric") {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF neg.values ARGUMENT IS SWITCHED TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if (inf.values == FALSE & mode != "numeric") {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN \"numeric\" IF inf.values ARGUMENT IS SWITCHED TO FALSE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (!is.null(length)) {
        if (!(is.numeric(length) & base::length(length) == 1L & 
            all(!grepl(length, pattern = "\\.")))) {
            tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                "", paste0(" INSIDE ", fun.name)), ": length ARGUMENT MUST BE A SINGLE INTEGER VALUE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (!(is.logical(prop) & base::length(prop) == 1L)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": prop ARGUMENT MUST BE TRUE OR FALSE ONLY")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    else if (prop == TRUE) {
        if (!is.null(class)) {
            if (!any(class \%in\% c("vector", "numeric", "matrix", 
                "array", "data.frame", "table"))) {
                tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                  "", paste0(" INSIDE ", fun.name)), ": class ARGUMENT CANNOT BE OTHER THAN NULL, \"vector\", \"numeric\", \"matrix\", \"array\", \"data.frame\", \"table\" IF prop ARGUMENT IS TRUE")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
        }
        if (!is.null(mode)) {
            if (mode != "numeric") {
                tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                  "", paste0(" INSIDE ", fun.name)), ": mode ARGUMENT CANNOT BE OTHER THAN NULL OR \"numeric\" IF prop ARGUMENT IS TRUE")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
        }
        if (!is.null(typeof)) {
            if (typeof != "double") {
                tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
                  "", paste0(" INSIDE ", fun.name)), ": typeof ARGUMENT CANNOT BE OTHER THAN NULL OR \"double\" IF prop ARGUMENT IS TRUE")
                stop(paste0("\n\n================\n\n", tempo.cat, 
                  "\n\n================\n\n"), call. = FALSE)
            }
        }
    }
    if (!(all(base::class(double.as.integer.allowed) == "logical") & 
        base::length(double.as.integer.allowed) == 1L)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": THE double.as.integer.allowed ARGUMENT MUST BE TRUE OR FALSE ONLY: ", 
            paste(double.as.integer.allowed, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!(is.logical(all.options.in.data) & base::length(all.options.in.data) == 
        1L)) {
        tempo.cat <- paste0("ERROR IN check()", ifelse(is.null(fun.name), 
            "", paste0(" INSIDE ", fun.name)), ": all.options.in.data ARGUMENT MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY): ", 
            paste(all.options.in.data, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!(all(base::class(na.contain) == "logical") & base::length(na.contain) == 
        1L)) {
        tempo.cat <- paste0("ERROR IN check(): THE na.contain ARGUMENT MUST BE TRUE OR FALSE ONLY: ", 
            paste(na.contain, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!(all(base::class(print) == "logical") & base::length(print) == 
        1L)) {
        tempo.cat <- paste0("ERROR IN check(): THE print ARGUMENT MUST BE TRUE OR FALSE ONLY: ", 
            paste(print, collapse = " "))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (is.null(data.name)) {
        data.name <- deparse(substitute(data))
    }
    problem <- FALSE
    text <- paste0(ifelse(is.null(fun.name), "", paste0("IN ", 
        fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", data.name, 
        " OBJECT")
    if ((!is.null(options)) & (all(base::typeof(data) == "character") | 
        all(base::typeof(data) == "integer") | all(base::typeof(data) == 
        "double"))) {
        test.log <- TRUE
        if (all(base::typeof(data) == "double")) {
            if (!all(data\%\%1 == 0L, na.rm = TRUE)) {
                problem <- TRUE
                text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                  paste0("ERROR IN ", fun.name)), ": THE ", data.name, 
                  " OBJECT MUST BE SOME OF THESE OPTIONS: ", 
                  paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
                test.log <- FALSE
            }
        }
        if (test.log == TRUE) {
            text <- ""
            if (!all(data \%in\% options)) {
                problem <- TRUE
                text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                  paste0("ERROR IN ", fun.name)), ": THE ", data.name, 
                  " OBJECT MUST BE SOME OF THESE OPTIONS: ", 
                  paste(options, collapse = " "), "\nTHE PROBLEMATIC ELEMENTS OF ", 
                  data.name, " ARE: ", paste(unique(data[!(data \%in\% 
                    options)]), collapse = " "))
            }
            if (all.options.in.data == TRUE) {
                if (!all(options \%in\% data)) {
                  problem <- TRUE
                  text <- paste0(ifelse(text == "", "", paste0(text, 
                    "\n")), ifelse(is.null(fun.name), "ERROR", 
                    paste0("ERROR IN ", fun.name)), ": THE ", 
                    data.name, " OBJECT MUST BE MADE OF ALL THESE OPTIONS: ", 
                    paste(options, collapse = " "), "\nTHE MISSING ELEMENTS OF THE options ARGUMENT ARE: ", 
                    paste(unique(options[!(options \%in\% data)]), 
                      collapse = " "))
                }
            }
            if (!is.null(length)) {
                if (base::length(data) != length) {
                  problem <- TRUE
                  text <- paste0(ifelse(text == "", "", paste0(text, 
                    "\n")), ifelse(is.null(fun.name), "ERROR", 
                    paste0("ERROR IN ", fun.name)), ": THE LENGTH OF ", 
                    data.name, " MUST BE ", length, " AND NOT ", 
                    base::length(data))
                }
            }
            if (text == "") {
                text <- paste0(ifelse(is.null(fun.name), "", 
                  paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", 
                  data.name, " OBJECT")
            }
        }
    }
    else if (!is.null(options)) {
        problem <- TRUE
        text <- paste0(ifelse(is.null(fun.name), "ERROR", paste0("ERROR IN ", 
            fun.name)), ": THE ", data.name, " OBJECT MUST BE SOME OF THESE OPTIONS: ", 
            paste(options, collapse = " "), "\nBUT IS NOT EVEN TYPE CHARACTER OR INTEGER")
    }
    arg.names <- c("class", "typeof", "mode", "length")
    if (!is.null(class)) {
        if (class == "matrix") {
            class <- c("matrix", "array")
        }
        else if (class == "factor" & all(base::class(data) \%in\% 
            c("factor", "ordered"))) {
            class <- c("factor", "ordered")
        }
    }
    if (is.null(options)) {
        for (i2 in 1:base::length(arg.names)) {
            if (!is.null(get(arg.names[i2], envir = sys.nframe(), 
                inherits = FALSE))) {
                tempo.script <- "\nproblem <- TRUE ;\nif(identical(text, paste0(ifelse(is.null(fun.name), \"\", paste0(\"IN \", fun.name, \": \")), \"NO PROBLEM DETECTED FOR THE \", data.name, \" OBJECT\"))){\ntext <- paste0(ifelse(is.null(fun.name), \"ERROR\", paste0(\"ERROR IN \", fun.name)), \": THE \", data.name, \" OBJECT MUST BE \") ;\n}else{\ntext <- paste0(text, \" AND \") ; \n}\ntext <- paste0(text, toupper(arg.names[i2]), \" \", if(all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) \%in\% c(\"matrix\", \"array\"))){\"matrix\"}else if(all(get(arg.names[i2], envir = sys.nframe(), inherits = FALSE) \%in\% c(\"factor\", \"ordered\"))){\"factor\"}else{get(arg.names[i2], envir = sys.nframe(), inherits = FALSE)})\n"
                if (base::typeof(data) == "double" & double.as.integer.allowed == 
                  TRUE & ((arg.names[i2] == "class" & all(get(arg.names[i2], 
                  envir = sys.nframe(), inherits = FALSE) == 
                  "integer")) | (arg.names[i2] == "typeof" & 
                  all(get(arg.names[i2], envir = sys.nframe(), 
                    inherits = FALSE) == "integer")))) {
                  if (!all(data\%\%1 == 0L, na.rm = TRUE)) {
                    eval(parse(text = tempo.script))
                  }
                }
                else if (!any(all(get(arg.names[i2], envir = sys.nframe(), 
                  inherits = FALSE) \%in\% c("vector", "ggplot2"))) & 
                  !all(eval(parse(text = paste0(arg.names[i2], 
                    "(data)"))) \%in\% get(arg.names[i2], envir = sys.nframe(), 
                    inherits = FALSE))) {
                  eval(parse(text = tempo.script))
                }
                else if (arg.names[i2] == "class" & all(get(arg.names[i2], 
                  envir = sys.nframe(), inherits = FALSE) == 
                  "vector") & !(all(base::class(data) \%in\% "numeric") | 
                  all(base::class(data) \%in\% "integer") | all(base::class(data) \%in\% 
                  "character") | all(base::class(data) \%in\% "logical"))) {
                  eval(parse(text = tempo.script))
                }
                else if (arg.names[i2] == "class" & all(get(arg.names[i2], 
                  envir = sys.nframe(), inherits = FALSE) == 
                  "ggplot2") & !all(base::class(data) \%in\% c("gg", 
                  "ggplot"))) {
                  eval(parse(text = tempo.script))
                }
            }
        }
    }
    if (prop == TRUE & all(base::typeof(data) == "double")) {
        if (is.null(data) | any(data < 0 | data > 1, na.rm = TRUE)) {
            problem <- TRUE
            if (identical(text, paste0(ifelse(is.null(fun.name), 
                "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", 
                data.name, " OBJECT"))) {
                text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                  paste0("ERROR IN ", fun.name)), ": ")
            }
            else {
                text <- paste0(text, " AND ")
            }
            text <- paste0(text, "THE ", data.name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
        }
    }
    else if (prop == TRUE) {
        problem <- TRUE
        if (identical(text, paste0(ifelse(is.null(fun.name), 
            "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", 
            data.name, " OBJECT"))) {
            text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                paste0("ERROR IN ", fun.name)), ": ")
        }
        else {
            text <- paste0(text, " AND ")
        }
        text <- paste0(text, "THE ", data.name, " OBJECT MUST BE DECIMAL VALUES BETWEEN 0 AND 1")
    }
    if (all(base::class(data) \%in\% "expression")) {
        data <- as.character(data)
    }
    if (na.contain == FALSE & (base::mode(data) \%in\% c("logical", 
        "numeric", "complex", "character", "list"))) {
        if (any(is.na(data)) == TRUE) {
            problem <- TRUE
            if (identical(text, paste0(ifelse(is.null(fun.name), 
                "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", 
                data.name, " OBJECT"))) {
                text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                  paste0("ERROR IN ", fun.name)), ": ")
            }
            else {
                text <- paste0(text, " AND ")
            }
            text <- paste0(text, "THE ", data.name, " OBJECT CONTAINS NA WHILE NOT AUTHORIZED")
        }
    }
    if (neg.values == FALSE & all(base::mode(data) \%in\% "numeric") & 
        !any(base::class(data) \%in\% "factor")) {
        if (any(data < 0, na.rm = TRUE)) {
            problem <- TRUE
            if (identical(text, paste0(ifelse(is.null(fun.name), 
                "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", 
                data.name, " OBJECT"))) {
                text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                  paste0("ERROR IN ", fun.name)), ": ")
            }
            else {
                text <- paste0(text, " AND ")
            }
            text <- paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON NEGATIVE NUMERIC VALUES")
        }
    }
    else if (neg.values == FALSE) {
        problem <- TRUE
        if (identical(text, paste0(ifelse(is.null(fun.name), 
            "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", 
            data.name, " OBJECT"))) {
            text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                paste0("ERROR IN ", fun.name)), ": ")
        }
        else {
            text <- paste0(text, " AND ")
        }
        text <- paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON NEGATIVE VALUES BUT IS ", 
            ifelse(any(base::class(data) \%in\% "factor"), "A FACTOR", 
                "NOT EVEN MODE NUMERIC"))
    }
    if (inf.values == FALSE & all(base::typeof(data) \%in\% "double") & 
        !any(base::class(data) \%in\% "factor")) {
        if (any(is.infinite(data), na.rm = TRUE)) {
            problem <- TRUE
            if (identical(text, paste0(ifelse(is.null(fun.name), 
                "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", 
                data.name, " OBJECT"))) {
                text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                  paste0("ERROR IN ", fun.name)), ": ")
            }
            else {
                text <- paste0(text, " AND ")
            }
            text <- paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON INFINITE NUMERIC VALUES")
        }
    }
    else if (inf.values == FALSE) {
        problem <- TRUE
        if (identical(text, paste0(ifelse(is.null(fun.name), 
            "", paste0("IN ", fun.name, ": ")), "NO PROBLEM DETECTED FOR THE ", 
            data.name, " OBJECT"))) {
            text <- paste0(ifelse(is.null(fun.name), "ERROR", 
                paste0("ERROR IN ", fun.name)), ": ")
        }
        else {
            text <- paste0(text, " AND ")
        }
        text <- paste0(text, "THE ", data.name, " OBJECT MUST BE MADE OF NON INFINITE VALUES BUT IS ", 
            ifelse(any(base::class(data) \%in\% "factor"), "A FACTOR", 
                "NOT EVEN TYPE DOUBLE"))
    }
    if (print == TRUE & problem == TRUE) {
        cat(paste0("\n\n================\n\n", text, "\n\n================\n\n"))
    }
    output <- list(problem = problem, text = text, object.name = data.name)
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
