
two_colons_check <- function(x){
    # x = test_fun
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    # recovering the basic functions of R
    s <- c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base") # basic search() scope
    if(any( ! s %in% search())){
        tempo.cat <- paste0(
            "INTERNAL ERROR IN double_dot_check: THE search() SCOPE µOF R HAS CHANGED.\nTHE PROBLEM IS:\n",
            paste(s[ ! s %in% search()], collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    fun <- unlist(sapply(X = s, FUN = function(x){ls(x, all.names = TRUE)})) # all the basic functions of R in all the scope
    # end recovering the basic functions of R
    # recovering the input function string
    ini <- paste0(deparse(x), collapse = "\\n") # recovering as single string separated by \\n (and not \n to avoid the eval(\n) when printing the error message)
    ini <- gsub(x = ini, pattern = " +", replacement = " ") # removal of multiple spaces
    # end recovering the input function string
    # all function names in x
    pattern <- "\\b[a-zA-Z\\.][a-zA-Z0-9\\.\\_]*\\b(?= *\\()" # pattern to detect a function name, i.e., name that is followed by "("
    # - `\\b`: These are word boundaries. It ensures that the pattern matches only a complete word and not a part of a word.
    # - `[a-zA-Z\\.]`: This portion of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), or a period (`\\.`).
    # - `[a-zA-Z0-9\\.\\_]*`: This part of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), number (`0-9`), period (`\\.`), or underscore (`\\_`), repeated zero or more times (`*`). This represents the possible characters inside an R function name.
    # - `\\b`: Again, these are word boundaries, making sure the pattern captures the entire word and not just part of it.
    # - `(?= *\\()`: This is a lookahead assertion. It checks that the preceding pattern is followed by any spaces and a parenthesis (`\\(`), but doesn't include the sapces and parenthesis in the match. This is because, in R code, a function call is usually followed by a parenthesis, but the parenthesis is not part of the function name.
    fun_name <- unlist(stringr::str_extract_all(ini, pattern)) # recover all the strings in pattern present in ini
    fun_name_wo_op <- fun_name[ ! fun_name %in% c("function", "if", "for", "while", "repeat")] # removal of special functions
    fun_name_wo_op_uni <- unique(fun_name_wo_op)
    # end all function names in x
    # basic function names in x
    in_basic_fun <- fun_name_wo_op_uni[fun_name_wo_op_uni %in% fun] #  names of all the basic functions used in x
    # end basic function names in x
    # other function names in x
    in_other_fun <- fun_name_wo_op_uni[ ! fun_name_wo_op_uni %in% fun]
    # end other function names in x
    # analyse of :: before basic functions in x
    if(length(in_basic_fun) > 0){
        pattern2 <- paste(paste0(in_basic_fun, "\\s*\\("), collapse = "|") # split string according to basic function name as splitter
        res <- unlist(strsplit(x = ini, split = pattern2))
        res <- res[-length(res)] # the last split section is removed because nothing to test at the end (end of code)
        res2 <- sapply(X = res, FUN = function(x){substr(x, nchar(x)-1, nchar(x))})
        names(res2) <- NULL
        tempo.log <- ! res2 %in% "::"
        if(any(tempo.log)){
            tempo.pos <- paste0(which(tempo.log), "\t", fun_name_wo_op[tempo.log], "\t\t", res[tempo.log])
            output.cat <- paste0(
                "ERROR IN double_dot_check: :: IS MISSING AT BASIC FUNCTION POSITION\n\n",
                "POS\tFUN\t\tSTRING_BEFORE\n",
                paste(tempo.pos, collapse = "\n")
            )
        }else{
            output.cat <- NULL
        }
    }else{
        tempo.log <- FALSE
        output.cat <- NULL
    }
    # end analyse of :: before basic functions in x
    # analyse of :: before other functions in x
    if(length(in_other_fun) > 0){
        pattern2.b <- paste(paste0(in_other_fun, "\\s*\\("), collapse = "|") # split string according to basic function name as splitter
        res.b <- unlist(strsplit(x = ini, split = pattern2.b))
        res.b <- res.b[-length(res.b)] # the last split section is removed because nothing to test at the end (end of code)
        res2.b <- sapply(X = res.b, FUN = function(x){substr(x, nchar(x)-1, nchar(x))})
        names(res2.b) <- NULL
        tempo.log.b <- ! res2.b %in% "::"
        if(any(tempo.log.b)){
            tempo.pos.b <- paste0(which(tempo.log.b), "\t", fun_name_wo_op[tempo.log.b], "\t\t", res.b[tempo.log.b])
            output.cat.b <- paste0(
                "ERROR IN double_dot_check: :: IS MISSING AT OTHER FUNCTION POSITION\n\n",
                "POS\tFUN\t\tSTRING_BEFORE\n",
                paste(tempo.pos.b, collapse = "\n")
            )
        }else{
            output.cat.b <- NULL
        }
    }else{
        tempo.log.b <- FALSE
        output.cat.b <- NULL
    }
    # end analyse of :: before basic functions in x
    if(any(tempo.log) | any(tempo.log.b)){
        tempo.cat <- paste(output.cat, ifelse(is.null(output.cat) | is.null(output.cat.b), "", "\n\n"), output.cat.b)
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        cat("\n\nEVERYTHING SEEMS CLEAN\n\n")
    }
    options(warning.length = ini.warning.length)
}



