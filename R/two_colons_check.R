#' @title two_colons_check
#' @description
#' Verify that all the functions used inside a function are all referenced by their package attribution. For instance: base::mean()
#' @param x a function name, written without quotes and brackets.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r) ? If TRUE, two_colons_check() checks before running that 1) the R scope for R operators (like "<-") is not overwritten by another package and 2) functions and related packages used are present in R lybraries. Set to FALSE if two_colons_check() is used inside another "safer" function to avoid pointless multiple checking.
#' @returns 
#' A message indicating the missing :: or a message saying that everything seems fine.
#' @details
#' - More precisely, two_colons_check() verifies that all the strings before an opening bracket "(" are preceeded by "::"
#' 
#' - The regex used is: "\\b[a-zA-Z.]{1}[a-zA-Z0-9._]+\\b(?= *\\()"
#'  
#' - The following R operators using bracket are not considered: "function", "if", "for", "while" and "repeat"
    # Dot at first position is removed (dur to stringr::str_extract_all() function ).
    # Function with dot at last position are not detected
#' 
#'  
#' @examples
#' two_colons_check(mean)
#' 
#' @export
two_colons_check <- function(
    x, 
    safer_check = TRUE
){

    # DEBUGGING
    # x = saferDev::get_message ; safer_check = TRUE
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
        .base_op_check(external.function.name = function.name)
    }
    # end critical operator checking
    # check of lib.path
    # end check of lib.path
    # check of the required function from the required packages

    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "x"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::missing(", base::paste0(mandat.args, collapse = ") | base::missing("), ")")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = x, mode = "function", length = 1,  fun.name = function.name) ; base::eval(ee)
    # lib.path already checked above
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
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) == "list", na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "x",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    ini.warning.length <- base::options()$warning.length # required to have the max characters of output messages
    base::options(warning.length = 8170)
    # end warning initiation
    # other checkings
    # end other checkings
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation

    # main code
    extract_all <- function(text, pattern) {
        # Find all matches, including trailing '('
        matches <- base::gregexpr(base::paste0(pattern, "\\("), text)
        matched_strings <- base::regmatches(text, matches)[[1]]
        
        # Remove trailing '(' from each match
        result <- base::sub("\\($", "", matched_strings)
        base::return(result)
    }
    # recovering the basic functions of R
    s <- base::c("package:stats", "package:graphics",  "package:grDevices", "package:utils", "package:datasets", "package:methods", "Autoloads", "package:base") # basic search() scope
    if(base::any( ! s %in% base::search())){
        tempo.cat <- base::paste0(
            "INTERNAL ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE base::search() SCOPE OF R HAS CHANGED.\nTHE PROBLEM IS:\n",
            base::paste(s[ ! s %in% base::search()], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    fun <- base::unlist(base::sapply(X = s, FUN = function(x){base::ls(x, all.names = TRUE)})) # all the basic functions of R in all the scope
    # end recovering the basic functions of R
    # recovering the input function string
    ini <- base::paste0(base::deparse(x), collapse = " \\n ") # recovering as single string separated by \\n (and not \n to avoid the eval(\n) when printing the error message)
    ini <- base::gsub(x = ini, pattern = " +", replacement = " ") # removal of multiple spaces
    # end recovering the input function string
    # all function names in x
    pattern <- "[a-zA-Z._]{1}[a-zA-Z0-9._]+" # pattern to detect a function name, i.e., name that is followed by "("
    # - `this does not work well, as it does not take dots: "\\b[a-zA-Z\\.\\_]{1}[a-zA-Z0-9\\.\\_]+\\b", because of `\\b`: These are word boundaries. It ensures that the pattern matches only a complete word and not a part of a word.
    # - `[a-zA-Z._]{1}`: This portion of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), or a period (`.`) a single time ({1}).
    # - `[a-zA-Z0-9._]*`: This part of the pattern matches any uppercase letter (`A-Z`), lowercase letter (`a-z`), number (`0-9`), period (`.`), or underscore (`_`), repeated one or more times (`+`). This represents the possible characters inside an R function name.
    # - `\\b`: Again, these are word boundaries, making sure the pattern captures the entire word and not just part of it.
    # -  not used: `(?= *\\()`: This is a lookahead assertion. It checks that the preceding pattern is followed by any spaces and a parenthesis (`\\(`), but doesn't include the sapces and parenthesis in the match. This is because, in R code, a function call is usually followed by a parenthesis, but the parenthesis is not part of the function name.
    fun_name <- extract_all(ini, pattern) # recover all the strings in pattern present in ini
    fun_name_wo_op <- fun_name[ ! fun_name %in% base::c("function", "if", "for", "while", "repeat")] # removal of special functions
    fun_name_wo_op_uni <- base::unique(fun_name_wo_op)
    # end all function names in x
    # basic function names in x
    in_basic_fun <- fun_name_wo_op[fun_name_wo_op %in% fun] #  names of all the basic functions used in x
    in_basic_fun_uni <- fun_name_wo_op_uni[fun_name_wo_op_uni %in% fun] #  names of unique basic functions used in x
    # end basic function names in x
    # other function names in x
    in_other_fun <- fun_name_wo_op[ ! fun_name_wo_op %in% base::c(fun, arg.user.setting$x)] #  names of all the other functions used in x, except the one tested (arg.user.setting$x), because can be in error messages
    in_other_fun_uni <- fun_name_wo_op_uni[ ! fun_name_wo_op_uni %in% base::c(fun, arg.user.setting$x)] #  names of unique basic functions used in x, except the one tested (arg.user.setting$x), because can be in error messages
    # end other function names in x
    # analyse of :: before basic functions in x
    if(base::length(in_basic_fun_uni) > 0){
        pattern2 <- base::paste(base::paste0(in_basic_fun_uni, "\\s*\\("), collapse = "|") # split string according to basic function name as splitter
        res <- base::unlist(base::strsplit(x = ini, split = pattern2))
        res <- res[-base::length(res)] # the last split section is removed because nothing to test at the end (end of code)
        res2 <- base::sapply(X = res, FUN = function(x){base::substr(x, base::nchar(x)-1, base::nchar(x))})
        base::names(res2) <- NULL
        tempo.log <- ! res2 %in% "::"
        if(base::any(tempo.log)){
            col1 <- base::which(tempo.log)
            col2 <- in_basic_fun[tempo.log]
            col3 <- res[tempo.log]
            if( ! (base::length(col1) == base::length(col2) & base::length(col1) == base::length(col3) & base::length(col2) == base::length(col3))){
                tempo.cat <- base::paste0(
                    "INTERNAL ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: LENGTHS OF col1 (", base::length(col1), "), col2 (", base::length(col2), "), AND col3 (", base::length(col3), "), SHOULD BE EQUAL\n")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            tempo.pos <- base::paste0(col1, "\t", col2, "\t", col3)
            output.cat <- base::paste0(
                "INSIDE ", arg.user.setting$x, "(), SOME :: ARE MISSING AT BASIC FUNCTION POSITIONS:\n\n",
                "BASIC_FUN_NB\tFUN\tSTRING_BEFORE\n",
                base::paste(tempo.pos, collapse = "\n")
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
    if(base::length(in_other_fun_uni) > 0){
        pattern2.b <- base::paste(base::paste0(in_other_fun_uni, "\\s*\\("), collapse = "|") # split string according to basic function name as splitter
        res.b <- base::unlist(base::strsplit(x = ini, split = pattern2.b))
        res.b <- res.b[-base::length(res.b)] # the last split section is removed because nothing to test at the end (end of code)
        res2.b <- base::sapply(X = res.b, FUN = function(x){base::substr(x, base::nchar(x)-1, base::nchar(x))})
        base::names(res2.b) <- NULL
        tempo.log.b <- ! res2.b %in% "::"
        if(base::any(tempo.log.b)){
            col1.b <- base::which(tempo.log.b)
            col2.b <- in_other_fun[tempo.log.b]
            col3.b <- res.b[tempo.log.b]
            if( ! (base::length(col1.b) == base::length(col2.b) & base::length(col1.b) == base::length(col3.b) & base::length(col2.b) == base::length(col3.b))){
                tempo.cat <- base::paste0(
                    "INTERNAL ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: LENGTHS OF col1.b (", base::length(col1.b), "), col2.b (", base::length(col2.b), "), AND col3.b (", base::length(col3.b), "), SHOULD BE EQUAL\n")
                base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
            }
            tempo.pos.b <- base::paste0(col1.b, "\t", col2.b, "\t", col3.b)
            output.cat.b <- base::paste0(
                "INSIDE ", arg.user.setting$x, "(), SOME :: ARE MISSING AT OTHER FUNCTION POSITIONS:\n\n",
                "OTHER_FUN_NB\tFUN\tSTRING_BEFORE\n",
                base::paste(tempo.pos.b, collapse = "\n")
            )
        }else{
            output.cat.b <- NULL
        }
    }else{
        tempo.log.b <- FALSE
        output.cat.b <- NULL
    }
    # end analyse of :: before basic functions in x
    if(base::any(tempo.log) | base::any(tempo.log.b)){
        tempo.cat <- base::paste(output.cat, base::ifelse(base::is.null(output.cat) | base::is.null(output.cat.b), "", "\n\n"), output.cat.b)
        base::cat(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        base::cat("\n\nEVERYTHING SEEMS CLEAN\n\n")
    }
    # end main code
    # output
    # warning output
    base::options(warning.length = ini.warning.length)
    # end warning output
    # end output
}


