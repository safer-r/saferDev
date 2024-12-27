#' @title .noclean_functions
#' @description
#' Indicate if function names are inside quotes or after $
#' @param col1 vector of strings.
#' @param col2 vector of strings of the function names.
#' @param col3 vector of strings of the code before the function name.
#' @param ini vector of string of the initial function code analyzed.
#' @returns A logical vector indicating if function names of col2 are inside quotes or after $ (TRUE) in ini or not (FALSE). Can be length 0
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @examples
#' \dontrun{ # Example that shouldn't be run because this is an internal function
#' source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R")
#' .noclean_functions(col1 =  c(15, 17), col2 = c("gregexpr", "regmatches"), col3 = c("matches <- ",  "matched_strings <- " ), ini = utils::capture.output(test))
#' }
#' @keywords internal
#' @rdname internal_function
.noclean_functions <- function(
    col1, 
    col2, 
    col3, 
    ini
){
    # AIM
    # Indicate if function names are inside quotes or after $
    # ARGUMENTS
    # col1: vector of integers of code line number
    # col2: vector of strings of the function names
    # ini: vector of string of the initial function code analyzed.
    # RETURN
    # A logical vector indicating if function names of col2 are inside quotes or after $ in ini.
    # DEBUGGING
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; col1 = c(15, 17) ; col2 = c("gregexpr", "regmatches") ; col3 = c("matches <- ",  "matched_strings <- " ) ; ini = utils::capture.output(test)
    # source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test.R") ; col1 = c(15, 22, 22) ; col2 = c("gregexpr", "col1", "roc1") ; col3 = c("matches <- ",  "matched_strings <- " ) ; ini = utils::capture.output(test)
    output <- base::vector(mode = "logical", length = 0) # here sum(output) = 0
    if(base::length(col1) > 0){
        # detection of a$fun() pattern
        tempo.log <- base::grepl(x = col3, pattern = "([a-zA-Z]|\\.[a-zA-Z._])[a-zA-Z0-9._]* *\\$ *$")
        # ([a-zA-Z]|\\.[a-zA-Z._]) is for the begining of R function name: either any single alphabet character or a dot and any single alphabet character or dot (because .. is ok for function name) or underscore (because ._ is ok for function name). Starting "dot and num" or underscore is not authorized for function name
        # [a-zA-Z0-9._]* is The rest of the function name: any several of these characters or nothing
        #  *\\$ *$ means end of the string is any space or not, a $ symbol and any space or not
        if(base::any(tempo.log, na.rm = TRUE)){
            output <- tempo.log
        }else{
            output <- base::rep(FALSE, base::length(col1))
        }
        # end detection of a$fun() pattern
        # detection of functions between quotes
        tempo.ini.order <- 1:base::length(col1) # to recover the initial order at the end
        tempo.order <- base::order(col2) # order according to function name
        tempo.ini.order <- tempo.ini.order[tempo.order]
        tempo.col1 <- col1[tempo.order] # reorder to work only once with duplicated functions
        tempo.col2 <- col2[tempo.order] # reorder to work only once with duplicated functions
        tempo.ini <- ini
        pos.rm <- NULL # positions to remove (functions between quotes)
        for(i2 in 1:base::length(tempo.col1)){
            pattern1 <- base::paste0(tempo.col2[i2], " *\\(")
            lines.split <- base::strsplit(tempo.ini[tempo.col1[i2]], split = pattern1)[[1]][1]
            # if odds number of quotes, it means that # has broken the string in the middle of a quoted part
            double.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = lines.split, pattern = '"') # here FALSE means even number of quotes, thus that the function is not between quotes, thus has to be kept. TRUE means that the function is between quotes, thus has to be removed
            simple.quote.test <- saferDev:::.has_odd_number_of_quotes(input_string = lines.split, pattern = "'") # idem
            odds.quotes.log <- double.quote.test |  simple.quote.test
            if(odds.quotes.log == FALSE){
                pos.rm <- base::c(pos.rm, i2)
            }else{
                pos.rm <- base::c(pos.rm, NA) # NA means betwwen quotes. pos.rm will becomes double if integer added, otherwise remains logical. Thus, do not use any()
            }
            tempo.ini[tempo.col1[i2]] <- base::sub(pattern = pattern1, replacement = "", x = tempo.ini[tempo.col1[i2]], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove the first fonction in the line, in case of identical function names in a code line. Like, that, the next round for the next same function can be easily tested for "between quotes" 
        }
        # initial order
        pos.rm.fin <- pos.rm[base::order(tempo.ini.order)]
        # end initial order
        if(base::any(base::is.na(pos.rm.fin), na.rm = TRUE)){
            output <- output | base::is.na(pos.rm.fin)
        }
        # end detection of functions between quotes
    }
    base::return(output)
}




