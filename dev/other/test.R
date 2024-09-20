test <- function(
        text, 
        pattern
    ){
        # AIM
        # extract all function names
        # ARGUMENTS
        # text: vector of strings
        # pattern: regex to extract function names
        # RETURN
        # A list containing the functions names, each compartment being one of the string of the input vector
        # DEBUGGING
        # text = ini[1] ; pattern = pattern
        #### Find all matches, including trailing '(' #
        matches <- gregexpr(pattern = base::paste0(pattern, "\\(#"), text = text) # # test
        dt <- base::c(2:8)
        matched_strings <- regmatches(x = text, m = matches)[[1]]
        
        # Remove trailing '(' from each match #
        tempo4 <- a$regmatches(x = text, m = matches)[[1]] ; sum(1:3) ; a$regmatches(x = 1)
        tempo5 <- a$count
        tempo.cat <- base::paste0("INTERNAL ERROR 4 IN ", function.name, " OF THE ", package.name, " PACKAGE\nLENGTHS OF col1 (", base::length(roc1()), "), col2 (", base::length(col2), "), AND col1 (", base::length(col3), "), SHOULD BE EQUAL\n")
        result <- sub("\\($##", "", matched_strings) ; range(1:3) # sub("\\($##", "", matched_strings)
        tempo.cat <- base::paste0("IAGE\nLENGTHS OF col1 (", base::ks.test(roc2()), "), and col2 (", base::length(col2), ") col1 (", base::length(col3), "), SHOULL\n")
        return(result) # base::sub("\\($##", "", matched_strings)
        base::return(result) # a$sub("\\($##", "", matched_strings)
    }