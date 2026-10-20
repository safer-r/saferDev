res <- pkgload::load_all(path = "C:/Users/gmillot/Documents/Git_projects/safer-r/saferDev", quiet = TRUE)
check_fun <- function(f, expected_findings = NULL){
    out <- base::suppressMessages(expr = saferDev::curl_bracket_check(x = f, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = NULL))
    base::invisible(out)
}
# collect results
cases <- list(
    fun2 = function(x){ if(x > 1) base::return(x) },
    fun14 = function(x){ if(x > 1) base::return(x) else base::return(0) },
    f28 = function(x){ if(a) if(b) x else y },
    f29 = function(x){ repeat repeat x <- x + 1 },
    fun18 = function(f = function(y) y) f(1),
    fun16 = function(x){ y <- if(x > 1) 1 else 2 ; y },
    fun19 = function(x){ if(x > 1) NULL },
    fun23 = function(x){ repeat x <- x + 1 },
    fun30 = function(x){ if(x > 1) x <- 1 else x <- 2 ; x },
    f32 = function(x){ if(x > 1) for(j in 1:3) print(j) else x },
    f33 = function(x){ if(x > 1) while(x < 3) x <- x + 1 },
    f34 = function(x){ if(x > 1) 1 },
    fun19b = function(x){ if(x > 1) NULL },
    fun11 = function(x){ base::sapply(X = 1:3, FUN = \(y) y * 2) },
    f39 = function(x){ if(a) .Primitive('+') },
    fun27 = function(x) .Primitive('+') + 1,
    f41 = function(x){ if(a) x -> y },
    f42 = function(x){ if(a) y <- if(b) 1 else 2 },
    f43 = function(x){ if(a) { x } else if(b) y },
    f44 = function(x){ if(a) x else if(b) y else z },
    f45 = function(x){ for(i in x) if(a) b },
    f46 = function(x){ if(a) x ; y },
    f48 = function(x){ if(a) (x) },
    f49 = function(x){ if(a) "x{y}" },
    f50 = function(x){ y <- "éé" ; if(a) x }
)
all_ok <- TRUE
for(n in base::names(x = cases)){
    f <- cases[[n]]
    out <- check_fun(f)
    if(base::is.null(x = out)){
        cat("[", n, "] NULL returned - UNEXPECTED\n")
        all_ok <- FALSE
        next
    }
    # verify the modified function passes curl_bracket_check cleanly
    m <- base::suppressMessages(expr = saferDev::curl_bracket_check(x = out, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = NULL))
    if( ! base::is.null(x = m)){
        cat("[", n, "] MODIFIED FUNCTION STILL NOT CLEAN\n")
        all_ok <- FALSE
    }
    # verify the modified function is a function
    if(base::is.function(x = out) == FALSE){
        cat("[", n, "] NOT A FUNCTION\n")
        all_ok <- FALSE
    }
}
cat("ALL OK:", all_ok, "\n")