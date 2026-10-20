res <- pkgload::load_all(path = "C:/Users/gmillot/Documents/Git_projects/safer-r/saferDev", quiet = TRUE)
setwd(tempdir(check = TRUE))
fun1 <- function(x){
    if(x > 1){
        base::return(x)
    }else{
        base::return(0)
    }
    for(i in 1:3){
        base::print(i)
    }
    while(x > 1){
        x <- x - 1
    }
    repeat{
        x <- x + 1
    }
    base::sapply(X = 1:3, FUN = function(y){
        y * 2
    })
}
fun2 <- function(x){
    if(x > 1)
        base::return(x)
    else
        base::return(0)
    for(i in 1:3)
        base::print(i)
    while(x > 1)
        x <- x - 1
    repeat
        x <- x + 1
    base::sapply(X = 1:3, FUN = function(y) y * 2)
}
out1 <- curl_bracket_check(x = fun1)
cat("fun1 NULL:", is.null(out1), "\n")
fun2_safer <- curl_bracket_check(x = fun2)
cat("fun2_safer is function:", is.function(fun2_safer), "\n")
cat("fun2(1):", fun2(1), "\n")
cat("fun2_safer(1):", fun2_safer(1), "\n")
m2 <- suppressMessages(curl_bracket_check(x = fun2_safer))
cat("fun2_safer clean:", is.null(m2), "\n")
invisible(suppressMessages(curl_bracket_check(x = fun2, export = TRUE, out_path = ".")))
cat("file exists:", file.exists("fun2_safer.R"), "\n")
cat("--- file ---\n")
cat(paste(readLines("fun2_safer.R"), collapse = "\n"), "\n")
e <- new.env(parent = globalenv())
sys.source(file = "fun2_safer.R", envir = e, chdir = FALSE)
cat("sourced fun2_safer(1):", e$fun2_safer(1), "\n")