# smoke test of the real lambda_check implementation
res <- pkgload::load_all(path = "C:/Users/gmillot/Documents/Git_projects/safer-r/saferDev", quiet = TRUE)

# lambda with srcref: reported and corrected
fun2 <- eval(parse(text = "function(x){ if(x > 1) base::return(\\(y) y + x) ; NULL }", keep.source = TRUE))
out <- saferDev::lambda_check(x = fun2, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
cat("RETURNED CLASS:", base::class(x = out), "\n")
cat("out(2)(3):", out(2)(3), "\n", sep = " ")

# clean function -> NULL
fun1 <- eval(parse(text = "function(x){ if(x > 1){ base::return(x) } else { base::return(0) } }", keep.source = TRUE))
out2 <- saferDev::lambda_check(x = fun1, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
cat("CLEAN -> ", if(base::is.null(x = out2)) "NULL" else "NOT NULL", "\n")

# export test in a temp dir
td <- base::tempdir()
out3 <- saferDev::lambda_check(x = fun2, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = td)
cat("EXPORT RETURNS: ", if(base::is.null(x = out3)) "NULL" else "NOT NULL", "\n")
fp <- base::file.path(td, "fun2_safer.R")
if(base::file.exists(fp)){
    cat("--- FILE CONTENT ---\n")
    base::cat(base::readLines(con = fp, n = 20, warn = FALSE), sep = "\n")
    cat("\n--- END ---\n")
    e <- base::new.env(parent = base::globalenv())
    base::sys.source(file = fp, envir = e, chdir = FALSE)
    cat("sourced fun2_safer(2)(3):", e$fun2_safer(2)(3), "\n")
    base::unlink(x = fp)
}
# clean + export: no file
invisible(saferDev::lambda_check(x = fun1, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = td))
cat("NO FILE FOR CLEAN:", ! base::file.exists(base::file.path(td, "fun1_safer.R")), "\n")
# backquoted `\\` immune
fun4 <- eval(parse(text = "function(x){ base::`\\\\`(x) }", keep.source = TRUE))
out4 <- saferDev::lambda_check(x = fun4, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
cat("BACKQUOTE -> ", if(base::is.null(x = out4)) "NULL (clean)" else "NOT NULL", "\n")
# no srcref: nothing reported
fun5 <- function(x){ base::sapply(X = 1:3, FUN = \(y) y * 2) }
out5 <- saferDev::lambda_check(x = fun5, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = ".")
cat("NO SRCREF -> ", if(base::is.null(x = out5)) "NULL (nothing reported, as documented)" else "NOT NULL", "\n")
cat("SMOKE DONE\n")
