# smoke test of the real curl_bracket_check implementation
res <- pkgload::load_all(path = "C:/Users/gmillot/Documents/Git_projects/safer-r/saferDev", quiet = TRUE)

fun2 <- function(x){ if(x > 1) base::return(x) }
out <- saferDev::curl_bracket_check(x = fun2, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = NULL)
cat("RETURNED CLASS:", base::class(x = out), "\n")
cat("RETURNED CODE:\n", base::deparse(expr = out), "\n")
cat("out(2):", out(2), "\n")

# clean function -> NULL
fun1 <- function(x){ if(x > 1){ base::return(x) } else { base::return(0) } }
out2 <- saferDev::curl_bracket_check(x = fun1, safer_check = FALSE, lib_path = NULL, error_text = "", export = FALSE, out_path = NULL)
cat("CLEAN -> ", if(base::is.null(x = out2)) "NULL" else "NOT NULL", "\n")

# export test in a temp dir
td <- base::tempdir()
out3 <- saferDev::curl_bracket_check(x = fun2, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = td)
cat("EXPORT RETURNS: ", if(base::is.null(x = out3)) "NULL" else "NOT NULL", "\n")
cat("FILES IN TMP:\n")
base::print(base::list.files(path = td, pattern = "_safer.R$"))
fp <- base::file.path(td, "fun2_safer.R")
if(base::file.exists(fp)){
    cat("--- FILE CONTENT ---\n")
    base::cat(base::readLines(con = fp, n = 20, warn = FALSE), sep = "\n")
    cat("\n--- END ---\n")
    e <- base::new.env(parent = base::globalenv())
    base::sys.source(fp, envir = e)
    cat("sourced fun2_safer(2):", e$fun2_safer(2), "\n")
}
# clean + export: no file
out4 <- saferDev::curl_bracket_check(x = fun1, safer_check = FALSE, lib_path = NULL, error_text = "", export = TRUE, out_path = td)
cat("CLEAN+EXPORT RETURNS: ", if(base::is.null(x = out4)) "NULL" else "NOT NULL", "\n")
cat("fun1_safer.R exists?", base::file.exists(base::file.path(td, "fun1_safer.R")), "\n")