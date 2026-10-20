res <- covr::package_coverage(path = "C:/Users/gmillot/AppData/Local/Temp/opencode/saferDev_cov", type = "tests", quiet = TRUE, clean = TRUE)
cat("TOTAL COVERAGE:", covr::percent_coverage(x = res), "%\n")
sub1 <- res[res$filename == "R/curl_bracket_check.R"]
sub2 <- res[res$filename == "R/intern_.find_construct_body.R"]
cat("curl_bracket_check.R:", covr::percent_coverage(x = sub1), "%\n")
cat("intern_.find_construct_body.R:", covr::percent_coverage(x = sub2), "%\n")
df <- base::as.data.frame(x = res)
zero <- df[df$value == 0, c("filename", "first_line", "last_line"), drop = FALSE]
zero <- zero[zero$filename %in% c("R/curl_bracket_check.R", "R/intern_.find_construct_body.R"), ]
if(nrow(zero) == 0){
    cat("MY FILES: NO UNCOVERED LINES\n")
}else{
    cat("--- MY UNCOVERED ---\n")
    base::print(x = base::unique(x = zero), row.names = FALSE)
}