test_that("test if the python package is in the computer", {
    lib.path <- "."
    fun <- c(
        "reticulate::py_run_string", 
        "reticulate::use_python",
        "reticulate::import_from_path"
    )
    pkg.fun.name.list <- base::strsplit(fun, "::") # package in 1 and function in 2
    pkg.name <- sapply(X = pkg.fun.name.list, FUN = function(x){x[1]})
    pkg.log <- pkg.name %in% rownames(utils::installed.packages(lib.loc = "."))
    tempo <- pkg.name[ ! pkg.log]
    p1 <- if( ! all(pkg.log)){paste0(
        "ERROR IN ", 
        "python_pkg_check()", 
        "() OF THE cuteDev PACKAGE. REQUIRED PACKAGE", 
        ifelse(length(tempo) == 1L, paste0(":\n", tempo), paste0("S:\n", paste(tempo, collapse = "\n"))), 
        "MUST BE INSTALLED IN", 
        ifelse(length(lib.path) == 1L, "", " ONE OF THESE FOLDERS"), 
        ":\n", 
        paste(lib.path, collapse = "\n")
    )}
    p2 <- paste0("\n\n================\n\n",p1, "\n\n================\n\n")
    expected <- paste0(p1,p2)
    
    expect_error(object = python_pkg_check(
        req.package = "serpentine", 
        python.exec.path = ".", 
        python.lib.path = ".",
        lib.path = lib.path
    ), regexp = NULL)
})
