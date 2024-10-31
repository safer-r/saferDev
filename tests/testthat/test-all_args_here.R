    test_that("all_args_here()", {
    source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R")

  # Simple examples
    testthat::expect_no_error(all_args_here(x = test2)) # default values
    testthat::expect_no_error(all_args_here(
        x = test2, # R function
        export = TRUE, # export the data frame into a .tsv file?
        path_out = ".", # pathway of the folder where to export the data frame
        df_name = "a.tsv", # name of the exported data frame file
        overwrite = FALSE, # Overwrite potential df_name file already existing in path_out?
        lib_path = NULL, # absolute pathways of the directories containing the required packages if not in the default directories
        safer_check = FALSE # perform some "safer" checks? Warning : always set this argument to FALSE if all_args_here() is used inside another safer function.
    ))

    testthat::expect_error(all_args_here(x = "a"))
    testthat::expect_error(all_args_here(x = test2, export = "a"))
    testthat::expect_error(all_args_here(x = test2, path_out = 1))
    testthat::expect_error(all_args_here(x = test2, path_out = "a"))
    testthat::expect_error(all_args_here(x = test2, df_name = 1))
    testthat::expect_error(all_args_here(x = test2, overwrite = "a"))
    testthat::expect_error(all_args_here(x = test2, lib_path = "a"))
    testthat::expect_error(all_args_here(x = test2, safer_check = "a"))

  # sophisticated example
    result1 <- capture.output(all_args_here(x = test2))
    expected1 <- c(
        "", 
        "", 
        "INSIDE test(), SOME :: OR ::: ARE MISSING AT BASIC FUNCTION POSITIONS:", 
        "", 
        "LINE\tFUN\t\tSTRING_BEFORE", 
        "3\tgregexpr\t\tmatches <- " , 
        "6\tregmatches\t\tmatched_strings <- " , 
        "8\tsum\t\t", 
        "15\tsub\t\tresult <- " , 
        "16\trange\t\t", 
        "19\treturn\t\t", 
        "", 
        "INSIDE test(), SOME :: OR ::: ARE MISSING AT OTHER FUNCTION POSITIONS:", 
        "", 
        "LINE\tFUN\t\tSTRING_BEFORE", 
        "13\troc1\t\tbase::length(", 
        "17\troc4\t\ttempo.cat <- base::paste0(\"IAGE\\nLENGTHS OF roc00() (\", base::ks.test(" ,  
        ""
    )
    testthat::expect_equal(result1, expected1)
})

