test_that("all_args_here()", {
    source("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/test2.R")
    expected3 <- read.table("https://raw.githubusercontent.com/safer-r/saferDev/main/dev/other/all_args_here_on_all_args_here_res.tsv", sep = "\t", header = TRUE)

  # Simple examples
    result1 <- saferDev::get_message("all_args_here(x = test2)", kind = "error", print.no = TRUE, text = NULL)
    expected1 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN all_args_here() OF THE saferDev PACKAGE\nTHE TESTED FUNCTION test2 SEEMS TO HAVE A WRITTING ERROR IN LINE 22 AND FUNCTION length.\nPLEASE, RUN THE TESTED FUNCTION FIRST.\n\n================\n\n\n"
    testthat::expect_equal(result1, expected1)

    result2 <- saferDev::get_message("all_args_here(x = mean)", kind = "error", print.no = TRUE, text = NULL)
    expected2 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN all_args_here() OF THE saferDev PACKAGE\nCANNOT GET THE ARGUMENTS OF A FUNCTION THAT IS NOT ASSOCIATED TO ITS PACKAGE IN LINE 2:\n\nUseMethod(\"mean\")\n\n1) PLEASE, RUN saferDev::colons_check(mean)\n2) ADD THE MISSING <PACKAGE>::<FUNCTION> (OR <PACKAGE>:::<FUNCTION> FOR FUNCTION STARTING BY A DOT)\n3) RERUN saferDev::all_args_here(mean)\n\n================\n\n\n"
    testthat::expect_equal(result2, expected2)

    testthat::expect_error(all_args_here(x = "a"))
    testthat::expect_error(all_args_here(x = test2, export = "a"))
    testthat::expect_error(all_args_here(x = test2, path_out = 1))
    testthat::expect_error(all_args_here(x = test2, path_out = "a"))
    testthat::expect_error(all_args_here(x = test2, df_name = 1))
    testthat::expect_error(all_args_here(x = test2, overwrite = "a"))
    testthat::expect_error(all_args_here(x = test2, lib_path = "a"))
    testthat::expect_error(all_args_here(x = test2, safer_check = "a"))

  # sophisticated example

    testthat::expect_no_error(all_args_here(
        x = all_args_here, # R function
        export = TRUE, # export the data frame into a .tsv file?
        path_out = ".", # pathway of the folder where to export the data frame
        df_name = "a.tsv", # name of the exported data frame file
        overwrite = FALSE, # Overwrite potential df_name file already existing in path_out?
        lib_path = NULL, # absolute pathways of the directories containing the required packages if not in the default directories
        safer_check = FALSE # perform some "safer" checks? Warning : always set this argument to FALSE if all_args_here() is used inside another safer function.
    ))

    all_args_here(
        x = all_args_here, # R function
        export = TRUE, # export the data frame into a .tsv file?
        path_out = ".", # pathway of the folder where to export the data frame
        df_name = "a.tsv", # name of the exported data frame file
        overwrite = TRUE, # Overwrite potential df_name file already existing in path_out?
        lib_path = NULL, # absolute pathways of the directories containing the required packages if not in the default directories
        safer_check = FALSE # perform some "safer" checks? Warning : always set this argument to FALSE if all_args_here() is used inside another safer function.
    )
    result3 <- read.table("./a.tsv", sep = "\t", header = TRUE)
    testthat::expect_equal(result3, expected3)
})

