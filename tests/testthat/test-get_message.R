testthat::test_that("get_message()", {
    char1 <- "wilcox.test(c(1,1,3), c(1, 2, 4), paired = TRUE)" # single character string
    char2 <- "message('ahah')"
    char3 <- "message('ahah'); warning('ohoh')"
    char4 <- "sum(1, 2, 3)"
    char5 <- "ggplot2::ggplot(data = data.frame(X = 1:10, stringsAsFactors = TRUE), mapping = ggplot2::aes(x = X)) + ggplot2::geom_histogram()"

    testthat::expect_error(saferDev::get_message(data = char1, kind = "error", print.no = TRUE, text = "IN A", safer_check = 'TRUE'))
    testthat::expect_error(saferDev::get_message( kind = "warning", print.no = TRUE, text = "IN A", safer_check = FALSE))
    testthat::expect_no_error(saferDev::get_message(data = char1, kind = "message", print.no = TRUE,text = NULL, safer_check = FALSE))
    testthat::expect_error(saferDev::get_message(data = char1, kind = NULL, print.no = TRUE,text = NULL, safer_check = FALSE))
    testthat::expect_no_error(saferDev::get_message(data = "", kind = "error", print.no = TRUE,text = NULL, safer_check = FALSE)) # because data can be ""
    testthat::expect_error(saferDev::get_message(data = NOT_CHARACTER, kind = "error", print.no = TRUE,text = NULL, safer_check = FALSE))

    result1 <- saferDev::get_message(data = char1, header = FALSE)
    testthat::expect_null(result1)
    
    result2 <- saferDev::get_message(data = char1, print.no = TRUE)
    expected2 <- "NO ERROR MESSAGE REPORTED"
    testthat::expect_equal(result2,expected2)
    
    result3 <- saferDev::get_message(data = char2, print.no = TRUE, text = "IN A")
    expected3 <- "NO ERROR MESSAGE REPORTED IN A"
    testthat::expect_equal(result3,expected3)

    result4 <- saferDev::get_message(data = char2, kind = "error", print.no = TRUE, text = "IN A")
    expected4 <- "NO ERROR MESSAGE REPORTED IN A"
    testthat::expect_equal(result4,expected4)
    
    result5 <- saferDev::get_message(
        data = char1, 
        kind = "warning", 
        header = FALSE,
        print.no = TRUE, 
        text = "IN A",
        env = NULL,
        safer_check = TRUE
    )
    expected5 <- "simpleWarning in wilcox.test.default(c(1, 1, 3), c(1, 2, 4), paired = TRUE): cannot compute exact p-value with zeroes\n"
    testthat::expect_equal(result5, expected5)

    result6 <- saferDev::get_message(
        data = char3, 
        kind = "warning", 
        header = FALSE,
        print.no = TRUE, 
        text = "IN A",
    )
    expected6 <- "simpleWarning in base::eval(base::parse(text = data), envir = if (base::is.null(env)) {: ohoh\n"
    testthat::expect_equal(result6, expected6)

    result7 <- saferDev::get_message(data = char4, kind = "error",print.no = TRUE, text = "IN A")
    expected7 <- "NO ERROR MESSAGE REPORTED IN A"
    testthat::expect_equal(result7,expected7)

    result8 <- saferDev::get_message(data = char5, kind = "message",print.no = TRUE, text = "IN INSTRUCTION 1")
    expected8 <- "NO STANDARD (NON ERROR AND NON WARNING) MESSAGE REPORTED IN INSTRUCTION 1"
    testthat::expect_equal(result8,expected8)

    result11 <- saferDev::get_message("arg_test(fun = 1, arg = argum, val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected11 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test()\nTHE fun OBJECT MUST BE MODE character\n\n================\n\n\n"
    testthat::expect_equal(result11, expected11)

    result12 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = 1, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected12 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS:\n1\n\n================\n\n\n"
    testthat::expect_equal(result12, expected12)

    result13 <- saferDev::get_message("arg_test(fun = f, arg = argum, val = value, expect.error = error, parall = TRUE, thread.nb = 0.3, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected13 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nthread.nb PARAMETER MUST EQUAL OR GREATER THAN 1: 0.3\n\n================\n\n\n"
    testthat::expect_equal(result13, expected13)

    result14 <- saferDev::get_message("arg_test(fun = f, arg = '', val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected14 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nTHIS ARGUMENT\narg\nCANNOT CONTAIN \"\"\n\n================\n\n\n"
    testthat::expect_equal(result14, expected14)

    result15 <- saferDev::get_message("arg_test(fun = 'not_function', arg = argum, val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected15 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nCHARACTER STRING IN fun ARGUMENT DOES NOT EXIST IN THE R WORKING ENVIRONMENT: not_function\n\n================\n\n\n"
    testthat::expect_equal(result15, expected15)

    result16 <- saferDev::get_message("arg_test(fun = f2, arg = argum, val = value2, expect.error = error2, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected16 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nSOME OF THE STRINGS IN arg ARE NOT ARGUMENTS OF fun\nfun ARGUMENTS: x y ...\nPROBLEMATIC STRINGS IN arg: incomparables\n\n================\n\n\n"
    testthat::expect_equal(result16, expected16)

    result17 <- saferDev::get_message("arg_test(fun = f2, arg = base::character(0), val = value2, expect.error = error2, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected17 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\narg ARGUMENT CANNOT BE LENGTH 0\n\n================\n\n\n"
    testthat::expect_equal(result17, expected17)

    result18 <- saferDev::get_message("arg_test(fun = f, arg = argum, val = value, expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NA, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected18 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test()\nTHE res.path OBJECT MUST BE MODE character AND THE res.path OBJECT CONTAINS NA WHILE NOT AUTHORIZED\n\n================\n\n\n"
    testthat::expect_equal(result18, expected18)

    result19 <- saferDev::get_message("arg_test(fun = f, arg = argum, val =  base::list(base::c(1, 2, 3), base::list(a = 1, b = 2)), expect.error = error, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected19 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nLENGTH OF COMPARTMENT 2 OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF COMPARTMENT 2 OF expect.error ARGUMENT:\nHERE IT IS: 2 VERSUS 3\n\n================\n\n\n"
    testthat::expect_equal(result19, expected19)

    result20 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = base::list(base::matrix(1:4, 2, 2)), expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected20 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nCOMPARTMENT 1 OF val ARGUMENT MUST BE A VECTOR OR A LIST\n\n================\n\n\n"
    testthat::expect_equal(result20, expected20)

    result21 <- saferDev::get_message("arg_test(fun = f2, arg = base::c('x'), val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected21 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nLENGTH OF arg ARGUMENT MUST BE IDENTICAL TO LENGTH OF val ARGUMENT:\nHERE IT IS: 1 VERSUS 2\n\n================\n\n\n"
    testthat::expect_equal(result21, expected21)

    result22 <- saferDev::get_message("arg_test(fun = f2, arg = base::rep(argum2,22), val = rep(value2, 22), expect.error = rep(error2,22), parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected22 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nCANNOT TEST MORE THAN 43 ARGUMENTS IF THEY ALL HAVE AT LEAST 2 VALUES EACH\nHERE THE NUMBER IS: 44\n\n================\n\n\n"
    testthat::expect_equal(result22, expected22)

    result23 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = base::list(x = base::list(1,2,3,4), y = base::list(3,5)), expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected23 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nLENGTH OF COMPARTMENT 2 OF val ARGUMENT MUST BE IDENTICAL TO LENGTH OF COMPARTMENT 2 OF expect.error ARGUMENT:\nHERE IT IS: 2 VERSUS 3\n\n================\n\n\n"
    testthat::expect_equal(result23, expected23)

    result24 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error5, parall = FALSE, thread.nb = 4, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = '.', lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected24 <- "ERROR MESSAGE REPORTED:\nIn expect.error[[1]][[i1]] | expect.error[[2]][[i2]] : \n  operations are possible only for numeric, logical or complex types\n"
    testthat::expect_equal(result24, expected24)

    result25 <- saferDev::get_message("arg_test(fun = f, arg = argum, val = value, expect.error = error, parall = TRUE, thread.nb = NULL, print.count = 10, plot.fun = FALSE, export = FALSE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected25 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nres.path ARGUMENT MUST BE SPECIFIED IF parall ARGUMENT IS TRUE\n\n================\n\n\n"
    testthat::expect_equal(result25, expected25)

    result26 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = TRUE, res.path = NULL, lib_path = NULL, safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected26 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nres.path ARGUMENT MUST BE SPECIFIED IF export ARGUMENT TRUE\n\n================\n\n\n"
    testthat::expect_equal(result26, expected26)

    result27 <- saferDev::get_message("arg_test(fun = f2, arg = argum2, val = value2, expect.error = error2, parall = FALSE, thread.nb = NULL, print.count = 10, plot.fun = TRUE, export = FALSE, res.path = NULL, lib_path = 'not_real_path', safer_check = FALSE)", kind = "error", print.no = TRUE, text = NULL)
    expected27 <- "ERROR MESSAGE REPORTED:\nError : \n\n================\n\nERROR IN arg_test() OF THE saferDev PACKAGE\nDIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXISTS:\nnot_real_path\n\n================\n\n\n"
    testthat::expect_equal(result27, expected27)
















})
