test_that("test .internal_function.R", {

    # .pack_and_function_check()
    fun_wrong1 <- "geom_point"  # incorrect input
    fun_wrong2 <- "ggplot2::non_existent_function"  # incorrect function name
    fun_good <- "ggplot2::geom_point" # correct input
    path_wrong <- "path/to/library"  # incorrect input
    path_good <- NULL  # incorrect input

    expect_no_error(.pack_and_function_check(
        fun = fun_good, 
        lib.path = path_good,
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))
    expect_error(.pack_and_function_check(
        fun = fun_wrong1, 
        lib.path = path_good,
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))
    expect_error(.pack_and_function_check(
        fun = fun_wrong2, 
        lib.path = path_good,
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))
    expect_error(.pack_and_function_check(
        fun = fun_good, 
        lib.path = path_wrong,
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

    # .base_op_check()
    expect_no_error(.base_op_check(
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))
    
})