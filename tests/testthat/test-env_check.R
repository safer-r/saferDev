testthat::test_that("env_check()", {
    mean <- 2
    result <- env_check(
        pos = 1,
        safer_check = TRUE
    )
    expected <- "SOME VARIABLES OF THE CHECKED ENVIRONMENT ARE ALSO PRESENT IN :\npackage:base: mean\n"
    testthat::expect_equal(result, expected)
    base::rm(mean)
    base::rm(result)
    base::rm(expected)

    sum <- "change"
    result2 <- env_check(
        pos = 1,
        safer_check = FALSE
    )
    expected2 <- "SOME VARIABLES OF THE CHECKED ENVIRONMENT ARE ALSO PRESENT IN :\npackage:base: sum\n"
    testthat::expect_equal(result2, expected2)
    base::rm(sum)
    base::rm(result2)
    base::rm(expected2)

    list_2 <- 0
    result3 <- env_check(
        pos = 1, # The environment two steps above the current local environment
        safer_check = FALSE
    )
    expected3 <- NULL
    testthat::expect_equal(result3, expected3)
    base::rm(list_2)
    base::rm(result3)
    base::rm(expected3)

    # Examples inside a function
    # env_check() checks if the object names inside the fun1 function 
    # exist in the .GlobalEnv environment and above:
    fun1 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1)} 
    result3 <- fun1()
    expected3 <- "SOME VARIABLES OF THE CHECKED ENVIRONMENT ARE ALSO PRESENT IN :\npackage:stats: t.test\npackage:base: mean\n"
    testthat::expect_equal(result3, expected3)
    base::rm(result3)
    base::rm(expected3)

    # env_check() checks if the object names inside the environment one step above fun2(), 
    # here .GlobalEnv, exist in the upper environments of .GlobalEnv:
    fun2 <- function(){sum <- 0 ; env_check(pos = 2)} 
    result4 <- fun2()
    expected4 <- NULL
    testthat::expect_equal(result4, expected4)
    base::rm(result4)
    base::rm(expected4)

    # With the name of the function fun3 indicated in the message:
    fun3 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1, name = "fun3")}
    result5 <- fun3()
    expected5 <- "SOME VARIABLES OF fun3 ARE ALSO PRESENT IN :\npackage:stats: t.test\npackage:base: mean\n"
    testthat::expect_equal(result5, expected5)
    base::rm(result5)
    base::rm(expected5)

    # Alternative way:
    fun4 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1, name = base::as.character(base::sys.calls()[[base::length(base::sys.calls())]]))}
    result6 <- fun4()
    expected6 <- "SOME VARIABLES OF fun4 ARE ALSO PRESENT IN :\npackage:stats: t.test\npackage:base: mean\n"
    testthat::expect_equal(result6, expected6)
    base::rm(result6)
    base::rm(expected6)

    # A way to have the name of the tested environment according to test.pos value:
    fun7 <- function(){
        min <- "a"
        fun8 <- function(){
            test.pos <- 2 # value 1 tests the fun8 env, 2 tests the fun7 env.
            range <- "a"
            env_check(pos = test.pos, name = if(base::length(base::sys.calls()) >= test.pos){
                base::as.character(base::sys.calls()[[base::length(base::sys.calls()) + 1 - test.pos]])
            }else{
                base::search()[(1:base::length(base::search()))[test.pos - base::length(base::sys.calls())]]
            }) 
        }
        fun8()
    }
    result7 <- fun7()
    expected7 <- "SOME VARIABLES OF fun7 ARE ALSO PRESENT IN :\npackage:base: min\n"
    testthat::expect_equal(result7, expected7)
    base::rm(result7)
    base::rm(expected7)

    testthat::expect_error(env_check(pos = 0, safer_check = 'FALSE'
))
    testthat::expect_error(env_check(pos = 0, name = NA,safer_check = FALSE
))
    testthat::expect_error(env_check(pos = 0, name = "", safer_check = FALSE
))


})
