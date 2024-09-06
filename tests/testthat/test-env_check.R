test_that("env_check()", {
    mean <- 2
    result <- env_check(
        pos = 1,
        safer_check = TRUE
    )
    expected <- "SOME VARIABLES OF THE CHECKED ENVIRONMENT ARE ALSO PRESENT IN :\npackage:base: mean\n"
    expect_equal(result, expected)
    rm(mean)
    rm(result)
    rm(expected)

    sum <- "change"
    result2 <- env_check(
        pos = 1,
        safer_check = FALSE
    )
    expected2 <- "SOME VARIABLES OF THE CHECKED ENVIRONMENT ARE ALSO PRESENT IN :\npackage:base: sum\n"
    expect_equal(result2, expected2)
    rm(sum)
    rm(result2)
    rm(expected2)

    # Examples inside a function
    # env_check() checks if the object names inside the fun1 function 
    # exist in the .GlobalEnv environment and above:
    fun1 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1)} 
    result3 <- fun1()
    expected3 <- "SOME VARIABLES OF THE CHECKED ENVIRONMENT ARE ALSO PRESENT IN :\npackage:stats: t.test\npackage:base: mean\n"
    expect_equal(result3, expected3)
    rm(result3)
    rm(expected3)

    # env_check() checks if the object names inside the environment one step above fun2(), 
    # here .GlobalEnv, exist in the upper environments of .GlobalEnv:
    fun2 <- function(){sum <- 0 ; env_check(pos = 2)} 
    result4 <- fun2()
    expected4 <- NULL
    expect_equal(result4, expected4)
    rm(result4)
    rm(expected4)

    # With the name of the function fun3 indicated in the message:
    fun3 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1, name = "fun3")}
    result5 <- fun3()
    expected5 <- "SOME VARIABLES OF fun3 ARE ALSO PRESENT IN :\npackage:stats: t.test\npackage:base: mean\n"
    expect_equal(result5, expected5)
    rm(result5)
    rm(expected5)

    # Alternative way:
    fun4 <- function(){t.test <- 0 ; mean <- 5 ; env_check(pos = 1, name = as.character(sys.calls()[[length(sys.calls())]]))}
    result6 <- fun4()
    expected6 <- "SOME VARIABLES OF fun4 ARE ALSO PRESENT IN :\npackage:stats: t.test\npackage:base: mean\n"
    expect_equal(result6, expected6)
    rm(result6)
    rm(expected6)

    # A way to have the name of the tested environment according to test.pos value:
    fun7 <- function(){
        min <- "a"
        fun8 <- function(){
            test.pos <- 2 # value 1 tests the fun8 env, 2 tests the fun7 env.
            range <- "a"
            env_check(pos = test.pos, name = if(length(sys.calls()) >= test.pos){
                as.character(sys.calls()[[length(sys.calls()) + 1 - test.pos]])
            }else{
                search()[(1:length(search()))[test.pos - length(sys.calls())]]
            }) 
        }
        fun8()
    }
    result7 <- fun7()
    expected7 <- "SOME VARIABLES OF fun7 ARE ALSO PRESENT IN :\npackage:base: min\n"
    expect_equal(result7, expected7)
    rm(result7)
    rm(expected7)
})
