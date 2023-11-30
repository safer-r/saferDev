test_that("check if the object is in the environment one step above the env_check() environment, and if yes, returns if the same name exists in above environments", {
    pos <- 2
    name <- "mean"
    mean <- 2
    tempo.name <- rev(as.character(unlist(sys.calls())))
    tempo.frame <- rev(sys.frames())

    ls.names <- c(tempo.name, search()) # names of the functions + names of the search() environments
    ls.input <- c(tempo.frame, as.list(search())) # environements of the functions + names of the search() environments
    names(match.list) <- ls.names # 
    match.list <- match.list[-c(1:(pos + 1))]
    result3 <- env_check(name = "mean")
    expected3 <- paste0("SOME VARIABLES ", "OF THE CHECKED ENVIRONMENT", paste0("OF ", name), " ARE ALSO PRESENT IN :\n", paste0(names(match.list[ ! sapply(match.list, FUN = is.null)]), ": ", sapply(match.list[ ! sapply(match.list, FUN = is.null)], FUN = paste0, collapse = " "), collapse = "\n"), "\n")
    expect_equal(result3,expected3)
    
    
    result4 <- env_check(
        pos = 1,
        name = "mean"
    )
    expected4 <- paste0("SOME VARIABLES ", "OF THE CHECKED ENVIRONMENT", paste0("OF ", name), " ARE ALSO PRESENT IN :\n", paste0(names(match.list[ ! sapply(match.list, FUN = is.null)]), ": ", sapply(match.list[ ! sapply(match.list, FUN = is.null)], FUN = paste0, collapse = " "), collapse = "\n"), "\n")
    expect_equal(result4, expected4)
})
