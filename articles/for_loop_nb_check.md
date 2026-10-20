# for_loop_nb_check: more examples

\

``` r

library(saferDev)
```

\

### Datasets

``` r

# Function with all the for() sequences written with hard numbers or seq_len()/seq_along() calls.
fun1 <- function(x){
    for(i in 1:3){
        base::print(i)
    }
    for(i in 0:0){ # equal bounds: not reported by for_loop_nb_check() (target of the runtime checking line)
        base::print(i)
    }
    for(i in -3:-1){ # both negative: not reported by for_loop_nb_check()
        base::print(i)
    }
    for(i in 1:0){ # decreasing: not reported by for_loop_nb_check()
        base::print(i)
    }
    for(i in base::seq_along(along.with = x)){ # recommended replacement of 1:length(x)
        base::print(i)
    }
    for(i in base::seq_len(length.out = 3)){ # recommended replacement of 1:n
        base::print(i)
    }
}
# Function with some for() sequences not written with hard numbers.
fun2 <- function(x){
    for(i in 1:length(x)){ # 1:0 when x is empty: 2 loops
        base::print(i)
    }
    for(i in 1:length(x) - 1){ # 0:-1 when x is empty: 2 loops, and only 0 when x has 1 element
        base::print(i)
    }
    for(i in 1:n){ # n a variable: the sequence content is not predictable from the writing
        base::print(i)
    }
    for(i in 1:(3 - 1)){ # parenthesized constant expression: reported (purely syntactic rule)
        base::print(i)
    }
    for(i in 1:3 - 1){ # gives c(-2, -1) at execution
        base::print(i)
    }
    for(i in seq_along(along.with = x) - 1){ # gives c(0, 1) when x has 2 elements
        base::print(i)
    }
    for(i in 1:2.5){ # silently truncated toward 0
        base::print(i)
    }
    for(i in 1:NA){ # error at execution
        base::print(i)
    }
    for(i in 1:Inf){ # runs for a very long time
        base::print(i)
    }
    for(i in 1:TRUE){ # TRUE is not a numeric literal
        base::print(i)
    }
    for(i in x){ # plain object
        base::print(i)
    }
    for(i in base::rev(x = x)){ # non seq_len()/seq_along() call
        base::print(i)
    }
    for(i in base::seq(x = x)){ # 2 loops when x = 0
        base::print(i)
    }
    for(i in base::names(x = x)){ # non seq_len()/seq_along() call
        base::print(i)
    }
}
```

\

### Datasets info

``` r

fun1 # Function.
```

    ## function (x) 
    ## {
    ##     for (i in 1:3) {
    ##         base::print(i)
    ##     }
    ##     for (i in 0:0) {
    ##         base::print(i)
    ##     }
    ##     for (i in -3:-1) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:0) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::seq_along(along.with = x)) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::seq_len(length.out = 3)) {
    ##         base::print(i)
    ##     }
    ## }

``` r

fun2 # Function.
```

    ## function (x) 
    ## {
    ##     for (i in 1:length(x)) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:length(x) - 1) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:n) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:(3 - 1)) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:3 - 1) {
    ##         base::print(i)
    ##     }
    ##     for (i in seq_along(along.with = x) - 1) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:2.5) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:NA) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:Inf) {
    ##         base::print(i)
    ##     }
    ##     for (i in 1:TRUE) {
    ##         base::print(i)
    ##     }
    ##     for (i in x) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::rev(x = x)) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::seq(x = x)) {
    ##         base::print(i)
    ##     }
    ##     for (i in base::names(x = x)) {
    ##         base::print(i)
    ##     }
    ## }

\

### Argument `x`

``` r

# Test if all the for() sequences of fun1 are written with hard numbers.
# Here everything is fine.
for_loop_nb_check(x = fun1)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::for_loop_nb_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

``` r

# Here some for() sequences are not written with hard numbers.
for_loop_nb_check(x = fun2)
```

    ## 
    ## 
    ## 
    ## INSIDE fun2(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    for     for (i in 1:length(x)) {
    ## 6    for     for (i in 1:length(x) - 1) {
    ## 9    for     for (i in 1:n) {
    ## 12   for     for (i in 1:(3 - 1)) {
    ## 15   for     for (i in 1:3 - 1) {
    ## 18   for     for (i in seq_along(along.with = x) - 1) {
    ## 21   for     for (i in 1:2.5) {
    ## 24   for     for (i in 1:NA) {
    ## 27   for     for (i in 1:Inf) {
    ## 30   for     for (i in 1:TRUE) {
    ## 33   for     for (i in x) {
    ## 36   for     for (i in base::rev(x = x)) {
    ## 39   for     for (i in base::seq(x = x)) {
    ## 42   for     for (i in base::names(x = x)) {
    ## 
    ## MODIFIED FUNCTION RETURNED BY return().

    ## function (x) 
    ## {
    ##     tempo_for_seq1 <- 1:length(x)
    ##     if (base::length(x = tempo_for_seq1) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq1)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq1) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq1) && base::any(base::is.infinite(x = tempo_for_seq1), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq1) && base::typeof(x = tempo_for_seq1) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq1 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq1) && base::typeof(x = tempo_for_seq1) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq1) >= 2 && tempo_for_seq1[1] > 
    ##         tempo_for_seq1[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq1) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq2 <- 1:length(x) - 1
    ##     if (base::length(x = tempo_for_seq2) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq2)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq2) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq2) && base::any(base::is.infinite(x = tempo_for_seq2), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq2) && base::typeof(x = tempo_for_seq2) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq2 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq2) && base::typeof(x = tempo_for_seq2) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq2) >= 2 && tempo_for_seq2[1] > 
    ##         tempo_for_seq2[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq2) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq3 <- 1:n
    ##     if (base::length(x = tempo_for_seq3) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq3)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq3) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq3) && base::any(base::is.infinite(x = tempo_for_seq3), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq3) && base::typeof(x = tempo_for_seq3) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq3 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq3) && base::typeof(x = tempo_for_seq3) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq3) >= 2 && tempo_for_seq3[1] > 
    ##         tempo_for_seq3[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq3) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq4 <- 1:(3 - 1)
    ##     if (base::length(x = tempo_for_seq4) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq4)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq4) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq4) && base::any(base::is.infinite(x = tempo_for_seq4), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq4) && base::typeof(x = tempo_for_seq4) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq4 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq4) && base::typeof(x = tempo_for_seq4) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq4) >= 2 && tempo_for_seq4[1] > 
    ##         tempo_for_seq4[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq4) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq5 <- 1:3 - 1
    ##     if (base::length(x = tempo_for_seq5) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq5)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq5) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq5) && base::any(base::is.infinite(x = tempo_for_seq5), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq5) && base::typeof(x = tempo_for_seq5) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq5 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq5) && base::typeof(x = tempo_for_seq5) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq5) >= 2 && tempo_for_seq5[1] > 
    ##         tempo_for_seq5[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq5) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq6 <- seq_along(along.with = x) - 1
    ##     if (base::length(x = tempo_for_seq6) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq6)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq6) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq6) && base::any(base::is.infinite(x = tempo_for_seq6), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq6) && base::typeof(x = tempo_for_seq6) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq6 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq6) && base::typeof(x = tempo_for_seq6) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq6) >= 2 && tempo_for_seq6[1] > 
    ##         tempo_for_seq6[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq6) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq7 <- 1:2.5
    ##     if (base::length(x = tempo_for_seq7) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq7)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq7) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq7) && base::any(base::is.infinite(x = tempo_for_seq7), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq7) && base::typeof(x = tempo_for_seq7) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq7 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq7) && base::typeof(x = tempo_for_seq7) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq7) >= 2 && tempo_for_seq7[1] > 
    ##         tempo_for_seq7[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq7) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq8 <- 1:NA
    ##     if (base::length(x = tempo_for_seq8) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq8)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq8) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq8) && base::any(base::is.infinite(x = tempo_for_seq8), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq8) && base::typeof(x = tempo_for_seq8) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq8 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq8) && base::typeof(x = tempo_for_seq8) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq8) >= 2 && tempo_for_seq8[1] > 
    ##         tempo_for_seq8[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq8) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq9 <- 1:Inf
    ##     if (base::length(x = tempo_for_seq9) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq9)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq9) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq9) && base::any(base::is.infinite(x = tempo_for_seq9), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq9) && base::typeof(x = tempo_for_seq9) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq9 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq9) && base::typeof(x = tempo_for_seq9) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq9) >= 2 && tempo_for_seq9[1] > 
    ##         tempo_for_seq9[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq9) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq10 <- 1:TRUE
    ##     if (base::length(x = tempo_for_seq10) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq10)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq10) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq10) && base::any(base::is.infinite(x = tempo_for_seq10), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq10) && base::typeof(x = tempo_for_seq10) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq10 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq10) && base::typeof(x = tempo_for_seq10) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq10) >= 2 && tempo_for_seq10[1] > 
    ##         tempo_for_seq10[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq10) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq11 <- x
    ##     if (base::length(x = tempo_for_seq11) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq11)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq11) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq11) && base::any(base::is.infinite(x = tempo_for_seq11), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq11) && base::typeof(x = tempo_for_seq11) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq11 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq11) && base::typeof(x = tempo_for_seq11) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq11) >= 2 && tempo_for_seq11[1] > 
    ##         tempo_for_seq11[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq11) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq12 <- base::rev(x = x)
    ##     if (base::length(x = tempo_for_seq12) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq12)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq12) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq12) && base::any(base::is.infinite(x = tempo_for_seq12), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq12) && base::typeof(x = tempo_for_seq12) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq12 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq12) && base::typeof(x = tempo_for_seq12) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq12) >= 2 && tempo_for_seq12[1] > 
    ##         tempo_for_seq12[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq12) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq13 <- base::seq(x = x)
    ##     if (base::length(x = tempo_for_seq13) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq13)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq13) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq13) && base::any(base::is.infinite(x = tempo_for_seq13), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq13) && base::typeof(x = tempo_for_seq13) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq13 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq13) && base::typeof(x = tempo_for_seq13) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq13) >= 2 && tempo_for_seq13[1] > 
    ##         tempo_for_seq13[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq13) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq14 <- base::names(x = x)
    ##     if (base::length(x = tempo_for_seq14) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq14)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq14) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq14) && base::any(base::is.infinite(x = tempo_for_seq14), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq14) && base::typeof(x = tempo_for_seq14) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq14 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq14) && base::typeof(x = tempo_for_seq14) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq14) >= 2 && tempo_for_seq14[1] > 
    ##         tempo_for_seq14[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq14) {
    ##         base::print(i)
    ##     }
    ## }
    ## <environment: 0x5647250bdca0>

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
for_loop_nb_check(
    x = fun1, 
    safer_check = TRUE
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::for_loop_nb_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
for_loop_nb_check(
    x = fun1,  
    lib_path = "NOTGOOD"
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::for_loop_nb_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by for_loop_nb_check().
for_loop_nb_check(
    x = fun1, 
    lib_path = "NOTGOOD", 
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::for_loop_nb_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

for_loop_nb_check(
    x = fun1, # function name to test.
    export = FALSE, # return the modified function (FALSE, default) or export it in a <function name>_safer.R file (TRUE)?
    out_path = ".", # directory where to export the file when export = TRUE (the R working directory by default).
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::for_loop_nb_check().
    ## INSIDE fun1(), EVERYTHING SEEMS CLEAN.

    ## NULL

\

### Modified function returned or exported

``` r

# When at least one checking line has been inserted, the modified function is returned by return() (NULL is returned when no modification is performed, e.g., when everything is clean or when the constructs cannot be modified, which are then listed in the message).
fun9 <- function(x){ for(i in 1:length(x)){ y <- i } ; y }
fun9_safer <- for_loop_nb_check(x = fun9, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = "")
```

    ## 
    ## 
    ## 
    ## INSIDE fun9(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    for     for (i in 1:length(x)) {
    ## 
    ## MODIFIED FUNCTION RETURNED BY return().

``` r

fun9_safer # the modified function: tempo_for_seq1 <- 1:length(x) ; <CHECKS> ; for(i in tempo_for_seq1){ y <- i }
```

    ## function (x) 
    ## {
    ##     tempo_for_seq1 <- 1:length(x)
    ##     if (base::length(x = tempo_for_seq1) == 0) {
    ##         base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::anyNA(x = tempo_for_seq1)) {
    ##         base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     else if (base::length(x = tempo_for_seq1) == 1) {
    ##         base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq1) && base::any(base::is.infinite(x = tempo_for_seq1), 
    ##         na.rm = TRUE)) {
    ##         base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq1) && base::typeof(x = tempo_for_seq1) %in% 
    ##         base::c("double", "integer") && base::all(x = tempo_for_seq1 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.atomic(x = tempo_for_seq1) && base::typeof(x = tempo_for_seq1) %in% 
    ##         base::c("double", "integer", "character", "complex") && 
    ##         base::length(x = tempo_for_seq1) >= 2 && tempo_for_seq1[1] > 
    ##         tempo_for_seq1[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq1) {
    ##         y <- i
    ##     }
    ##     y
    ## }
    ## <environment: 0x56472808a308>

``` r

# With export = TRUE, the modified code is written in a fun9_safer.R file, in the directory indicated by out_path (the R working directory by default, when out_path = "."), and NULL is returned. An error is issued if the file already exists, and nothing is written when no modification is performed.
# for_loop_nb_check(x = fun9, export = TRUE, out_path = tempdir(), safer_check = TRUE, lib_path = NULL, error_text = "")
```

\

### Sophisticated example

``` r
# The for() sequences written with hard numbers are all clean, whatever the bounds (0:0, 1:0 and -3:-1 are deliberately not reported by for_loop_nb_check(): they are the target of a specific checking line added in the function bodies, which warns at execution when the computed sequence is equal, both negative or decreasing).
fun3 <- function(x){ x <- 0 ; for(i in 0:0){ y <- i } ; y }
for_loop_nb_check(x = fun3)

# But the same writing computed from an object is reported, like 1:length(x), which gives c(1, 0) when x is empty (base::seq_along(x = x) should be used instead).
fun4 <- function(x){ x <- character() ; for(i in 1:length(x)){ y <- i } ; y }
for_loop_nb_check(x = fun4)

# Nested for() loops are all checked.
fun5 <- function(x){
    for(i in 1:3){
        for(i2 in 1:length(x)){ # reported
            base::print(i2)
        }
    }
}
for_loop_nb_check(x = fun5)

# A for() loop inside a formal default is also checked, and the whole construct is wrapped in curly brackets when needed (here, the for() construct is the unbraced body of the function() writing, so the checking lines must be inserted inside { }).
fun6 <- function(f = function(y) for(i in y){ base::print(i) }) f(1)
for_loop_nb_check(x = fun6)

# Aliased bounds cannot be detected (documented limitation): n <- length(x) ; for(i in 1:n) is reported as a plain variable, but the aliasing itself is invisible.
fun7 <- function(x){ n <- base::length(x = x) ; for(i in 1:n){ y <- i } ; y }
for_loop_nb_check(x = fun7)

# Backquoted constructs cannot be checked (documented limitation).
fun8 <- function(x){ `for`(i in 1:3, base::print(i)) }
try(for_loop_nb_check(x = fun8), silent = FALSE)
```

    ## Error in parse(text = input): <text>:28:30: unexpected 'in'
    ## 27: # Backquoted constructs cannot be checked (documented limitation).
    ## 28: fun8 <- function(x){ `for`(i in
    ##                                  ^

\
