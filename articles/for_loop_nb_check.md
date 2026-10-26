# for_loop_nb_check(): more examples

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
    for(i in 0:0){ # equal bounds: hard numbers, not reported
        base::print(i)
    }
    for(i in -3:-1){ # negative: hard numbers, not reported
        base::print(i)
    }
    for(i in 1:0){ # decreasing: hard numbers, not reported
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
    for(i in base::seq(x)){ # 2 loops when x = 0
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
    ##     for (i in base::seq(x)) {
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
    ## 39   for     for (i in base::seq(x)) {
    ## 42   for     for (i in base::names(x = x)) {

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
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::all(tempo_for_seq1 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::length(x = tempo_for_seq1) >= 
    ##         2 && tempo_for_seq1[1] > tempo_for_seq1[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq2) && base::all(tempo_for_seq2 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq2) && base::length(x = tempo_for_seq2) >= 
    ##         2 && tempo_for_seq2[1] > tempo_for_seq2[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq3) && base::all(tempo_for_seq3 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq3) && base::length(x = tempo_for_seq3) >= 
    ##         2 && tempo_for_seq3[1] > tempo_for_seq3[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq4) && base::all(tempo_for_seq4 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq4) && base::length(x = tempo_for_seq4) >= 
    ##         2 && tempo_for_seq4[1] > tempo_for_seq4[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq5) && base::all(tempo_for_seq5 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq5) && base::length(x = tempo_for_seq5) >= 
    ##         2 && tempo_for_seq5[1] > tempo_for_seq5[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq6) && base::all(tempo_for_seq6 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq6) && base::length(x = tempo_for_seq6) >= 
    ##         2 && tempo_for_seq6[1] > tempo_for_seq6[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq7) && base::all(tempo_for_seq7 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq7) && base::length(x = tempo_for_seq7) >= 
    ##         2 && tempo_for_seq7[1] > tempo_for_seq7[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq8) && base::all(tempo_for_seq8 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq8) && base::length(x = tempo_for_seq8) >= 
    ##         2 && tempo_for_seq8[1] > tempo_for_seq8[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq9) && base::all(tempo_for_seq9 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq9) && base::length(x = tempo_for_seq9) >= 
    ##         2 && tempo_for_seq9[1] > tempo_for_seq9[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq10) && base::all(tempo_for_seq10 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq10) && base::length(x = tempo_for_seq10) >= 
    ##         2 && tempo_for_seq10[1] > tempo_for_seq10[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq11) && base::all(tempo_for_seq11 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq11) && base::length(x = tempo_for_seq11) >= 
    ##         2 && tempo_for_seq11[1] > tempo_for_seq11[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq12) && base::all(tempo_for_seq12 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq12) && base::length(x = tempo_for_seq12) >= 
    ##         2 && tempo_for_seq12[1] > tempo_for_seq12[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq12) {
    ##         base::print(i)
    ##     }
    ##     tempo_for_seq13 <- base::seq(x)
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
    ##     else if (base::is.numeric(x = tempo_for_seq13) && base::all(tempo_for_seq13 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq13) && base::length(x = tempo_for_seq13) >= 
    ##         2 && tempo_for_seq13[1] > tempo_for_seq13[2]) {
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
    ##     else if (base::is.numeric(x = tempo_for_seq14) && base::all(tempo_for_seq14 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq14) && base::length(x = tempo_for_seq14) >= 
    ##         2 && tempo_for_seq14[1] > tempo_for_seq14[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq14) {
    ##         base::print(i)
    ##     }
    ## }
    ## <environment: 0x55ae008dc410>

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
    export = FALSE, # write the corrected function into a .R file instead of returning it?
    out_path = ".", # folder where to write the file (used only if export = TRUE).
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

### Arguments `export` and `out_path`: return or export the corrected function

``` r

# The corrected function is returned (NULL if nothing has to be corrected).
fun9 <- function(x){ for(i in 1:length(x)){ y <- i } ; y }
fun9_safer <- for_loop_nb_check(x = fun9)
```

    ## 
    ## 
    ## 
    ## INSIDE fun9(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    for     for (i in 1:length(x)) {

``` r

fun9_safer # a checking line is added before the loop.
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
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::all(tempo_for_seq1 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::length(x = tempo_for_seq1) >= 
    ##         2 && tempo_for_seq1[1] > tempo_for_seq1[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq1) {
    ##         y <- i
    ##     }
    ##     y
    ## }
    ## <environment: 0x55ae036159c8>

``` r

# The corrected function warns when x is empty, because 1:length(x) is then c(1, 0), a decreasing sequence.
fun9_safer(x = base::character())
```

    ## Warning: FOR SEQUENCE DECREASING

    ## [1] 0

``` r

# Export the corrected function into the fun9_safer.R file, here in a new temporary folder.
# out_path = "." (default) is the R working directory.
# An existing file is never overwritten (an error is returned instead).
tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
for_loop_nb_check(
    x = fun9,
    export = TRUE,
    out_path = tempo_dir
)
```

    ## 
    ## 
    ## 
    ## INSIDE fun9(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 3    for     for (i in 1:length(x)) {

    ## MODIFIED CODE EXPORTED IN:
    ## /tmp/Rtmp4zRN2Q/dir1ba04f33e667/fun9_safer.R

``` r

base::readLines(con = base::file.path(tempo_dir, "fun9_safer.R", fsep = "/"))
```

    ## [1] "fun9_safer <- function (x) "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    ## [2] "{"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
    ## [3] "    tempo_for_seq1 <- 1:length(x) ; if(base::length(x = tempo_for_seq1) == 0){base::warning(\"FOR SEQUENCE EMPTY: 0 ITERATION\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::anyNA(x = tempo_for_seq1)){base::warning(\"FOR SEQUENCE WITH NA VALUES\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::length(x = tempo_for_seq1) == 1){base::warning(\"FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::is.atomic(x = tempo_for_seq1) && base::any(base::is.infinite(x = tempo_for_seq1), na.rm = TRUE)){base::warning(\"FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::is.numeric(x = tempo_for_seq1) && base::all(tempo_for_seq1 < 0, na.rm = FALSE)){base::warning(\"FOR SEQUENCE WITH ALL NEGATIVE VALUES\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)}else if(base::is.numeric(x = tempo_for_seq1) && base::length(x = tempo_for_seq1) >= 2 && tempo_for_seq1[1] > tempo_for_seq1[2]){base::warning(\"FOR SEQUENCE DECREASING\", call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL)} ; for (i in tempo_for_seq1) {"
    ## [4] "        y <- i"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    ## [5] "    }"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    ## [6] "    y"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    ## [7] "}"

\

### Sophisticated example

``` r

# Hard numbers are never reported, whatever the bounds (0:0, 1:0 or -3:-1):
# the writing itself fixes the iterations, so their control is left to the writer.
fun3 <- function(x){ x <- 0 ; for(i in 0:0){ y <- i } ; y }
for_loop_nb_check(x = fun3)
```

    ## 
    ## 
    ## AFTER RUNNING saferDev::for_loop_nb_check().
    ## INSIDE fun3(), EVERYTHING SEEMS CLEAN.

    ## NULL

``` r

# But a sequence computed from an object is reported, like 1:length(x), 
# which gives c(1, 0) when x is empty (use base::seq_along(along.with = x) instead).
fun4 <- function(x){ x <- character() ; for(i in 1:length(x)){ y <- i } ; y }
for_loop_nb_check(x = fun4)
```

    ## 
    ## 
    ## 
    ## INSIDE fun4(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 4    for     for (i in 1:length(x)) {

    ## function (x) 
    ## {
    ##     x <- character()
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
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::all(tempo_for_seq1 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::length(x = tempo_for_seq1) >= 
    ##         2 && tempo_for_seq1[1] > tempo_for_seq1[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq1) {
    ##         y <- i
    ##     }
    ##     y
    ## }
    ## <environment: 0x55ae065b6828>

``` r

# Nested for() loops are all checked.
fun5 <- function(x){
    for(i in 1:3){
        for(i2 in 1:length(x)){ # reported
            base::print(i2)
        }
    }
}
for_loop_nb_check(x = fun5)
```

    ## 
    ## 
    ## 
    ## INSIDE fun5(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 4    for     for (i2 in 1:length(x)) {

    ## function (x) 
    ## {
    ##     for (i in 1:3) {
    ##         tempo_for_seq1 <- 1:length(x)
    ##         if (base::length(x = tempo_for_seq1) == 0) {
    ##             base::warning("FOR SEQUENCE EMPTY: 0 ITERATION", 
    ##                 call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##                 domain = NULL)
    ##         }
    ##         else if (base::anyNA(x = tempo_for_seq1)) {
    ##             base::warning("FOR SEQUENCE WITH NA VALUES", call. = FALSE, 
    ##                 immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##         }
    ##         else if (base::length(x = tempo_for_seq1) == 1) {
    ##             base::warning("FOR SEQUENCE WITH A SINGLE VALUE (EQUAL BOUNDS?): SINGLE ITERATION", 
    ##                 call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##                 domain = NULL)
    ##         }
    ##         else if (base::is.atomic(x = tempo_for_seq1) && base::any(base::is.infinite(x = tempo_for_seq1), 
    ##             na.rm = TRUE)) {
    ##             base::warning("FOR SEQUENCE WITH INFINITE VALUES: NO ENDLESS LOOP POSSIBLE BECAUSE THE for() CONTROLLER IS FULLY EVALUATED BEFORE ITERATING, BUT CHECK THAT AN INFINITE VALUE IS EXPECTED", 
    ##                 call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##                 domain = NULL)
    ##         }
    ##         else if (base::is.numeric(x = tempo_for_seq1) && base::all(tempo_for_seq1 < 
    ##             0, na.rm = FALSE)) {
    ##             base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##                 call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##                 domain = NULL)
    ##         }
    ##         else if (base::is.numeric(x = tempo_for_seq1) && base::length(x = tempo_for_seq1) >= 
    ##             2 && tempo_for_seq1[1] > tempo_for_seq1[2]) {
    ##             base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##                 immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##         }
    ##         for (i2 in tempo_for_seq1) {
    ##             base::print(i2)
    ##         }
    ##     }
    ## }
    ## <environment: 0x55ae06b96468>

``` r

# A for() loop inside an argument default is also checked.
# Here, the loop is the body of function(y) without { }: the correction adds { } around it.
fun6 <- function(f = function(y) for(i in y){ base::print(i) }) f(1)
for_loop_nb_check(x = fun6)
```

    ## 
    ## 
    ## 
    ## INSIDE fun6(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 1    for     function (f = function(y) for (i in y) {

    ## function (f = function(y) {
    ##     tempo_for_seq1 <- y
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
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::all(tempo_for_seq1 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::length(x = tempo_for_seq1) >= 
    ##         2 && tempo_for_seq1[1] > tempo_for_seq1[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq1) {
    ##         base::print(i)
    ##     }
    ## }) 
    ## f(1)
    ## <environment: 0x55ae07174a78>

``` r

# Only the writing is analyzed: 1:n is reported as a variable, whatever n.
fun7 <- function(x){ n <- base::length(x = x) ; for(i in 1:n){ y <- i } ; y }
for_loop_nb_check(x = fun7)
```

    ## 
    ## 
    ## 
    ## INSIDE fun7(), SOME for() SEQUENCES ARE NOT WRITTEN WITH HARD NUMBERS:
    ## 
    ## LINE CONSTRUCT       CODE
    ## 4    for     for (i in 1:n) {

    ## function (x) 
    ## {
    ##     n <- base::length(x = x)
    ##     tempo_for_seq1 <- 1:n
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
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::all(tempo_for_seq1 < 
    ##         0, na.rm = FALSE)) {
    ##         base::warning("FOR SEQUENCE WITH ALL NEGATIVE VALUES", 
    ##             call. = FALSE, immediate. = FALSE, noBreaks. = FALSE, 
    ##             domain = NULL)
    ##     }
    ##     else if (base::is.numeric(x = tempo_for_seq1) && base::length(x = tempo_for_seq1) >= 
    ##         2 && tempo_for_seq1[1] > tempo_for_seq1[2]) {
    ##         base::warning("FOR SEQUENCE DECREASING", call. = FALSE, 
    ##             immediate. = FALSE, noBreaks. = FALSE, domain = NULL)
    ##     }
    ##     for (i in tempo_for_seq1) {
    ##         y <- i
    ##     }
    ##     y
    ## }
    ## <environment: 0x55ae0774b6e8>

\
