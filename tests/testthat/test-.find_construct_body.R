# This file tests .find_construct_body()
# Same structure as test-.has_odd_number_of_quotes.R

testthat::test_that(".find_construct_body()", {

    ## data argument values
    str1 <- "NOTGOOD::arg_check"
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor(str1)
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    int1 <- 1L
    txt_ifelse <- "f <- function(x){ if(x > 1) base::return(x) else base::return(0) }"
    txt_if <- "f <- function(x){ if(x > 1) base::return(x) }"
    txt_for <- "f <- function(x){ for(i in x) base::print(i) }"
    txt_fun <- "f <- function(x) x"
    ## end data argument values

    ## initialization of tests
    # helper to extract the call arguments of .find_construct_body() from a parsed code
    get_args <- function(txt, token_kind = "IF", kind = "then"){
        pd <- utils::getParseData(x = base::parse(text = txt, keep.source = TRUE))
        term_rows <- base::which(x = pd$terminal == TRUE & pd$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
        term_rows <- term_rows[base::order(x = pd$line1[term_rows], pd$col1[term_rows])]
        kid_idx <- base::which(x = pd$token[term_rows] == token_kind, arr.ind = FALSE, useNames = TRUE)
        kid <- term_rows[kid_idx[1]]
        kid_expr_id <- pd$parent[kid]
        if(kind == "then"){
            # paren scan like the caller: first '(' after kid opens, matching ')' closes
            depth <- 0L
            body_start <- NA_integer_
            for(j in (kid_idx[1] + 1):base::length(x = term_rows)){
                if(pd$token[term_rows[j]] == "'('"){
                    depth <- depth + 1L
                }else if(pd$token[term_rows[j]] == "')'"){
                    depth <- depth - 1L
                    if(depth == 0L){
                        body_start <- term_rows[j]
                        break
                    }
                }
            }
            else_tok <- NULL
            else_branch <- FALSE
        }else if(kind == "else"){
            body_start <- base::which(x = pd$parent == kid_expr_id & pd$token == "ELSE", arr.ind = FALSE, useNames = TRUE)[1]
            else_tok <- body_start
            else_branch <- TRUE
        }else{
            body_start <- NULL
            else_tok <- NULL
            else_branch <- FALSE
        }
        if(base::is.null(x = body_start) || base::is.na(x = body_start)){
            body_start_pos <- NA_integer_
        }else{
            body_start_pos <- base::which(x = term_rows == body_start, arr.ind = FALSE, useNames = TRUE)[1]
        }
        base::list(pd = pd, kid = kid, body_start = body_start, body_start_pos = body_start_pos, term_rows = term_rows, else_branch = else_branch, else_tok = else_tok)
    }
    args_ifelse <- get_args(txt_ifelse, token_kind = "IF", kind = "then")
    args_else <- get_args(txt_ifelse, token_kind = "IF", kind = "else")
    args_if <- get_args(txt_if, token_kind = "IF", kind = "then")
    args_for <- get_args(txt_for, token_kind = "FOR", kind = "then")
    args_fun <- get_args(txt_fun, token_kind = "FUNCTION", kind = "then")
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferDev:::.find_construct_body()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()"){
    #### end function name

    ########  argument with no default values
    # all internals have no defaults values
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")) # term_rows missing: no-default-values block (all the mandatory args are present)
    ########  end argument with no default values

    ######## management of NULL arguments
    testthat::expect_error(saferDev:::.find_construct_body(pd = NULL, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = NULL, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = NULL, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = NULL, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = NULL, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = NULL, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_no_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = NULL, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")) # else_tok can be NULL
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(saferDev:::.find_construct_body(pd = data.frame(), kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = integer(), else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = character(), error_text = ""))
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(saferDev:::.find_construct_body(pd = NA, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = NA_integer_, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = NA_integer_, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = NA_integer_, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = NA_integer_, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = NA, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    ######## end management of NA arguments

    #### end argument primary checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # pd
    testthat::expect_no_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = NULL, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = NA, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = 1, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = mat1, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = factor1, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = expr1, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = fun1, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    # end pd
    # kid
    testthat::expect_no_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = NULL, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = NA_integer_, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = 1.5, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")) # non integer double
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = base::c(args_ifelse$kid, args_ifelse$kid), body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")) # length 2
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = mat1, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = factor1, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = expr1, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = fun1, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    # end kid
    # body_start
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = NULL, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = 1.5, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")) # double
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = base::c(args_ifelse$body_start, args_ifelse$body_start), body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    # end body_start
    # body_start_pos
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = 1.5, term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")) # double
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = base::c(args_ifelse$body_start_pos, args_ifelse$body_start_pos), term_rows = args_ifelse$term_rows, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    # end body_start_pos
    # term_rows
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = 1.5, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")) # double
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = base::as.character(x = args_ifelse$term_rows), else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = mat1, else_branch = args_ifelse$else_branch, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    # end term_rows
    # else_branch
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = 1, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = "TRUE", else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = base::c(TRUE, FALSE), else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = mat1, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = factor1, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = expr1, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    testthat::expect_error(saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = fun1, else_tok = args_ifelse$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = ""))
    # end else_branch
    # lib_path cannot be tested because safer_check is not present and lib_path is checked only if safer_check = TRUE in the enclosing function
    ######## end argument checking with arg_check()

    #### end argument secondary checking

    #### main code
    # then branch of an if() with else: second-to-last expr child
    res <- saferDev:::.find_construct_body(pd = args_ifelse$pd, kid = args_ifelse$kid, body_start = args_ifelse$body_start, body_start_pos = args_ifelse$body_start_pos, term_rows = args_ifelse$term_rows, else_branch = FALSE, else_tok = NULL, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")
    testthat::expect_identical(object = res$col1, expected = 29L) # base::return(x) after if(x > 1)
    # else branch of an if() with else: last expr child
    res <- saferDev:::.find_construct_body(pd = args_else$pd, kid = args_else$kid, body_start = args_else$body_start, body_start_pos = args_else$body_start_pos, term_rows = args_else$term_rows, else_branch = TRUE, else_tok = args_else$else_tok, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")
    testthat::expect_identical(object = res$col1, expected = 50L) # base::return(0) after else
    # if() without else: last expr child
    res <- saferDev:::.find_construct_body(pd = args_if$pd, kid = args_if$kid, body_start = args_if$body_start, body_start_pos = args_if$body_start_pos, term_rows = args_if$term_rows, else_branch = FALSE, else_tok = NULL, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")
    testthat::expect_identical(object = res$col1, expected = 29L) # base::return(x) after if(x > 1)
    # for(): last expr child (forcond excluded because its token is not expr)
    res <- saferDev:::.find_construct_body(pd = args_for$pd, kid = args_for$kid, body_start = args_for$body_start, body_start_pos = args_for$body_start_pos, term_rows = args_for$term_rows, else_branch = FALSE, else_tok = NULL, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")
    testthat::expect_identical(object = res$col1, expected = 31L) # base::print(i) after for(i in x)
    # function(): last expr child (multi-line body)
    res <- saferDev:::.find_construct_body(pd = args_fun$pd, kid = args_fun$kid, body_start = args_fun$body_start, body_start_pos = args_fun$body_start_pos, term_rows = args_fun$term_rows, else_branch = FALSE, else_tok = NULL, intern_error_text_start = "T.", intern_error_text_end = "", lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.finite(x = res$col1), label = "function(): finite col1")
    testthat::expect_identical(object = res$line1, expected = 1L)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    rm(list = ls()) # to avoid a warning that block CRAN tests 
})
