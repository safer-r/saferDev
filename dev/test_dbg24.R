txt1 <- "f <- function(x){ if(x > 1) base::return(x) else base::return(0) }"
pd1 <- utils::getParseData(base::parse(text = txt1, keep.source = TRUE))
term_rows1 <- base::which(x = pd1$terminal == TRUE & pd1$token != "COMMENT", arr.ind = FALSE, useNames = TRUE)
term_rows1 <- term_rows1[base::order(pd1$line1[term_rows1], pd1$col1[term_rows1])]
kid1 <- term_rows1[base::which(x = pd1$token[term_rows1] == "IF", arr.ind = FALSE, useNames = TRUE)][1]
kid_expr1 <- pd1$parent[kid1]
cat("kid:", kid1, "kid_expr:", kid_expr1, "\n")
close_paren1 <- base::max(base::which(x = pd1$parent == kid_expr1 & pd1$token == "')'", arr.ind = FALSE, useNames = TRUE))
else_tok1 <- base::which(x = pd1$parent == kid_expr1 & pd1$token == "ELSE", arr.ind = FALSE, useNames = TRUE)[1]
cat("close_paren1:", close_paren1, "else_tok1:", else_tok1, "body_start_pos1:", which(term_rows1 == close_paren1), "else_pos1:", which(term_rows1 == else_tok1), "\n")
# expected then body: 2nd-to-last expr child; else body: last
ch <- base::which(x = pd1$parent == kid_expr1 & pd1$token == "expr", arr.ind = FALSE, useNames = TRUE)
ch <- ch[base::order(pd1$line1[ch], pd1$col1[ch])]
cat("expr children:", ch, "\n")
cat("then body col1:", pd1$col1[ch[2]], " else body col1:", pd1$col1[ch[3]], "\n")
# for case
txt2 <- "f <- function(x){ for(i in x) base::print(i) }"
pd2 <- utils::getParseData(base::parse(text = txt2, keep.source = TRUE))
kid2 <- base::which(x = pd2$token == "FOR", arr.ind = FALSE, useNames = TRUE)[1]
kid_expr2 <- pd2$parent[kid2]
ch2 <- base::which(x = pd2$parent == kid_expr2 & pd2$token == "expr", arr.ind = FALSE, useNames = TRUE)
ch2 <- ch2[base::order(pd2$line1[ch2], pd2$col1[ch2])]
cat("for: kid:", kid2, "kid_expr:", kid_expr2, "expr children:", ch2, "body col1:", pd2$col1[ch2[length(ch2)]], "\n")
print(pd2[pd2$parent == kid_expr2, c("id","token","line1","col1")], row.names = FALSE)