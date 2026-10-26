# env_check(): more examples

\

``` r

library(saferDev)
```

\

These examples may not work well here because of a particular
environment. Please, copy-paste in a local environment.\

### Argument `pos`

``` r

# Checking of the environment one step above \code{env_check()} local environment.
env_check(pos = 1)
```

    ## [1] "SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :\n.GlobalEnv: .Random.seed\nSEARCH PATH CHECKED:\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n.GlobalEnv\npackage:saferDev\npackage:stats\npackage:graphics\npackage:grDevices\npackage:utils\npackage:datasets\npackage:methods\nAutoloads\ntools:callr\npackage:base\n"

``` r

# With the message correctly displayed.
output <- env_check(pos = 1)
cat(output)
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :
    ## .GlobalEnv: .Random.seed
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

``` r

# Checking of the environment two steps above \code{env_check()} local environment.
# NULL means that no object in the tested environment exists above.
env_check(pos = 2)
```

    ## [1] "SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :\nwithCallingHandlers(code, error = function (e) \nrlang::entrace(e), message = function (cnd) \n{\n    watcher$capture_plot_and_output()\n    if (on_message$capture) {\n        watcher$push(cnd)\n    }\n    if (on_message$silence) {\n        invokeRestart(\"muffleMessage\")\n    }\n}, warning = function (cnd) \n{\n    if (getOption(\"warn\") >= 2 || getOption(\"warn\") < 0) {\n        return()\n    }\n    watcher$capture_plot_and_output()\n    if (on_warning$capture) {\n        cnd <- sanitize_call(cnd)\n        watcher$push(cnd)\n    }\n    if (on_warning$silence) {\n        invokeRestart(\"muffleWarning\")\n    }\n}, error = function (cnd) \n{\n    watcher$capture_plot_and_output()\n    cnd <- sanitize_call(cnd)\n    watcher$push(cnd)\n    switch(on_error, continue = invokeRestart(\"eval_continue\"), stop = invokeRestart(\"eval_stop\"), error = NULL)\n}): expr\neval(call): enclos envir expr\ndoWithOneRestart(return(expr), restart): expr\nwithOneRestart(expr, restarts[[1]]): expr\nwithRestartList(expr, restarts[-nr]): expr\ndoWithOneRestart(return(expr), restart): expr\nwithOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]]): expr\nwithRestartList(expr, restarts): expr\nwithRestarts(with_handlers({\n    for (expr in tle$exprs) {\n        ev <- withVisible(eval(expr, envir))\n        watcher$capture_plot_and_output()\n        watcher$print_value(ev$value, ev$visible, envir)\n    }\n    TRUE\n}, handlers), eval_continue = function() TRUE, eval_stop = function() FALSE): expr\nevaluate::evaluate(...): enclos envir expr\nin_dir(input_dir(), expr): expr\nin_input_dir(evaluate(code, envir = env, new_device = FALSE, keep_warning = if (is.numeric(options$warning)) TRUE else options$warning, keep_message = if (is.numeric(options$message)) TRUE else options$message, stop_on_error = if (is.numeric(options$error)) options$error else {\n    if (options$error && options$include) \n        0\n    else 2\n}, output_handler = knit_handlers(options$render, options))): expr\nwithCallingHandlers(if (tangle) process_tangle(group) else process_group(group), error = function(e) {\n    if (progress && is.function(pb$interrupt)) \n        pb$interrupt()\n    if (is_R_CMD_build() || is_R_CMD_check()) \n        error <<- format(e)\n}): expr\nwith_options(withCallingHandlers(if (tangle) process_tangle(group) else process_group(group), error = function(e) {\n    if (progress && is.function(pb$interrupt)) \n        pb$interrupt()\n    if (is_R_CMD_build() || is_R_CMD_check()) \n        error <<- format(e)\n}), list(rlang_trace_top_env = knit_global())): expr\nxfun:::handle_error(with_options(withCallingHandlers(if (tangle) process_tangle(group) else process_group(group), error = function(e) {\n    if (progress && is.function(pb$interrupt)) \n        pb$interrupt()\n    if (is_R_CMD_build() || is_R_CMD_check()) \n        error <<- format(e)\n}), list(rlang_trace_top_env = knit_global())), function(loc) {\n    setwd(wd)\n    write_utf8(res, output %n% stdout())\n    paste0(\"\\nQuitting from \", loc, if (!is.null(error)) \n        paste0(\"\\n\", rule(), error, \"\\n\", rule()))\n}, if (labels[i] != \"\") sprintf(\" [%s]\", labels[i]), get_loc): expr\nknitr::knit(knit_input, knit_output, envir = envir, quiet = quiet): envir\nrmarkdown::render(envir = globalenv(), ...): envir\n(function (what, args, quote = FALSE, envir = parent.frame()) \n{\n    if (!is.list(args)) \n        stop(\"second argument must be a list\")\n    if (quote) \n        args <- lapply(args, enquote)\n    .Internal(do.call(what, args, envir))\n})(base::quote(function (..., seed = NULL) \n{\n    if (!is.null(seed)) {\n        set.seed(seed)\n        if (requireNamespace(\"htmlwidgets\", quietly = TRUE)) {\n            htmlwidgets::setWidgetIdSeed(seed)\n        }\n    }\n    options(knitr.graphics.rel_path = FALSE)\n    rmarkdown::render(envir = globalenv(), ...)\n}), base::quote(list(input = \"/__w/saferDev/saferDev/vignettes/articles/env_check.Rmd\", output_file = \"env_check.html\", output_dir = \"/__w/saferDev/saferDev/docs/articles\", intermediates_dir = \"/tmp/RtmplIblwY\", encoding = \"UTF-8\", seed = 1014, output_format = list(knitr = list(opts_knit = NULL, opts_chunk = list(dev = \"ragg_png\", dpi = 96, fig.width = 7.29166666666667, fig.height = 4.50659250103008, fig.retina = 2, dev.args = list(bg = NA), fig.ext = \"png\", other.parameters = list(), fig.class = \"r-plt\"), \n    knit_hooks = list(plot = function (x, options) \n    {\n        hook_output <- knitr::hook_plot_md(x, options)\n        if (!is.null(options$fig.class)) {\n            hook_output <- gsub(\"<img src=\\\"([^\\\"]+)\\\"\", sprintf(\"<img src=\\\"\\\\1\\\" class=\\\"%s\\\"\", options$fig.class), hook_output)\n        }\n        hook_output\n    }), opts_hooks = NULL, opts_template = NULL), pandoc = list(to = \"html\", from = \"markdown+autolink_bare_uris+tex_math_single_backslash\", args = c(\"--standalone\", \"--section-divs\", \"--template\", \"/tmp/RtmplIblwY/pkgdown-rmd-template-16ea4cc6f4fe.html\", \"--syntax-highlighting\", \"pygments\"), keep_tex = FALSE, latex_engine = \"pdflatex\", ext = NULL, convert_fun = NULL, lua_filters = c(\"/__w/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua\", \"/__w/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua\", \n\"/__w/_temp/Library/rmarkdown/rmarkdown/lua/table-classes.lua\")), keep_md = FALSE, clean_supporting = FALSE, df_print = \"default\", pre_knit = function (...) \n{\n    options(width = width)\n    if (is.function(old_pre)) {\n        old_pre(...)\n    }\n}, post_knit = function (...) \n{\n    op(base(...), overlay(...))\n}, pre_processor = function (...) \n{\n    op(base(...), overlay(...))\n}, intermediates_generator = function (original_input, intermediates_dir) \n{\n    copy_render_intermediates(original_input, intermediates_dir, !self_contained)\n}, post_processor = function (metadata, input_file, output_file, ...) \n{\n    original_output_file <- output_file\n    output_file <- overlay(metadata, input_file, output_file, ...)\n    if (!is.null(attr(output_file, \"post_process_original\"))) \n        base(metadata, input_file, original_output_file, ...)\n    base(metadata, input_file, output_file, ...)\n}, file_scope = NULL, on_exit = function () \n{\n    if (is.function(base)) \n        base()\n    if (is.function(overlay)) \n        overlay()\n}), output_options = NULL, quiet = TRUE)), envir = base::quote(<environment>), quote = base::quote(TRUE)): envir\nbase::do.call(base::do.call, base::c(base::readRDS(\"/tmp/RtmplIblwY/callr-fun-16ea234aa310\"), base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv, quote = TRUE): envir\nbase::withCallingHandlers({\n    NULL\n    base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS(\"/tmp/RtmplIblwY/callr-fun-16ea234aa310\"), base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv, quote = TRUE), file = \"/tmp/RtmplIblwY/callr-res-16ea297bc4ea\", compress = FALSE)\n    base::flush(base::stdout())\n    base::flush(base::stderr())\n    NULL\n    base::invisible()\n}, error = function(e) {\n    {\n        callr_data <- base::as.environment(\"tools:callr\")$`__callr_data__`\n        err <- callr_data$err\n        if (FALSE) {\n            base::assign(\".Traceback\", base::.traceback(4), envir = callr_data)\n            utils::dump.frames(\"__callr_dump__\")\n            base::assign(\".Last.dump\", .GlobalEnv$`__callr_dump__`, envir = callr_data)\n            base::rm(\"__callr_dump__\", envir = .GlobalEnv)\n        }\n        e <- err$process_call(e)\n        e2 <- err$new_error(\"error in callr subprocess\")\n        class <- base::class\n        class(e2) <- base::c(\"callr_remote_error\", class(e2))\n        e2 <- err$add_trace_back(e2)\n        cut <- base::which(e2$trace$scope == \"global\")[1]\n        if (!base::is.na(cut)) {\n            e2$trace <- e2$trace[-(1:cut), ]\n        }\n        if (callr_data$has_otel) {\n            callr_data$otel_span$record_exception(e2)\n        }\n        base::saveRDS(base::list(\"error\", e2, e), file = base::paste0(\"/tmp/RtmplIblwY/callr-res-16ea297bc4ea\", \".error\"))\n    }\n}, interrupt = function(e) {\n    {\n        callr_data <- base::as.environment(\"tools:callr\")$`__callr_data__`\n        err <- callr_data$err\n        if (FALSE) {\n            base::assign(\".Traceback\", base::.traceback(4), envir = callr_data)\n            utils::dump.frames(\"__callr_dump__\")\n            base::assign(\".Last.dump\", .GlobalEnv$`__callr_dump__`, envir = callr_data)\n            base::rm(\"__callr_dump__\", envir = .GlobalEnv)\n        }\n        e <- err$process_call(e)\n        e2 <- err$new_error(\"error in callr subprocess\")\n        class <- base::class\n        class(e2) <- base::c(\"callr_remote_error\", class(e2))\n        e2 <- err$add_trace_back(e2)\n        cut <- base::which(e2$trace$scope == \"global\")[1]\n        if (!base::is.na(cut)) {\n            e2$trace <- e2$trace[-(1:cut), ]\n        }\n        if (callr_data$has_otel) {\n            callr_data$otel_span$record_exception(e2)\n        }\n        base::saveRDS(base::list(\"error\", e2, e), file = base::paste0(\"/tmp/RtmplIblwY/callr-res-16ea297bc4ea\", \".error\"))\n    }\n}, callr_message = function(e) {\n    base::try(base::signalCondition(e))\n}): expr\ndoTryCatch(return(expr), name, parentenv, handler): expr\ntryCatchOne(expr, names, parentenv, handlers[[1]]): expr\ntryCatchList(expr, names[-nh], parentenv, handlers[-nh]): expr\ndoTryCatch(return(expr), name, parentenv, handler): expr\ntryCatchOne(tryCatchList(expr, names[-nh], parentenv, handlers[-nh]), names[nh], parentenv, handlers[[nh]]): expr\ntryCatchList(expr, classes, parentenv, handlers): expr\nbase::tryCatch(base::withCallingHandlers({\n    NULL\n    base::saveRDS(base::do.call(base::do.call, base::c(base::readRDS(\"/tmp/RtmplIblwY/callr-fun-16ea234aa310\"), base::list(envir = .GlobalEnv, quote = TRUE)), envir = .GlobalEnv, quote = TRUE), file = \"/tmp/RtmplIblwY/callr-res-16ea297bc4ea\", compress = FALSE)\n    base::flush(base::stdout())\n    base::flush(base::stderr())\n    NULL\n    base::invisible()\n}, error = function(e) {\n    {\n        callr_data <- base::as.environment(\"tools:callr\")$`__callr_data__`\n        err <- callr_data$err\n        if (FALSE) {\n            base::assign(\".Traceback\", base::.traceback(4), envir = callr_data)\n            utils::dump.frames(\"__callr_dump__\")\n            base::assign(\".Last.dump\", .GlobalEnv$`__callr_dump__`, envir = callr_data)\n            base::rm(\"__callr_dump__\", envir = .GlobalEnv)\n        }\n        e <- err$process_call(e)\n        e2 <- err$new_error(\"error in callr subprocess\")\n        class <- base::class\n        class(e2) <- base::c(\"callr_remote_error\", class(e2))\n        e2 <- err$add_trace_back(e2)\n        cut <- base::which(e2$trace$scope == \"global\")[1]\n        if (!base::is.na(cut)) {\n            e2$trace <- e2$trace[-(1:cut), ]\n        }\n        if (callr_data$has_otel) {\n            callr_data$otel_span$record_exception(e2)\n        }\n        base::saveRDS(base::list(\"error\", e2, e), file = base::paste0(\"/tmp/RtmplIblwY/callr-res-16ea297bc4ea\", \".error\"))\n    }\n}, interrupt = function(e) {\n    {\n        callr_data <- base::as.environment(\"tools:callr\")$`__callr_data__`\n        err <- callr_data$err\n        if (FALSE) {\n            base::assign(\".Traceback\", base::.traceback(4), envir = callr_data)\n            utils::dump.frames(\"__callr_dump__\")\n            base::assign(\".Last.dump\", .GlobalEnv$`__callr_dump__`, envir = callr_data)\n            base::rm(\"__callr_dump__\", envir = .GlobalEnv)\n        }\n        e <- err$process_call(e)\n        e2 <- err$new_error(\"error in callr subprocess\")\n        class <- base::class\n        class(e2) <- base::c(\"callr_remote_error\", class(e2))\n        e2 <- err$add_trace_back(e2)\n        cut <- base::which(e2$trace$scope == \"global\")[1]\n        if (!base::is.na(cut)) {\n            e2$trace <- e2$trace[-(1:cut), ]\n        }\n        if (callr_data$has_otel) {\n            callr_data$otel_span$record_exception(e2)\n        }\n        base::saveRDS(base::list(\"error\", e2, e), file = base::paste0(\"/tmp/RtmplIblwY/callr-res-16ea297bc4ea\", \".error\"))\n    }\n}, callr_message = function(e) {\n    base::try(base::signalCondition(e))\n}), error = function(e) {\n    NULL\n    if (FALSE) {\n        base::try(base::stop(e))\n    }\n    if (TRUE) {\n        base::q(save = \"no\", status = 1)\n    }\n    base::invisible()\n}, interrupt = function(e) {\n    NULL\n    if (TRUE) {\n        base::q(save = \"no\", status = 1)\n    }\n    base::invisible()\n}): expr\nSEARCH PATH CHECKED:\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n.GlobalEnv\npackage:saferDev\npackage:stats\npackage:graphics\npackage:grDevices\npackage:utils\npackage:datasets\npackage:methods\nAutoloads\ntools:callr\npackage:base\n"

\

### Argument `name`

``` r

 # The name of the tested environment is replaced by "MY_ENV" in the message.
env_check(pos = 1, name = "MY_ENV")
```

    ## [1] "SOME VARIABLES OF THE CHECKED ENVIRONMENT MY_ENV ARE ALSO PRESENT IN :\nprocess_file(text, output): output\nknitr::knit(knit_input, knit_output, envir = envir, quiet = quiet): output\n.GlobalEnv: .Random.seed output\nSEARCH PATH CHECKED:\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n.GlobalEnv\npackage:saferDev\npackage:stats\npackage:graphics\npackage:grDevices\npackage:utils\npackage:datasets\npackage:methods\nAutoloads\ntools:callr\npackage:base\n"

``` r

# Inside a function
fun1 <- function(){
    mean <- 1
    mes <- env_check(pos = 1, name = "fun1")
    if( ! is.null(mes)){
        cat(mes)
    }
}
fun1()
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT fun1 ARE ALSO PRESENT IN :
    ## package:base: mean
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

\

### Argument `safer_check`

``` r

# Safer checkings are performed before main code running.
env_check(safer_check = TRUE)
```

    ## [1] "SOME VARIABLES OF THE CHECKED ENVIRONMENT UNNAMED ARE ALSO PRESENT IN :\nprocess_file(text, output): output\nknitr::knit(knit_input, knit_output, envir = envir, quiet = quiet): output\n.GlobalEnv: .Random.seed fun1 output\nSEARCH PATH CHECKED:\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n.GlobalEnv\npackage:saferDev\npackage:stats\npackage:graphics\npackage:grDevices\npackage:utils\npackage:datasets\npackage:methods\nAutoloads\ntools:callr\npackage:base\n"

\

### Argument `lib_path`

``` r

# The "NOTGOOD" path does not exists.
env_check(lib_path = "NOTGOOD")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::env_check().
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### Argument `error_text`

``` r

# Add a text in error messages returned by env_check().
env_check(
    lib_path = "NOTGOOD", 
    error_text = " === TEXT ADDED ==="
)
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferDev::env_check() === TEXT ADDED ===
    ## 
    ## THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT DOES NOT EXIST:
    ## NOTGOOD
    ## 
    ## ================

\

### All the arguments

``` r

env_check(
    pos = 1, # environment tested "pos" steps above the env_check() local environment.
    name = "MY_ENV", # name of the tested environment (used in the potential output message).
    safer_check = TRUE, # perform some "safer" checks (see https://github.com/safer-r)?
    lib_path = NULL, # where the required packages are if not in the default directories.
    error_text = "" # add information in error messages returned by the function.
) 
```

    ## [1] "SOME VARIABLES OF THE CHECKED ENVIRONMENT MY_ENV ARE ALSO PRESENT IN :\nprocess_file(text, output): output\nknitr::knit(knit_input, knit_output, envir = envir, quiet = quiet): output\n.GlobalEnv: .Random.seed fun1 output\nSEARCH PATH CHECKED:\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n<environment>\n.GlobalEnv\npackage:saferDev\npackage:stats\npackage:graphics\npackage:grDevices\npackage:utils\npackage:datasets\npackage:methods\nAutoloads\ntools:callr\npackage:base\n"

\

### Sophisticated examples

``` r

# The name of the function one step above can be retrieved with base::sys.calls():
fun4 <- function(){
    t.test <- 0
    mean <- 5
    name <- base::as.character(x = base::sys.calls()[[base::length(x = base::sys.calls())]])
    saferDev::env_check(pos = 1, name = name)
}
a <- fun4()
base::cat(a)
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT fun4 ARE ALSO PRESENT IN :
    ## doTryCatch(return(expr), name, parentenv, handler): name
    ## tryCatchOne(expr, names, parentenv, handlers[[1]]): name
    ## doTryCatch(return(expr), name, parentenv, handler): name
    ## tryCatchOne(tryCatchList(expr, names[-nh], parentenv, handlers[-nh]), names[nh], parentenv, handlers[[nh]]): name
    ## package:stats: t.test
    ## package:base: mean
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base

``` r

# Name of the tested environment according to the test_pos value, in nested functions:
fun7 <- function(){
    min <- "VALUE"
    fun8 <- function(){
        test_pos <- 1 # value 1 tests the fun8 environment, 2 tests the fun7 environment
        range <- "VALUE"
        if(base::length(x = base::sys.calls()) >= test_pos){
            name <- base::as.character(x = base::sys.calls()[[base::length(x = base::sys.calls()) + 1 - test_pos]])
        }else{
            name <- base::search()[test_pos - base::length(x = base::sys.calls())]
        }
        saferDev::env_check(pos = test_pos, name = name)
    }
    fun8()
}
a <- fun7()
base::cat(a)
```

    ## SOME VARIABLES OF THE CHECKED ENVIRONMENT fun8 ARE ALSO PRESENT IN :
    ## doTryCatch(return(expr), name, parentenv, handler): name
    ## tryCatchOne(expr, names, parentenv, handlers[[1]]): name
    ## doTryCatch(return(expr), name, parentenv, handler): name
    ## tryCatchOne(tryCatchList(expr, names[-nh], parentenv, handlers[-nh]), names[nh], parentenv, handlers[[nh]]): name
    ## package:base: range
    ## SEARCH PATH CHECKED:
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## <environment>
    ## .GlobalEnv
    ## package:saferDev
    ## package:stats
    ## package:graphics
    ## package:grDevices
    ## package:utils
    ## package:datasets
    ## package:methods
    ## Autoloads
    ## tools:callr
    ## package:base
