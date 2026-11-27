# saferDev [](https://safer-r.github.io/saferDev/)[![](reference/figures/saferDev.png)](https://safer-r.github.io/saferDev/)

\

\

## Table of contents

- [Description](#description)
- [Content](#content)
- [Versions](#versions)
- [Installation](#installation)
- [Docker images](#docker-images)
- [License](#license)
- [Citation](#citation)
- [Credits](#credits)
- [Acknowledgments](#acknowledgments)

\

## Description

Set of functions for the development of R functions and pipelines,
written according to the [safer-r project](https://github.com/safer-r)
specifications.

\

## Content

\

Read this [webpage](https://safer-r.github.io/saferDev/) for more
details.

\

| Function | Description |
|:---|:---|
| **all_args_here()** | Check that all the functions used inside a function are written with all their arguments. For instance: `base::paste0(letters[1:2], collapse = NULL, recycle0 = FALSE)` and not `base::paste0(letters[1:2])`. Print a table-like message of the detected function calls with their missing arguments, and return a corrected version of the submitted function, if any. |
| **arg_check()** | Check the expected features of an argument: class (compared exactly, e.g., `c("matrix", "array")` for a matrix), type, mode, length, allowed values, and for numeric values: proportions, negative values, infinite values and integers stored as double. The problems are returned as a text, not raised as errors (except for an argument made of `NA` only). |
| **colons_check()** | Check that the functions called inside a function are preceded by their package (e.g., [`base::mean()`](https://rdrr.io/r/base/mean.html) and not [`mean()`](https://rdrr.io/r/base/mean.html)). Print a table-like message of the calls without prefix, if any, and return a corrected version of the submitted function, where `<package>::` is added before the basic R functions (base, stats, graphics, grDevices, utils, datasets and methods packages); the other calls are only reported. |
| **curly_bracket_check()** | Check that the bodies of the `if`/`else`, `for`, `while`, `repeat` and `function` constructs are delimited by curly brackets [`{ }`](https://rdrr.io/r/base/Paren.html). Print a table-like message of the problematic lines, if any, and return a corrected version of the submitted function. |
| **env_check()** | Check whether the object names of an environment (by default, the one calling [`env_check()`](reference/env_check.md)) are also present in the frames of the calling functions and in the environments of the search path (global environment, attached packages), where objects of the same name could be confused. |
| **for_loop_nb_check()** | Check the sequence expression of every `for` loop of a function. If the number of iterations is not fixed by the writing itself (e.g., `1:length(x)`), a checking line is added before the loop, which issues a warning at execution if the sequence is empty, contains `NA` or infinite values, has a single value, has only negative values, or starts decreasing (first value greater than the second). Print a table-like message of the problematic lines, if any, and return a corrected version of the submitted function. |
| **get_message()** | Catch the error, the last warning or the standard messages of an instruction written as a character string, which is evaluated once. |
| **is_function_here()** | Check that the required functions are present in the installed packages (e.g., not removed or renamed in another version of the package). |
| **is_package_here()** | Check that the required packages are installed, without loading them. |
| **lambda_check()** | Check that no lambda function is written with the `\(x)` shorthand (like `\(x) x * 2`), and replace it by the standard `function(x)` writing. Print a table-like message of the problematic lines, if any, and return a corrected version of the submitted function. Warning: the `\(x)` writing can only be seen in a function that carries its source references (attribute `srcref`). |
| **report()** | Write a character string or a data object into a log file (added at the end of the file with `append = TRUE`). |
| **super_assign_check()** | Check that the superassignment operators `<<-` and `->>` are not used in a function. Return an error with a table of the problematic lines, if any, since these operators are not allowed in safer-r functions. |
| **true_false_check()** | Check that the `TRUE` and `FALSE` logical constants are written in full, and not abbreviated as `T` or `F`, which can be overwritten by the user. Print a table-like message of the problematic lines, if any, and return a corrected version of the submitted function. |

\

## Versions

The different *saferDev* releases are tagged
[here](https://github.com/safer-r/saferDev/releases).

\

## Installation

``` r

install.packages("saferDev")
```

Older versions can be installed from their GitHub
[tag](https://github.com/safer-r/saferDev/tags), like this:

``` r

remotes::install_github(repo = "safer-r/saferDev@CRAN_v2.0.0") # CRAN version 2.0.0
```

## Docker images

saferDev v1.0.0 alone in R 4.6.0:
[gmillot/r_v4.6.0_safer_v1.0:gitlab_v11.7](https://hub.docker.com/repository/docker/gmillot/r_v4.6.0_safer_v1.0/general)

saferDev v1.0.0 and [295 other
packages](https://gitlab.pasteur.fr/gmillot/dockerfiles/-/blob/master/R/v4.6.0/extended/v1.0/installed_r_packages.csv)
in R 4.6.0:
[gmillot/r_v4.6.0_extended_v1.0:gitlab_v11.7](https://hub.docker.com/repository/docker/gmillot/r_v4.6.0_extended_v1.0/general)

\

## License

This package can be redistributed and/or modified under the terms of the
GNU General Public License version 3, as published by the Free Software
Foundation. Distributed in the hope that it will be useful, but without
any warranty; without even the implied warranty of merchantability or
fitness for a particular purpose. See the GNU General Public License for
more details at <https://www.gnu.org/licenses> or in the attached
Licence.txt file.

\

## Citation

If you are using functions of *saferDev*, please cite:

> Wang H, Han Y, Legras M, Millot GA (2026). *The R saferDev package*.
> <https://github.com/safer-r/saferDev/>.

\

## Credits

[Haiding Wang](https://github.com/Tintin2710), Bioinformatics and
Biostatistics Hub, Institut Pasteur, Paris, France

[Yushi Han](https://github.com/yushiHn), Bioinformatics and
Biostatistics Hub, Institut Pasteur, Paris, France

[Mia Legras](https://github.com/mialgrs), Bioinformatics and
Biostatistics Hub, Institut Pasteur, Paris, France

[Jacques Serizay](https://github.com/js2264), Spatial Regulation of
Genomes team, Institut Pasteur, Paris, France

[GLM-5.3](https://docs.z.ai/guides/llm/glm-5.3), hosted at Institut
Pasteur, Paris, France

[Gael A. Millot](https://github.com/gael-millot), Bioinformatics and
Biostatistics Hub, Institut Pasteur, Paris, France

\

## Acknowledgments

The developers & maintainers of the mentioned software and packages,
including:

- [R](https://www.r-project.org/)
- [Git](https://git-scm.com/)
- [GitHub](https://github.com/)
- [rworkflows](https://github.com/neurogenomics/rworkflows)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [opencode](https://opencode.ai/)
- [GLM-5.3](https://docs.z.ai/guides/llm/glm-5.3)

The Information Technology team of Institut Pasteur.
