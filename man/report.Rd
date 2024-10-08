% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{report}
\alias{report}
\title{report}
\usage{
report(
  data,
  output = "results.txt",
  path,
  overwrite = FALSE,
  rownames.kept = FALSE,
  vector.cat = FALSE,
  noquote = TRUE,
  sep = 2,
  safer_check = TRUE
)
}
\arguments{
\item{data}{Object to print in the output file. If NULL, nothing is done, with no warning.}

\item{output}{Single character string. Name of the output file.}

\item{path}{Location of the output file.}

\item{overwrite}{Single logical value. If output file already exists, defines if the printing is appended (default FALSE) or if the output file content is erased before printing (TRUE).}

\item{rownames.kept}{Single logical value. Defines whether row names have to be removed or not in small tables (less than length.rows rows).}

\item{vector.cat}{Single logical value. If TRUE print a vector of length > 1 using cat() instead of capture.output(). Otherwise (default FALSE) the opposite.}

\item{noquote}{Single logical value. If TRUE no quote are present for the characters.}

\item{sep}{Single integer representing the number of empty lines after printed data.}

\item{safer_check}{Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.}
}
\value{
Nothing.
}
\description{
Log file function: print a character string or a data object into a same output file.
}
\examples{
#report()
report(data = 1:3, output = "results.txt", path = ".", overwrite = TRUE, 
rownames.kept = FALSE, vector.cat = FALSE, noquote = FALSE, sep = 2)
}
\seealso{
\code{\link{capture.output}}.
}
\author{
Gael Millot <gael.millot@pasteur.fr>

Yushi Han <yushi.han2000@gmail.com>

Haiding Wang <wanghaiding442@gmail.com>
}
