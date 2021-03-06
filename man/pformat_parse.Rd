% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{pformat_parse}
\alias{pformat_parse}
\title{Preparses a format string}
\usage{
pformat_parse(format_string)
}
\arguments{
\item{format_string}{the format string}
}
\value{
an object of class \code{pformat.compiled} containing an internal
representation of the format string.
}
\description{
Parses a format string into an internal representation
compatible with \code{pformat()}.
}
\details{
This function actually only parses a "layer" of the format string
markup. Recursive format strings depend on data, and thus can't be
completely parsed yet. In these cases, \code{pformat_parse()} will still be
called by \code{pformat()} with format specifications needing expanding.
}
\examples{
pformat_parse("{} {}")
pformat_parse("{1} {2}")
}
