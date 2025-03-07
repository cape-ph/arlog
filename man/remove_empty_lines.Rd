% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ToolsPdfExtraction.R
\name{remove_empty_lines}
\alias{remove_empty_lines}
\title{Remove Empty Lines}
\usage{
remove_empty_lines(lines)
}
\arguments{
\item{lines}{A character vector containing the lines of text.}
}
\value{
A character vector containing only the non-empty lines from the input.
}
\description{
This function removes empty or whitespace-only lines from a character vector.
It trims the whitespace from each line and then filters out any lines that
are empty. The function returns a vector of lines with all non-empty content.
}
\examples{
lines <- c("Line 1", "  ", "Line 2", "", "Line 3")
result <- remove_empty_lines(lines)
# result will be c("Line 1", "Line 2", "Line 3")
print(result)

}
