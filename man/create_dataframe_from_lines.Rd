% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ToolsPdfExtraction.R
\name{create_dataframe_from_lines}
\alias{create_dataframe_from_lines}
\title{Create Data Frame from Lines of Text}
\usage{
create_dataframe_from_lines(lines)
}
\arguments{
\item{lines}{A character vector where each element is a line of text to be
processed. Each line is expected to have columns separated by two or more
spaces.}
}
\value{
A data frame where the first row is used as the column names, and
the remaining rows are the data.
}
\description{
This function takes a character vector of lines, removes any empty lines,
splits each line into columns based on 2 or more spaces, and converts the
result into a data frame. The first row of the parsed lines is used as the
column names. The function ensures that all rows have the same number of
columns by padding with \code{NA} if necessary.
}
\examples{
lines <- c("Column1   Column2   Column3", "A   B   C", "D   E   F")
df <- create_dataframe_from_lines(lines)
# df will be a data frame with the first row as column names and the rest
# as data
print(df)

}
