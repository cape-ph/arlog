% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ToolsPdfExtraction.R
\name{select_rows_by_list}
\alias{select_rows_by_list}
\title{Select Rows by Column Values from a List}
\usage{
select_rows_by_list(data, col_name, value_list)
}
\arguments{
\item{data}{A dataframe to filter.}

\item{col_name}{A string representing the column name to filter by.}

\item{value_list}{A vector of values. The function will return all rows where
the specified column contains a value from this list.}
}
\value{
A filtered dataframe that includes only the rows where the value in the
specified column matches one of the values from the provided list.
}
\description{
This function filters a dataframe to select rows where the values in a
specified column match any of the values in a provided list. The function
uses the \code{dplyr::filter} function and the \code{\%in\%} operator to filter rows
based on the given list of values. It is useful for subsetting a dataframe
based on a  specific set of values from a column.
}
\examples{
# Example 1: Tenn ARLN pdf
# Use system.file to get the path to the example document
  pdf_file_path <- system.file("extdata",
                               "pdf_tnarln_example_1.pdf",
                                package = "arlog")

# Extract the lines from the pdf
  pdf <- pdftools::pdf_text(pdf_file_path)
# Convert the list of list of lists into a single list where each item is a
# line from the document.
  lines <- unlist(strsplit(pdf, "\n"))
  result <- pdf_extract_lines_between(lines, "Accession", "Key")
# Convert the selected lines into a dataframe of three columns.
  result_df <- pdf_split_lines_into_columns(result)
# Extract the three columns from the dataframe.
  extract_table <- extract_from_dataframe(result_df)
  selection <- select_rows_by_list(extract_table, "Keys", VALUES_TO_KEEP)
  print(extract_table)

# Example 2: Filter rows where 'col2' contains values 3 or 5
data2 <- data.frame(
  col1 = c("E", "F", "G", "H", "I"),
  col2 = c(3, 4, 5, 6, 7),
  stringsAsFactors = FALSE
)
select_rows_by_list(data2, "col2", c(3, 5))
# Returns a dataframe with rows where col2 is 3 or 5:
# col1 col2
# 1   E    3
# 3   G    5

}
