#' Process Date Columns in a List of Data Frames
#'
#' This function takes a list of data frames and standardizes the format of the
#' `date_of_birth` and `date_of_collection` columns to a consistent Date format.
#' It ensures that all date columns in the data frames are converted to the
#' specified format (`%m/%d/%Y`), which is typically used for mm/dd/yyyy formatted dates.
#'
#' @param dataframes A list of data frames that need to be processed. Each data frame
#'   should contain `date_of_birth` and `date_of_collection` columns that will be
#'   converted to `Date` objects.
#'
#' @return A list of data frames with the `date_of_birth` and `date_of_collection` columns
#'   converted to Date format. The input list is modified in place.
#'
#' @examples
#' # Example usage with a list of data frames
#' dataframes <- list(ta, wa, ecp, esp, ewp)
#' processed_dataframes <- process_dates(dataframes)
#'
#' # Access processed data frames
#' processed_dataframes[[1]]  # ta
#' processed_dataframes[[2]]  # wa
#'
#' @seealso \code{\link{as.Date}}, \code{\link{bind_rows}}
process_dates <- function(dataframes) {
  # Loop through each dataframe in the list
  for (i in seq_along(dataframes)) {
    df <- dataframes[[i]]

    # Convert 'date_of_birth' column to Date (using a consistent format)
    df$date_of_birth <- as.Date(df$date_of_birth, format = "%m/%d/%Y")

    # Convert 'date_of_collection' column to Date (using the same consistent format)
    df$date_of_collection <- as.Date(df$date_of_collection, format = "%m/%d/%Y")

    # Save the modified dataframe back into the list
    dataframes[[i]] <- df
  }

  # Return the list of modified dataframes
  return(dataframes)
}
