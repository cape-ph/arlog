#' Write Multiple DataFrames to CSV Files with Custom Filenames
#'
#' This function takes a list of data frames and a corresponding list of filenames, then writes each data frame to a CSV file with the provided filenames.
#'
#' @param dfs A list of data frames to be written to CSV files.
#' @param output_dir A character string indicating the directory where the CSV files should be saved.
#' @param filenames A character vector containing the names for the output CSV files. The length must match the number of data frames in `dfs`.
#'
#' @return NULL This function writes the CSVs to the provided directory but does not return any value.
#'
#' @examples
#' dfs <- list(df1, df2, df3)  # List of data frames
#' filenames <- c("file1.csv", "file2.csv", "file3.csv")  # Corresponding filenames
#' write_dfs_to_csv(dfs, "output_directory", filenames)
#'
#' @export
write_dfs_to_csv <- function(dfs, output_dir, filenames) {
  # Check that the number of filenames matches the number of data frames
  if (length(dfs) != length(filenames)) {
    stop("The number of data frames does not match the number of filenames.")
  }

  # Check if output_dir exists, if not create it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Loop through each data frame and write it to a CSV with the corresponding filename
  for (i in seq_along(dfs)) {
    # Create the full file path
    file_path <- file.path(output_dir, filenames[i])

    # Write the data frame to a CSV file
    write.csv(dfs[[i]], file_path, row.names = FALSE)

    # Print a message confirming the write
    message(paste("DataFrame", i, "written to", file_path))
  }
}
