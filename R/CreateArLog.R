#' Create and Combine Data Frames with Date Processing and Join
#'
#' This function processes a list of data frames, performs date conversions, and
#' combines them into a single data frame. It then joins the combined data frame
#' with another processed data frame (`pdf_cpo_seq_processed`) based on common
#' columns (`arln_accession_id` and `date_of_collection`).
#'
#' @param CSV_OUTPUT_DIR A character string representing the directory path where
#'   the output will be saved (currently not used in the function, but can be
#'   included for future extensions).
#' @param dfs_for_mod A list of data frames that need to be processed. The date
#'   columns (`date_of_birth` and `date_of_collection`) in these data frames
#'   will be standardized to a common date format (`ymd`).
#' @param pdf_cpo_seq_processed A data frame that contains the `COLLECTION_DATE`
#'   column to be joined with the `date_of_collection` column in the combined data
#'   frame.
#'
#' @return A data frame that is the result of the left join between the combined
#'   `dfs_for_mod` data frames and `pdf_cpo_seq_processed`, with the common columns
#'   `arln_accession_id` and `date_of_collection` properly aligned.
#'
#' @details This function first processes the list of data frames provided in
#'   `dfs_for_mod`, converting the `date_of_collection` column to a consistent
#'   `Date` format using `lubridate::ymd()`. The processed data frames are then
#'   combined into a single data frame. After that, the function performs a left
#'   join with the `pdf_cpo_seq_processed` data frame, based on the `arln_accession_id`
#'   and `date_of_collection` columns.
#'
#' @importFrom dplyr bind_rows, left_join, na_if
#' @importFrom lubridate ymd
create_arlog <- function(CSV_OUTPUT_DIR, dfs_for_mod, pdf_cpo_seq_processed) {

  dfs = process_dates(dfs_for_mod)

  combined_df <- dplyr::bind_rows(dfs)

  combined_df <- combined_df %>%
    mutate(date_of_collection = as.character(date_of_collection)) %>%  # Ensure it's character
    mutate(date_of_collection = dplyr::na_if(date_of_collection, 'NA')) %>%  # Replace 'NA' with NA
    mutate(date_of_collection = lubridate::ymd(combined_df$date_of_collection))  # Convert to Date

  pdf_cpo <- pdf_cpo_seq_processed %>%
    mutate(COLLECTION_DATE = lubridate::ymd(COLLECTION_DATE))

  # Perform the left join between combined_df and pdf_cpo
  joined_df <- combined_df %>%
    dplyr::left_join(pdf_cpo, by = c("arln_accession_id" = "ACCESSION_ID",
                              "date_of_collection" = "COLLECTION_DATE"))

  return(joined_df)
}
