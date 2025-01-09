#' @title Process the 'tenn_arln' Data Frame and Save as CSV
#'
#' @description
#' This function processes the `tenn_arln_df` data frame by applying a series of
#' transformations:
#' 1. Replaces spaces in column names with underscores.
#' 2. Replaces any `#` characters in column names with the string "Num".
#' 3. Capitalizes all column names.
#'
#' @param tenn_arln_df A data frame with columns that may contain spaces, hash characters,
#' and uncapitalized letters in the column names.
#'
#' @return A data frame with modified column names: spaces replaced by underscores, hash characters replaced by 'num', and capitalized column names.
#'
#' @examples
#' # Example usage:
#' processed_df <- process_tenn_arln(tenn_arln_df)
#'
#' @export
process_tenn_arln <- function(tenn_arln_df) {
  s1 <- replace_spaces_with_underscores(tenn_arln_df)
  s2 <- replace_hash_with_num(s1)
  s3 <- capitalize_column_names(s2)

  return(s3)
}


#' @title Process the 'word_alert' Data Frame
#'
#' @description
#' This function processes the `word_alert_df` data frame by applying a series of
#' transformations:
#' 1. Replaces spaces in column names with underscores.
#' 2. Capitalizes all column names.
#' 3. Replaces "DOB" with "DATE_OF_BIRTH" in column names.
#' 4. Replaces hyphens (more than 2 consecutive hyphens) in character columns
#' with `NA`.
#' 5. Converts date columns ("DATE_OF_BIRTH" and "DATE_OF_COLLECTION") from
#' character to Date type using the specified date format (`%m-%d-%Y`).
#'
#' @param word_alert_df A data frame with columns that may contain spaces, hyphens,
#' and need to have certain names replaced (e.g., DOB).
#'
#' @return A processed data frame with:
#' \itemize{
#'   \item Column names with spaces replaced by underscores and capitalized.
#'   \item "DOB" replaced with "DATE_OF_BIRTH".
#'   \item Hyphens in the data replaced with NA.
#'   \item Date columns converted to the format specified.
#' }
#'
#' @examples
#' # Example usage:
#' processed_df <- process_word_alert(word_alert_df)
#'
#' @export
process_word_alert <- function(word_alert_df) {
  date_columns <- c("DATE_OF_BIRTH", "DATE_OF_COLLECTION")
  s1 <- replace_spaces_with_underscores(word_alert_df)
  s2 <- capitalize_column_names(s1)
  s3 <- replace_dob_with_date_of_birth(s2)
  s4 <- replace_hyphens_with_na(s3)
  s5 <- convert_date_columns(s4,date_columns, date_format = "%m-%d-%Y")

return(s5)
}


#' @title Process Excel CPO Data
#'
#' @description This function processes raw CPO data from an Excel file by
#' cleaning the column names and converting date columns to the correct format.
#'
#' @param cpo_df A DataFrame containing the raw CPO data from an Excel sheet.
#'
#' @details The function performs the following operations on the input DataFrame:
#' - Cleans Excel column names (removes carriage returns, newlines, and
#' converts characters).
#' - Capitalizes all column names.
#' - Replaces "DOB" with "DATE_OF_BIRTH" and capitalizes.
#' - Replaces double underscores in column names with single underscores.
#' - Replaces spaces in column names with underscores.
#' - Converts date columns to the correct date format (MM/DD/YYYY).
#'
#'
#' @return A processed data frame with:
#' \itemize{
#'   \item Column names cleaned and modified according to the steps outlined above.
#'   \item Date columns converted to the format specified.
#' }
#'
#' @examples
#' # Example usage:
#' processed_df <- process_excel_cpo(cpo_df)
#'
#' @export
process_excel_cpo <- function(cpo_df) {
  date_columns <- c("DATE_SPECIMEN_RECEIVED", "DATE_REPORTED",
                    "DATE_OF_BIRTH", "DATE_OF_COLLECTION_(MM/DD/YYYY)")

  s1 <- clean_excel_column_names(cpo_df)
  s2 <- capitalize_column_names(s1)
  s3 <- replace_dob_with_date_of_birth(s2)
  s4 <- replace_double_underscore(s3)
  s5 <- replace_spaces_with_underscores(s4)
  s6 <- replace_excel_dots_with_underscores(s5)
  s7 <- convert_excel_dates(s6, date_columns)

  return(s7)
}


#' @title Process Excel Sentinel Data
#'
#' @description This function processes raw Sentinel data from an Excel file,
#' specifically converting certain date columns to the correct date format.
#'
#' @importFrom utils write.csv
#' @param df A DataFrame containing the raw Sentinel data from an Excel sheet.
#'
#' @details The function performs the following operations on the input
#' DataFrame:
#' - Replaces "DOB" with "DATE_OF_BIRTH" and ensures correct date format.
#' - Converts the columns `DATE_OF_BIRTH`, `CULTUREDT`, and `COLLECTIONDT`
#' to the correct date format (MM/DD/YYYY).
#'
#'
#' @return A processed data frame with:
#' \itemize{
#'   \item The column name "DOB" replaced by "DATE_OF_BIRTH".
#'   \item Date columns converted to the format specified (e.g., "%m/%d/%Y").
#' }
#'
#' @examples
#' # Example usage:
#' processed_df <- process_excel_sentinel(df)
#'
#' @export
process_excel_sentinel <- function(df){
  date_columns <- c("DATE_OF_BIRTH", "CULTUREDT","COLLECTIONDT")
  s1 <- replace_dob_with_date_of_birth(df)
  s2 <- convert_date_columns(s1, date_columns, date_format = "%m/%d/%Y")

  return(s2)
}


#' @title Process PDF CPO Sequence Data
#'
#' @description This function processes raw PDF CPO sequence data, specifically
#' converting the `COLLECTION_DATE` column to the correct date format
#' (MM/DD/YYYY).
#'
#' @param df A DataFrame containing the raw CPO sequence data extracted from a
#' PDF.
#'
#' @details The function performs the following operations on the input
#' DataFrame:
#' - Capitalizes the column names to ensure uniformity.
#' - Converts the `COLLECTION_DATE` column to the correct date format
#' (MM/DD/YYYY).
#'
#' @return A processed data frame with:
#' \itemize{
#'   \item Column names capitalized.
#'   \item Date columns converted to the format specified (e.g., "%m/%d/%Y").
#' }
#'
#' @examples
#' # Example usage:
#' processed_df <- process_pdf_cpo_seq(df)
#'
#' @export
process_pdf_cpo_seq <- function(df){
  date_columns <- c("COLLECTION_DATE")
  s1 <- capitalize_column_names(df)
  s2 <- convert_date_columns(s1, date_columns, date_format = "%m/%d/%Y")

  return(s2)
}


#' @title Process Web Portal Data
#'
#' @description This function processes web portal data by capitalizing column
#' names, replacing spaces with underscores, replacing Excel-style dots with
#' underscores, and trimming datetime columns to remove the time component.
#'
#' @param df A DataFrame containing the raw web portal data.
#'
#' @details The function performs the following operations:
#' - Capitalizes all column names in the data frame.
#' - Replaces spaces with underscores in column names.
#' - Replaces dots (.) in column names with underscores, handling Excel-specific
#' formatting.
#' - Trims datetime columns to remove the time portion, leaving only the date.
#'
#' @return A processed data frame with:
#' \itemize{
#'   \item Column names capitalized, spaces replaced with underscores, and dots replaced with underscores.
#'   \item Adjustments to datetime columns based on the `cut_off_time` function.
#' }
#'
#' @examples
#' # Example usage:
#' processed_df <- process_web_portal(df)
#'
#' @export
process_web_portal <- function(df){
  datetime_columns <- c("DATE_COLLECTED","DATE_RECEIVED", "DATE_RELEASED")

  s1 <- capitalize_column_names(df)
  s2 <- replace_spaces_with_underscores(s1)
  s3 <- replace_excel_dots_with_underscores(s2)
  s4 <- cut_off_time(s3, datetime_columns)

  return(s4)
}
