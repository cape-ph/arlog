#' Process the Tenn ARLN Dataframe
#'
#' This function processes the input `tenn_arln_processed` dataframe by selecting specific
#' columns, renaming them, and adding constant columns as described in the SQL query.
#'
#' @param df A data frame (usually `tenn_arln_processed`) that contains the necessary columns
#'   such as `PATIENT_NAME`, `DATE_OF_BIRTH`, `FACILITY`, `ACCESSION_NUM`, etc.
#'
#' @return A transformed data frame with renamed columns and additional constant columns.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr setdiff
#'
#' @examples
#' # Assuming `tenn_arln_processed` is loaded:
#' transformed_df <- process_tenn_arln(tenn_arln_processed)
process_tenn_arln <- function(df) {

  # Check if the input dataframe has the required columns
  required_columns <- c("PATIENT_NAME", "DATE_OF_BIRTH", "FACILITY",
                        "ACCESSION_NUM", "RESULT", "DATE_COLLECTED",
                        "SPECIMEN_TYPE", "DATE_RECEIVED")

  missing_columns <- dplyr::setdiff(required_columns, colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Transform the dataframe
  df_transformed <- df %>%
    dplyr::select(
      patient_name = PATIENT_NAME,
      date_of_birth = DATE_OF_BIRTH,
      screening_or_clinical = 'not in dataset',
      district = 'not in dataset',
      facility_of_origin = 'not in dataset',
      state_lab_id = FACILITY,
      arln_accession_id = ACCESSION_NUM,
      sentinel_lab_id = 'not in dataset',
      organism = RESULT,
      mechanism_submitters_report = 'not in dataset',
      date_of_collection = DATE_COLLECTED,
      source = SPECIMEN_TYPE,
      date_received = DATE_RECEIVED,
      date_reported = 'not in dataset',
      testing_lab = FACILITY,
      wgs_id = 'not in dataset',
      mlst_st = 'not in dataset',
      blaimp_27 = 999,
      blakpc_4 = 999,
      blandm_1 = 999,
      blandm_5 = 999,
      blaoxa_181_blaoxa_48_like = 999,
      blaoxa_1_blaoxa_1_like = 999
    )

  # Return the transformed dataframe
  return(df_transformed)
}
