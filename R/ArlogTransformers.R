#' Transform Tennessee ARLN Data
#'
#' This function transforms the column names of a dataframe to match the structure
#' of the SQL query format for Tennessee ARLN data.
#'
#' @param df A dataframe containing columns related to Tennessee ARLN data.
#' @return A transformed dataframe with the same structure as the desired output.
#' @examples
#' df <- data.frame(PATIENT_NAME = c("John Doe", "Jane Smith"),
#'                  DATE_OF_BIRTH = as.Date(c("1990-01-01", "1985-05-15")),
#'                  FACILITY = c("Facility A", "Facility B"),
#'                  ACCESSION_NUM = c("A123", "B456"),
#'                  RESULT = c("Positive", "Negative"),
#'                  DATE_COLLECTED = as.Date(c("2024-01-01", "2024-02-02")),
#'                  SPECIMEN_TYPE = c("Blood", "Saliva"),
#'                  DATE_RECEIVED = as.Date(c("2024-01-02", "2024-02-03")))
#' transformed_df <- transform_tenn_arln(df)
#' @export
transform_tenn_arln <- function(df) {

  # Ensure necessary columns are present in the dataframe
  if(!all(c("PATIENT_NAME", "DATE_OF_BIRTH", "FACILITY", "ACCESSION_NUM",
            "RESULT", "DATE_COLLECTED", "SPECIMEN_TYPE", "DATE_RECEIVED") %in% colnames(df))) {
    stop("Input dataframe must contain the following columns: PATIENT_NAME, DATE_OF_BIRTH,
          FACILITY, ACCESSION_NUM, RESULT, DATE_COLLECTED, SPECIMEN_TYPE, DATE_RECEIVED.")
  }

  # Create the transformed dataframe
  df_transformed <- data.frame(
    patient_name = df$PATIENT_NAME,
    date_of_birth = df$DATE_OF_BIRTH,
    screening_or_clinical = NA,
    district = NA,
    facility_of_origin = NA,
    state_lab_id = df$FACILITY,
    arln_accession_id = df$ACCESSION_NUM,
    sentinel_lab_id = NA,
    organism = df$RESULT,
    mechanism_submitters_report = NA,
    date_of_collection = df$DATE_COLLECTED,
    source = df$SPECIMEN_TYPE,
    date_received = df$DATE_RECEIVED,
    date_reported = NA,
    testing_lab = df$FACILITY,
    wgs_id = NA,
    mlst_st = NA,
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


#' Transform Web Portal Data
#'
#' This function transforms the column names of an R dataframe to match the structure
#' specified in the given SQL query for the `public.web_portal` table.
#'
#' @param df A dataframe with the appropriate columns from the web portal data.
#' @return A transformed dataframe with the same structure as the SQL query.
#' @examples
#' df <- data.frame(PATIENT_NAME = c("John Doe", "Jane Smith"),
#'                  PATIENT_DOB = as.Date(c("1990-01-01", "1985-05-15")),
#'                  LIMS_ACCESSION_ID = c("L123", "L456"),
#'                  DATE_COLLECTED = as.Date(c("2024-01-01", "2024-02-02")),
#'                  DATE_RECEIVED = as.Date(c("2024-01-02", "2024-02-03")),
#'                  FACILITY_NAME = c("Test Lab 1", "Test Lab 2"))
#' transformed_df <- transform_web_portal(df)
#' @export
transform_web_portal <- function(df) {

  # Ensure necessary columns are present in the dataframe
  if(!all(c("PATIENT_NAME", "PATIENT_DOB", "LIMS_ACCESSION_ID", "DATE_COLLECTED",
            "DATE_RECEIVED", "FACILITY_NAME") %in% colnames(df))) {
    stop("Input dataframe must contain the following columns: PATIENT_NAME, PATIENT_DOB,
          LIMS_ACCESSION_ID, DATE_COLLECTED, DATE_RECEIVED, and FACILITY_NAME.")
  }

  # Create the transformed dataframe
  transformed_df <- data.frame(
    patient_name = df$PATIENT_NAME,
    date_of_birth = df$PATIENT_DOB,
    screening_or_clinical = NA,
    district = NA,
    facility_of_origin = NA,
    state_lab_id = NA,
    arln_accession_id = df$LIMS_ACCESSION_ID,
    sentinel_lab_id = NA,
    organism = NA,
    mechanism_submitters_report = NA,
    date_of_collection = df$DATE_COLLECTED,
    source = NA,
    date_received = df$DATE_RECEIVED,
    date_reported = NA,
    testing_lab = df$FACILITY_NAME,
    wgs_id = NA,
    mlst_st = NA,
    blaimp_27 = 999,
    blakpc_4 = 999,
    blandm_1 = 999,
    blandm_5 = 999,
    blaoxa_181_blaoxa_48_like = 999,
    blaoxa_1_blaoxa_1_like = 999
  )

  return(transformed_df)
}


#' Transform Word Alert Data
#'
#' This function transforms the column names of a dataframe to match the structure
#' of the SQL query format for Word Alert data.
#'
#' @param df A dataframe containing columns related to Word Alert data.
#' @return A transformed dataframe with the same structure as the desired output.
#' @examples
#' df <- data.frame(PATIENT_NAME = c("John Doe", "Jane Smith"),
#'                  LAB_ID = c("L123", "L456"),
#'                  DATE_OF_BIRTH = as.Date(c("1990-01-01", "1985-05-15")),
#'                  RECEIVED_FROM = c("Facility A", "Facility B"),
#'                  SOURCE = c("Blood", "Saliva"),
#'                  DATE_OF_COLLECTION = as.Date(c("2024-01-01", "2024-02-02")),
#'                  ORGANISM_ID = c("Org1", "Org2"),
#'                  CARBA_R_RESULT = c("Positive", "Negative"))
#' transformed_df <- transform_word_alert(df)
#' @export
transform_word_alert <- function(df) {

  # Ensure necessary columns are present in the dataframe
  required_columns <- c("PATIENT_NAME", "LAB_ID", "DATE_OF_BIRTH", "RECEIVED_FROM",
                        "SOURCE", "DATE_OF_COLLECTION", "ORGANISM_ID", "CARBA_R_RESULT")

  missing_columns <- dplyr::setdiff(required_columns, colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Create the transformed dataframe
  df_transformed <- data.frame(
    patient_name = df$PATIENT_NAME,
    date_of_birth = df$DATE_OF_BIRTH,
    screening_or_clinical = NA,
    district = NA,
    facility_of_origin = df$RECEIVED_FROM,
    state_lab_id = df$LAB_ID,
    arln_accession_id = df$LAB_ID,
    sentinel_lab_id = NA,
    organism = df$ORGANISM_ID,
    mechanism_submitters_report = df$CARBA_R_RESULT,
    date_of_collection = df$DATE_OF_COLLECTION,
    source = df$SOURCE,
    date_received = NA,
    date_reported = NA,
    testing_lab = NA,
    wgs_id = NA,
    mlst_st = NA,
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



#' Transform Excel CPO Data
#'
#' This function transforms the column names of a dataframe to match the structure
#' of the SQL query format for Excel CPO data, ensuring all columns are accounted for
#' including those with hyphens and special characters.
#'
#' @param df A dataframe containing columns related to Excel CPO data.
#' @return A transformed dataframe with the same structure as the desired output.
#' @export
transform_excel_cpo <- function(df) {

  # Ensure necessary columns are present in the dataframe
  required_columns <- c("DESCRIPTION_OF_TESTING_COMPLETED_AND_RESULTS_(INCLUDING_ORGANISM_NAME)",
                        "LAST_NAME", "FIRST_NAME", "DATE_OF_BIRTH", "DATE_OF_COLLECTION_(MM/DD/YYYY)",
                        "SPECIMEN_SOURCE", "DATE_SPECIMEN_RECEIVED", "DATE_REPORTED",
                        "SUBMITTING_FACILITY", "SAMPLE_COLLECTION_FACILITY_(IF_INCLUDED_ON_PH-4182_FORM)")

  missing_columns <- dplyr::setdiff(required_columns, colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Create the transformed dataframe
  df_transformed <- data.frame(
    patient_name = paste(df$LAST_NAME, df$FIRST_NAME, sep = ", "),
    date_of_birth = df$DATE_OF_BIRTH,
    screening_or_clinical = NA,
    district = NA,
    facility_of_origin = df$`SAMPLE_COLLECTION_FACILITY_(IF_INCLUDED_ON_PH-4182_FORM)`,
    state_lab_id = NA,
    arln_accession_id = NA,
    sentinel_lab_id = NA,
    organism = df$`DESCRIPTION_OF_TESTING_COMPLETED_AND_RESULTS_(INCLUDING_ORGANISM_NAME)`,
    mechanism_submitters_report = NA,
    date_of_collection = df$`DATE_OF_COLLECTION_(MM/DD/YYYY)`,
    source = df$SPECIMEN_SOURCE,
    date_received = df$DATE_SPECIMEN_RECEIVED,
    date_reported = df$DATE_REPORTED,
    testing_lab = df$SUBMITTING_FACILITY,
    wgs_id = NA,
    mlst_st = NA,
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


#' Transform Excel Sent Data
#'
#' This function transforms the column names of a dataframe to match the structure
#' of the SQL query format for Excel Sent data, ensuring all columns are accounted for.
#'
#' @param df A dataframe containing columns related to Excel Sent data.
#' @return A transformed dataframe with the same structure as the desired output.
#' @examples
#' df <- data.frame(
#'   DISEASE = c("Disease1", "Disease2"),
#'   ADMINSTATUS = c("Active", "Inactive"),
#'   PATIENTID = c("P123", "P456"),
#'   INCIDENTID = c("I123", "I456"),
#'   LASTNAME = c("Doe", "Smith"),
#'   FIRSTNAME = c("John", "Jane"),
#'   DATE_OF_BIRTH = as.Date(c("1990-01-01", "1985-05-15")),
#'   LAB = c("Lab1", "Lab2"),
#'   LABOTHER = c("OtherLab1", "OtherLab2"),
#'   ACCESSION_NUM = c("A123", "A456"),
#'   CULTUREDT = as.Date(c("2024-01-01", "2024-02-01")),
#'   ORGANISM = c("Organism1", "Organism2"),
#'   SPECIMENTYPE = c("Type1", "Type2"),
#'   CULTURE...14 = c("Culture1", "Culture2"),
#'   PHENOTYPIC = c("Positive", "Negative"),
#'   CIMRESULT = c("Resistant", "Sensitive"),
#'   MCIMRESULT = c("Positive", "Negative"),
#'   MHTRESULT = c("Positive", "Negative"),
#'   MBLRESULT = c("Positive", "Negative"),
#'   CARBANPRESULT = c("Positive", "Negative"),
#'   GENOTYPIC = c("Gene1", "Gene2"),
#'   PCRRESULT = c("Positive", "Negative"),
#'   CARBARRESULT = c("Positive", "Negative"),
#'   CULTURE...24 = c("Culture3", "Culture4"),
#'   CULTUREFAC = c("Facility1", "Facility2"),
#'   HOSPFAC = c("Hospital1", "Hospital2"),
#'   OUTBREAKRELATE = c("Yes", "No"),
#'   OTHEROUTBREAK = c("No", "Yes"),
#'   COLLECTIONDT = as.Date(c("2024-01-01", "2024-02-01")),
#'   ORG = c("Org1", "Org2"),
#'   SPECIMENSOURCE = c("Blood", "Saliva"),
#'   CULTURE = c("Culture5", "Culture6")
#' )
#' transformed_df <- transform_excel_sent(df)
#' @export
transform_excel_sent <- function(df) {

  # Ensure necessary columns are present in the dataframe
  required_columns <- c("LASTNAME", "FIRSTNAME", "DATE_OF_BIRTH", "SPECIMENTYPE", "HOSPFAC",
                        "LAB", "ACCESSION_NUM", "DISEASE", "COLLECTIONDT", "SPECIMENSOURCE")

  missing_columns <- dplyr::setdiff(required_columns, colnames(df))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Create the transformed dataframe
  df_transformed <- data.frame(
    patient_name = paste(df$LASTNAME, df$FIRSTNAME, sep = ", "),
    date_of_birth = df$DATE_OF_BIRTH,
    screening_or_clinical = df$SPECIMENTYPE,
    district = NA,
    facility_of_origin = df$HOSPFAC,
    state_lab_id = df$LAB,
    arln_accession_id = df$ACCESSION_NUM,
    sentinel_lab_id = NA,
    organism = df$DISEASE,
    mechanism_submitters_report = NA,
    date_of_collection = df$COLLECTIONDT,
    source = df$SPECIMENSOURCE,
    date_received = NA,
    date_reported = NA,
    testing_lab = df$LAB,
    wgs_id = NA,
    mlst_st = NA,
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


