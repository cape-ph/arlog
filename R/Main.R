EXCEL_ARLN_WEB_PORTAL_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/ExcelArlnWebPortal"
EXCEL_CPO_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/ExcelCpo"
EXCEL_SENTINEL_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/ExcelSentinel"
PDF_ARLN_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/PdfArln"
PDF_CPO_SEQ_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/PdfCpoSeq"
WORD_ALERT_DIR = "/Users/edonate3/Documents/CAPE/HAI_Data/WordAlert"

CSV_OUTPUT_DIR = "/Users/edonate3/Documents/CAPE/HAI_Processed/"

export_data <- function(CSV_OUTPUT_DIR) {
  tenn_arln_df <- create_tenn_arln(PDF_ARLN_DIR)
  word_alert_df <- create_word_alert(WORD_ALERT_DIR)
  cpo_df <- create_excel_cpo(EXCEL_CPO_DIR)
  excel_sentinel_df <- create_sentinel(EXCEL_SENTINEL_DIR)
  pdf_cpo_seq <- create_cpo_seq(PDF_CPO_SEQ_DIR)
  excel_web_port_df <-create_web_portal(EXCEL_ARLN_WEB_PORTAL_DIR)

  process_tenn_arln(tenn_arln_df, CSV_OUTPUT_DIR)
  process_word_alert(word_alert_df, CSV_OUTPUT_DIR)
  process_excel_cpo(cpo_df, CSV_OUTPUT_DIR)
  process_excel_sentinel(excel_sentinel_df, CSV_OUTPUT_DIR)
  process_pdf_cpo_seq(pdf_cpo_seq, CSV_OUTPUT_DIR)
  process_web_portal(excel_web_port_df, CSV_OUTPUT_DIR)
}
