create_arlog <- function(CSV_OUTPUT_DIR) {
  tenn_arln_df <- create_tenn_arln(PDF_ARLN_DIR)
  tenn_arln_processed = process_tenn_arln(tenn_arln_df)


  word_alert_df <- create_word_alert(WORD_ALERT_DIR)
  word_alert_processed = process_word_alert(word_alert_df)


  cpo_df <- create_excel_cpo(EXCEL_CPO_DIR)
  excel_cpo_processed = process_excel_cpo(cpo_df)


  excel_sentinel_df <- create_sentinel(EXCEL_SENTINEL_DIR)
  excel_sentinel_processed = process_excel_sentinel(excel_sentinel_df)


  pdf_cpo_seq <- create_cpo_seq(PDF_CPO_SEQ_DIR)
  pdf_cpo_seq_processed = process_pdf_cpo_seq(pdf_cpo_seq)


  excel_web_port_df <-create_web_portal(EXCEL_ARLN_WEB_PORTAL_DIR)
  excel_web_portal_processed = process_web_portal(excel_web_port_df)

