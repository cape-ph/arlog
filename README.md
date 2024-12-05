# arlog
Package to process files for AR Log database creation.

**arlog** is an R package that converts pdf, excel, csv, and docx files to csv's to facilitate importing into an SQL database. 

## Installation

You can install **arlog** directly from GitHub.

```r
install.packages("arlog")
```

## Usage
Users should alter the Main.R file such that the files of each type are sorted into their respective folders. 
i.e. Excel Web Portal files to the EXCEL_ARLN_WEB_PORTAL directory, etc. This allows for use of the bulk export function: 
```r
export_data()
```
Execution will fail if any of the input folders are empty, or if files are put into incorrect folders, i.e. CRE Microsft Word Alert docx files are put into ARLN Pdf folders, etc.  

Once the import files are correctly sorted run export_data() and the ouput folder will be populated with the csv files from the input data files. 

For specific files use any of the singular process_xxx functions. 
  ```r
  # Tennessee ARLN ad PDF files
  tenn_arln_df <- create_tenn_arln(PDF_ARLN_DIR)
  process_tenn_arln(tenn_arln_df, CSV_OUTPUT_DIR)

  # CRE Alerts as docx files
  word_alert_df <- create_word_alert(WORD_ALERT_DIR)
  process_word_alert(word_alert_df, CSV_OUTPUT_DIR)

  # CPO Sequencing Files as PDF files
  cpo_df <- create_excel_cpo(EXCEL_CPO_DIR)
  process_excel_cpo(cpo_df, CSV_OUTPUT_DIR)

  # Sentinel Reports as xlsx files
  excel_sentinel_df <- create_sentinel(EXCEL_SENTINEL_DIR)
  process_excel_sentinel(excel_sentinel_df, CSV_OUTPUT_DIR)

  # CPO Seuqncing Files as pdfs
  pdf_cpo_seq <- create_cpo_seq(PDF_CPO_SEQ_DIR)
  process_pdf_cpo_seq(pdf_cpo_seq, CSV_OUTPUT_DIR)

  # ARLN Web Portal Files as csvs
  excel_web_port_df <-create_web_portal(EXCEL_ARLN_WEB_PORTAL_DIR)
  process_excel_sentinel(excel_sentinel_df, CSV_OUTPUT_DIR)
  

