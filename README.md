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
i.e. Excel Web Portal files to the EXCEL_ARLN_WEB_PORTAL directory, etc. 

For bulk processing of all file types in all folders, sort all the files into the folders and run export_data() from Main.R. 

For specific files use any of the commands in the Main.R, export_data function with the preferred directories for input and output.
  ```r
  process_tenn_arln(create_tenn_arln(PDF_ARLN_DIR), CSV_OUTPUT_DIR)
  process_word_alert(create_word_alert(WORD_ALERT_DIR), CSV_OUTPUT_DIR)
  process_excel_cpo(create_excel_cpo(EXCEL_CPO_DIR), CSV_OUTPUT_DIR)
  process_excel_sentinel(create_sentinel(EXCEL_SENTINEL_DIR), CSV_OUTPUT_DIR)
  process_pdf_cpo_seq(create_cpo_seq(PDF_CPO_SEQ_DIR), CSV_OUTPUT_DIR)
  process_web_portal(create_web_portal(EXCEL_ARLN_WEB_PORTAL_DIR), CSV_OUTPUT_DIR)
```
