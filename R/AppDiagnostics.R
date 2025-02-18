#' Diagnose data structure and quality
#'
#' @param data A data frame to analyze
#' @return Prints diagnostic information and returns a list of diagnostics
#' @export
diagnose_surveillance_data <- function(data) {
  # Structure overview
  message("\nData Structure:")
  message("Dimensions: ", nrow(data), " rows x ", ncol(data), " columns")

  # Column types
  col_types <- sapply(data, class)
  message("\nColumn Types:")
  print(col_types)

  # NA count per column
  na_counts <- sapply(data, function(x) sum(is.na(x)))
  message("\nNA Counts per Column:")
  print(na_counts)

  # Unique values in factor columns
  factor_cols <- names(data)[sapply(data, is.factor)]
  if(length(factor_cols) > 0) {
    message("\nUnique Values in Factor Columns:")
    for(col in factor_cols) {
      message(col, ":")
      print(table(data[[col]], useNA = "ifany"))
    }
  }

  # Date range for date columns
  date_cols <- names(data)[sapply(data, inherits, "Date")]
  if(length(date_cols) > 0) {
    message("\nDate Ranges:")
    for(col in date_cols) {
      message(col, ":")
      message("  Min: ", min(data[[col]], na.rm = TRUE))
      message("  Max: ", max(data[[col]], na.rm = TRUE))
    }
  }

  # Return diagnostics as a list
  return(invisible(list(
    dimensions = dim(data),
    column_types = col_types,
    na_counts = na_counts,
    factor_levels = lapply(data[factor_cols], levels),
    date_ranges = if(length(date_cols) > 0)
      lapply(data[date_cols], range, na.rm = TRUE) else NULL
  )))
}

#' Validate processed data for dashboard
#'
#' @param data A data frame to validate
#' @return Logical indicating if data is valid, with warning messages for issues
#' @export
validate_dashboard_data <- function(data) {
  valid <- TRUE

  # Required columns
  required_cols <- c("date_of_birth", "date_of_collection",
                     "date_received", "date_reported",
                     "screening_or_clinical", "facility_of_origin",
                     "organism", "source")

  missing_cols <- setdiff(required_cols, names(data))
  if(length(missing_cols) > 0) {
    warning("Missing required columns: ",
            paste(missing_cols, collapse = ", "))
    valid <- FALSE
  }

  # Check date columns
  date_cols <- c("date_of_birth", "date_of_collection",
                 "date_received", "date_reported")
  for(col in intersect(date_cols, names(data))) {
    if(!inherits(data[[col]], "Date")) {
      warning(col, " is not a Date class")
      valid <- FALSE
    }
  }

  # Check factor columns
  factor_cols <- c("screening_or_clinical", "facility_of_origin",
                   "organism", "source")
  for(col in intersect(factor_cols, names(data))) {
    if(!is.factor(data[[col]])) {
      warning(col, " is not a factor")
      valid <- FALSE
    }
  }

  # Check for empty data
  if(nrow(data) == 0) {
    warning("Data frame has 0 rows")
    valid <- FALSE
  }

  # Check for all-NA columns
  all_na_cols <- names(which(sapply(data, function(x) all(is.na(x)))))
  if(length(all_na_cols) > 0) {
    warning("Columns with all NA values: ",
            paste(all_na_cols, collapse = ", "))
    valid <- FALSE
  }

  return(valid)
}
