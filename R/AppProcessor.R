#' Process surveillance data for dashboard display
#'
#' @param data A data frame or path to CSV file containing surveillance data
#' @param na.strings Vector of strings to be treated as NA values
#' @param remove_empty Logical, whether to remove rows where all required fields are NA
#' @return A tibble with processed dates and standardized columns
#' @importFrom readr read_csv
#' @importFrom dplyr %>% filter_at vars any_vars
#' @importFrom lubridate as_date parse_date_time
#' @importFrom purrr map_vec
#' @export
process_surveillance_data <- function(data,
                                      na.strings = c("", "NA", "N/A", "NULL", "<NA>"),
                                      remove_empty = TRUE) {
  # If input is a file path, read it with NA strings specified
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) {
      stop("File does not exist: ", data)
    }
    data <- read_csv(data,
                     na = na.strings,
                     show_col_types = FALSE)
  }

  # Validate that it's a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or path to CSV file")
  }

  # List of required columns
  required_cols <- c("date_of_birth", "date_of_collection",
                     "date_received", "date_reported",
                     "screening_or_clinical", "facility_of_origin",
                     "organism", "source")

  # Check for required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Process dates
  parse_flexible_date <- function(x) {
    if (is.na(x) || x %in% na.strings) return(NA)
    parsed_date <- try({
      as_date(parse_date_time(x, orders = c(
        "ymd", "mdy", "dmy",
        "ymd HMS", "mdy HMS", "dmy HMS",
        "ymd HM", "mdy HM", "dmy HM"
      )))
    }, silent = TRUE)
    if (inherits(parsed_date, "try-error")) return(NA)
    return(parsed_date)
  }

  # Process date columns with warning for NA conversion
  date_cols <- c("date_of_birth", "date_of_collection",
                 "date_received", "date_reported")
  for (col in date_cols) {
    original_nas <- sum(is.na(data[[col]]))
    data[[col]] <- purrr::map_vec(as.character(data[[col]]), parse_flexible_date)
    new_nas <- sum(is.na(data[[col]]))
    if (new_nas > original_nas) {
      warning(sprintf("Column %s: %d values converted to NA during date parsing",
                      col, new_nas - original_nas))
    }
  }

  # Convert categorical variables to factors, handling NAs
  factor_cols <- c("screening_or_clinical", "facility_of_origin",
                   "organism", "source")
  for (col in factor_cols) {
    data[[col]] <- factor(data[[col]],
                          exclude = NULL)  # Keep NA as level
  }

  # Remove rows where all required fields are NA if requested
  if (remove_empty) {
    required_for_display <- c("date_of_collection", "organism", "facility_of_origin")
    initial_rows <- nrow(data)
    data <- data %>%
      filter_at(vars(one_of(required_for_display)),
                any_vars(!is.na(.)))
    rows_removed <- initial_rows - nrow(data)
    if (rows_removed > 0) {
      warning(sprintf("Removed %d rows where all required fields were NA",
                      rows_removed))
    }
  }

  return(data)
}
