#' Validate required columns
#' @param df A data.frame
#' @param required Character vector of required column names
#' @return Invisibly returns df; errors if missing columns
#' @export
validate_columns = function(df, required=c("word", "label")) {
  miss = setdiff(required, names(df))
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "))
  }
  invisible(df)
}
