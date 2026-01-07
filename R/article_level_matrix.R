#' Article-level matrix (counts and shares)
#'
#' Builds a per-document wide table for a given label (e.g., "label"),
#' returning counts and *_share columns.
#'
#' @param df        Data frame with at least an id column and a label column.
#' @param label_col Name of the label column (string), e.g. "label".
#' @param id_col    Name of the document id column (string), e.g. "doc_id".
#' @param value_col Optional numeric column to count; if NULL, each row counts as 1.
#' @return A wide data frame with one row per document id, count columns per label and corresponding *_share columns.
#' @import dplyr tidyr rlang
#' @export
article_level_matrix <- function(df,
                                 label_col = "label",
                                 id_col    = "doc_id",
                                 value_col = NULL) {
  
  # Validation
  reqs <- c(id_col, label_col)
  if (!is.null(value_col)) reqs <- c(reqs, value_col)
  validate_columns(df, reqs)
  
  # Syms for tidy-eval
  id_sym  <- rlang::sym(id_col)
  lab_sym <- rlang::sym(label_col)
  
  dat <- df
  # Define per-row contribution n
  if (is.null(value_col)) {
    dat$n <- 1
  } else {
    dat$n <- dat[[value_col]]
  }
  
  wide <- dat |>
    dplyr::group_by(!!id_sym, !!lab_sym) |>
    dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = !!lab_sym,
                       values_from = n,
                       values_fill = 0)
  
  # Compute shares per row
  count_cols <- setdiff(names(wide), id_col)
  totals <- rowSums(wide[count_cols], na.rm = TRUE)
  
  share_df <- dplyr::mutate(
    wide,
    dplyr::across(
      dplyr::all_of(count_cols),
      ~ .x / ifelse(totals > 0, totals, NA_real_),
      .names = "{col}_share"
    )
  )
  
  dplyr::bind_cols(
    wide,
    share_df[grep("_share$", names(share_df), fixed = FALSE)]
  )
}
