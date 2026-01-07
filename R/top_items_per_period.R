#' Top-k items (labels or entities) per period (e.g., month)
#'
#' @param df Long data.frame containing a date column and the columns specified in `item` and `label_col`.
#' @param item Name of the column to be ranked (e.g., "label", "word", "clean_label").
#' @param k Number of top elements to return per period/group.
#' @param label_of_interest Optional: Filter for this specific value first (e.g., only "Europe").
#' @param by Optional: Character vector of columns for additional grouping besides period.
#' @param label_col Name of the column containing country labels (used for filtering). Default: "label".
#' @param date_col Name of the column containing publication dates.
#' @param unit Unit for time aggregation (passed to `lubridate::floor_date`, e.g., "month", "week", "quarter").
#' @param include_share If TRUE, calculates the share of the item within its period/group.
#' @param ties How to handle ties: "keep" (include all tied items) or "first" (strict limit to k).
#' @return A tibble with columns: `period`, (any columns in `by`), `item`, `n`, (optional `share`), `rank`.
#' @import dplyr tidyr lubridate rlang stringr
#' @export
top_items_per_period <- function(df,
                                 item  = "label",
                                 k     = 5,
                                 label_of_interest = NULL,
                                 by    = NULL,
                                 label_col = "label",
                                 date_col = "date",
                                 unit  = "month",
                                 include_share = TRUE,
                                 ties  = c("keep","first")) {
  ties  <- match.arg(ties)
  
  # --- Validation
  if (!item %in% names(df)) stop(sprintf("Column '%s' not found.", item))
  if (!label_col %in% names(df)) stop(sprintf("Column '%s' not found.", label_col))
  
  if (!is.null(label_of_interest)) {
    df <- df[df[[label_col]] == label_of_interest, , drop = FALSE]
  }
  
  date_sym <- rlang::sym(date_col)
  itm_sym  <- rlang::sym(item)
  
  # Create floor dates
  df <- dplyr::mutate(df, period = lubridate::floor_date(!!date_sym, unit = unit))
  
  # Grouping: period + optionale 'by'
  grp_base <- c("period", by %||% character())
  
  # Count: n per (period, by..., item)
  counts <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grp_base, item)))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last")
  
  # In total per (period, by...) if share
  if (isTRUE(include_share)) {
    totals <- counts |>
      dplyr::summarise(n_total = sum(n, na.rm = TRUE), .groups = "drop")
    counts <- dplyr::left_join(counts, totals, by = grp_base) |>
      dplyr::mutate(share = ifelse(n_total > 0, n / n_total, NA_real_))
  }
  
  # ank within each (period, by...)
  rank_fun <- function(d) {
    d <- d[order(d$n, decreasing = TRUE), , drop = FALSE]
    if (ties == "first") {
      d <- utils::head(d, k)
    } else {
      if (nrow(d) > k) {
        kth <- d$n[k]
        d <- d[d$n >= kth, , drop = FALSE]
      }
    }
    d$rank <- seq_len(nrow(d))
    d
  }
  
  out <- counts |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_base))) |>
    dplyr::group_modify(~ rank_fun(.x)) |>
    dplyr::ungroup() |>
    dplyr::rename(item = !!itm_sym) |>
    dplyr::select(dplyr::all_of(grp_base), item, n, dplyr::any_of(c("share","n_total")), rank) |>
    dplyr::arrange(period, dplyr::across(dplyr::all_of(by %||% character())), dplyr::desc(n), item)
  
  out
}

# top_items_per_period(welt_sub, item="word")
# top_items_per_period(welt_sub, item="label")
# top_items_per_period(welt_sub, item="clean_label") # falls zuvor erzeugt
