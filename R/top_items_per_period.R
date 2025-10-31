# gute Nummer

#' Top-k items (labels or entities) per period (e.g., month)
#'
#' @param df   Long data.frame mit mind. Spalten `date`, `label`, `word`.
#' @param item  Was soll gerankt werden? "label" oder "word".
#' @param k     Anzahl Top-Elemente pro Zeitraum/Gruppe.
#' @param label_of_interest Optional: erst nach diesem Label filtern (z.B. nur "business").
#' @param by    Optionale Gruppierung neben dem Zeitraum (z.B. c("country")).
#' @param date_col Datums-Spalte.
#' @param unit  Aggregationseinheit für floor_date (z.B. "month", "week", "quarter").
#' @param include_share TRUE → zusätzlich Anteil je Zeitraum/Gruppe berechnen.
#' @param ties  "keep" (alle bei Gleichstand) oder "first" (hart auf k begrenzen).
#' @return Tibble mit Spalten: period, (by ...), item, n, (optional share), rank
#' @import dplyr tidyr lubridate rlang stringr
#' @export
top_items_per_period <- function(df,
                                 item  = c("label","word"),
                                 k     = 5,
                                 label_of_interest = NULL,
                                 by    = NULL,
                                 date_col = "date",
                                 unit  = "month",
                                 include_share = TRUE,
                                 ties  = c("keep","first")) {
  item  <- match.arg(item)
  ties  <- match.arg(ties)
  
  # --- Validierung
  req_cols <- c(date_col, "label", "word")
  req_cols <- unique(c(req_cols, by %||% character()))
  stopifnot(all(req_cols %in% names(df)))
  
  if (!is.null(label_of_interest)) {
    df <- df[df[["label"]] == label_of_interest, , drop = FALSE]
  }
  
  date_sym <- rlang::sym(date_col)
  itm_sym  <- rlang::sym(item)
  
  # Zeitraumspalte
  df <- dplyr::mutate(df, period = lubridate::floor_date(!!date_sym, unit = unit))
  
  # Gruppierung: period + optionale 'by'
  grp_base <- c("period", by %||% character())
  
  # Zählen: n je (period, by..., item)
  counts <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grp_base, item)))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last")
  
  # Gesamt je (period, by...) für share
  if (isTRUE(include_share)) {
    totals <- counts |>
      dplyr::summarise(n_total = sum(n, na.rm = TRUE), .groups = "drop")
    counts <- dplyr::left_join(counts, totals, by = grp_base) |>
      dplyr::mutate(share = ifelse(n_total > 0, n / n_total, NA_real_))
  }
  
  # Ranking innerhalb jeder (period, by...)
  # dplyr::slice_max mit Ties-Option
  rank_fun <- function(d) {
    d <- d[order(d$n, decreasing = TRUE), , drop = FALSE]
    if (ties == "first") {
      d <- utils::head(d, k)
    } else {
      # keep: alle mit gleichem n wie der k-te bleiben drin
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
