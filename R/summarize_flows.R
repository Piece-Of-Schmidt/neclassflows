# gute Nummer

#' Summarize flows by month at entity- or document-margin
#'
#' @param df         Long df with at least: date, doc_id, label
#' @param targets    Character vector of target label values (in label_col)
#' @param margin     "entities" for entity counts/shares; "docs" for doc hits
#' @param date_col   Date column name (article-level date)
#' @param id_col     Article id column name
#' @param label_col  Label column name (e.g., "label" or "location_label")
#' @param doc_threshold Share threshold for doc hits (only for margin="docs")
#' @param date_unit  Floor unit (e.g., "month")
#' @param recalculate_population_after_selection If TRUE (entities):
#'        shares werden innerhalb der ausgewählten targets normiert; sonst ggü. allen Labels
#' @param melt       If TRUE: long output
#' @return tibble. For entities: date, nEntities, n<target>, share<target>.
#'         For docs:     date, nArticles, n<target>.
#' @import dplyr tidyr lubridate rlang stringr
#' @export
summarize_flows <- function(df,
                            targets,
                            margin  = c("entities","docs"),
                            date_col = "date",
                            id_col   = "doc_id",
                            label_col= "label",
                            doc_threshold = 0.3,
                            date_unit= "month",
                            recalculate_population_after_selection = FALSE,
                            melt = FALSE) {
  margin   <- match.arg(margin)
  validate_columns(df, c(date_col, id_col, label_col))
  
  date_sym <- rlang::sym(date_col)
  id_sym   <- rlang::sym(id_col)
  lab_sym  <- rlang::sym(label_col)
  
  # Period pro Artikel
  art_dates <- dplyr::distinct(df, !!id_sym, !!date_sym) |>
    dplyr::mutate(.period = lubridate::floor_date(!!date_sym, unit = date_unit))
  
  if (margin == "entities") {
    # Entity counts je Periode & Label für die GESAMT-Population
    counts_all <- df |>
      dplyr::mutate(.period = lubridate::floor_date(!!date_sym, unit = date_unit)) |>
      dplyr::count(.period, !!lab_sym, name = "n")
    
    # Gesamtmenge je Periode für Share-Normierung:
    if (recalculate_population_after_selection) {
      base_for_share <- counts_all |>
        dplyr::filter(!!lab_sym %in% targets) |>
        dplyr::group_by(.period) |>
        dplyr::summarise(n_total = sum(n, na.rm = TRUE), .groups = "drop")
    } else {
      base_for_share <- counts_all |>
        dplyr::group_by(.period) |>
        dplyr::summarise(n_total = sum(n, na.rm = TRUE), .groups = "drop")
    }
    
    # Nur Targets in die Ausgabe nehmen
    targ_counts <- counts_all |>
      dplyr::filter(!!lab_sym %in% targets)
    
    wide_n <- tidyr::pivot_wider(
      targ_counts,
      names_from = !!lab_sym, values_from = n,
      names_prefix = "n", values_fill = 0
    )
    
    # Shares innerhalb gewählter Population
    wide <- dplyr::left_join(base_for_share, wide_n, by = ".period")
    
    # share-Spalten berechnen (n<label> / n_total), NA wenn n_total=0
    n_cols <- grep("^n(?!total$)", names(wide), perl = TRUE, value = TRUE)
    for (nc in n_cols) {
      wide[[sub("^n", "share", nc)]] <- ifelse(wide$n_total > 0, wide[[nc]] / wide$n_total, NA_real_)
    }
    
    out <- dplyr::rename(wide, date = .period, nEntities = n_total)
    
    if (melt) {
      out <- tidyr::pivot_longer(
        out, cols = -date,
        names_to = "metric", values_to = "value"
      )
    }
    return(out)
  }
  
  # ===== margin == "docs" =====
  # Counts je Artikel × Label
  counts <- df |>
    dplyr::group_by(!!id_sym, !!lab_sym) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  totals <- counts |>
    dplyr::group_by(!!id_sym) |>
    dplyr::summarise(total = sum(n, na.rm = TRUE), .groups = "drop")
  
  targ_counts <- counts |>
    dplyr::filter(!!lab_sym %in% targets)
  
  shares <- dplyr::left_join(targ_counts, totals, by = rlang::as_string(id_sym)) |>
    dplyr::mutate(share = ifelse(total > 0, n / total, NA_real_)) |>
    dplyr::left_join(art_dates, by = rlang::as_string(id_sym))
  
  # Artikel pro Periode
  n_articles <- dplyr::distinct(art_dates, .period, !!id_sym) |>
    dplyr::count(.period, name = "nArticles")
  
  hits_long <- shares |>
    dplyr::mutate(hit = share >= doc_threshold) |>
    dplyr::group_by(.period, !!lab_sym) |>
    dplyr::summarise(value = sum(hit, na.rm = TRUE), .groups = "drop")
  
  wide_hits <- tidyr::pivot_wider(
    hits_long,
    names_from  = !!lab_sym,
    values_from = value,
    values_fill = 0,
    names_prefix = "n"
  )
  
  out <- dplyr::left_join(n_articles, wide_hits, by = ".period") |>
    dplyr::rename(date = .period)
  
  n_cols <- grep("^n(?!Articles$)", names(out), perl = TRUE, value = TRUE)
  for (nc in n_cols) {
    out[[sub("^n", "share", nc)]] <- ifelse(out$nArticles > 0,
                                            out[[nc]] / out$nArticles,
                                            NA_real_)
  }
  
  if (melt) {
    out <- tidyr::pivot_longer(out, cols = -c(date, nArticles),
                               names_to = "target", values_to = "value")
  }
  as_tibble(out)
}


# (out1 = summarize_flows(welt_sub, c("Usa", "Germany"), margin = "docs"))
# (out2 = summarize_flows(welt_sub, c("Usa", "Germany"), margin = "entities"))
