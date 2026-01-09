#' Summarize flows by month at entity- or document-margin
#'
#' @param df         Long df with at least: date, doc_id, label
#' @param targets    Character vector of target label values (in label_col)
#' @param margin     "entities" for entity counts/shares; "docs" for doc hits
#' @param date_col   Date column name (article-level date)
#' @param id_col     Article id column name
#' @param label_col  Label column name (e.g., "label" or "clean_label")
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
  
  # Period per article
  art_dates <- dplyr::distinct(df, !!id_sym, !!date_sym) |>
    dplyr::mutate(.period = lubridate::floor_date(!!date_sym, unit = date_unit))
  
  if (margin == "entities") {
    # Entity counts per period and label for total population
    counts_all <- df |>
      dplyr::mutate(.period = lubridate::floor_date(!!date_sym, unit = date_unit)) |>
      dplyr::count(.period, !!lab_sym, name = "n")
    
    # Total length per eriod for normalizing shares:
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
    
    # Only keep targets
    targ_counts <- counts_all |>
      dplyr::filter(!!lab_sym %in% targets)
    
    wide_n <- tidyr::pivot_wider(
      targ_counts,
      names_from = !!lab_sym, values_from = n,
      names_prefix = "n", values_fill = 0
    )
    
    # Shares within selected population
    wide <- dplyr::left_join(base_for_share, wide_n, by = ".period")
    
    # Calculate share column (n<label> / n_total), NA if n_total=0
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

