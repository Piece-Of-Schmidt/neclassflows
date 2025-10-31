#' Merge / reassign entities or labels to broader groups
#' @param df   Data frame with at least columns `word` and `label`.
#' @param maps Named list: names = target labels, values = character vectors of aliases/patterns.
#' @param along One of c("label","word"). See above.
#' @param regex Logical; if TRUE, aliases are treated as regex patterns.
#' @param case_insensitive Logical; if TRUE, matching ignores case.
#' @param out_col Name of the output column to write (default "new_label").
#' Will not overwrite `label` unless you set out_col = "label".
#' @param conflict How to resolve multiple matches per row:
#' "last_wins" (default) or "first_wins".
#' @return Data frame with an added/overwritten column `out_col`.
#' @import dplyr tidyr lubridate rlang stringr
#' @importFrom stats setNames
#' @export
harmonize_data <- function(df,
                           maps = list(),
                           along = c("label","word"),
                           regex = FALSE,
                           case_insensitive = TRUE,
                           out_col = "new_label",
                           conflict = c("last_wins","first_wins")) {
  along <- match.arg(along)
  conflict <- match.arg(conflict)
  stopifnot(all(c("word","label") %in% names(df)))
  if (!length(maps)) { df[[out_col]] <- df$label; return(df) }
  
  
  df[[out_col]] <- df$label
  target_vec <- if (along == "label") df$label else df$word
  
  
  already_set <- rep(FALSE, nrow(df))
  
  
  if (!regex) {
    # exact match path (fast)
    if (case_insensitive) {
      target_vec_fold <- tolower(target_vec)
    } else {
      target_vec_fold <- target_vec
    }
    all_aliases <- unlist(maps, use.names = FALSE)
    all_targets <- rep(names(maps), lengths(maps))
    if (case_insensitive) all_aliases <- tolower(all_aliases)
    
    
    lookup <- setNames(all_targets, all_aliases)
    if (conflict == "first_wins") {
      keep <- !duplicated(all_aliases)
      lookup <- setNames(all_targets[keep], all_aliases[keep])
    }
    
    
    hit <- match(target_vec_fold, names(lookup), nomatch = 0L)
    if (any(hit > 0L)) {
      idx <- which(hit > 0L)
      if (conflict == "first_wins") idx <- idx[!already_set[idx]]
      df[[out_col]][idx] <- lookup[ hit[idx] ]
      already_set[idx] <- TRUE
    }
  } else {
    # regex path
    for (tgt in names(maps)) {
      pats <- maps[[tgt]]
      if (!length(pats)) next
      pat <- paste(pats, collapse = "|")
      if (case_insensitive) pat <- paste0("(?i)", pat)
      idx <- grepl(pat, target_vec, perl = TRUE)
      if (any(idx)) {
        if (conflict == "first_wins") idx <- idx & !already_set
        df[[out_col]][idx] <- tgt
        already_set[idx] <- TRUE
      }
    }
  }
  df
}
