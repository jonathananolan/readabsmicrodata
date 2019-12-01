#' Extract column labels
#' @keywords internal
#' @export
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom rio factorize
#' @importFrom purr map_dfr

extract_labels_col <- function(df, col) {

  labels <- attr(df[[col]], "label") %>%
    enframe() %>%
    mutate(var_name = col, name=as.character(name),value=as.character(value))

  labels
}

extract_labels_df <- function(df) {
  df_labels <- map_dfr(colnames(df),
                       ~ extract_labels_col(df = df, col = .))
  df_labels
}
