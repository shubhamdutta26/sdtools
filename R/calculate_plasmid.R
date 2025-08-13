#' Calculate Plasmid Amounts for Transfection
#'
#' Computes the amount of plasmid (in µg) required for transfection based on DNA concentration and total transfection volume.
#' Supports both heavy chain (HC) only and HC-light chain (LC) plasmid pairs.
#'
#' @param plasmid_hc A character vector of heavy chain plasmid names.
#' @param conc_hc A numeric vector of heavy chain plasmid concentrations (in µg/µL), matching the order of \code{plasmid_hc}.
#' @param transfection_volume A numeric scalar giving the total transfection volume per plasmid pair (in mL).
#' @param plasmid_lc (Optional) A character vector of light chain plasmid names, matching the length of \code{plasmid_hc} or shorter.
#' Missing entries are interpreted as no LC for those HC plasmids.
#' @param conc_lc (Optional) A numeric vector of light chain plasmid concentrations (in µg/µL), matching the order of \code{plasmid_lc}.
#' Must be the same length as \code{plasmid_lc}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{pair_id}{An integer ID indicating each plasmid pair (or HC-only entry).}
#'   \item{plasmid}{The name of the plasmid (HC or LC).}
#'   \item{concentration}{The concentration of the plasmid (in µg/µL).}
#'   \item{amount}{The calculated amount of plasmid to use (rounded to 0 decimal places, in µL).}
#' }
#'
#' @examples
#' calculate_plasmid(
#'   plasmid_hc = c("BACE1_HC", "BE10", "BF2", "BF8"),
#'   conc_hc = c(1.444, 1.822, 1.739, 1.703),
#'   plasmid_lc = c("BACE1_LC"),
#'   conc_lc = c(1.816),
#'   transfection_volume = 200
#' )
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
calculate_plasmid <- function(plasmid_hc, conc_hc, transfection_volume, plasmid_lc = NULL, conc_lc = NULL) {
  n <- length(plasmid_hc)

  # Ensure plasmid_lc and conc_lc are same length or NULL
  if (!is.null(plasmid_lc) && length(plasmid_lc) < n) {
    plasmid_lc <- c(plasmid_lc, rep(NA, n - length(plasmid_lc)))
  }
  if (!is.null(conc_lc) && length(conc_lc) < n) {
    conc_lc <- c(conc_lc, rep(NA, n - length(conc_lc)))
  }

  # Prepare output list
  output <- list()

  for (i in seq_len(n)) {
    if (!is.na(plasmid_lc[i]) && !is.na(conc_lc[i])) {
      # HC + LC pair: split volume
      vol_each <- transfection_volume / 2
      amt_hc <- round(vol_each / conc_hc[i], 0)
      amt_lc <- round(vol_each / conc_lc[i], 0)

      output[[length(output) + 1]] <- tibble::tibble(
        pair_id = i,
        plasmid = plasmid_hc[i],
        concentration = conc_hc[i],
        amount = amt_hc
      )
      output[[length(output) + 1]] <- tibble::tibble(
        pair_id = i,
        plasmid = plasmid_lc[i],
        concentration = conc_lc[i],
        amount = amt_lc
      )
    } else {
      # HC only
      amt_hc <- round(transfection_volume / conc_hc[i], 0)
      output[[length(output) + 1]] <- tibble::tibble(
        pair_id = i,
        plasmid = plasmid_hc[i],
        concentration = conc_hc[i],
        amount = amt_hc
      )
    }
  }

  dplyr::bind_rows(output)
}
