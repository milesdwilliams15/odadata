#' Only keep relevant ODA cases
#' 
#' The `filter_rdr()` function will filter a dataset populated with ODA
#' values by way of the `add_oda()` function down to relevant cases
#' for studying ODA. In the case of a dyad-year level dataset, it will
#' remove rows where a donor (side 1) and recipient (side 2) are not a valid 
#' donor-recipient pair in the OECD's DAC2a and DAC3a data tables. In the case
#' of a state-year level dataset, it will remove rows where either a donor
#' or a recipient (depending on the type specified in `add_oda()`) that is
#' not a valid donor or recipient included in the OECD's data. This function 
#' is useful when performing standard dyadic or monadic analysis of aid giving
#' where the analyst is only interested in the relevant universe of foreign
#' aid donors and/or recipients.
#' 
#' @param data A data frame constructed with the `{peacesciencer}` API and populated with ODA values using the `add_oda()` function.
#' @param by A character string indicating whether to filter the data by valid ODA commitments cases or valid ODA disbursements cases. Options are `"commitments"` (default) or `"disbursements"`.
#' @return A data frame whittled down to just the relevant universe of donor and/or recipient cases.
#' @examples 
#' library(tidyverse)
#' library(peacesciencer)
#' library(odadata)
#' 
#' dt <- create_dyadyears(subset_years = 1960:2026) |>
#'   add_oda() |>
#'   filter_rdr()
#' @export
filter_rdr <- function(data, by = "commitments") {
  ## do you have a dataset?
  if(missing(data)) {
    stop("You must provide an existing data object to which to add ODA data.")
  }
  
  ## check that you have the right kind of dataset
  valid_vars <- 
    any(colnames(data) %in% c("oda_committed", "oda_disbursed"))
  if(!valid_vars) {
    stop("Expecting column names 'oda_committed' and 'oda_disbursed' in the data. Make sure you constructed a base dataset with {peacesciencer} first that you populated using add_oda().")
  }
  
  ## filter 
  if(by == "commitments") {
    flt_data <- data |>
      drop_na(oda_committed)
  } else {
    flt_data <- data |>
      drop_na(oda_disbursed)
  }
  
  ## return
  flt_data
}