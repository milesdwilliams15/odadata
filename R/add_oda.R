#' Populate a `{peacesciencer}` constructed data set with ODA data
#' 
#' This function takes either a dyad-year or state-year dataset made
#' with the `{peacesciencer}` R package and populates it with official
#' development assistance (ODA) comments and disbursements from the 
#' Organization for Economic Cooperation and Development (OECD), specifically the data tables 
#' DAC2a (disbursements) and DAC3a (commitments). Values are in millions of
#' 2023 U.S. dollars.
#' 
#' @param data A data frame created by one of the `create_*()` functions in the `{peacesciencer}` R package.
#' @param type A character string indicating either `"donor"` or `"recipient"`. Relevant only if the data object is a state-year level data frame. The value `"donor"` (default) means the data will be populated with ODA values given by donor countries. The value `"recipient"` means the data will be populated with ODA received by countries.
#' @return `add_oda()` returns either a state-year or dyad-year data frame populated with bilateral official development assistance (ODA) commitments and disbursements in millions of 2023 US Dollars. All zero values are true zeros and all `NA` values are truly invalid. ODA data runs from 1960 to 2023 for disbursements and from 1966 to 2023 for commitments. If the supplied data object is a dyad-year data frame, ODA values are totals for a donor-recipient pair. If the supplied data is a state-year data frame, ODA values are total bilateral ODA for either a donor (if `type = "donor"`) or a recipient (if `type = "recipient"`).
#' @description The `add_oda()` function works by reading in ODA data from DAC2a and DAC3a data tables via a developer API from the OECD data explorer (https://data-explorer.oecd.org/). This approach has certain trade-offs. The main benefit is that the user does not have to eat up extra RAM storing a large-ish dyadic donor-recipient dataset in memory permanently and locally on your own machine. The downside is that to access the data in an active R session you need an internet connection, and calling the data via the OECD's API may take a few seconds to a minute depending on the quality of your connection. I think these costs are well worth the benefit of saving RAM.
#' @examples 
#' library(tidyverse)
#' library(peacesciencer)
#' library(odadata)
#' 
#' # make a dyad-year dataset:
#' dt <- create_dyadyears(subset_years = 1960:2023) |>
#'   add_oda() |>
#'   drop_na(oda_committed) # drops invalid donor-recipient pairs
#'
#' # make a state-year dataset for aid repients:
#' dt <- create_stateyears(subset_years = 1960:2023) |>
#'   add_oda(type = "recipient") |>
#'   drop_na(oda_committed) # drops non-recipients 
#' @export
add_oda <- function(data, type = "donor") {
  
  ## do you have a dataset?
  if(missing(data)) {
    stop("You must provide an existing data object to which to add ODA data.")
  }
  
  ## check that you have the right kind of dataset
  valid_codes <- 
    any(colnames(data) %in% c("ccode", "ccode1", "gwcode", "gwcode1"))
  if(!valid_codes) {
    stop("Expecting column names 'ccode[1,2]' or 'gwcode[1,2]' in data. Make sure you constructed a base dataset with {peacesciencer} first.")
  }
  
  ## check which kind of dataset it is
  dyadic <- 
    any(colnames(data) %in% c("ccode1", "gwcode1")) | 
    any(colnames(data) %in% c("ccode2", "gwcode2"))
  cow <-
    any(colnames(data) %in% c("ccode", "ccode1", "ccode2"))
  
  ## OECD api for DAC2a:
  url1 <-
    "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC2@DF_DAC2A,/..206.USD.Q?startPeriod=1960&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfile"
  
  ## OECD api for DAC3a:
  url2 <-
    "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC2@DF_DAC3A,1.3/..305.USD.Q?startPeriod=1960&endPeriod=2023&dimensionAtObservation=AllDimensions&format=csvfile"
  
  ## get the data
  dac2a <- read.csv(url1)
  dac3a <- read.csv(url2)
  dac_stacked <- dplyr::bind_rows(dac2a, dac3a)
  
  ## clean the data
  if(dyadic & cow) {
    donor_names <- suppressWarnings(tibble::tibble(
      DONOR = unique(dac_stacked$DONOR),
      ccode1 = countrycode::countrycode(
        DONOR, "iso3c", "cown"
      )
    ))
    recipient_names <- suppressWarnings(tibble::tibble(
      RECIPIENT = unique(dac_stacked$RECIPIENT),
      ccode2 = countrycode::countrycode(
        RECIPIENT, "iso3c", "cown"
      )
    ))
    oda_data <- dac_stacked |>
      dplyr::left_join(donor_names, by = "DONOR") |>
      dplyr::left_join(recipient_names, by = "RECIPIENT") |>
      tidyr::drop_na(ccode1, ccode2) |>
      dplyr::transmute(
        ccode1, ccode2, year = TIME_PERIOD,
        oda = round(OBS_VALUE, 3),
        flow = ifelse(
          FLOW_TYPE == "D",
          "oda_disbursed",
          "oda_committed"
        )
      ) |>
      tidyr::pivot_wider(
        names_from = flow,
        values_from = oda
      )
    final_data <- data |>
      dplyr::left_join(oda_data, by = c("ccode1", "ccode2", "year")) |>
      dplyr::mutate(
        oda_disbursed = ifelse(
          ccode1 %in% donor_names$ccode1 & 
            ccode2 %in% recipient_names$ccode2 &
            year %in% 1960:2023,
          tidyr::replace_na(oda_disbursed, 0),
          oda_disbursed
        ),
        oda_committed = ifelse(
          ccode1 %in% donor_names$ccode1 &
            ccode2 %in% recipient_names$ccode2 &
            year %in% 1966:2023,
          tidyr::replace_na(oda_committed, 0),
          oda_committed
        )
      )
  } else if(dyadic & !cow) {
    donor_names <- suppressWarnings(tibble::tibble(
      DONOR = unique(dac_stacked$DONOR),
      gwcode1 = countrycode::countrycode(
        DONOR, "iso3c", "gwn"
      )
    ))
    recipient_names <- suppressWarnings(tibble::tibble(
      RECIPIENT = unique(dac_stacked$RECIPIENT),
      gwcode2 = countrycode::countrycode(
        RECIPIENT, "iso3c", "gwn"
      )
    ))
    oda_data <- dac_stacked |>
      dplyr::left_join(donor_names, by = "DONOR") |>
      dplyr::left_join(recipient_names, by = "RECIPIENT") |>
      tidyr::drop_na(gwcode1, gwcode2) |>
      dplyr::transmute(
        gwcode1, gwcode2, year = TIME_PERIOD,
        oda = round(OBS_VALUE, 3),
        flow = ifelse(
          FLOW_TYPE == "D",
          "oda_disbursed",
          "oda_committed"
        )
      ) |>
      tidyr::pivot_wider(
        names_from = flow,
        values_from = oda
      )
    final_data <- data |>
      dplyr::left_join(oda_data, by = c("gwcode1", "gwcode2", "year")) |>
      dplyr::mutate(
        oda_disbursed = ifelse(
          gwcode1 %in% donor_names$gwcode1 & 
            gwcode2 %in% recipient_names$gwcode2 &
            year %in% 1960:2023,
          tidyr::replace_na(oda_disbursed, 0),
          oda_disbursed
        ),
        oda_committed = ifelse(
          gwcode1 %in% donor_names$gwcode1 &
            gwcode2 %in% recipient_names$gwcode2 &
            year %in% 1966:2023,
          tidyr::replace_na(oda_committed, 0),
          oda_committed
        )
      )
  } else if(!dyadic & cow) {
    donor_names <- suppressWarnings(tibble::tibble(
      DONOR = unique(dac_stacked$DONOR),
      ccode1 = countrycode::countrycode(
        DONOR, "iso3c", "cown"
      )
    ))
    recipient_names <- suppressWarnings(tibble::tibble(
      RECIPIENT = unique(dac_stacked$RECIPIENT),
      ccode2 = countrycode::countrycode(
        RECIPIENT, "iso3c", "cown"
      )
    ))
  
    oda_data <- dac_stacked |>
      dplyr::left_join(donor_names, by = "DONOR") |>
      dplyr::left_join(recipient_names, by = "RECIPIENT") |>
      tidyr::drop_na(ccode1, ccode2)
    if(type == "donor") {
      oda_data <- oda_data |>
        rename(ccode = ccode1)
    } else {
      oda_data <- oda_data |>
        rename(ccode = ccode2)
    }
    oda_data <- oda_data |>
      dplyr::transmute(
        ccode, year = TIME_PERIOD,
        oda = OBS_VALUE,
        flow = ifelse(
          FLOW_TYPE == "D",
          "oda_disbursed",
          "oda_committed"
        )
      ) |>
      group_by(ccode, year, flow) |>
      summarize(
        oda = sum(oda),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        names_from = flow,
        values_from = oda
      )
    final_data <- data |>
      dplyr::left_join(oda_data, by = c("ccode", "year")) |>
      dplyr::mutate(
        oda_disbursed = ifelse(
          ccode %in% donor_names$ccode1 &
            year %in% 1960:2023,
          tidyr::replace_na(oda_disbursed, 0),
          oda_disbursed
        ),
        oda_committed = ifelse(
          ccode %in% donor_names$ccode1 &
            year %in% 1966:2023,
          tidyr::replace_na(oda_committed, 0),
          oda_committed
        )
      )
  } else {
    donor_names <- suppressWarnings(tibble::tibble(
      DONOR = unique(dac_stacked$DONOR),
      gwcode1 = countrycode::countrycode(
        DONOR, "iso3c", "gwn"
      )
    ))
    recipient_names <- suppressWarnings(tibble::tibble(
      RECIPIENT = unique(dac_stacked$RECIPIENT),
      gwcode2 = countrycode::countrycode(
        RECIPIENT, "iso3c", "gwn"
      )
    ))
    
    oda_data <- dac_stacked |>
      dplyr::left_join(donor_names, by = "DONOR") |>
      dplyr::left_join(recipient_names, by = "RECIPIENT") |>
      tidyr::drop_na(gwcode1, gwcode2)
    if(type == "donor") {
      oda_data <- oda_data |>
        rename(gwcode = gwcode1)
    } else {
      oda_data <- oda_data |>
        rename(gwcode = gwcode2)
    }
    oda_data <- oda_data |>
      dplyr::transmute(
        gwcode, year = TIME_PERIOD,
        oda = OBS_VALUE,
        flow = ifelse(
          FLOW_TYPE == "D",
          "oda_disbursed",
          "oda_committed"
        )
      ) |>
      group_by(gwcode, year, flow) |>
      summarize(
        oda = sum(oda),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        names_from = flow,
        values_from = oda
      )
    final_data <- data |>
      dplyr::left_join(oda_data, by = c("gwcode", "year")) |>
      dplyr::mutate(
        oda_disbursed = ifelse(
          gwcode %in% donor_names$gwcode1 &
            year %in% 1960:2023,
          tidyr::replace_na(oda_disbursed, 0),
          oda_disbursed
        ),
        oda_committed = ifelse(
          gwcode %in% donor_names$gwcode1 &
            year %in% 1966:2023,
          tidyr::replace_na(oda_committed, 0),
          oda_committed
        )
      )
  }
  
  ## return final data
  final_data
  
}

