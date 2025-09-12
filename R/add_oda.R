#' Populate a `{peacesciencer}` constructed data set with ODA data
#' 
#' This function takes either a dyad-year or state-year dataset made
#' with the `{peacesciencer}` R package and populates it with official
#' development assistance (ODA) comments and disbursements from the 
#' Organization for Economic Cooperation and Development (OECD) --- tables 
#' DAC2a (disbursements) and DAC3a (commitments). Values are in millions of
#' 2023 U.S. dollars.
#' 
add_oda <- function(data, type = "donor") {
  
  ## check that you have the right kind of dataset
  valid_codes <- 
    any(colnames(data) %in% c("ccode", "ccode1", "gwcode", "gwcode1"))
  if(!valid_codes) {
    stop("Expecting column names 'ccode[1,2]' or 'gwcode[1,2]' in data. Make sure you constructed a base dataset with {peacesciencer} first.")
  }
  
  ## check which kind of dataset it is
  dyadic <- 
    any(colnames(data) %in% c("ccode1", "gwcode1")) &
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
  dac_stacked <- bind_rows(dac2a, dac3a)
  
  ## clean the data
  if(dyadic & cow) {
    donor_names <- suppressWarnings(tibble(
      DONOR = unique(dac_stacked$DONOR),
      ccode1 = countrycode::countrycode(
        DONOR, "iso3c", "cown"
      )
    ))
    recipient_names <- suppressWarnings(tibble(
      RECIPIENT = unique(dac_stacked$RECIPIENT),
      ccode2 = countrycode::countrycode(
        RECIPIENT, "iso3c", "cown"
      )
    ))
    oda_data <- dac_stacked |>
      left_join(donor_names, by = "DONOR") |>
      left_join(recipient_names, by = "RECIPIENT") |>
      drop_na(ccode1, ccode2) |>
      transmute(
        ccode1, ccode2, year = TIME_PERIOD,
        oda = round(OBS_VALUE, 3),
        flow = ifelse(
          FLOW_TYPE == "D",
          "oda_disbursed",
          "oda_committed"
        )
      ) |>
      pivot_wider(
        names_from = flow,
        values_from = oda
      )
    final_data <- data |>
      left_join(oda_data, by = c("ccode1", "ccode2", "year")) |>
      mutate(
        oda_disbursed = ifelse(
          ccode1 %in% donor_names$ccode1 & 
            ccode2 %in% recipient_names$ccode2 &
            year %in% 1960:2023,
          replace_na(oda_disbursed, 0),
          oda_disbursed
        ),
        oda_committed = ifelse(
          ccode1 %in% donor_names$ccode1 &
            ccode2 %in% recipient_names$ccode2 &
            year %in% 1966:2023,
          replace_na(oda_committed, 0),
          oda_committed
        )
      )
  } else if(dyadic & !cow) {
    donor_names <- suppressWarnings(tibble(
      DONOR = unique(dac_stacked$DONOR),
      gwcode1 = countrycode::countrycode(
        DONOR, "iso3c", "gwn"
      )
    ))
    recipient_names <- suppressWarnings(tibble(
      RECIPIENT = unique(dac_stacked$RECIPIENT),
      gwcode2 = countrycode::countrycode(
        RECIPIENT, "iso3c", "gwn"
      )
    ))
    oda_data <- dac_stacked |>
      left_join(donor_names, by = "DONOR") |>
      left_join(recipient_names, by = "RECIPIENT") |>
      drop_na(gwcode1, gwcode2) |>
      transmute(
        gwcode1, gwcode2, year = TIME_PERIOD,
        oda = round(OBS_VALUE, 3),
        flow = ifelse(
          FLOW_TYPE == "D",
          "oda_disbursed",
          "oda_committed"
        )
      ) |>
      pivot_wider(
        names_from = flow,
        values_from = oda
      )
    final_data <- data |>
      left_join(oda_data, by = c("gwcode1", "gwcode2", "year")) |>
      mutate(
        oda_disbursed = ifelse(
          gwcode1 %in% donor_names$gwcode1 & 
            gwcode2 %in% recipient_names$gwcode2 &
            year %in% 1960:2023,
          replace_na(oda_disbursed, 0),
          oda_disbursed
        ),
        oda_committed = ifelse(
          gwcode1 %in% donor_names$gwcode1 &
            gwcode2 %in% recipient_names$gwcode2 &
            year %in% 1966:2023,
          replace_na(oda_committed, 0),
          oda_committed
        )
      )
  } else if(!dyadic & cow) {
    if(type == "donor") {
      c_names <- suppressWarnings(tibble(
        DONOR = unique(dac_stacked$DONOR),
        ccode = countrycode::countrycode(
          DONOR, "iso3c", "cown"
        )
      ))
    } else if(type == "recipient") {
      c_names <- suppressWarnings(tibble(
        RECIPIENT = unique(dac_stacked$RECIPIENT),
        ccode = countrycode::countrycode(
          RECIPIENT, "iso3c", "cown"
        )
      ))
    }
  
    oda_data <- dac_stacked |>
      left_join(c_names, by = str_to_upper(type)) |>
      drop_na(ccode) |>
      transmute(
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
      pivot_wider(
        names_from = flow,
        values_from = oda
      )
    final_data <- data |>
      left_join(oda_data, by = c("ccode", "year")) |>
      mutate(
        oda_disbursed = ifelse(
          ccode %in% c_names$ccode &
            year %in% 1960:2023,
          replace_na(oda_disbursed, 0),
          oda_disbursed
        ),
        oda_committed = ifelse(
          gwcode %in% c_names$gwcode &
            year %in% 1966:2023,
          replace_na(oda_committed, 0),
          oda_committed
        )
      )
  } else {
    if(type == "donor") {
      c_names <- suppressWarnings(tibble(
        DONOR = unique(dac_stacked$DONOR),
        gwcode = countrycode::countrycode(
          DONOR, "iso3c", "gwn"
        )
      ))
    } else if(type == "recipient") {
      c_names <- suppressWarnings(tibble(
        RECIPIENT = unique(dac_stacked$RECIPIENT),
        gwcode = countrycode::countrycode(
          RECIPIENT, "iso3c", "gwn"
        )
      ))
    }
    
    oda_data <- dac_stacked |>
      left_join(c_names, by = str_to_upper(type)) |>
      drop_na(gwcode) |>
      transmute(
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
      pivot_wider(
        names_from = flow,
        values_from = oda
      )
    final_data <- data |>
      left_join(oda_data, by = c("gwcode", "year")) |>
      mutate(
        oda_disbursed = ifelse(
          gwcode %in% c_names$gwcode &
            year %in% 1960:2023,
          replace_na(oda_disbursed, 0),
          oda_disbursed
        ),
        oda_committed = ifelse(
          gwcode %in% c_names$gwcode &
            year %in% 1966:2023,
          replace_na(oda_committed, 0),
          oda_committed
        )
      )
  }
  
  ## return final data
  final_data
  
}


## TEST 
library(peacesciencer)
library(tidyverse)

# dyadic + cow
dt <- create_stateyears(
  system = "gw",
  subset_years = 1960:1970
) |> add_oda()
