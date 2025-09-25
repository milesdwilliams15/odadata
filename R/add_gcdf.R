#' Populate a `{peacesciencer}` constructed data set with AidData's Global Chinese Development Finance Dataset
#' 
#' This function takes a state-year dataset made
#' with the `{peacesciencer}` R package and populates it with country level
#' totals of Chinese development financing to aid recipients from 2000 to 2021.
#' Data come from AidData's Geospatial Global Chinese Development Finance Dataset, 
#' Version 3.0.
#' 
#' @param data A state-year data frame created with the `{peacesciencer}` R package.
#' @return `add_gcdf()` returns a state-year data frame populated with bilateral commitments of Chinese official development assistance (ODA), other official flows (OOF), and vague official flows (VOF) in both amounts and adjusted amounts. Values are in 2021 constant USD and are valid from 2000 to 2021.
#' @description The `add_gcdf()` function works by populating a state-year dataset with recipient-year aggregates of Chinese development financing commitments to that country in 2021 constant USD. It adds six columns to a dataset. Three are what AidData defines as "amounts" and three are what AidData refers to as "adjusted amounts." The latter are recommended when doing yearly cumulative totals. For both amounts and adjusted amounts, values of official development assistance (ODA), other official flows (OOF), and vague official flows (VOF) are included. For a detailed description of what these designations mean, users are encouraged to consult AidData's codebook for the Global Chinese Development Finance Dataset (Version 3.0). In brief, ODA values reflect ODA-like equivalents to the OECD's criteria for ODA (in other words, "aid"). OOF refers to financing with a significant loan component (making it more akin to a "loan" or "debt"). VOF refers to financing where the terms are too vague to designate it as either ODA or OOF.
#' 
#' Coverage runs from 2000 to 2021. Only countries that appear as recipients at some point in the dataset take zero values in years where there is no financing recorded. Otherwise, an NA value is returned.
#' @examples 
#' library(tidyverse)
#' library(peacesciencer)
#' library(odadata)
#'
#' # make a state-year dataset of chinese aid repients:
#' dt <- create_stateyears(subset_years = 1960:2023) |>
#'   add_gcdf() |>
#'   filter_rdr() # drops non-recipients 
#' @references 
#' Custer, S., Dreher, A., Elston, T.B., Escobar, B., Fedorochko, R., Fuchs, A., Ghose, S., Lin, J., Malik, A., Parks, B.C., Solomon, K., Strange, A., Tierney, M.J., Vlasto, L., Walsh, K., Wang, F., Zaleski, L., and Zhang, S. 2023. Tracking Chinese Development Finance: An Application of AidData’s TUFF 3.0 Methodology. Williamsburg, VA: AidData at William & Mary.
#' 
#' Dreher, A., Fuchs, A., Parks, B. C., Strange, A., & Tierney, M.J. 2022. Banking on Beijing: The Aims and Impacts of China’s Overseas Development Program. Cambridge, UK: Cambridge University Press.
#' @export
add_gcdf <- function(data) {
  ## do you have a dataset?
  if(missing(data)) {
    stop("You must provide an existing data object to which to add ODA data.")
  }
  
  ## check that you have the right kind of dataset
  valid_codes <- 
    any(colnames(data) %in% c("ccode", "gwcode"))
  if(!valid_codes) {
    stop("Expecting a state-year dataset with columns 'ccode' or 'gwcode' in data. Make sure you constructed a base state-year dataset with {peacesciencer} first.")
  }
  
 ## which kind of dataset it is
  cow <-
    any(colnames(data) == "ccode")
  
  ## add the data
  if(cow) {
    gcdf <- read.csv(
      "https://raw.githubusercontent.com/milesdwilliams15/odadata/refs/heads/main/data/cow_gcdf.csv"
    )
    suppressMessages(
      data |>
        left_join(gcdf)
      ) -> final_data
    final_data |>
      mutate(
        across(
          ends_with("amt") | ends_with("adj"),
          ~ ifelse(
            ccode %in% unique(gcdf$ccode),
            replace_na(.x, 0),
            .x
          )
        )
      ) -> final_data
  } else {
    gcdf <- read.csv(
      "https://raw.githubusercontent.com/milesdwilliams15/odadata/refs/heads/main/data/gw_gcdf.csv"
    )
    suppressMessages(
      data |>
        left_join(gcdf)
    ) -> final_data
    final_data |>
      mutate(
        across(
          ends_with("amt") | ends_with("adj"),
          ~ ifelse(
            gwcode %in% unique(gcdf$gwcode),
            replace_na(.x, 0),
            .x
          )
        )
      ) -> final_data
  }
  
  ## return
  final_data
}
