# Clean up the Global Chinese Development Finance Dataset


# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)


# read in data ------------------------------------------------------------

dt <- read_excel(
  here::here(
    "data", 
    "AidDatasGlobalChineseDevelopmentFinanceDataset_v3.0.xlsx"
  ),
  sheet = "GCDF_3.0"
)

dt <- dt |> janitor::clean_names()


# clean and aggregate to country-year -------------------------------------

dt |>
  filter(recommended_for_aggregates == "Yes") |>
  transmute(
    ccode = countrycode::countrycode(
      recipient_iso_3, "iso3c", "cown"
    ),
    gwcode = countrycode::countrycode(
      recipient_iso_3, "iso3c", "gwn"
    ),
    year = commitment_year,
    type = flow_class,
    amt = amount_constant_usd_2021,
    adj = adjusted_amount_constant_usd_2021
  ) -> smdt

smdt |>
  drop_na(ccode) |>
  group_by(ccode, year, type) |>
  summarize(across(amt:adj, ~ sum(.x, na.rm = T))) |>
  mutate(
    type = case_when(
      type == "OOF-like" ~ "oof",
      type == "ODA-like" ~ "oda",
      TRUE ~ "vof"
    )
  ) |> ungroup() -> cow_dt

cow_dt |>
  filter(type == "oof") |>
  rename(oof_amt = amt, oof_adj = adj) |>
  select(-type) |>
  full_join(
    cow_dt |>
      filter(type == "oda") |>
      rename(oda_amt = amt, oda_adj = adj) |>
      select(-type)
  ) |>
  full_join(
    cow_dt |>
      filter(type == "vof") |>
      rename(vof_amt = amt, vof_adj = adj) |>
      select(-type)
  ) |>
  mutate(
    across(everything(), ~ replace_na(.x, 0))
  ) -> final_cow_dt


smdt |>
  drop_na(gwcode) |>
  group_by(gwcode, year, type) |>
  summarize(across(amt:adj, ~ sum(.x, na.rm = T))) |>
  mutate(
    type = case_when(
      type == "OOF-like" ~ "oof",
      type == "ODA-like" ~ "oda",
      TRUE ~ "vof"
    )
  ) |> ungroup() -> gw_dt

gw_dt |>
  filter(type == "oof") |>
  rename(oof_amt = amt, oof_adj = adj) |>
  select(-type) |>
  full_join(
    gw_dt |>
      filter(type == "oda") |>
      rename(oda_amt = amt, oda_adj = adj) |>
      select(-type)
  ) |>
  full_join(
    gw_dt |>
      filter(type == "vof") |>
      rename(vof_amt = amt, vof_adj = adj) |>
      select(-type)
  ) |>
  mutate(
    across(everything(), ~ replace_na(.x, 0))
  ) -> final_gw_dt



# save --------------------------------------------------------------------

write_csv(
  final_cow_dt,
  here::here("data", "cow_gcdf.csv")
)
write_csv(
  final_gw_dt,
  here::here("data", "gw_gcdf.csv")
)
