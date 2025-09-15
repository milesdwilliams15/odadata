# {odadata} <img src="inst/logo.png" align="right" height="130"/>

![R-version](https://img.shields.io/badge/R%20%3E%3D-4.2.1-brightgreen)
![updated](https://img.shields.io/badge/last%20update-09--14--2025-brightgreen)
![version](https://img.shields.io/badge/version-1.0.0-brightgreen)
![license](https://img.shields.io/badge/license-MIT-red)
![encoding](https://img.shields.io/badge/encoding-UTF--8-red)
[![orchid](https://img.shields.io/badge/ORCID-0000--0003--0192--5542-brightgreen)](https://orcid.org/0000-0003-0192-5542)

An R package for adding dyadic and country-level ODA data to {peacesciencer} constructed datasets.

------------------------------------------------------------------------

To install the latest development version of the package:

```
library(devtools)
devtools::install_github("milesdwilliams15/odadata")
```

The workhorse function of `{odadata}` is `add_oda()`. It is meant to work in conjunction with the [`{peacesciencer}`](https://github.com/svmiller/peacesciencer/tree/master) R package. The function will populate either a dyad-year or state-year dataset with bilateral ODA commitments and disbursements from the [OECD's API](https://www.oecd.org/en/data/insights/data-explainers/2024/09/api.html). Values are in millions of 2023 US Dollars.

To use the function, you must first create a country dyad-year or state-year dataset using `{peacesciencer}`. Then you can use `add_oda()` to populate your data with ODA values.

First, you should open up the `{tidyverse}`, `{peacesciencer}`, and `{odadata}`:

```
library(tidyverse)
library(peacesciencer)
library(odadata)
```

Then you can start making datasets. For example, a dyad-year dataset:

```
create_dyadyears(subset_years = 1960:2023) |>
  add_oda() -> dt
```

Or you can make a state-year dataset:

```
create_stateyears(subset_years = 1960:2023) |>
  add_oda() -> dt
```

By default, `add_oda()` adds ODA giving by donor at the state-year level. If you want it to provide ODA received simply set `type = "recipient"`

```
create_stateyears(subset_years = 1960:2023, type = "recipient") |>
  add_oda() -> dt
```