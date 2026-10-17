
# 3.2: Change your code to load the saved data in the future + 3.1: Load and save your data
library(tidyquant)
library(tidyverse)

unemp_raw <- tq_get(
  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
  get  = "economic.data",
  from = "1976-01-01"
)

head(unemp_raw)

dir.create("data", showWarnings = FALSE)

saveRDS(unemp_raw, "data/unemp_raw.rds")
unemp_raw = read_rds("data/unemp_raw.rds")

# 4.1 Clean up your data
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon", 
      symbol == "CAUR"  ~ "California",
      symbol == "TXUR"   ~ "Texas",
      symbol == "NYUR" ~ "New York"
      
    )
  )

# 4.2: Wrangle your data
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  )
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )
# 4.3 Find the extremes

unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5)
# 4.4 Find the extremes excluding covid    
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5)

