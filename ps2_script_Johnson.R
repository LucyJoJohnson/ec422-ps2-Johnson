
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

# 5.1 Produce a Line Plot    
ggplot(unemp, aes(x = date, y = unemployment_rate, color = "red")) +
  geom_line() +        # or geom_col(), geom_point(), etc.
  labs(title = "Monthly Unemployment Rate 1976 - present", x = "Date", y = "Unemployment Rate (%)")
theme_minimal()

# 5.2 Produce Individual Line Plots 
ggplot(unemp, aes(x = date, y = unemployment_rate, color = "red")) +
  geom_line() +        # or geom_col(), geom_point(), etc.
  facet_wrap(~ region)
labs(title = "Monthly Unemployment Rate 1976 - present per Region", x = "Date", y = "Unemployment Rate (%)")

# 5.3 Create a Bar Chart     
unemp_summary |>
  ggplot(aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col(fill = "red") +
  labs(
    title = "Mean Unemployment Rate per Region",
    x = "Region",
    y = "Mean Unemployment Rate (%)"
  )
# 5.4 Your Choice
unemp |>
  filter(region == "United States") |>
  ggplot(aes(x = date, y = unemployment_rate_change, color = "red")) +
  geom_line() +        # or geom_col(), geom_point(), etc.
  labs(title = "Monthly US Unemployment Rate Change 1976 - present", x = "Date", y = "Unemployment Rate Change (%)")
# I chose to track the unemployment rate change of the general US from 1976 to the present, because I was interested in maybe the effect from 2020 Covid years, and I was correct in that the unemployment rate change changed drastically.

