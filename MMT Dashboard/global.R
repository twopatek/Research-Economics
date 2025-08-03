#load libraries 
library(tidyverse)
library(plotly)
library(eFRED)
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinyWidgets)
library(scales)
library(DT)
library(memoise)

# set key to pull data from FRED
api_key <- "21489194ba838be7e47627eb82142f3a"
set_fred_key(api_key)

# define a list of series to load
series_list <- list(
  yield_curve            = list(value = "T10Y2YM"),
  
  # Jobs
  payroll           = list(value = "PAYEMS"),
  unemployment      = list(value = "UNRATE"),
  
  # Fiscal aggregates
  total_debt             = list(value = "GFDEBTN"),    # Total public debt
  federal_deficit        = list(value = "FYFSD"),      # Federal surplus/deficit
  interest_payments      = list(value = "A091RC1Q027SBEA"), # Interets on public debt
  
  # Output
  gdp                    = list(value = "GDP"),        # Nominal GDP
  
  # Inflation measures
  cpi                    = list(value = "CPIAUCSL"),   # CPI (headline)
  core_cpi               = list(value = "CPILFESL"),   # CPI less food & energy
  pce_price              = list(value = "PCEPI"),      # PCE price index
  pce_ex_food_energy     = list(value = "PCEPILFE"),   # Core PCE
  
  # Money aggregates
  m2                     = list(value = "M2SL"),       # M2 money stock
  real_m2                = list(value = "M2REAL"),     # Real M2
  
  # Fed policy and balance sheet
  interest_rate          = list(value = "DFF"),
  fed_assets             = list(value = "WALCL")       # Fed total assets (QE)
)

series_meta <- tibble(
  series_id   = names(series_list),
  series_code = map_chr(series_list, "value"),
  series_name = c(
    "10Y–2Y Yield Curve",
    "Nonfarm Payroll",
    "Unemployment Rate",
    "Total Public Debt",
    "Federal Surplus/Deficit",
    "Federal Interest Payments",
    "Nominal GDP",
    "Headline CPI",
    "Core CPI",
    "PCE Price Index",
    "Core PCE",
    "M2 Money Stock",
    "Real M2",
    "Effective Fed Funds Rate",
    "Fed Total Assets"
  )
)

# Create a memoised version of fred()
fred_cached <- memoise(function(series_code) {
  fred(series = series_code, all = FALSE) %>% as_tibble()
})


df <- series_meta %>% 
  mutate(
    raw = map(
      series_code,
      ~ fred_cached(.x) %>%
        as_tibble()             
    )
  ) %>% 
  unnest(raw) %>%   
  group_by(series_id, series_name) %>%       
  arrange(date) %>% 
  mutate(
    value_norm = series / max(series, na.rm = TRUE),
    pct_change  = (series / lag(series) - 1) * 100
  ) %>% 
  ungroup() %>% 
  arrange(series_id)

df <- df %>% 
  filter(!is.na(series))

