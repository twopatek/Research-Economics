#load libraries 
library(tidyverse)
library(janitor)
library(plotly)
library(eFRED)
library(shiny)
library(htmltools)
library(shinydashboard)
library(shinyWidgets)
library(scales)
library(DT)
library(memoise)
library(readxl)
library(networkD3)

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

# CBP Budget Data
path_to_file <- "51134-2025-01-Historical-Budget-Data.xlsx"

df_cbo_budget_receipts <- read_excel(path_to_file,
                     sheet = "2. Revenues",
                     skip  = 7,
                     n_max = 63) %>%        
  clean_names() %>%
  rename(fiscal_year = x1) %>% 
  pivot_longer(-fiscal_year,
               names_to  = "category",
               values_to = "amount") %>% 
  filter(category != "total") %>% 
  mutate(category = str_to_title(str_replace_all(category, "_", " "))) %>% 
  arrange(desc(fiscal_year))

df_cbo_budget_outlays <- read_excel(path_to_file,
                     sheet = "3. Outlays",
                     skip  = 8,
                     n_max = 63) %>%      
  clean_names() %>%
  rename(fiscal_year = x1)

df_cbo_budget_discretionary_outlays <- read_excel(path_to_file,
                          sheet = "4. Discretionary Outlays",
                          skip  = 7,
                          n_max = 63) %>%      
  clean_names() %>%
  rename(fiscal_year = x1)

df_cbo_budget_mandatory_outlays <- read_excel(path_to_file,
                          sheet = "5. Mandatory Outlays",
                          skip  = 7,
                          n_max = 63,
                          na    = "n.a.") %>%      
  clean_names() %>%
  rename(fiscal_year = x1)

df_cbo_budget_outlays <- df_cbo_budget_outlays %>% 
  full_join(df_cbo_budget_discretionary_outlays %>% select(-total), by = "fiscal_year") %>% 
  full_join(df_cbo_budget_mandatory_outlays %>% select(-offsetting_receipts, -total), by = "fiscal_year") %>% 
  pivot_longer(-fiscal_year,
               names_to  = "category",
               values_to = "amount") %>% 
  filter(!category %in% c("major_health_care_programs_net", "total")) %>% 
  mutate(category = str_to_title(str_replace_all(category, "_", " "))) %>% 
  arrange(desc(fiscal_year))

# Choices
budget_year_choices <- unique(df_cbo_budget_receipts$fiscal_year)


budget_guide <- tibble(
  Category = c(
    # Receipts
    "Individual Income Taxes",
    "Payroll Taxes",
    "Corporate Income Taxes",
    "Excise Taxes",
    "Estate and Gift Taxes",
    "Customs Duties",
    "Miscellaneous Receipts",
    # Outlays – Discretionary
    "Defense",
    "Nondefense",
    # Outlays – Mandatory (Programmatic)
    "Social Security",
    "Medicare",
    "Medicaid",
    "Income Security",
    "Federal Civilian and Military Retirement",
    "Veterans Programs",
    "Other Programs",
    # Outlays – Other
    "Offsetting Receipts",
    "Net Interest"
  ),
  Tier = c(
    rep("Receipts", 7),
    rep("Discretionary Outlays", 2),
    rep("Mandatory Outlays", 7),
    "Offsetting Receipts",
    "Net Interest"
  ),
  Definition = c(
    # Receipts definitions
    "Taxes on individual wages, salaries, investments, and other personal income.",
    "Employer/employee levies that fund Social Security and Medicare.",
    "Taxes on profits earned by U.S. corporations.",
    "Per-unit taxes on specific goods and activities (e.g. fuel, alcohol, tobacco).",
    "Taxes on property transfers at death (estate) or during life above exemption thresholds.",
    "Tariffs imposed on imported goods.",
    "All other revenues (fees, fines, earnings, etc.) not classified elsewhere.",
    # Discretionary definitions
    "Department of Defense spending: personnel, operations, procurement, R&D, infrastructure.",
    "All other annually appropriated programs (Education, Transportation, EPA, grants, etc.).",
    # Mandatory definitions
    "Retirement, survivor, and disability benefits under Social Security law.",
    "Health insurance for age 65+ and certain disabled individuals.",
    "Joint federal-state health coverage for low-income individuals and families.",
    "Unemployment insurance, SNAP, TANF, housing subsidies, and related supports.",
    "Pensions and benefits for retired federal civilian employees and military personnel.",
    "Health care, education, disability comp., and other VA-administered veteran services.",
    "Other entitlement programs (student loans, agriculture subsidies, energy assistance).",
    # Other outlays
    "Negative outlays credited back to the budget (e.g. fees, employer trust fund contributions).",
    "Interest the government pays on its debt minus interest it receives."
  )
)

preselected_series_names <- c(
  "Federal Interest Payments",
  "Effective Fed Funds Rate"
  )






