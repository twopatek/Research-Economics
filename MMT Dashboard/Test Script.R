library(readxl)
library(tidyverse)
library(janitor)

path_to_file <- "51134-2025-01-Historical-Budget-Data.xlsx"

# Read & clean Revenues
df_rev <- read_excel(path_to_file,
                             sheet = "2. Revenues",
                             skip  = 7,
                             n_max = 63) %>%        
  clean_names() %>%
  rename(fiscal_year = x1) %>% 
  pivot_longer(-fiscal_year,
               names_to  = "category",
               values_to = "amount")

# Read & clean Outlays
df_out <- read_excel(path_to_file,
                     sheet = "3. Outlays",
                     skip  = 8,
                     n_max = 63) %>%      
  clean_names() %>%
  rename(fiscal_year = x1) %>% 
  pivot_longer(-fiscal_year,
               names_to  = "category",
               values_to = "amount")

# Read & clean Discretionary Outlays
df_Disc_out <- read_excel(path_to_file,
                     sheet = "4. Discretionary Outlays",
                     skip  = 7,
                     n_max = 63) %>%      
  clean_names() %>%
  rename(fiscal_year = x1) %>% 
  pivot_longer(-fiscal_year,
               names_to  = "category",
               values_to = "amount")

# Read & clean Mandatory Outlays
df_Mand_out <- read_excel(path_to_file,
                     sheet = "5. Mandatory Outlays",
                     skip  = 7,
                     n_max = 63,
                     na    = "n.a.") %>%      
  clean_names() %>%
  rename(fiscal_year = x1) %>% 
  pivot_longer(-fiscal_year,
               names_to  = "category",
               values_to = "amount")




