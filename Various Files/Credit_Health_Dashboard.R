#load libraries 
pacman::p_load(tidyverse, gganimate, eFRED, zoo)

# set key to pull data from FRED
api_key <- "21489194ba838be7e47627eb82142f3a"
set_fred_key(api_key)

# Pull date from FRED API
debt <- fred(credit_debt = "TOTALSL")
default <- fred(default_rate = "DRCCLACBS")
interest <- fred(interest_rate = "DFF")

# Convert data to quarterly format
debt_quarterly <- debt %>%
  mutate(date = as.Date(date)) %>%
  group_by(quarter = as.yearqtr(date)) %>%
  summarise(credit_debt = mean(credit_debt, na.rm = TRUE))

# Convert data to quarterly format
interest_quarterly <- interest %>%
  mutate(date = as.Date(date)) %>%
  group_by(quarter = as.yearqtr(date)) %>%
  summarise(interest_rate = mean(interest_rate, na.rm = TRUE))

# Convert data to quarterly format
default_quarterly <- default %>%
  mutate(date = as.Date(date)) %>%
  group_by(quarter = as.yearqtr(date)) %>%
  summarise(default_rate = sum(default_rate, na.rm = TRUE))

# Merge data sets 
merged_data <- merge(debt_quarterly, default_quarterly, by = "quarter")
merged_data <- merge(merged_data, interest_quarterly, by = "quarter")

# Normalize data series and convert to long format
final_data <- merged_data %>%
  mutate(default_risk_ratio = (default_rate / credit_debt)*100) %>% # proportion of credit debt that is at risk of default
  mutate(debt_norm = ifelse(is.na(credit_debt), NA, round((credit_debt / max(credit_debt, na.rm = TRUE)), 2))) %>% 
  mutate(default_norm = ifelse(is.na(default_rate), NA, round((default_rate / max(default_rate, na.rm = TRUE)), 2))) %>% 
  mutate(interest_norm = ifelse(is.na(interest_rate), NA, round((interest_rate / max(interest_rate, na.rm = TRUE)), 2))) %>% 
  mutate(default_risk_ratio_norm = ifelse(is.na(default_risk_ratio), NA, round((default_risk_ratio / max(default_risk_ratio, na.rm = TRUE)), 2))) %>% 
  mutate(quarter = as.Date(as.yearqtr(merged_data$quarter), frac = 1)) %>% 
  pivot_longer(cols = 2:9, names_to = "series_name", values_to = "series_value") %>% 
  mutate(series_value = round(series_value, 2))

# Filter data for visualizing
filtered_data <- final_data %>% 
  filter(series_name %in% c("debt_norm", "default_norm", "default_risk_ratio_norm"))
  # filter(!series_name %in% c("credit_debt", "default_rate", "interest_rate", "default_risk_ratio"))
  
# Define recession periods
recessions <- tibble(
  x0 = as.Date(c("1979-10-01", "1981-04-01", "1989-10-01", "2001-01-01", "2007-10-01", "2020-01-01")),
  x1 = as.Date(c("1979-10-31", "1982-04-01", "1991-01-01", "2001-07-01", "2009-04-01", "2020-04-01")),
  y0 = -Inf,
  y1 = Inf
)

# Visualization
p <- ggplot(filtered_data, aes(x = quarter, y = series_value, color = series_name)) +
  geom_line(linewidth = .95) +
  # geom_rect(data = recessions, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1), fill = "blue", alpha = 0.1) +
  transition_reveal(quarter) +
  labs(title = "Time Period : {frame_along}",
       x = "",
       y = "Normalized Value") + 
  view_follow(fixed_y = TRUE) + 
  ease_aes("linear")

# animate plot in view pane
animate(p, nframes = 396, fps = 30, width = 800, height = 500)

# animate plot in view pane
animate(p, width = 800, height = 500)


