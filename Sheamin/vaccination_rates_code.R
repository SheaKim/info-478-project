# Dataset #1: Monthly Flu Vaccinations

df1 <- read.csv("monthly_cumulative.csv")

# Define the correct month order
month_levels <- c("SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG")

# Step 1: Ensure month is a factor for proper sorting
df1 <- df1 %>%
  mutate(month = factor(month, levels = month_levels))

# getting new dose numbers
rates_df <- df1 %>%
  arrange(current_season, jurisdiction, age_group_label, month) %>%
  group_by(current_season, jurisdiction, age_group_label) %>%
  mutate(
    new_doses = numerator - lag(numerator, default = NA)  # New doses = current - previous
  ) %>%
  ungroup()

# summary table
monthly_trends <- rates_df %>%
  group_by(current_season, month) %>%
  summarise(new_doses = sum(new_doses, na.rm = TRUE)) %>%
  arrange(current_season, month)

print(monthly_trends)


# plot
rates_df %>%
  filter(!is.na(estimate)) %>%
  group_by(jurisdiction) %>%
  summarise(avg_vaccination_rate = mean(estimate, na.rm = TRUE)) %>%
  top_n(10, avg_vaccination_rate) %>%
  ggplot(aes(x = reorder(jurisdiction, avg_vaccination_rate), y = avg_vaccination_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Jurisdictions by Avg. Flu Vaccination Rate",
       x = "Jurisdiction", y = "Avg. Vaccination Rate") +
  theme_minimal()


# getting vaccination rates (new doses/population)
overall_pop <- rates_df %>%
  filter(age_group_label == "Overall") %>%
  select(jurisdiction, current_season, population) %>%
  rename(overall_population = population)

rates_df <- rates_df %>%
  left_join(overall_pop, by = c("jurisdiction", "current_season"))

rates_df <- rates_df %>%
  mutate(vax_rate = new_doses / coalesce(population, overall_population))

rates_df <- rates_df %>%
  mutate(start_year = as.integer(substr(current_season, 1, 4)),  # Extract the first year
         # Combine starting year with month to create a valid date
         date = as.Date(paste(start_year, month, "01", sep = "-"), format = "%Y-%b-%d"))


## plotting rates over time?? experimenting
temp_df <- rates_df %>%
  filter(age_group_label == "Overall") %>%
  group_by(date) %>%
  summarise(avg_rate = mean(new_doses, na.rm = TRUE))

test <- temp_df %>%
  ggplot(aes(x = date, y = avg_rate)) +
  geom_line()

test

# 22-23 and 23-24 have significantly different doses
season_22_23 <- rates_df |>
  filter(current_season == "2022-23")

season_23_24 <- rates_df |>
  filter(current_season == "2023-24")

test_1<-t.test(season_22_23$numerator,season_23_24$numerator)
test_1



## reading in vax price data

vax_df <- read.csv("cdc_vaccine_prices_full.csv")
inflation_df <- read.csv("inflation_cpis.csv")

#standardizing values, getting rid of $
vax_df$Private.Sector.Cost..Dose = gsub("\\$", "", vax_df$Private.Sector.Cost..Dose) 
vax_df$CDC.Cost..Dose = gsub("\\$", "", vax_df$CDC.Cost..Dose) 

vax_df$Private.Sector.Cost..Dose <- as.numeric(as.character(vax_df$Private.Sector.Cost..Dose))
sapply(vax_df, class)

vax_df$CDC.Cost..Dose <- as.numeric(as.character(vax_df$CDC.Cost..Dose))
sapply(vax_df, class)

vax_df$Date <- as.Date(vax_df$Date)


## Adding inflation data
# extract the year and convert to numeric format
vax_df$year <- as.numeric(format(vax_df$Date, "%Y"))

vax_df = merge(x = vax_df, y = inflation_df, by = "year")
vax_df$CPI <- as.numeric(as.character(vax_df$CPI))
sapply(vax_df, class)

reference_year <- 2009

# Get CPI for the reference year
reference_cpi <- vax_df$CPI[vax_df$year == reference_year]

# the type of below should be double
# print(typeof(reference_cpi)) 

# Adjust prices for inflation based on the reference CPI
vax_df$adjusted_price <- vax_df$Private.Sector.Cost..Dose * (reference_cpi / vax_df$CPI)
vax_df$adjusted_price_cdc <- vax_df$CDC.Cost..Dose * (reference_cpi / vax_df$CPI)


rates_df$numerator <- as.numeric(rates_df$numerator)

rates_df$date <- as.Date(rates_df$date)

# 1. Deduplicate by keeping only the first row for each date, jurisdiction, age group, and season
df_dedup <- rates_df %>%
  group_by(jurisdiction, age_group_label, current_season, date) %>%
  slice(1) %>%               # Keep only the first row for each group
  ungroup()

# 2. Sort by jurisdiction, age group, season, and date to ensure proper order for calculating new doses
df_dedup <- df_dedup %>%
  arrange(jurisdiction, age_group_label, current_season, date)

# 3. Calculate new doses by comparing the cumulative totals
df_dedup <- df_dedup %>%
  group_by(jurisdiction, age_group_label, current_season) %>%
  mutate(new_doses = numerator - lag(numerator)) %>%    # Subtract previous month from current month
  ungroup() %>%
  mutate(new_doses = ifelse(new_doses < 0, 0, new_doses))

all_totals <- df_dedup %>%
  group_by(date) %>%
  summarise(total_doses = sum(new_doses, na.rm = TRUE))

all_totals$year <- format(all_totals$date, "%Y")

all_totals$doses_div_ten <- (all_totals$total_doses) / 10

yr_totals <- all_totals %>%
  group_by(year) %>%
  summarise(total_doses = sum(total_doses))


price_table <- vax_df %>%
  group_by(year) %>%
  summarise(cost = mean(adjusted_price))

price_table$cost_per_dose <- (price_table$cost) / 10

price_yr_totals <- merge(x = yr_totals, y = price_table, by = "year")

price_yr_totals$money_spent <- price_yr_totals$total_doses * price_yr_totals$cost_per_dose

anova_result <- aov(money_spent ~ as.factor(year), data = price_yr_totals)
summary(anova_result)
print(anova_result)


df_for_cop <- df_dedup %>%
  group_by(date) %>%
  summarise(plot_col = sum(new_doses, na.rm = TRUE)) %>%
  

exp_plot <- vax_df %>%
  group_by(Date) %>%
  summarise(new_col = mean(adjusted_price))
  
plot01 <- ggplot() +
  geom_line(data=exp_plot, aes(x=Date, y=new_col)) +
  geom_line(data=df_for_cop, aes(x=date, y=plot_col)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*10, name="Second Axis")
  )

plot01
