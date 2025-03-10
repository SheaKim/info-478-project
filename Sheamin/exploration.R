library(tidyverse)

vax_df <- read.csv("cdc_vaccine_prices.csv")
inflation_df <- read.csv("inflation_cpis.csv")

vax_df$Private.Sector.Cost..Dose = gsub("\\$", "", vax_df$Private.Sector.Cost..Dose) 
vax_df$CDC.Cost..Dose = gsub("\\$", "", vax_df$CDC.Cost..Dose) 

vax_df$Private.Sector.Cost..Dose <- as.numeric(as.character(vax_df$Private.Sector.Cost..Dose))
sapply(vax_df, class)

vax_df$CDC.Cost..Dose <- as.numeric(as.character(vax_df$CDC.Cost..Dose))
sapply(vax_df, class)

vax_df$Date <- as.Date(vax_df$Date)

# extract the year and convert to numeric format
vax_df$year <- as.numeric(format(vax_df$Date, "%Y"))

vax_df = merge(x = vax_df, y = inflation_df, by = "year")
vax_df$CPI <- as.numeric(as.character(vax_df$CPI))
sapply(vax_df, class)

reference_year <- 2009

# Get CPI for the reference year
reference_cpi <- vax_df$CPI[vax_df$year == reference_year]

# print(typeof(reference_cpi))

# Adjust prices for inflation based on the reference CPI
vax_df$adjusted_price <- vax_df$Private.Sector.Cost..Dose * (reference_cpi / vax_df$CPI)

#Year 2 Price = Year 1 Price x (Year 2 CPI/Year 1 CPI)

plot_1 <- vax_df %>%
  group_by(year) %>%
  summarise(average_price = mean(Private.Sector.Cost..Dose), 
            average_adjust_price = mean(adjusted_price)) %>%
  pivot_longer(cols = c("average_price", "average_adjust_price"), 
               names_to = "price_type", 
               values_to = "price") %>%
  ggplot(aes(x=factor(year), y=price, fill=price_type)) +
  geom_col(position="dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(labels = 2009:2025, breaks = 2009:2025) +
  labs(title = "Average Price of 10 Influenza Vaccine Doses",
       x = "Year",
       y = "Price",
       fill = "Adjusted for Inflation") 

print(plot_1)

private_cdc_comparison_plot <- vax_df %>%
  group_by(year) %>%
  summarise(average_priv_price = mean(Private.Sector.Cost..Dose), 
            average_cdc_price = mean(CDC.Cost..Dose)) %>%
  pivot_longer(cols = c("average_priv_price", "average_cdc_price"), 
               names_to = "price_type", 
               values_to = "price") %>%
  ggplot(aes(x=factor(year), y=price, group=price_type)) +
  geom_line(aes(color=price_type))

private_cdc_comparison_plot

# lin reg
model <- lm(adjusted_price ~ year, data = vax_df)
print(summary(model))

ggplot(vax_df, aes(x = year, y = adjusted_price)) +
  geom_point() +
  geom_smooth(method = "lm")

#one tailed t test

test <- t.test(vax_df$Private.Sector.Cost..Dose, vax_df$CDC.Cost..Dose,
               alternative = "greater")
test


# Boxplots of adjusted_price by Manufacturer

vax_df$Manufacturer <- str_replace_all(vax_df$Manufacturer,"CSL",'bioCSL')
vax_df$Manufacturer <- str_replace_all(vax_df$Manufacturer,"biobioCSL",'bioCSL')


boxplot <- ggplot(vax_df, aes(x = Manufacturer, y = Private.Sector.Cost..Dose)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribution of Adjusted Prices by Manufacturer",
    x = "Manufacturer",
    y = "Private Sector Price"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

boxplot



