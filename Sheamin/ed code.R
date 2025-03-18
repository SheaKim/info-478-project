file1 <- "ed_visits"
file2 <- "ed_traj.csv"
df1 <- read.csv(file1)
df2 <- read.csv(file2)
# Convert week_end to Date format
df1$week_end <- as.Date(df1$week_end, format="%Y-%m-%d")
df2$week_end <- as.Date(df2$week_end, format="%Y-%m-%d")

# Clean df1 (Trajectories dataset) - Select relevant columns
df1_clean <- df1 %>%
  select(week_end, geography, county, percent_visits_influenza) %>%
  filter(!is.na(percent_visits_influenza))
# Clean df2 (Demographics dataset) - Select flu data only
df2_clean <- df2 %>%
  filter(pathogen == "Influenza") %>% # Select only Influenza-related ED visits
  
  select(week_end, geography, percent_visits) %>%
  rename(percent_visits_influenza = percent_visits) %>%
  filter(!is.na(percent_visits_influenza))
# Merge both datasets for better insights
df_combined <- bind_rows(df1_clean, df2_clean)
# Create yearly summary table (limit to top 5 states)
summary_table <- df_combined %>%
  mutate(year = year(week_end)) %>%
  group_by(year, geography) %>%
  summarize(avg_percent_influenza = mean(percent_visits_influenza, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_percent_influenza)) %>%
  group_by(year) %>%
  slice_max(order_by = avg_percent_influenza, n = 5) # Keep only the top 5 states

# Filter dataset for only top 5 states to avoid clutter in the bar plot
top_states <- unique(summary_table$geography)
df_filtered <- df_combined %>%
  mutate(year = year(week_end)) %>%
  filter(geography %in% top_states)
# Bar Plot: Yearly average flu-related ED visits for top 5 states
ggplot(summary_table, aes(x = as.factor(year), y = avg_percent_influenza, fill = geography)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Top 5 States with Highest Influenza-Related ED Visits (2022-2025)",
       x = "Year",
       y = "Average % of ED Visits",
       fill = "State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for clarity

anova_result <- aov(percent_visits_influenza ~ as.factor(year), data = df_filtered)
summary(anova_result) # Print ANOVA test result

df_demo <- read.csv(file2)
df_demo_clean <- df_demo %>%
  filter(pathogen == "Influenza") %>% # Keep only Influenza-related ED visits
  select(week_end, demographics_type, demographics_values, percent_visits) %>%
  rename(percent_visits_influenza = percent_visits) %>%
  filter(!is.na(percent_visits_influenza)) # Remove missing values

top_age <- df_demo_clean %>%
  filter(demographics_type == "Age Group") %>%
  group_by(demographics_values) %>%
  summarize(avg_percent_influenza = mean(percent_visits_influenza, na.rm = TRUE)) %>%
  arrange(desc(avg_percent_influenza))
top_race <- df_demo_clean %>%
  filter(demographics_type == "Race/Ethnicity") %>%
  group_by(demographics_values) %>%
  summarize(avg_percent_influenza = mean(percent_visits_influenza, na.rm = TRUE)) %>%
  arrange(desc(avg_percent_influenza))
top_sex <- df_demo_clean %>%
  filter(demographics_type == "Sex") %>%
  group_by(demographics_values) %>%
  summarize(avg_percent_influenza = mean(percent_visits_influenza, na.rm = TRUE)) %>%
  arrange(desc(avg_percent_influenza))
# Print top demographic groups
print(top_age)


top_demo <- bind_rows(top_age, top_race, top_sex)
ggplot(top_demo, aes(x = demographics_values, y = avg_percent_influenza)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Highest Influenza-Related ED Visit Rate by Demographic Category from 2022-25",
       x = "Demographic Group (Age, Race, Sex)",
       y = "Average % of ED Visits",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate labels for better readability


