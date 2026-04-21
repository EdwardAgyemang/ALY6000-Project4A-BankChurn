# Name: Edward Agyemang | Date: 2024 | Class: ALY 6000
# ============================================================
# ALY 6000 - Project 4A: Bank Customer Churn Analysis
# Dataset: Churn_Modelling.csv (10,000 customers)
# ============================================================

# Clear environment
rm(list = ls())

# Set working directory
setwd("C:/Users/elitebook folio/Documents/EDWARD PROJECTS/ALY6000/Project 4/Project 4A")

# Load packages
library(pacman)
p_load(tidyverse, janitor, scales, corrplot)

# ============================================================
# STEP 1 - Load the dataset
# read_csv loads the file as a tibble
# ============================================================
churn <- read_csv("Churn_Modelling.csv")

# First look at the data
head(churn)
glimpse(churn)
summary(churn)

# ============================================================
# STEP 2 - Clean column names
# clean_names() converts to snake_case
# ============================================================
churn <- clean_names(churn)
names(churn)

# ============================================================
# STEP 3 - Remove unnecessary columns
# row_number and customer_id are just identifiers
# surname is personal data not useful for analysis
# ============================================================
churn <- churn %>%
  select(-row_number, -customer_id, -surname)

# ============================================================
# STEP 4 - Convert binary columns to meaningful labels
# 0 and 1 are hard to read in charts and reports
# ============================================================
churn <- churn %>%
  mutate(
    exited        = ifelse(exited == 1, "Churned", "Stayed"),
    has_cr_card   = ifelse(has_cr_card == 1, "Yes", "No"),
    is_active_member = ifelse(is_active_member == 1, "Active", "Inactive")
  )

# ============================================================
# STEP 5 - Convert categorical columns to factors
# Factors tell R these are categories not numbers
# ============================================================
churn <- churn %>%
  mutate(
    geography        = as.factor(geography),
    gender           = as.factor(gender),
    exited           = as.factor(exited),
    has_cr_card      = as.factor(has_cr_card),
    is_active_member = as.factor(is_active_member),
    num_of_products  = as.factor(num_of_products)
  )

# ============================================================
# STEP 6 - Check for missing values
# ============================================================
cat("Missing values per column:\n")
colSums(is.na(churn))

# ============================================================
# STEP 7 - Check cleaned dataset
# ============================================================
glimpse(churn)
summary(churn)

# ============================================================
# STEP 8 - Overall churn rate
# ============================================================
churn_rate <- churn %>%
  count(exited) %>%
  mutate(percentage = round(n / sum(n) * 100, 2))
churn_rate

# ============================================================
# STEP 9 - Churn by geography
# ============================================================
churn_by_country <- churn %>%
  group_by(geography, exited) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(geography) %>%
  mutate(pct = round(count / sum(count) * 100, 2))
churn_by_country

# ============================================================
# STEP 10 - Churn by gender
# ============================================================
churn_by_gender <- churn %>%
  group_by(gender, exited) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(pct = round(count / sum(count) * 100, 2))
churn_by_gender

# ============================================================
# STEP 11 - Average stats for churned vs stayed customers
# ============================================================
churn_stats <- churn %>%
  group_by(exited) %>%
  summarise(
    avg_credit_score  = round(mean(credit_score), 2),
    avg_age           = round(mean(age), 2),
    avg_tenure        = round(mean(tenure), 2),
    avg_balance       = round(mean(balance), 2),
    avg_salary        = round(mean(estimated_salary), 2)
  )
churn_stats

# ============================================================
# STEP 12 - Active vs inactive member churn
# ============================================================
churn_by_activity <- churn %>%
  group_by(is_active_member, exited) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(is_active_member) %>%
  mutate(pct = round(count / sum(count) * 100, 2))
churn_by_activity

# ============================================================
# VISUALIZATION 1 - Overall Churn Rate Bar Chart
# ============================================================
ggplot(churn, aes(x = exited, fill = exited)) +
  geom_bar() +
  geom_text(stat = "count",
            aes(label = paste0(..count.., "\n(",
                               round(..count.. / nrow(churn) * 100, 1), "%)")),
            vjust = 0.5,
            position = position_stack(vjust = 0.5),
            size = 5,
            color = "white",
            fontface = "bold") +
  scale_fill_manual(values = c("Churned" = "#C0392B",
                               "Stayed"  = "#2471A3")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Customer Churn Distribution",
       subtitle = "Bank Customer Dataset — 10,000 Customers",
       x        = "Customer Status",
       y        = "Number of Customers") +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    legend.position  = "none",
    plot.background  = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text        = element_text(color = "black", size = 11)
  )

ggsave("viz1_churn_distribution.png",
       width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# VISUALIZATION 2 - Churn Rate by Geography
# ============================================================
ggplot(churn_by_country %>% filter(exited == "Churned"),
       aes(x = reorder(geography, -pct),
           y = pct, fill = geography)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            size = 5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("France"  = "#2471A3",
                               "Germany" = "#C0392B",
                               "Spain"   = "#1E8449")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Churn Rate by Country",
       subtitle = "Germany has the highest churn rate",
       x        = "Country",
       y        = "Churn Rate (%)",
       fill     = "Country") +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    legend.position  = "none",
    plot.background  = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text        = element_text(color = "black", size = 11)
  )

ggsave("viz2_churn_by_country.png",
       width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# VISUALIZATION 3 - Age Distribution by Churn Status
# ============================================================
ggplot(churn, aes(x = age, fill = exited)) +
  geom_histogram(binwidth = 5, color = "white",
                 position = "dodge") +
  scale_fill_manual(values = c("Churned" = "#C0392B",
                               "Stayed"  = "#2471A3")) +
  labs(title    = "Age Distribution by Churn Status",
       subtitle = "Older customers tend to churn more",
       x        = "Age",
       y        = "Number of Customers",
       fill     = "Status") +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    plot.background  = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text        = element_text(color = "black", size = 11)
  )

ggsave("viz3_age_distribution.png",
       width = 8, height = 6, dpi = 300, bg = "white")


# ============================================================
# VISUALIZATION 4 - Balance Distribution by Churn Status
# ============================================================
ggplot(churn, aes(x = exited, y = balance, fill = exited)) +
  geom_boxplot(color = "black", outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Churned" = "#C0392B",
                               "Stayed"  = "#2471A3")) +
  scale_y_continuous(labels = comma) +
  labs(title    = "Account Balance by Churn Status",
       subtitle = "Churned customers tend to have higher balances",
       x        = "Customer Status",
       y        = "Account Balance (USD)",
       fill     = "Status") +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    legend.position  = "none",
    plot.background  = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text        = element_text(color = "black", size = 11)
  )

ggsave("viz4_balance_boxplot.png",
       width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# VISUALIZATION 5 - Churn by Active Membership Status
# ============================================================
ggplot(churn_by_activity,
       aes(x = is_active_member, y = pct,
           fill = exited)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.9),
            vjust = 5,
            color = "white",
            fontface = "bold",
            size = 5) +
  scale_fill_manual(values = c("Churned" = "#C0392B",
                               "Stayed"  = "#2471A3")) +
  labs(title    = "Churn Rate by Membership Activity",
       subtitle = "Inactive members are far more likely to churn",
       x        = "Membership Status",
       y        = "Percentage (%)",
       fill     = "Customer Status") +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    plot.background  = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text        = element_text(color = "black", size = 11)
  )

ggsave("viz5_active_membership.png",
       width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# STEP 13 - Create new variables
# ============================================================
churn <- churn %>%
  mutate(
    # Age group categories
    age_group = case_when(
      age < 30             ~ "Under 30",
      age >= 30 & age < 45 ~ "30 to 44",
      age >= 45 & age < 60 ~ "45 to 59",
      age >= 60            ~ "60 and above"
    ),
    # Balance category
    balance_category = case_when(
      balance == 0              ~ "Zero Balance",
      balance < 50000           ~ "Low Balance",
      balance < 100000          ~ "Medium Balance",
      TRUE                      ~ "High Balance"
    ),
    # Credit score category
    credit_category = case_when(
      credit_score < 580  ~ "Poor",
      credit_score < 670  ~ "Fair",
      credit_score < 740  ~ "Good",
      credit_score < 800  ~ "Very Good",
      TRUE                ~ "Exceptional"
    )
  )

# ============================================================
# VISUALIZATION 6 - Churn Rate by Age Group
# ============================================================
churn_age_group <- churn %>%
  group_by(age_group, exited) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(age_group) %>%
  mutate(pct = round(count / sum(count) * 100, 2))

ggplot(churn_age_group %>% filter(exited == "Churned"),
       aes(x = age_group, y = pct,
           fill = age_group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(pct, "%")),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(title    = "Churn Rate by Age Group",
       subtitle = "Customers aged 45-59 have the highest churn rate",
       x        = "Age Group",
       y        = "Churn Rate (%)",
       fill     = "Age Group") +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    legend.position  = "none",
    plot.background  = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text        = element_text(color = "black", size = 11)
  )

ggsave("viz6_churn_by_age_group.png",
       width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# VISUALIZATION 7 - Credit Score Category by Geography Heatmap
# ============================================================

churn_heat <- churn %>%
  mutate(credit_category = case_when(
    credit_score < 580  ~ "Poor (< 580)",
    credit_score < 670  ~ "Fair (580-669)",
    credit_score < 740  ~ "Good (670-739)",
    credit_score < 800  ~ "Very Good (740-799)",
    TRUE                ~ "Exceptional (800+)"
  )) %>%
  group_by(geography, credit_category, exited) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(geography, credit_category) %>%
  mutate(churn_rate = round(count / sum(count) * 100, 1)) %>%
  filter(exited == "Churned")

ggplot(churn_heat,
       aes(x = credit_category,
           y = geography,
           fill = churn_rate)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(churn_rate, "%")),
            color = "white", fontface = "bold", size = 4.5) +
  scale_fill_gradient(low = "#2471A3",
                      high = "#C0392B",
                      name = "Churn Rate (%)") +
  scale_x_discrete(limits = c("Poor (< 580)",
                              "Fair (580-669)",
                              "Good (670-739)",
                              "Very Good (740-799)",
                              "Exceptional (800+)")) +
  labs(title    = "Churn Rate by Credit Score and Country",
       subtitle = "Germany shows high churn across all credit score ranges",
       x        = "Credit Score Category",
       y        = "Country") +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    axis.text.x      = element_text(angle = 25, hjust = 1,
                                    color = "black", size = 10),
    axis.text.y      = element_text(color = "black", size = 11),
    plot.background  = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

ggsave("viz7_credit_geography_heatmap.png",
       width = 10, height = 6, dpi = 300, bg = "white")

# ============================================================
# VISUALIZATION 8 - Number of Products vs Churn Rate
# ============================================================

churn_products <- churn %>%
  group_by(num_of_products, exited) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(num_of_products) %>%
  mutate(pct = round(count / sum(count) * 100, 2)) %>%
  filter(exited == "Churned")

ggplot(churn_products,
       aes(x = num_of_products,
           y = pct,
           fill = num_of_products)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(pct, "%")),
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("1" = "#2471A3",
                               "2" = "#1E8449",
                               "3" = "#E67E22",
                               "4" = "#C0392B")) +
  labs(title    = "Churn Rate by Number of Products",
       subtitle = "Customers with 3-4 products have dramatically higher churn",
       x        = "Number of Products",
       y        = "Churn Rate (%)",
       fill     = "Products") +
  theme_classic() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    legend.position  = "none",
    plot.background  = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text        = element_text(color = "black", size = 11)
  )

ggsave("viz8_products_churn.png",
       width = 8, height = 6, dpi = 300, bg = "white")

# ============================================================
# ANALYSIS 9 - Churn Risk Profile Summary
# Most at-risk customer profile
# ============================================================

# High risk customers — churned profile
high_risk <- churn %>%
  filter(exited == "Churned") %>%
  summarise(
    avg_age          = round(mean(age), 1),
    avg_credit_score = round(mean(credit_score), 0),
    avg_balance      = round(mean(balance), 0),
    avg_tenure       = round(mean(tenure), 1),
    avg_salary       = round(mean(estimated_salary), 0),
    pct_female       = round(mean(gender == "Female") * 100, 1),
    pct_germany      = round(mean(geography == "Germany") * 100, 1),
    pct_inactive     = round(mean(is_active_member == "Inactive") * 100, 1)
  )

# Low risk customers — stayed profile
low_risk <- churn %>%
  filter(exited == "Stayed") %>%
  summarise(
    avg_age          = round(mean(age), 1),
    avg_credit_score = round(mean(credit_score), 0),
    avg_balance      = round(mean(balance), 0),
    avg_tenure       = round(mean(tenure), 1),
    avg_salary       = round(mean(estimated_salary), 0),
    pct_female       = round(mean(gender == "Female") * 100, 1),
    pct_germany      = round(mean(geography == "Germany") * 100, 1),
    pct_inactive     = round(mean(is_active_member == "Inactive") * 100, 1)
  )

# Combine into comparison table
risk_comparison <- data.frame(
  Metric           = c("Average Age", "Average Credit Score",
                       "Average Balance (USD)", "Average Tenure (years)",
                       "Average Salary (USD)", "% Female",
                       "% Germany", "% Inactive Members"),
  Churned_Profile  = c(high_risk$avg_age, high_risk$avg_credit_score,
                       high_risk$avg_balance, high_risk$avg_tenure,
                       high_risk$avg_salary, high_risk$pct_female,
                       high_risk$pct_germany, high_risk$pct_inactive),
  Stayed_Profile   = c(low_risk$avg_age, low_risk$avg_credit_score,
                       low_risk$avg_balance, low_risk$avg_tenure,
                       low_risk$avg_salary, low_risk$pct_female,
                       low_risk$pct_germany, low_risk$pct_inactive)
)

print(risk_comparison)

# Save as CSV for slides reference
write_csv(risk_comparison, "churn_risk_profile.csv")