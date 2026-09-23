
# ── 1. LOAD THE FILE ──────────────────────────────────────────────────────────

data <- read.table(
  "/Users/sulaimanmirzai/desktop/PLFD/data students.txt",
  header    = TRUE,      # first row = column names
  sep       = ";",       # semicolon separator
  quote     = "\"",      # double quotes
  row.names = 1          # first column is index, not a variable
)

# ── 2. INITIAL CLEANING ───────────────────────────────────────────────────────

# Remove group 1 rows: rows 11, 1 and 40
# (before any analysis)
data <- data[-c(1, 11, 40), ]

# Remove a priori excluded variables
data$roof_color    <- NULL
data$pet_ownership <- NULL

# Convert categorical variables to factor
data$epc_label    <- factor(data$epc_label,
                            levels = c("A","B","C","D","E","F"))# A = reference

data$building_era <- factor(data$building_era,
                            levels = c("Pre-1970","1970-2000","Post-2000"))

# ── 3. MISSING VALUES CHECK ───────────────────────────────────────────────────

# Total NAs per variable
colSums(is.na(data))

# Percentage of NAs per variable
round(colSums(is.na(data)) / nrow(data) * 100, 2)

# Total rows with at least one NA
sum(!complete.cases(data))


# ── 4. REMOVE MISSING VALUES ──────────────────────────────────────────────────

# Remove all rows with at least one NA
data_clean <- na.omit(data)

# Verify final dimensions
nrow(data_clean)   # should be 2482
ncol(data_clean)   # should be 6 variables


# ── 5. DESCRIPTIVE STATISTICS - CONTINUOUS VARIABLES ─────────────────────────

# Select numeric variables only
numeric_vars <- data_clean[, c("annual_kwh", "sq_meters", "income_euro", 
                               "occupancy_count", "dist_to_brussels")]

# Summary per variable: mean, sd, median, IQR, min, max
summary_stats <- data.frame(
  mean   = round(sapply(numeric_vars, mean),   2),
  sd     = round(sapply(numeric_vars, sd),     2),
  median = round(sapply(numeric_vars, median), 2),
  IQR    = round(sapply(numeric_vars, IQR),    2),
  min    = round(sapply(numeric_vars, min),    2),
  max    = round(sapply(numeric_vars, max),    2)
)

print(summary_stats)

# ── 6. HISTOGRAMS - CONTINUOUS VARIABLES ─────────────────────────────────────

library(ggplot2)

# --- annual_kwh ---
ggplot(data_clean, aes(x = annual_kwh)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Annual Energy Consumption",
       x = "Annual kWh", y = "Density") +
  theme_minimal()

# --- sq_meters ---
ggplot(data_clean, aes(x = sq_meters)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Living Area",
       x = "Square Meters", y = "Density") +
  theme_minimal()

# --- income_euro ---
ggplot(data_clean, aes(x = income_euro)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Annual Household Income",
       x = "Income (€)", y = "Density") +
  theme_minimal()

# --- occupancy_count ---
ggplot(data_clean, aes(x = occupancy_count)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Occupancy Count",
       x = "Number of Residents", y = "Density") +
  theme_minimal()

# --- dist_to_brussels ---
ggplot(data_clean, aes(x = dist_to_brussels)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Distance to Brussels",
       x = "Distance (km)", y = "Density") +
  theme_minimal()

# ── 7. BOXPLOTS - CONTINUOUS VARIABLES ───────────────────────────────────────

# --- annual_kwh ---
ggplot(data_clean, aes(y = annual_kwh)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Annual Energy Consumption",
       y = "Annual kWh") +
  theme_minimal()

# --- sq_meters ---
ggplot(data_clean, aes(y = sq_meters)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Living Area",
       y = "Square Meters") +
  theme_minimal()

# --- income_euro ---
ggplot(data_clean, aes(y = income_euro)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Annual Household Income",
       y = "Income (€)") +
  theme_minimal()


# --- occupancy_count ---
ggplot(data_clean, aes(y = occupancy_count)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Occupancy Count",
       y = "Number of Residents") +
  theme_minimal()

# --- dist_to_brussels ---
ggplot(data_clean, aes(y = dist_to_brussels)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Distance to Brussels",
       y = "Distance (km)") +
  theme_minimal()

# ── 8. OUTLIER ANALYSIS ───────────────────────────────────────────────────────

# Dataframe with extreme outliers (3.0 x IQR rule)
# Row numbers refer to the cleaned dataset (after group 1 removals)
outliers_df <- data_clean[data_clean$annual_kwh > 16179.90, ]

# Inspect all variables for these observations
print(outliers_df)

# ── 9. OUTLIER JUSTIFICATION ──────────────────────────────────────────────────


# Define extreme outliers (5 impossible values)
outliers_extreme <- data_clean[data_clean$annual_kwh > 1000000, ]

# Remaining data
data_no_extreme <- data_clean[data_clean$annual_kwh <= 1000000, ]

# --- Comparison table ---
comparison_table <- data.frame(
  Metric          = c("annual_kwh (mean)", "annual_kwh (max)",
                      "sq_meters (mean)",  "sq_meters (max)",
                      "occupancy_count (mean)", "occupancy_count (max)",
                      "income_euro (mean)", "income_euro (max)"),
  Outliers_5      = c(
    round(mean(outliers_extreme$annual_kwh), 2),
    round(max(outliers_extreme$annual_kwh),  2),
    round(mean(outliers_extreme$sq_meters),  2),
    round(max(outliers_extreme$sq_meters),   2),
    round(mean(outliers_extreme$occupancy_count), 2),
    round(max(outliers_extreme$occupancy_count),  2),
    round(mean(outliers_extreme$income_euro), 2),
    round(max(outliers_extreme$income_euro),  2)
  ),
  Rest_of_sample  = c(
    round(mean(data_no_extreme$annual_kwh), 2),
    round(max(data_no_extreme$annual_kwh),  2),
    round(mean(data_no_extreme$sq_meters),  2),
    round(max(data_no_extreme$sq_meters),   2),
    round(mean(data_no_extreme$occupancy_count), 2),
    round(max(data_no_extreme$occupancy_count),  2),
    round(mean(data_no_extreme$income_euro), 2),
    round(max(data_no_extreme$income_euro),  2)
  )
)

print(comparison_table)

# --- Scatterplots highlighting the 5 extreme outliers ---

# Label variable: outlier or not
data_clean$outlier_flag <- ifelse(data_clean$annual_kwh > 1000000,
                                  "Extreme outlier", "Normal observation")

# Plot 1: annual_kwh vs sq_meters
ggplot(data_clean, aes(x = sq_meters, y = annual_kwh,
                       color = outlier_flag, size = outlier_flag)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Extreme outlier"    = "darkred",
                                "Normal observation" = "grey60")) +
  scale_size_manual(values  = c("Extreme outlier"    = 3,
                                "Normal observation" = 1)) +
  labs(title  = "Annual kWh vs Living Area",
       x      = "Square Meters",
       y      = "Annual kWh",
       color  = "",
       size   = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: annual_kwh vs income_euro
ggplot(data_clean, aes(x = income_euro, y = annual_kwh,
                       color = outlier_flag, size = outlier_flag)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Extreme outlier"    = "darkred",
                                "Normal observation" = "grey60")) +
  scale_size_manual(values  = c("Extreme outlier"    = 3,
                                "Normal observation" = 1)) +
  labs(title = "Annual kWh vs Household Income",
       x     = "Income (€)",
       y     = "Annual kWh",
       color = "",
       size  = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Clean up flag variable after plotting
data_clean$outlier_flag <- NULL


# ── 10. DESCRIPTIVE STATISTICS - CLEAN DATASET ───────────────────────────────

numeric_vars_clean <- data_no_extreme[, c("annual_kwh", "sq_meters", "income_euro",
                                          "occupancy_count", "dist_to_brussels")]

summary_stats_clean <- data.frame(
  mean   = round(sapply(numeric_vars_clean, mean),   2),
  sd     = round(sapply(numeric_vars_clean, sd),     2),
  median = round(sapply(numeric_vars_clean, median), 2),
  IQR    = round(sapply(numeric_vars_clean, IQR),    2),
  min    = round(sapply(numeric_vars_clean, min),    2),
  max    = round(sapply(numeric_vars_clean, max),    2)
)

print(summary_stats_clean)
plot(data_no_extreme$dist_to_brussels, data_no_extreme$annual_kwh,
     xlab = "Distance to Brussels",
     ylab = "Annual Energy Consumption (kWh)",
     main = "Scatterplot of Distance vs Energy")
lines(lowess(data_no_extreme$dist_to_brussels, data_no_extreme$annual_kwh), col = "blue", lwd = 2)

# ── 11. HISTOGRAMS - CLEAN DATASET ────────────────────────────────────────────

# --- annual_kwh ---
ggplot(data_no_extreme, aes(x = annual_kwh)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Annual Energy Consumption (Clean Data)",
       x = "Annual kWh", y = "Density") +
  theme_minimal()

# --- sq_meters ---
ggplot(data_no_extreme, aes(x = sq_meters)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Living Area (Clean Data)",
       x = "Square Meters", y = "Density") +
  theme_minimal()

# --- income_euro ---
ggplot(data_no_extreme, aes(x = income_euro)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Annual Household Income (Clean Data)",
       x = "Income (€)", y = "Density") +
  theme_minimal()

# --- occupancy_count ---
ggplot(data_no_extreme, aes(x = occupancy_count)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Occupancy Count (Clean Data)",
       x = "Number of Residents", y = "Density") +
  theme_minimal()

# --- dist_to_brussels ---
ggplot(data_no_extreme, aes(x = dist_to_brussels)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "darkred", linewidth = 0.8) +
  labs(title = "Distribution of Distance to Brussels (Clean Data)",
       x = "Distance (km)", y = "Density") +
  theme_minimal()

# ── 12. BOXPLOTS - CLEAN DATSET ──────────────────────────────────────────────

# --- annual_kwh ---
ggplot(data_no_extreme, aes(y = annual_kwh)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Annual Energy Consumption (Clean Data)",
       y = "Annual kWh") +
  theme_minimal()

# --- sq_meters ---
ggplot(data_no_extreme, aes(y = sq_meters)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Living Area (Clean Data)",
       y = "Square Meters") +
  theme_minimal()

# --- income_euro ---
ggplot(data_no_extreme, aes(y = income_euro)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Annual Household Income (Clean Data)",
       y = "Income (€)") +
  theme_minimal()

# --- occupancy_count ---
ggplot(data_no_extreme, aes(y = occupancy_count)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Occupancy Count (Clean Data)",
       y = "Number of Residents") +
  theme_minimal()

# --- dist_to_brussels ---
ggplot(data_no_extreme, aes(y = dist_to_brussels)) +
  geom_boxplot(fill = "steelblue", color = "darkblue", outlier.color = "darkred") +
  labs(title = "Boxplot of Distance to Brussels (Clean Data)",
       y = "Distance (km)") +
  theme_minimal()

# ── 12.1 NORMALITY CHECK - KEY CONTINUOUS VARIABLES ───────────────────────────

# --- annual_kwh ---

# Q-Q plot
qqnorm(data_no_extreme$annual_kwh,
       main = "Q-Q Plot - Annual Energy Consumption",
       pch  = 16,
       col  = adjustcolor("steelblue", alpha.f = 0.4),
       cex  = 0.6)
qqline(data_no_extreme$annual_kwh, col = "darkred", lwd = 1.5)

# KS test
ks.test(scale(data_no_extreme$annual_kwh), "pnorm")

# --- sq_meters ---

# Q-Q plot
qqnorm(data_no_extreme$sq_meters,
       main = "Q-Q Plot - Living Area",
       pch  = 16,
       col  = adjustcolor("steelblue", alpha.f = 0.4),
       cex  = 0.6)
qqline(data_no_extreme$sq_meters, col = "darkred", lwd = 1.5)

# KS test
ks.test(scale(data_no_extreme$sq_meters), "pnorm")

# --- income_euro ---

# Q-Q plot
qqnorm(data_no_extreme$income_euro,
       main = "Q-Q Plot - Household Income",
       pch  = 16,
       col  = adjustcolor("steelblue", alpha.f = 0.4),
       cex  = 0.6)
qqline(data_no_extreme$income_euro, col = "darkred", lwd = 1.5)

# KS test
ks.test(scale(data_no_extreme$income_euro), "pnorm")

# ── 13. CORRELATIONS - NUMRIC VARIABLES ─────────────────────────────────────

# Correlation matrix
cor_matrix <- round(cor(data_no_extreme[, c("annual_kwh", "sq_meters", "income_euro",
                                            "occupancy_count", "dist_to_brussels")],
                        use = "complete.obs"), 2)

print(cor_matrix)

# Scatterplot matrix
pairs(data_no_extreme[, c("annual_kwh", "sq_meters", "income_euro",
                          "occupancy_count", "dist_to_brussels")],
      main  = "Scatterplot Matrix - Numeric Variables",
      pch   = 16,
      col   = adjustcolor("steelblue", alpha.f = 0.3),
      cex   = 0.5)

# ── 13.1 CORRELATION EXPLORATION - TRANSFORMATIONS ───────────────────────────

# --- Scatterplot matrix: log(annual_kwh), sq_meters, income_euro, occupancy_count ---
pairs(data.frame(
  log_annual_kwh  = log(data_no_extreme$annual_kwh),
  sq_meters       = data_no_extreme$sq_meters,
  income_euro     = data_no_extreme$income_euro,
  occupancy_count = data_no_extreme$occupancy_count),
  main = "Scatterplot Matrix - log(annual_kwh)",
  pch  = 16,
  col  = adjustcolor("steelblue", alpha.f = 0.3),
  cex  = 0.5)

# --- Scatterplot: sq_meters vs income_euro with smoothing line ---
ggplot(data_no_extreme, aes(x = income_euro, y = sq_meters)) +
  geom_point(color = "steelblue", alpha = 0.3, size = 0.8) +
  geom_smooth(color = "darkred", linewidth = 0.8) +
  labs(title = "Living Area vs Household Income",
       x     = "Income (€)",
       y     = "Square Meters") +
  theme_minimal()

# ── 13.2 SCATTERPLOT MATRIX - log(annual_kwh) WITH CORRELATIONS ───────────────

# Lower panel: scatterplot points
panel.scatter <- function(x, y) {
  points(x, y,
         pch = 16,
         col = adjustcolor("steelblue", alpha.f = 0.3),
         cex = 0.5)
}

# Upper panel: correlation values
panel.cor <- function(x, y) {
  r   <- round(cor(x, y, use = "complete.obs"), 2)
  txt <- paste0("r = ", r)
  text(mean(range(x)), mean(range(y)), txt,
       cex  = 1.2,
       col  = ifelse(abs(r) > 0.5, "darkred", "black"),
       font = ifelse(abs(r) > 0.5, 2, 1))
}

# Scatterplot matrix
pairs(data.frame(
  log_annual_kwh  = log(data_no_extreme$annual_kwh),
  sq_meters       = data_no_extreme$sq_meters,
  income_euro     = data_no_extreme$income_euro,
  occupancy_count = data_no_extreme$occupancy_count),
  main        = "Scatterplot Matrix - log(annual_kwh) with correlations",
  lower.panel = panel.scatter,
  upper.panel = panel.cor)

# ── 14. CATEGORICAL VARIABLES - UNIVARIATE ANALYSIS ──────────────────────────

# --- Frequency tables ---

# epc_label
epc_freq <- data.frame(
  table(data_no_extreme$epc_label)
)
colnames(epc_freq) <- c("epc_label", "frequency")
epc_freq$percentage <- round(epc_freq$frequency / sum(epc_freq$frequency) * 100, 2)
print(epc_freq)

# building_era
era_freq <- data.frame(
  table(data_no_extreme$building_era)
)
colnames(era_freq) <- c("building_era", "frequency")
era_freq$percentage <- round(era_freq$frequency / sum(era_freq$frequency) * 100, 2)
print(era_freq)

# --- Bar charts ---

# epc_label
ggplot(data_no_extreme, aes(x = epc_label)) +
  geom_bar(fill = "steelblue", color = "white") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 3.5) +
  labs(title = "Distribution of EPC Labels",
       x     = "EPC Label",
       y     = "Count") +
  theme_minimal()

# building_era
ggplot(data_no_extreme, aes(x = building_era)) +
  geom_bar(fill = "steelblue", color = "white") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 3.5) +
  labs(title = "Distribution of Building Era",
       x     = "Building Era",
       y     = "Count") +
  theme_minimal()

# ── 15. ASSOCIATION BETWEEN CATEGORICAL VARIABLES ─────────────────────────────

# --- Contingency table ---
cont_table <- table(data_no_extreme$building_era, data_no_extreme$epc_label)
print(cont_table)

# Row percentages (within each building era)
print(round(prop.table(cont_table, margin = 1) * 100, 2))

# --- Chi-square test ---
chi_test <- chisq.test(cont_table)
print(chi_test)

# --- Stacked bar chart ---
ggplot(data_no_extreme, aes(x = building_era, fill = epc_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("A" = "dodgerblue",
                               "B" = "tomato",
                               "C" = "darkseagreen",
                               "D" = "darkcyan",
                               "E" = "mediumorchid",
                               "F" = "violet")) +
  labs(title = "EPC Label Distribution by Building Era",
       x     = "Building Era",
       y     = "Proportion",
       fill  = "EPC Label") +
  theme_minimal()

  # ── 16. NUMERIC VS CATEGORICAL VARIABLS ──────────────────────────────────────

# --- annual_kwh by epc_label ---
ggplot(data_no_extreme, aes(x = epc_label, y = annual_kwh, fill = epc_label)) +
  geom_boxplot(outlier.color = "darkred") +
  labs(title = "Annual Energy Consumption by EPC Label",
       x     = "EPC Label",
       y     = "Annual kWh") +
  theme_minimal() +
  theme(legend.position = "none")

# --- annual_kwh by building_era ---
ggplot(data_no_extreme, aes(x = building_era, y = annual_kwh, fill = building_era)) +
  geom_boxplot(outlier.color = "darkred") +
  labs(title = "Annual Energy Consumption by Building Era",
       x     = "Building Era",
       y     = "Annual kWh") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Descriptive statistics by epc_label ---
tapply(data_no_extreme$annual_kwh, data_no_extreme$epc_label, function(x)
  round(c(mean   = mean(x),
          median = median(x),
          sd     = sd(x),
          min    = min(x),
          max    = max(x)), 2))

# --- Descriptive statistics by building_era ---
tapply(data_no_extreme$annual_kwh, data_no_extreme$building_era, function(x)
  round(c(mean   = mean(x),
          median = median(x),
          sd     = sd(x),
          min    = min(x),
          max    = max(x)), 2))

# ── 16.1 ANNUAL KWH vs INCOME BY OCCUPANCY COUNT ──────────────────────────────

# Convert occupancy_count to factorfor coloring
data_no_extreme$occupancy_factor <- factor(data_no_extreme$occupancy_count)

# Scatterplot: annual_kwh vs income_euro colored by occupancy_count
ggplot(data_no_extreme, aes(x = income_euro, y = annual_kwh,
                            color = occupancy_factor)) +
  geom_point(alpha = 0.4, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  labs(title = "Annual Energy Consumption vs Income by Occupancy",
       x     = "Income (€)",
       y     = "Annual kWh",
       color = "Occupancy") +
  theme_minimal() +
  theme(legend.position = "right")

# ── 16.2 ANUAL KWH vs OCCUPANCY COUNT BY EPC LABEL ──────────────────────────

# Scatterplot: annual_kwh vs occupancy_count colored by epc_label
ggplot(data_no_extreme, aes(x = occupancy_count, y = annual_kwh,
                            color = epc_label)) +
  geom_point(alpha = 0.4, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  labs(title = "Annual Energy Consumption vs Occupancy by EPC Label",
       x     = "Occupancy Count",
       y     = "Annual kWh",
       color = "EPC Label") +
  theme_minimal() +
  theme(legend.position = "right")

# ── 17. MODEL BUILDING - FORWARD SELECTION ────────────────────────────────────

# Final modeling dataset
data_model <- data_no_extreme[, c("annual_kwh", "sq_meters", "epc_label",
                                  "occupancy_count", "income_euro")]

# Verify dimensions and structure
nrow(data_model)
str(data_model)

# ── STEP 1: Null model ────────────────────────────────────────────────────────
model_null <- lm(annual_kwh ~ 1, data = data_model)
summary(model_null)
AIC(model_null)

# ── STEP 2: Add sq_meters ─────────────────────────────────────────────────────
model_1 <- lm(annual_kwh ~ sq_meters, data = data_model)
summary(model_1)
AIC(model_1)

# ── STEP 3: Add epc_label ─────────────────────────────────────────────────────
model_2 <- lm(annual_kwh ~ sq_meters + epc_label, data = data_model)
summary(model_2)
AIC(model_2)

# ── STEP 4: Add occupancy_count ───────────────────────────────────────────────
model_3 <- lm(annual_kwh ~ sq_meters + epc_label + occupancy_count,
              data = data_model)
summary(model_3)
AIC(model_3)

# ── STEP 5: Add income_euro ───────────────────────────────────────────────────
model_4 <- lm(annual_kwh ~ sq_meters + epc_label + occupancy_count + income_euro,
              data = data_model)
summary(model_4)
AIC(model_4)

# ── STEP 6: Test interaction epc_label x occupancy_count ─────────────────────
model_5 <- lm(annual_kwh ~ sq_meters + epc_label + occupancy_count +
                epc_label:occupancy_count, data = data_model)
summary(model_5)
AIC(model_5)

# ── 18. FINAL MODEL REPORT - Q1 ───────────────────────────────────────────────

# Model summary
summary(model_5)

# 95% Confidence intervals for all coefficients
confint(model_5, level = 0.95)

# AIC
AIC(model_5)

# RMSE
rmse <- sqrt(mean(model_5$residuals^2))
round(rmse, 2)

# ── 19. MODEL DIAGNOSTICS ─────────────────────────────────────────────────────

# --- Normality of residuals ---
# Q-Q plot
qqnorm(model_5$residuals,
       main = "Q-Q Plot of Residuals",
       pch  = 16,
       col  = adjustcolor("steelblue", alpha.f = 0.4),
       cex  = 0.6)
qqline(model_5$residuals, col = "darkred", lwd = 1.5)

# KS test
ks.test(scale(model_5$residuals), "pnorm")

# --- Homoscedasticity ---
# Residuals vs Fitted
plot(model_5$fitted.values, model_5$residuals,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch  = 16,
     col  = adjustcolor("steelblue", alpha.f = 0.4),
     cex  = 0.6)
abline(h = 0, col = "darkred", lwd = 1.5)

# --- Linearity: residuals vs each continuous predictor ---
plot(data_model$sq_meters, model_5$residuals,
     main = "Residuals vs Square Meters",
     xlab = "Square Meters",
     ylab = "Residuals",
     pch  = 16,
     col  = adjustcolor("steelblue", alpha.f = 0.4),
     cex  = 0.6)
abline(h = 0, col = "darkred", lwd = 1.5)

plot(data_model$occupancy_count, model_5$residuals,
     main = "Residuals vs Occupancy Count",
     xlab = "Occupancy Count",
     ylab = "Residuals",
     pch  = 16,
     col  = adjustcolor("steelblue", alpha.f = 0.4),
     cex  = 0.6)
abline(h = 0, col = "darkred", lwd = 1.5)

# --- Multicollinearity: GVIF ---
library(car)
vif(model_5)

# ── 20. LOG TRANSFORMATION OF OUTCOME ─────────────────────────────────────────

# Fit final model with log(annual_kwh)
model_log <- lm(log(annual_kwh) ~ sq_meters + epc_label + occupancy_count +
                  epc_label:occupancy_count, data = data_model)

summary(model_log)
AIC(model_log)

# RMSE on log scale
rmse_log <- sqrt(mean(model_log$residuals^2))
round(rmse_log, 4)

# ── DIAGNOSTICS ON LOG MODEL ──────────────────────────────────────────────────

# Q-Q plot
qqnorm(model_log$residuals,
       main = "Q-Q Plot of Residuals - log(annual_kwh)",
       pch  = 16,
       col  = adjustcolor("steelblue", alpha.f = 0.4),
       cex  = 0.6)
qqline(model_log$residuals, col = "darkred", lwd = 1.5)

# KS test
ks.test(scale(model_log$residuals), "pnorm")

# Residuals vs Fitted
plot(model_log$fitted.values, model_log$residuals,
     main = "Residuals vs Fitted Values - log(annual_kwh)",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch  = 16,
     col  = adjustcolor("steelblue", alpha.f = 0.4),
     cex  = 0.6)
abline(h = 0, col = "darkred", lwd = 1.5)

# Residuals vs sq_meters
plot(data_model$sq_meters, model_log$residuals,
     main = "Residuals vs Square Meters - log(annual_kwh)",
     xlab = "Square Meters",
     ylab = "Residuals",
     pch  = 16,
     col  = adjustcolor("steelblue", alpha.f = 0.4),
     cex  = 0.6)
abline(h = 0, col = "darkred", lwd = 1.5)

# Residuals vs occupancy_count
plot(data_model$occupancy_count, model_log$residuals,
     main = "Residuals vs Occupancy Count - log(annual_kwh)",
     xlab = "Occupancy Count",
     ylab = "Residuals",
     pch  = 16,
     col  = adjustcolor("steelblue", alpha.f = 0.4),
     cex  = 0.6)
abline(h = 0, col = "darkred", lwd = 1.5)

# GVIF
vif(model_log)

# ── 21. FORWARD SELECTION - LOG SCALE ─────────────────────────────────────────

# Step 1: sq_meters
m_log1 <- lm(log(annual_kwh) ~ sq_meters, data = data_model)

# Step 2: + epc_label
m_log2 <- lm(log(annual_kwh) ~ sq_meters + epc_label, data = data_model)

# Step 3: + occupancy_count
m_log3 <- lm(log(annual_kwh) ~ sq_meters + epc_label + occupancy_count,
             data = data_model)

# Step 4: + income_euro
m_log4 <- lm(log(annual_kwh) ~ sq_meters + epc_label + occupancy_count +
               income_euro, data = data_model)
anova(m_log3, m_log5)
# Step 5: + interaction epc_label x occupancy_count
m_log5 <- lm(log(annual_kwh) ~ sq_meters + epc_label + occupancy_count +
               epc_label:occupancy_count, data = data_model)

# Compare AIC at each step
AIC(m_log1, m_log2, m_log3, m_log4, m_log5)

# Summary of candidate final model
summary(m_log3)
summary(m_log4)
summary(m_log5)

# ── 22. MODEL COMPARISON - RMSE ───────────────────────────────────────────────

rmse <- function(model) round(sqrt(mean(model$residuals^2)), 4)

comparison <- data.frame(
  Model    = c("m_log1", "m_log2", "m_log3", "m_log4", "m_log5"),
  Terms    = c("sq_meters",
               "+ epc_label",
               "+ occupancy_count",
               "+ income_euro",
               "+ epc_label:occupancy_count"),
  AIC      = round(AIC(m_log1, m_log2, m_log3, m_log4, m_log5)$AIC, 2),
  R2       = round(c(summary(m_log1)$r.squared,
                     summary(m_log2)$r.squared,
                     summary(m_log3)$r.squared,
                     summary(m_log4)$r.squared,
                     summary(m_log5)$r.squared), 4),
  Adj_R2   = round(c(summary(m_log1)$adj.r.squared,
                     summary(m_log2)$adj.r.squared,
                     summary(m_log3)$adj.r.squared,
                     summary(m_log4)$adj.r.squared,
                     summary(m_log5)$adj.r.squared), 4),
  RMSE     = c(rmse(m_log1), rmse(m_log2), rmse(m_log3),
               rmse(m_log4), rmse(m_log5))
)

print(comparison)

# ── 23. RESEARCH QUESTIONS Q2, Q3, Q4 - HOLM-BONFERRONI ──────────────────────

library(multcomp)

# ── Q2: Contrast A vs average B-F ────────────────────────────────────────────

# Parameter order in model:
# (Intercept), sq_meters, epc_labelB, epc_labelC, epc_labelD,
# epc_labelE, epc_labelF, occupancy_count

contrast_Q2 <- rbind(
  "A vs mean(B-F)" = c(0, 0, -1/5, -1/5, -1/5, -1/5, -1/5, 0)
)

test_Q2 <- glht(m_log3, linfct = contrast_Q2)
summary(test_Q2)
confint(test_Q2, level = 0.95)

# ── Q2: INTERPRETATION - BACK TRANSFORMATION ──────────────────────────────────

# Extract estimate and CI from Q2
est_Q2 <- confint(test_Q2)$confint[1, "Estimate"]
lwr_Q2 <- confint(test_Q2)$confint[1, "lwr"]
upr_Q2 <- confint(test_Q2)$confint[1, "upr"]

# Back-transform to original scale
ratio_Q2     <- round(exp(est_Q2), 4)
ratio_lwr_Q2 <- round(exp(lwr_Q2), 4)
ratio_upr_Q2 <- round(exp(upr_Q2), 4)
pct_diff_Q2  <- round((exp(est_Q2) - 1) * 100, 2)

# ── Q3: Income effect ─────────────────────────────────────────────────────────

# income_euro tested in m_log4 (model with income added)
contrast_Q3 <- rbind(
  "income_euro effect" = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
)

test_Q3 <- glht(m_log4, linfct = contrast_Q3)
summary(test_Q3)
confint(test_Q3, level = 0.95)

# ── HOLM-BONFERRONI CORRECTION - Q2 and Q3 only ───────────────────────────────

p_raw <- c(
  Q2 = summary(test_Q2)$test$pvalues[1],
  Q3 = summary(test_Q3)$test$pvalues[1]
)

p_adjusted <- p.adjust(p_raw, method = "holm")

results_holm <- data.frame(
  Question   = c("Q2: A vs mean(B-F)", "Q3: income_euro effect"),
  Raw_pvalue = round(p_raw, 6),
  Holm_adj_p = round(p_adjusted, 6)
)

print(results_holm)

# ── Q3: INTERPRETATION - BACK TRANSFORMATION ──────────────────────────────────

est_Q3 <- confint(test_Q3)$confint[1, "Estimate"]
lwr_Q3 <- confint(test_Q3)$confint[1, "lwr"]
upr_Q3 <- confint(test_Q3)$confint[1, "upr"]

# Effect of 10,000 euro increase in income (more meaningful unit)
unit <- 10000

ratio_Q3     <- round(exp(est_Q3 * unit), 4)
ratio_lwr_Q3 <- round(exp(lwr_Q3 * unit), 4)
ratio_upr_Q3 <- round(exp(upr_Q3 * unit), 4)
pct_diff_Q3  <- round((exp(est_Q3 * unit) - 1) * 100, 4)



# ── Q4: EPC x occupancy interaction ──────────────────────────────────────────

# Interaction was tested formally via forward selection in m_log5
# None of the interaction terms reached p < 0.05
# No further test required - result reported from m_log5 summary


# ── 24. INTERACTION PLOT - EPC LABEL x OCCUPANCY COUNT ───────────────────────

# Calculate observed means per epc_label x occupancy_count combination
interaction_data <- aggregate(
  log(annual_kwh) ~ epc_label + occupancy_count,
  data  = data_model,
  FUN   = mean
)

# Convert occupancy to factor for coloring
interaction_data$occupancy_factor <- factor(interaction_data$occupancy_count)

# Interaction plot
ggplot(interaction_data, aes(x = epc_label,
                             y = `log(annual_kwh)`,
                             color = occupancy_factor,
                             group = occupancy_factor)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  labs(title  = "Interaction Plot - log(annual_kwh)",
       x      = "EPC Label",
       y      = "Mean log(annual_kwh)",
       color  = "Occupancy") +
  theme_minimal() +
  theme(legend.position = "bottom")

