##############################################################################
# Lab Problem Statement 6
# Statistical Analysis of Physical Characteristics of Palmer Penguins
# Topics: Descriptive Statistics, Hypothesis Testing, ANOVA & Non-Parametric Tests
##############################################################################

# ---------------------------------------------------------------------------
# 0. SETUP: PACKAGES & DATA
# ---------------------------------------------------------------------------

required_packages <- c("palmerpenguins", "dplyr", "ggplot2", "moments",
                        "car", "rstatix", "effsize", "tidyr")

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(moments)   # skewness, kurtosis
library(car)       # Levene's test
library(rstatix)   # Tukey HSD, effect sizes
library(effsize)   # Cohen's d
library(tidyr)

# Load and clean data (remove rows with NA in key variables)
data("penguins")
df <- penguins %>%
  filter(!is.na(body_mass_g), !is.na(flipper_length_mm), !is.na(sex))

cat("Dataset dimensions after removing NAs:", dim(df), "\n")
str(df)


# ---------------------------------------------------------------------------
# 1. DESCRIPTIVE STATISTICAL ANALYSIS (Body Mass)
# ---------------------------------------------------------------------------

cat("\n================ 1. DESCRIPTIVE STATISTICS ================\n")

# --- Overall descriptive statistics for body_mass_g ---
overall_stats <- df %>%
  summarise(
    Mean      = mean(body_mass_g),
    Median    = median(body_mass_g),
    Min       = min(body_mass_g),
    Max       = max(body_mass_g),
    Variance  = var(body_mass_g),
    SD        = sd(body_mass_g),
    Q1        = quantile(body_mass_g, 0.25),
    Q3        = quantile(body_mass_g, 0.75),
    IQR       = IQR(body_mass_g),
    Skewness  = skewness(body_mass_g),
    Kurtosis  = kurtosis(body_mass_g)
  )
print(overall_stats)

# --- Species-wise descriptive statistics ---
species_stats <- df %>%
  group_by(species) %>%
  summarise(
    N         = n(),
    Mean      = mean(body_mass_g),
    Median    = median(body_mass_g),
    Min       = min(body_mass_g),
    Max       = max(body_mass_g),
    Variance  = var(body_mass_g),
    SD        = sd(body_mass_g),
    Q1        = quantile(body_mass_g, 0.25),
    Q3        = quantile(body_mass_g, 0.75),
    IQR       = IQR(body_mass_g),
    Skewness  = skewness(body_mass_g),
    Kurtosis  = kurtosis(body_mass_g)
  )
print(species_stats)

# --- Visualizations: Histogram, Boxplot, Density plot ---

# Histogram of body mass
p1 <- ggplot(df, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Penguin Body Mass",
       x = "Body Mass (g)", y = "Frequency") +
  theme_minimal()
print(p1)
ggsave("hist_body_mass.png", p1, width = 7, height = 5)

# Species-wise boxplot
p2 <- ggplot(df, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  labs(title = "Species-wise Boxplot of Body Mass",
       x = "Species", y = "Body Mass (g)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p2)
ggsave("boxplot_species_body_mass.png", p2, width = 7, height = 5)

# Sex-wise boxplot
p3 <- ggplot(df, aes(x = sex, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  labs(title = "Sex-wise Boxplot of Body Mass",
       x = "Sex", y = "Body Mass (g)") +
  theme_minimal()
print(p3)
ggsave("boxplot_sex_body_mass.png", p3, width = 7, height = 5)

# Density plot
p4 <- ggplot(df, aes(x = body_mass_g, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Body Mass by Species",
       x = "Body Mass (g)", y = "Density") +
  theme_minimal()
print(p4)
ggsave("density_body_mass.png", p4, width = 7, height = 5)


# ---------------------------------------------------------------------------
# 2. HYPOTHESIS TESTING: Male vs Female Body Mass
# ---------------------------------------------------------------------------

cat("\n================ 2. HYPOTHESIS TESTING (Sex) ================\n")

# H0: mu_male = mu_female  (no difference in mean body mass)
# H1: mu_male != mu_female (means differ)

male_mass   <- df$body_mass_g[df$sex == "male"]
female_mass <- df$body_mass_g[df$sex == "female"]

# --- 2.2 Normality check ---
cat("\n-- Shapiro-Wilk test (male) --\n")
print(shapiro.test(male_mass))

cat("\n-- Shapiro-Wilk test (female) --\n")
print(shapiro.test(female_mass))

# QQ-plots
png("qqplot_male_female.png", width = 800, height = 400)
par(mfrow = c(1, 2))
qqnorm(male_mass, main = "QQ-Plot: Male Body Mass"); qqline(male_mass, col = "red")
qqnorm(female_mass, main = "QQ-Plot: Female Body Mass"); qqline(female_mass, col = "red")
dev.off()

par(mfrow = c(1, 2))
qqnorm(male_mass, main = "QQ-Plot: Male Body Mass"); qqline(male_mass, col = "red")
qqnorm(female_mass, main = "QQ-Plot: Female Body Mass"); qqline(female_mass, col = "red")
par(mfrow = c(1, 1))

# --- 2.3 Independent two-sample t-test ---
# Check variance equality first
cat("\n-- Variance check (F-test) --\n")
print(var.test(body_mass_g ~ sex, data = df))

# Welch's t-test (does not assume equal variance; safe default)
t_test_result <- t.test(body_mass_g ~ sex, data = df, var.equal = FALSE)
cat("\n-- Independent Two-Sample t-test (Welch) --\n")
print(t_test_result)

# --- 2.4 95% Confidence Interval for difference in means ---
cat("\n95% CI for difference in means:\n")
print(t_test_result$conf.int)

# --- 2.5 Cohen's d effect size ---
cohend_result <- cohen.d(body_mass_g ~ sex, data = df)
cat("\n-- Cohen's d --\n")
print(cohend_result)

# --- 2.6 Interpretation (printed to console) ---
cat("\nInterpretation:\n")
cat("p-value =", t_test_result$p.value,
    "-> ", ifelse(t_test_result$p.value < 0.05,
                   "Reject H0: mean body mass differs significantly by sex.",
                   "Fail to reject H0: no significant difference by sex."), "\n")


# ---------------------------------------------------------------------------
# 3. ONE-WAY ANOVA: Body Mass ~ Species
# ---------------------------------------------------------------------------

cat("\n================ 3. ONE-WAY ANOVA (Species) ================\n")

# --- Normality within each species ---
cat("\n-- Shapiro-Wilk test by species --\n")
df %>%
  group_by(species) %>%
  summarise(p_value = shapiro.test(body_mass_g)$p.value) %>%
  print()

# QQ-plots by species
png("qqplot_species.png", width = 900, height = 400)
par(mfrow = c(1, 3))
for (sp in levels(df$species)) {
  vals <- df$body_mass_g[df$species == sp]
  qqnorm(vals, main = paste("QQ-Plot:", sp)); qqline(vals, col = "red")
}
dev.off()
par(mfrow = c(1, 3))
for (sp in levels(df$species)) {
  vals <- df$body_mass_g[df$species == sp]
  qqnorm(vals, main = paste("QQ-Plot:", sp)); qqline(vals, col = "red")
}
par(mfrow = c(1, 1))

# --- Homogeneity of variance: Levene's test ---
cat("\n-- Levene's Test (Homogeneity of Variance) --\n")
levene_result <- leveneTest(body_mass_g ~ species, data = df)
print(levene_result)

# --- One-way ANOVA ---
anova_model <- aov(body_mass_g ~ species, data = df)
cat("\n-- One-Way ANOVA Summary --\n")
print(summary(anova_model))

anova_summary <- summary(anova_model)[[1]]
cat("\nF-statistic:", anova_summary["species", "F value"],
    "| df1 =", anova_summary["species", "Df"],
    "| df2 =", anova_summary["Residuals", "Df"],
    "| p-value:", anova_summary["species", "Pr(>F)"], "\n")

# --- Tukey HSD post-hoc (if significant) ---
if (anova_summary["species", "Pr(>F)"] < 0.05) {
  cat("\n-- Tukey HSD Post-Hoc Test --\n")
  tukey_result <- TukeyHSD(anova_model)
  print(tukey_result)

  png("tukey_hsd_species.png", width = 700, height = 500)
  plot(tukey_result, las = 1)
  dev.off()
  plot(tukey_result, las = 1)
}


# ---------------------------------------------------------------------------
# 4. NON-PARAMETRIC ANALYSIS: Kruskal-Wallis Test
# ---------------------------------------------------------------------------

cat("\n================ 4. KRUSKAL-WALLIS TEST ================\n")

kruskal_result <- kruskal.test(body_mass_g ~ species, data = df)
print(kruskal_result)

cat("\nComparison of ANOVA vs Kruskal-Wallis:\n")
cat("ANOVA p-value          :", anova_summary["species", "Pr(>F)"], "\n")
cat("Kruskal-Wallis p-value  :", kruskal_result$p.value, "\n")
cat("Both tests lead to the same conclusion:",
    (anova_summary["species", "Pr(>F)"] < 0.05) == (kruskal_result$p.value < 0.05), "\n")

# Optional post-hoc for Kruskal-Wallis: Dunn's test (pairwise Wilcoxon with correction)
pairwise_result <- pairwise.wilcox.test(df$body_mass_g, df$species, p.adjust.method = "BH")
cat("\n-- Pairwise Wilcoxon Test (BH-adjusted) --\n")
print(pairwise_result)


# ---------------------------------------------------------------------------
# 5. TWO-WAY ANOVA: Body Mass ~ Species * Sex
# ---------------------------------------------------------------------------

cat("\n================ 5. TWO-WAY ANOVA (Species x Sex) ================\n")

twoway_model <- aov(body_mass_g ~ species * sex, data = df)
cat("\n-- Two-Way ANOVA Summary --\n")
print(summary(twoway_model))

# Interaction plot
png("interaction_plot_species_sex.png", width = 700, height = 500)
interaction.plot(df$species, df$sex, df$body_mass_g,
                  col = c("red", "blue"), lwd = 2,
                  xlab = "Species", ylab = "Mean Body Mass (g)",
                  trace.label = "Sex",
                  main = "Interaction Plot: Species x Sex on Body Mass")
dev.off()
interaction.plot(df$species, df$sex, df$body_mass_g,
                  col = c("red", "blue"), lwd = 2,
                  xlab = "Species", ylab = "Mean Body Mass (g)",
                  trace.label = "Sex",
                  main = "Interaction Plot: Species x Sex on Body Mass")

# Grouped boxplot for visual confirmation
p5 <- ggplot(df, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species and Sex",
       x = "Species", y = "Body Mass (g)") +
  theme_minimal()
print(p5)
ggsave("boxplot_species_sex.png", p5, width = 7, height = 5)

cat("\nInterpretation:\n")
cat("Check the p-values above for 'species', 'sex', and 'species:sex' rows.\n")
cat("p < 0.05 indicates a statistically significant effect.\n")


# ---------------------------------------------------------------------------
# 6. ADDITIONAL ANALYSIS: Flipper Length ~ Species
# ---------------------------------------------------------------------------

cat("\n================ 6. FLIPPER LENGTH ANALYSIS ================\n")

# Descriptive stats for flipper length by species
flipper_stats <- df %>%
  group_by(species) %>%
  summarise(
    N        = n(),
    Mean     = mean(flipper_length_mm),
    Median   = median(flipper_length_mm),
    SD       = sd(flipper_length_mm),
    Skewness = skewness(flipper_length_mm),
    Kurtosis = kurtosis(flipper_length_mm)
  )
print(flipper_stats)

# Normality check
cat("\n-- Shapiro-Wilk test (Flipper Length) by species --\n")
df %>%
  group_by(species) %>%
  summarise(p_value = shapiro.test(flipper_length_mm)$p.value) %>%
  print()

# Levene's test
cat("\n-- Levene's Test (Flipper Length) --\n")
print(leveneTest(flipper_length_mm ~ species, data = df))

# One-way ANOVA for flipper length
flipper_anova <- aov(flipper_length_mm ~ species, data = df)
cat("\n-- One-Way ANOVA: Flipper Length ~ Species --\n")
print(summary(flipper_anova))

flipper_anova_summary <- summary(flipper_anova)[[1]]

# Tukey HSD if significant
if (flipper_anova_summary["species", "Pr(>F)"] < 0.05) {
  cat("\n-- Tukey HSD Post-Hoc: Flipper Length --\n")
  print(TukeyHSD(flipper_anova))
}

# Kruskal-Wallis for flipper length
cat("\n-- Kruskal-Wallis: Flipper Length ~ Species --\n")
print(kruskal.test(flipper_length_mm ~ species, data = df))

# Species-wise flipper length boxplot
p6 <- ggplot(df, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  labs(title = "Species-wise Comparison of Flipper Length",
       x = "Species", y = "Flipper Length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p6)
ggsave("boxplot_species_flipper.png", p6, width = 7, height = 5)

cat("\nConsistency check:\n")
cat("Body mass ANOVA p-value    :", anova_summary["species", "Pr(>F)"], "\n")
cat("Flipper length ANOVA p-value:", flipper_anova_summary["species", "Pr(>F)"], "\n")
cat("If both are significant, larger species tend to have both higher body mass\n")
cat("and longer flippers (Gentoo typically largest on both measures).\n")


# ---------------------------------------------------------------------------
# 7. GROUP COMPARISON PLOT (Significance Annotated)
# ---------------------------------------------------------------------------

cat("\n================ 7. FINAL VISUALIZATION ================\n")

# Boxplot with mean points, showing group separation clearly
p7 <- ggplot(df, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(title = "Group Comparison: Body Mass Across Penguin Species",
       subtitle = "Diamond = group mean | ANOVA & Tukey HSD confirm significant differences",
       x = "Species", y = "Body Mass (g)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p7)
ggsave("final_group_comparison.png", p7, width = 7, height = 5)

cat("\n================ ANALYSIS COMPLETE ================\n")
cat("All plots saved as PNG files in the working directory.\n")
cat("Review console output above for all statistical test results.\n")
