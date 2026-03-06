# MOE Analysis Script
# Purpose: Compare MOE (GPa) across measurement methods and stand type
# Design: Method (4 levels) x Stand (2 levels), unbalanced (Destructive n=18/stand)
# Output: tables + plots saved to /outputs

# Packages
library(tidyverse)
library(lme4)
library(nlme)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)

# Data
url <- "https://raw.githubusercontent.com/azd169/paper-moe-analysis/main/data/raw/data.csv"

df_raw <- read.csv(url, stringsAsFactors = FALSE)

# Remove rows with missing values
df_no_missing <- df_raw %>%
  drop_na()

# Build the list of destructive tree IDs by Stand + Tree_ID
destructive_keys <- df_no_missing %>%
  filter(Treatment == "Destructive") %>%
  distinct(Stand, Tree_ID)

# Keep only rows whose Stand + Tree_ID appear in the destructive set
df_clean <- df_no_missing %>%
  semi_join(destructive_keys, by = c("Stand", "Tree_ID"))

# Sanity check
table(df_clean$Treatment, df_clean$Stand)
df_clean %>% count(Stand, Tree_ID)

# Descriptive statistics
desc_tbl <- df_clean %>%
  group_by(Stand, Treatment) %>%
  summarise(
    n = n(),
    mean = mean(MOE),
    sd = sd(MOE),
    median = median(MOE),
    iqr = IQR(MOE),
    .groups = "drop"
  )

# Violin plots inspection
ggplot(df_clean, aes(Treatment, MOE, fill = Treatment)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", size = 1.5, shape = 18, color = "red") +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 14)
  ) +
  labs(
    x = "Measurement Method",
    y = "Modulus of Elasticity (GPa)"
  )

ggplot(df_clean, aes(Stand, MOE, fill = Stand)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", size = 1.5, shape = 18, color = "red") +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 14)
  ) +
  labs(
    x = "Measurement Method",
    y = "Modulus of Elasticity (GPa)"
  )

ggplot(df_clean, aes(Treatment, MOE, fill = Treatment)) +
  geom_violin() +
  facet_wrap(~Stand) +
  stat_summary(fun = mean, geom = "point", size = 1.5, shape = 18, color = "red") +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 14)
  ) +
  labs(
    x = "Measurement Method",
    y = "Modulus of Elasticity (GPa)"
  )

# Model
model <- lme(
  MOE ~ Treatment * Stand,
  random = ~1 | Tree_ID,
  weights = varIdent(form = ~1 | Treatment),
  data = df_clean
)

# ANOVA
anova(model)

# Pairwise comparison
emmeans(model, pairwise ~ Treatment)
emmeans(model, pairwise ~ Stand)

# Compute marginal means and Tukey comparisons
emm <- emmeans(model, ~ Treatment)
pairs(emm, adjust = "tukey")

# Significance letters
letters_df <- data.frame(
  Treatment = c("Destructive", "TreeSonic", "Resistrograph", "Microsecond"),
  .group = c("A", "B", "B", "C")
)

y_pos <- df_clean %>%
  group_by(Treatment) %>%
  summarise(y = max(MOE, na.rm = TRUE) + 0.5)

letters_df <- letters_df %>%
  left_join(y_pos, by = "Treatment")

ggplot(df_clean, aes(Treatment, MOE, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", size = 1.5, shape = 18, color = "red") +
  geom_text(
    data = letters_df,
    aes(x = Treatment, y = y, label = .group),
    inherit.aes = FALSE,
    size = 4
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Linearity and homoscedasticity check
plot(model)

qqnorm(residuals(model))
qqline(residuals(model))

# Residual outliers
df_clean$resid <- resid(model, type = "normalized")

df_clean %>%
  filter(abs(resid) > 3)

ggplot(df_clean, aes(Treatment, resid)) +
  geom_boxplot() +
  geom_hline(yintercept = c(-3,3), linetype="dashed", color="red") +
  theme_bw()


