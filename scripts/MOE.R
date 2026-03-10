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
library(here)

setwd("..")
getwd()

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

#write.csv(desc_tbl,
#  here("outputs", "tables", "descriptive_statistics.csv"),
#  row.names = FALSE)

# Violin plots inspection
p1_violin_trt <- ggplot(df_clean, aes(Treatment, MOE, fill = Treatment)) +
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

# ggsave( p1_violin_trt,
#  here("outputs", "figures", "violin_treatment.png"),
#  width = 7,
#  height = 5,
#  dpi = 300)

p2_violin_stand <- ggplot(df_clean, aes(Stand, MOE, fill = Stand)) +
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

# ggsave(p2_violin_stand,
#  here("outputs", "figures", "violin_stand.png"),
#  width = 7,
#  height = 12,
#  dpi = 300)

p3_violin_stand_x_trt <- ggplot(df_clean, aes(Treatment, MOE, fill = Treatment)) +
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

# ggsave(p3_violin_stand_x_trt,
#  here("outputs", "figures", "violin_stand_x_trt.png"),
#  width = 25,
#  height = 25,
#  dpi = 300)

# Model
model <- lme(
  MOE ~ Treatment * Stand,
  random = ~1 | Tree_ID,
  weights = varIdent(form = ~1 | Treatment),
  data = df_clean
)

# ANOVA
anova_tbl <- as.data.frame(anova(model))

VarCorr(model)
intervals(model)

# write.csv(anova_tbl,
#  here("outputs", "tables", "anova_mixed_model.csv"))

# Marginal means
emm <- emmeans(model, ~ Treatment * Stand)

# Pairwise comparisons for Treatment (main effect)
pairs(emmeans(model, ~ Treatment), adjust = "tukey")

# Pairwise comparisons for Stand (main effect)
pairs(
  emmeans(model, ~ Stand),
  adjust = "tukey"
)

# Pairwise comparisons for Treatment within each Stand
pairs(
  emmeans(model, ~ Treatment | Stand),
  adjust = "tukey"
)

# Pairwise comparisons for Stand within each Treatment
pairs(
  emmeans(model, ~ Stand | Treatment),
  adjust = "tukey"
)

#Letters
letters_trt <- cld(
  emmeans(model, ~ Treatment | Stand),
  adjust = "tukey",
  Letters = LETTERS) %>%
  as.data.frame()
  
letters_trt$.group <- trimws(letters_trt$.group)

letters_trt_stand <- cld(
  emmeans(model, ~ Treatment | Stand),
  by = "Stand",
  adjust = "tukey",
  Letters = LETTERS) %>%
  as.data.frame()

letters_trt_stand$.group <- trimws(letters_trt_stand$.group)

# y-axis position for letters
y_pos <- df_clean %>%
  group_by(Treatment) %>%
  summarise(y = max(MOE, na.rm = TRUE) + 0.5, .groups = "drop")

letters_trt <- letters_trt %>%
  left_join(y_pos, by = "Treatment")

y_pos1 <- df_clean %>%
  group_by(Stand, Treatment) %>%
  summarise(y = max(MOE, na.rm = TRUE) + 0.5, .groups = "drop")

letters_trt_stand <- letters_trt_stand %>%
  left_join(y_pos1, by = c("Stand", "Treatment"))

# Pairwise tables
# write.csv(as.data.frame(pairs(emmeans(model, ~ Treatment), adjust = "tukey")),
#  here("outputs","tables","pairwise_treatment_tukey.csv"),
#  row.names = FALSE)

# write.csv(as.data.frame(pairs(emmeans(model, ~ Stand), adjust = "tukey")),
#  here("outputs","tables","pairwise_stand_tukey.csv"),
#  row.names = FALSE)

# write.csv(as.data.frame(pairs(emmeans(model, ~ Treatment | Stand), adjust = "tukey")),
#  here("outputs","tables","pairwise_treatment_by_stand_tukey.csv"),
#  row.names = FALSE)

p4_boxplot_trt <- ggplot(df_clean, aes(Treatment, MOE, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", size = 1.5, shape = 18, color = "red") +
  geom_text(
    data = letters_trt,
    aes(x = Treatment, y = y, label = .group),
    inherit.aes = FALSE,
    size = 4
  ) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 14)
  ) +
  labs(
    x = "Measurement Method",
    y = "Modulus of Elasticity (GPa)"
    )

# ggsave(plot = p4_boxplot_trt,
#       filename = here("outputs", "figures", "p4_boxplot_trt.png"),
#       width = 7,
#       height = 5,
#       dpi = 300)

p5_boxplot_trt_stand <- ggplot(df_clean, aes(x = Treatment, y = MOE, fill = Treatment)) +
  geom_boxplot(width = 0.65, outlier.shape = NA, alpha = 0.8) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,   # diamond
    size = 3,
    color = "red"
  ) +
  geom_text(
    data = letters_trt_stand,
    aes(x = Treatment, y = y, label = .group),
    inherit.aes = FALSE,
    size = 5
  ) +
  facet_wrap(~Stand) +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 14)
  ) +
  labs(
    x = "Measurement Method",
    y = "Modulus of Elasticity (GPa)"
  )

# ggsave(plot = p5_boxplot_trt_stand,
#       filename = here("outputs", "figures", "p5_boxplot_trt_stand.png"),
#       width = 7,
#       height = 5,
#       dpi = 300)

# Linearity and homoscedasticity check
p6_fitted_vs_res <- plot(model)

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


