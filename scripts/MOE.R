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
library(BlandAltmanLeh)
library(ggpubr)
library(Metrics)
library(corrplot)
library(reshape2)

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
    text = element_text(size = 14),
  ) +
  labs(
    x = "Measurement Method",
    y = "Modulus of Elasticity (GPa)"
  )

ggsave(plot = p1_violin_trt,
       filename = here("outputs", "figures", "plot1_violin_treatment.png"),
       width = 7,
       height = 5,
       dpi = 300)

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

ggsave(plot = p2_violin_stand,
       filename = here("outputs", "figures", "plot2_violin_stand.png"),
       width = 7,
       height = 5,
       dpi = 300)

p3_violin_stand_x_trt <- ggplot(df_clean, aes(Treatment, MOE, fill = Treatment)) +
  geom_violin() +
  facet_wrap(~Stand) +
  stat_summary(fun = mean, geom = "point", size = 1.5, shape = 18, color = "red") +
  theme_bw() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Measurement Method",
    y = "Modulus of Elasticity (GPa)"
  )

ggsave(plot = p3_violin_stand_x_trt,
       filename = here("outputs", "figures", "plot3_violin_stand_x_trt.png"),
       width = 7,
       height = 5,
       dpi = 300)

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

ggsave(plot = p4_boxplot_trt,
       filename = here("outputs", "figures", "plot4_boxplot_trt.png"),
       width = 7,
       height = 5,
       dpi = 300)

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
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14)
  ) +
  labs(
    x = "Measurement Method",
    y = "Modulus of Elasticity (GPa)"
  )

ggsave(plot = p5_boxplot_trt_stand,
       filename = here("outputs", "figures", "plot5_boxplot_trt_stand.png"),
       width = 7,
       height = 5,
       dpi = 300)

# Diagnostics
# Shapiro-Wilk test
shapiro.test(residuals(model))

# Levene test
leveneTest(MOE ~ Treatment * Stand, data = df_clean)

# Linearity and homoscedasticity check
df_clean$resid  <- resid(model)
df_clean$fitted <- fitted(model)

p6_fitted_vs_res <- ggplot(df_clean, aes(fitted, resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted"
  )

ggsave(plot = p6_fitted_vs_res,
       filename = here("outputs", "figures", "plot6_fitted_vs_residuals.png"),
       width = 7,
       height = 5,
       dpi = 300)

p7_qqplot <- ggplot(df_clean, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_bw() +
  labs(
    title = "Normal Q-Q Plot of Model Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
qqline(residuals(model))

ggsave(plot = p7_qqplot,
       filename = here("outputs", "figures", "plot7_qnorm.png"),
       width = 7,
       height = 5,
       dpi = 300)

p8_resid_hist <- ggplot(df_clean, aes(resid)) +
  geom_histogram(bins = 20, fill = "grey70", color = "black") +
  theme_bw() +
  labs(
    title = "Distribution of Model Residuals",
    x = "Residuals",
    y = "Frequency"
  )

ggsave(plot = p8_resid_hist,
       filename = here("outputs", "figures", "plot8_res_distr.png"),
       width = 7,
       height = 5,
       dpi = 300)

# Residual outliers
df_clean$resid <- resid(model, type = "normalized")

df_clean %>%
  filter(abs(resid) > 3)

p9_res_out <- ggplot(df_clean, aes(Treatment, resid)) +
  geom_boxplot() +
  labs(title = "Residual Outliers") +
  geom_hline(yintercept = c(-3,3), linetype = "dashed", color="red") +
  theme_bw()

ggsave(plot = p9_res_out,
       filename = here("outputs", "figures", "plot9_res_out.png"),
       width = 7,
       height = 5,
       dpi = 300)

# RMSE, bias, and Bland-Altman plots
# Data in wide-format
df_wide <- df_clean %>%
  dplyr::select(Tree_ID, Stand, Treatment, MOE) %>%
  tidyr::pivot_wider(names_from = Treatment, values_from = MOE)

RMSE <- "RMSE (GPa) ="
BIAS <- "Bias (GPa) ="
P_BIAS <- "Bias (%) ="

# Destructive vs. TreeSonic
TS_RMSE <- round(rmse(df_wide$TreeSonic, df_wide$Destructive), digits = 2)
TS_BIAS <- round(bias(df_wide$TreeSonic, df_wide$Destructive), digits = 2)
TS_PBIAS <- round(percent_bias(df_wide$TreeSonic, df_wide$Destructive), digits = 2)

# Destructive vs. Resistograph
RG_RMSE <- round(rmse(df_wide$Resistrograph, df_wide$Destructive), digits = 2)
RG_BIAS <- round(bias(df_wide$Resistrograph, df_wide$Destructive), digits = 2)
RG_PBIAS <- round(percent_bias(df_wide$Resistrograph, df_wide$Destructive), digits = 2)

# Destructive vs. Microsecond
MS_RMSE <- round(rmse(df_wide$Microsecond, df_wide$Destructive), digits = 2)
MS_BIAS <- round(bias(df_wide$Microsecond, df_wide$Destructive), digits = 2)
MS_PBIAS <- round(percent_bias(df_wide$Microsecond, df_wide$Destructive), digits = 2)

# Bland-Altman plot stats
BA_TS_DS_stats <- bland.altman.stats(df_wide$TreeSonic, df_wide$Destructive)
BA_RG_DS_stats <- bland.altman.stats(df_wide$Resistrograph, df_wide$Destructive)
BA_MS_DS_stats <- bland.altman.stats(df_wide$Microsecond, df_wide$Destructive)

# Bland-Altman datasets
BA_TS_DS_data <- cbind(BA_TS_DS_stats$means, BA_TS_DS_stats$diffs) %>%
  as_tibble() %>%
  dplyr::rename(means = V1,
                differences = V2)

BA_RG_DS_data <- cbind(BA_RG_DS_stats$means, BA_RG_DS_stats$diffs) %>%
  as_tibble() %>%
  dplyr::rename(means = V1,
                differences = V2)

BA_MS_DS_data <- cbind(BA_MS_DS_stats$means, BA_MS_DS_stats$diffs) %>%
  as_tibble() %>%
  dplyr::rename(means = V1,
                differences = V2)

p10_BA_TS <- ggscatter(BA_TS_DS_data, x = "means", y = "differences",
                      shape = 16,
                      color = "steelblue",
                      size = 2,
                      xlab = "Average measurement (GPa)",  
                      ylab = "Differences between measurements (GPa)",
                      title = "Bland Altman: TreeSonic vs. Destructive") +
  geom_hline(yintercept = BA_TS_DS_stats$mean.diffs, color = "red") +
  geom_hline(yintercept = BA_TS_DS_stats$upper.limit, linetype = 2) +
  geom_hline(yintercept = BA_TS_DS_stats$lower.limit, linetype = 2 ) +
  font("title", size = 18) +
  font("xlab", size = 16) +
  font("ylab", size = 16) +
  font("xy.text", size = 14) +
  annotate("text", x = 14, y = -9, label = paste(RMSE, TS_RMSE), size = 5, color = "black") +
  annotate("text", x = 14, y = -10, label = paste(BIAS, TS_BIAS), size = 5, color = "black") +
  annotate("text", x = 14, y = -11, label = paste(P_BIAS, TS_PBIAS), size = 5, color = "black")

ggsave(plot = p10_BA_TS,
       filename = here("outputs", "figures", "plot10_BA_TS.png"),
       width = 7,
       height = 5,
       dpi = 300)

p11_BA_RG <- ggscatter(BA_RG_DS_data, x = "means", y = "differences",
                       shape = 16,
                       color = "steelblue",
                       size = 2,
                       xlab = "Average measurement (GPa)",  
                       ylab = "Differences between measurements (GPa)",
                       title = "Bland Altman: Resistograph vs. Destructive") +
  geom_hline(yintercept = BA_RG_DS_stats$mean.diffs, color = "red") +
  geom_hline(yintercept = BA_RG_DS_stats$upper.limit, linetype = 2) +
  geom_hline(yintercept = BA_RG_DS_stats$lower.limit, linetype = 2 ) +
  font("title", size = 18) +
  font("xlab", size = 16) +
  font("ylab", size = 16) +
  font("xy.text", size = 14) +
  annotate("text", x = 14, y = -10, label = paste(RMSE, RG_RMSE), size = 5, color = "black") +
  annotate("text", x = 14, y = -11, label = paste(BIAS, RG_BIAS), size = 5, color = "black") +
  annotate("text", x = 14, y = -12, label = paste(P_BIAS, RG_PBIAS), size = 5, color = "black")

ggsave(plot = p11_BA_RG,
       filename = here("outputs", "figures", "plot11_BA_RG.png"),
       width = 7,
       height = 5,
       dpi = 300)

p12_BA_MS <- ggscatter(BA_MS_DS_data, x = "means", y = "differences",
                       shape = 16,
                       color = "steelblue",
                       size = 2,
                       xlab = "Average measurement (GPa)",  
                       ylab = "Differences between measurements (GPa)",
                       title = "Bland Altman: Resistograph vs. Destructive") +
  geom_hline(yintercept = BA_MS_DS_stats$mean.diffs, color = "red") +
  geom_hline(yintercept = BA_MS_DS_stats$upper.limit, linetype = 2) +
  geom_hline(yintercept = BA_MS_DS_stats$lower.limit, linetype = 2 ) +
  font("title", size = 18) +
  font("xlab", size = 16) +
  font("ylab", size = 16) +
  font("xy.text", size = 14) +
  annotate("text", x = 9, y = -10, label = paste(RMSE, MS_RMSE), size = 5, color = "black") +
  annotate("text", x = 9, y = -11, label = paste(BIAS, MS_BIAS), size = 5, color = "black") +
  annotate("text", x = 9, y = -12, label = paste(P_BIAS, MS_PBIAS), size = 5, color = "black")

ggsave(plot = p12_BA_MS,
       filename = here("outputs", "figures", "plot12_BA_MS.png"),
       width = 7,
       height = 5,
       dpi = 300)

# Correlation analyisis
cor_mat <- df_wide %>%
  dplyr::select(Destructive, TreeSonic, Resistrograph, Microsecond) %>%
  cor(use = "complete.obs", method = "pearson")

cor_df <- reshape2::melt(cor_mat)

p13_corr <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.7) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  ) +
  labs(
    title = "Correlation among MOE measurement methods",
    fill = "Pearson r"
  )

ggsave(
  here("outputs", "figures", "p13_corr.png"),
  p13_corr,
  width = 6,
  height = 5,
  dpi = 300
)

# Scatterplots
p14_scatter_TS <- ggplot(df_wide, aes(x = Destructive, y = TreeSonic)) +
   geom_point(aes(color = Stand), alpha = 0.7, size = 2.5) +
   geom_smooth(method = "lm", se = FALSE) +
   theme_bw() +
  labs(
    title = "Comparison of Destructive with TreeSonic",
    x = "Destructive MOE",
    y = "TreeSonic MOE",
    color = "Stand"
  )

ggsave(
  here("outputs", "figures", "p14_Scatter_TS.png"),
  p14_scatter_TS,
  width = 6,
  height = 5,
  dpi = 300
)

p15_scatter_RG <- ggplot(df_wide, aes(x = Destructive, y = Resistrograph)) +
  geom_point(aes(color = Stand), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(
    title = "Comparison of Destructive with Resistograph",
    x = "Destructive MOE",
    y = "Resistograph MOE",
    color = "Stand"
  )

ggsave(
  here("outputs", "figures", "p15_scatter_RG.png"),
  p15_scatter_RG,
  width = 6,
  height = 5,
  dpi = 300
)

p16_scatter_MS <- ggplot(df_wide, aes(x = Destructive, y = Microsecond)) +
  geom_point(aes(color = Stand), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(
    title = "Comparison of Destructive with Microsecond",
    x = "Destructive MOE",
    y = "Microsecond MOE",
    color = "Stand"
  )

ggsave(
  here("outputs", "figures", "p16_scatter_MS.png"),
  p16_scatter_MS,
  width = 6,
  height = 5,
  dpi = 300
)