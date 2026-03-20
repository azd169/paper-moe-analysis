# Statistical analysis

## Experimental design

Trees were measured using four MOE measurement methods:
- Destructive
- TreeSonic
- Resistograph
- Microsecond

Two stand types were considered:
- Managed
- Unmanaged

Each tree was measured with all non-destructive methods. Destructive testing was performed on a subset of trees.

## MOE Association between treatments: Pearson correlation analysis and scatter plots

Pearson correlation analysis and scatter plots were used to evaluate the strength of association between non-destructive testing (NDT) methods and destructive measurements of modulus of elasticity (MOE).

## MOE differences between treatments: Model specification

The modulus of elasticity (MOE) was analyzed using a linear mixed-effects model:

```math
MOE_{ijk} = \beta_0
+ \beta_1 \,\text{Treatment}_i
+ \beta_2 \,\text{Stand}_j
+ \beta_3 \,(\text{Treatment}_i \times \text{Stand}_j)
+ b_k
+ \varepsilon_{ijk}
```

where:

- $MOE_{ijk}$ is the modulus of elasticity measured on tree $k$ using treatment $i$ in stand $j$
- $\beta_0$ is the intercept (Destructive method in Managed stands)
- $\beta_1$ represents the effect of measurement method (Treatment)
- $\beta_2$ represents the effect of stand type (Managed vs Unmanaged)
- $\beta_3$ represents the interaction between Treatment and Stand
- $b_k \sim N(0, \sigma^2_{tree})$ is the random intercept for tree $k$
- $\varepsilon_{ijk} \sim N(0, \sigma^2_i)$ is the residual error, allowing variance to differ among treatments

Tree identity was included as a random intercept to account for repeated measurements on the same tree.

Residual variance was allowed to differ among measurement methods using a variance identity structure.

$$
\varepsilon_{ijk} \sim N(0, \sigma_i^2)
$$

## Pairwise comparisons

Pairwise comparisons among treatments and stand types were conducted using Tukey-adjusted estimated marginal means (`emmeans`).

## Diagnostics

Model diagnostics included:

- residual vs fitted plots
- QQ plots
- Levene test for variance heterogeneity
- inspection of random effects

## Bland-Altman plots

Bland–Altman plots were used to assess the agreement between each non-destructive testing (NDT) method (TreeSonic, Resistograph, and Microsecond) and the destructive reference measurements of modulus of elasticity (MOE). For each pairwise comparison, the difference between the NDT estimate and the destructive measurement was plotted against their mean value for each tree.

The mean difference (bias) represents the systematic deviation of each NDT method relative to the destructive reference, while the limits of agreement (LoA), defined as the mean difference ± 1.96 standard deviations, describe the range within which most differences are expected to lie.

## RMSE and bias

Methods performance was quantified using root mean square error (RMSE), mean bias, and percent bias relative to the destructive measurements. These metrics provide complementary information on both the magnitude and direction of error.

$$
RMSE = \sqrt{\frac{1}{n} \sum_{i=1}^{n} (y_i - \hat{y}_i)^2}
$$

$$
\text{Bias} = \frac{1}{n} \sum_{i=1}^{n} (\hat{y}_i - y_i)
$$

$$
\text{Percent Bias} = \left( \frac{\sum_{i=1}^{n} (\hat{y}_i - y_i)}{\sum_{i=1}^{n} y_i} \right)
$$

Where:

 - $y_i$ is the actual MOE value (measured through distructive sampling)
 - and $\hat{y}_i$ is the predicted MOE value (estimated trhough non-destructive sampling methods)
