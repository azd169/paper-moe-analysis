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

## Model specification

The modulus of elasticity (MOE) was analyzed using a linear mixed-effects model:

$$
\MOE_{ijk} = \beta_0
+ \beta_1 \,\text{Treatment}_i
+ \beta_2 \,\text{Stand}_j
+ \beta_3 \,(\text{Treatment}_i \times \text{Stand}_j)
+ b_k
+ \varepsilon_{ijk}
$$

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