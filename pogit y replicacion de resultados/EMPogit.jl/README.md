# EMPogit

A Julia package for Poisson–logistic (pogit) models estimated via an efficient
Expectation–Maximization (EM) algorithm.

EMPogit provides tools to generate synthetic data, construct pogit models,
estimate parameters using EM with Polya–Gamma augmentation, and conduct
post–estimation inference based on the Fisher information and the 
delta method.

## Installation

EMPogit can be installed directly using Julia’s package manager.

From the Julia REPL:

```julia
using Pkg
Pkg.add(url="https://github.com/igutierrezm/EMPogit.jl")
```

If you are working with a local clone of the repository:

```julia
using Pkg
Pkg.activate("path/to/EMPogit")
Pkg.develop(path="path/to/EMPogit")
```

## Quick start

```julia
using EMPogit

# Generate synthetic data
y, e, X1, X2, r = toy_data()

# Construct a model
model = Model(y, e, X1, X2, r)

# Fit the model using EM
status = fit!(model)
```

The function `fit!` updates the model in place.
A return value of `0` indicates convergence 
before the iteration limit is reached.

## Custom initialization

Users can supply custom initial values for the regression coefficients:

```julia
beta1_init = zeros(size(X1, 2))
beta2_init = zeros(size(X2, 2))

fit!(
    model;
    initial_beta1 = beta1_init,
    initial_beta2 = beta2_init,
)
```

## Post–estimation summaries

EMPogit provides several `summarize_*` functions for extracting estimates
and uncertainty measures after fitting.

All summaries return `DataFrame` objects.

### Regression coefficients

```julia
summarize_bhat(model)
```

Returns a DataFrame with:

- coefficient names
- point estimates
- standard errors
- z–statistics
- p–values
- confidence intervals

Inference is based on the Fisher information matrix.

### Reporting probabilities

```julia
summarize_theta(model)
```

Summarizes the estimated reporting probabilities

```math
\hat\theta_i = \text{logit}^{-1}(x_{i1}'\hat\beta_1).
```

Uncertainty is quantified using confidence intervals obtained via the
delta method and the Fisher precision block for the reporting component.
No hypothesis tests are reported for this derived quantity.

### Intensity parameters

```julia
summarize_lambda(model)
```

Summarizes the estimated latent intensities

```math
\hat\lambda_i = \exp(x_{i2}'\hat\beta_2).
```

Confidence intervals are computed using the delta method and the Fisher
precision block for the intensity component.

### Latent counts

```julia
summarize_nhat(model)
```

Summarizes the estimated latent counts

```math
\hat n_i = y_i + e_i \hat\lambda_i (1 - \hat\theta_i).
```

Confidence intervals are obtained via the delta method using the full
Fisher precision matrix for \((\beta_1, \beta_2)\).
No hypothesis tests are reported for this derived quantity.

## Synthetic data generation

```julia
y, e, X1, X2, r = toy_data()
```

Generates synthetic data from the observed–data distribution implied by the
pogit model, with

```math
y_i \sim \text{Poisson}(e_i \lambda_i \theta_i),
\quad
\theta_i = \text{logit}^{-1}(x_{i1}'\beta_1),
\quad
\lambda_i = \exp(x_{i2}'\beta_2).
```

## Current functionality

- Synthetic data generation (`toy_data`)
- Pogit model construction (`Model`)
- EM estimation with Polya–Gamma augmentation (`fit!`)
- Fisher information and precision matrix computation
- Delta–method inference for:
  - latent counts (`summarize_nhat`)
  - regression coefficients (`summarize_bhat`)
  - reporting probabilities (`summarize_theta`)
  - intensities (`summarize_lambda`)

---

## Notes

- The tuning parameter `r` controls the accuracy of the Poisson approximation
  used internally for Polya–Gamma augmentation. Values around `100` work well
  in practice.
- All temporary memory is allocated once and reused across EM iterations.
- The package is designed with scalability and large datasets in mind.
