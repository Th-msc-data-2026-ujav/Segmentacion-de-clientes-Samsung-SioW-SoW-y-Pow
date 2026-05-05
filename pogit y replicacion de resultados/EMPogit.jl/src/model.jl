"""
Model(y, e, X1, X2, r)

Model object for fitting a pogit regression via EM.

This type stores the data, design matrices, and internal workspace required
to estimate a pogit model using `fit!`. Users typically construct a `Model`
once and then call `fit!` to obtain coefficient estimates.

# Arguments

- `y::Vector{Int}`:  
  Observed counts.

- `e::Vector{Float64}`:
  Positive exposure or offset.

- `X1::Matrix{Float64}`:  
  Covariates for the reporting (logistic) component.

- `X2::Matrix{Float64}`:  
  Covariates for the intensity (log-linear) component.

- `r::Float64`:  
  Tuning parameter controlling the accuracy of the Poisson approximation.
  Values around 100 work well in practice.

# Example

```julia
y, e, X1, X2, r = toy_data()
model = Model(y, e, X1, X2, r)
```

# Notes

- Initial coefficient values are supplied to `fit!`.
- All temporary memory is allocated once and reused across iterations.
- Estimation is performed by `fit!`; the constructor only stores inputs.
"""
struct Model
    r::Float64
    y::Vector{Int}
    e::Vector{Float64}
    X1::Matrix{Float64}
    X2::Matrix{Float64}
    logre::Vector{Float64}
    scratch::Scratch
    function Model(
        y::Vector{Int},
        e::Vector{Float64},
        X1::Matrix{Float64},
        X2::Matrix{Float64},
        r::Float64,
    )
        N = size(X1, 1)
        J1 = size(X1, 2)
        J2 = size(X2, 2)
        logre = @. log(r / e)
        scratch = Scratch(N, J1, J2)
        new(r, y, e, X1, X2, logre, scratch)
    end
end
