"""
    fit!(model; <keyword arguments>)

Fit a Poisson-logistic (pogit) model using an EM algorithm.

This function runs the EM iterations in place, updating the regression
coefficients stored inside the model object. The results can then 
be retrieved using `summarize_*()`.

# Arguments

- `model::Model`:
    Pogit model created with `Model()`.

# Keyword arguments

- `initial_beta1::Vector{Float64}`:
    Initial values for the reporting (logistic) coefficients β₁.
    Defaults to a vector of i.i.d. U(-2, 2) rv's.

- `initial_beta2::Vector{Float64}`:
    Initial values for the intensity (log-linear) coefficients β₂.
    Defaults to a vector of i.i.d. U(-2, 2) rv's.

- `x_reltol::Float64`:
    Relative tolerance for convergence of the stacked coefficient vector.
    Defined as in `Optim.jl`. Defaults to `1e-4`.

- `iterations::Int`:
    Maximum number of EM iterations.
    Defaults to `100`.

# Returns

0 if the algorithm converged before reaching the iteration limit; 1 otherwise.

# Example

```julia
y, e, X1, X2, r = toy_data()
model = Model(y, e, X1, X2, r)
status = fit!(model)
```
"""
function fit!(
    model::Model; 
    initial_beta1::Vector{Float64} = init_β1(model),
    initial_beta2::Vector{Float64} = init_β2(model),
    x_reltol::Float64 = 1e-4,
    iterations::Int = 100, 
)
    (; β1, β2, β) = model.scratch
    
    @. β1 = initial_beta1
    @. β2 = initial_beta2
    vcat!(β, β1, β2)

    β_old = copy(β)
    for _ in 1:iterations
        estep!(model)
        mstep!(model)
        if has_converged(β, β_old, x_reltol) 
            estep!(model)
            return 0
        end
        @. β_old = β
    end
    estep!(model)
    return 1
end

"Initialize β1 (stan style)"
function init_β1(model::Model)
    J1 = length(model.scratch.β1)
    4 * rand(J1) .- 2
end

"Initialize β2 (stan style)"
function init_β2(model::Model)
    J2 = length(model.scratch.β2)
    4 * rand(J2) .- 2
end

"Check convergence of the EM algorithm based on relative change in parameters"
function has_converged(β, β_old, x_reltol)
    num = max_dist(β, β_old)
    den = max(1.0, max_norm(β_old))
    num / den <= x_reltol
end
