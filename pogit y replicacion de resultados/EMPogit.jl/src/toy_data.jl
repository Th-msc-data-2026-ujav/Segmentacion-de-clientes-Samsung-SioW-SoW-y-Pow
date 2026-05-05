"""
    toy_data(; J1 = 9, J2 = 9, N = 500, r = 100.0)

Generate a synthetic dataset for the pogit model.

The data are generated according to the following process:

    yᵢ ~ Poisson(eᵢ θᵢ λᵢ),
    θᵢ = expit(xᵢ₁'β₁),
    λᵢ = exp(xᵢ₂'β₂),

where eᵢ = 1, and

    β₁ = (1, 2, 0, …, 0),
    β₂ = (2, 1, 0, …, 0),
    xᵢₖ ~ N(0, 1 / Jₖ),

# Return

The model components `(y, e, X1, X2, r)`, where `y` are simulated counts, 
`e` is a unit exposure vector, `X1` and `X2` are design matrices, and 
`r` is passed through unchanged.
"""
function toy_data(; J1 = 9, J2 = 9, N = 500, r = 100.0)
    X1 = randn(N, J1) / √J1
    X2 = randn(N, J2) / √J2

    β1 = [1; 2; zeros(J1 - 2)]
    β2 = [2; 1; zeros(J2 - 2)]

    η1 = X1 * β1
    η2 = X2 * β2

    e = ones(N)
    μ = @. e * exp(η2) * expit(η1)
    y = [rand(DT.Poisson(μ[i])) for i in 1:N]
    
    return y, e, X1, X2, r
end
