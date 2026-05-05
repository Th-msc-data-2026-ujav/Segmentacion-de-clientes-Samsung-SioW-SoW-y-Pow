"""
    estep!(model::Model)

Run the E-step of the EM algorithm.

Updates latent expectations, Polya-Gamma moments, and sufficient statistics
in place based on the current parameter values stored in the scratch space.
"""
function estep!(model::Model)
    (; r, y, e, X1, X2, logre, scratch) = model
    (; θ, λ, n, η1, η2, w1, w2, κ1, κ2, h1, h2, Q1, Q2, β1, β2) = scratch

    turbo_gemv!(η1, X1, β1)
    turbo_gemv!(η2, X2, β2)

    @. λ = exp(η2)
    @. θ = expit(η1)
    @. n = y + e * λ * (1 - θ)
    @. w1 = 0.25 * n * tanhc(0.5 * η1)
    @. w2 = 0.25 * (n + r) * tanhc(0.5 * (η2 - logre))
    @. κ2 = 0.5 * (n - r) + w2 * logre
    @. κ1 = y - 0.5 * n

    turbo_gemvt!(h1, X1, κ1)
    turbo_gemvt!(h2, X2, κ2)
    turbo_wsyrk!(Q1, X1, w1)
    turbo_wsyrk!(Q2, X2, w2)

    return nothing
end
