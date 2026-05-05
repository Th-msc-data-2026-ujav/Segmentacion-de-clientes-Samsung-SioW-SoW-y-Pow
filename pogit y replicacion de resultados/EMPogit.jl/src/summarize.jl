"""
    summarize_bhat(model; conf_level = 0.95)

Return a DataFrame summarizing the MLE of the regression coefficients β.

The summary includes parameter names, MLEs, SEs, z-values, p-values, 
and confidence intervals based on the Fisher information.

# Example

```julia
y, e, X1, X2, r = toy_data()
model = Model(y, e, X1, X2, r)
status = fit!(model)
summarize_bhat(model)
```
"""
function summarize_bhat(model::Model; conf_level::Float64 = 0.95)
    (; β1, β2) = model.scratch

    # MLE
    bh = vcat(β1, β2)

    # Standard errors
    se = model |> prec_chol |> LA.inv |> LA.diag .|> sqrt

    # z-values
    zval = @. bh / se
    
    # p-values
    pval = @. 2 * (1 - DT.cdf(DT.Normal(), abs(zval)))

    # Confidence intervals
    a  = 1 - conf_level
    za = DT.quantile(DT.Normal(), 1 - a / 2)
    lo = @. bh - za * se
    hi = @. bh + za * se

    # Terms names
    names_β1 = ["beta1[$j]" for j in eachindex(β1)]
    names_β2 = ["beta2[$j]" for j in eachindex(β2)]
    term = vcat(names_β1, names_β2)

    # Output (broom-inspired column names)
    return DF.DataFrame(
        term = term,
        estimate = bh,
        std_error = se,
        statistic = zval,
        p_value = pval,
        conf_low = lo,
        conf_high = hi,
    )
end

"""
    summarize_theta(model; conf_level = 0.95)

Return a DataFrame summarizing the estimated reporting probabilities θ̂.

Uncertainty is quantified using confidence intervals obtained via the
delta method and the Fisher precision block for β₁. No hypothesis tests
are reported for this derived quantity.
"""
function summarize_theta(model::Model; conf_level::Float64 = 0.95)
    (; X1, scratch) = model
    (; θ, β1) = scratch

    # Fisher precision for β₁ and Cholesky factor
    P1 = model |> prec_11 |> LA.Symmetric
    F1 = LA.cholesky(P1)

    # Estimates 
    estimate = copy(θ)

    # Delta-method standard errors
    se = similar(θ)
    tmp = similar(β1)
    @inbounds for i in eachindex(θ)
        xi = @view X1[i, :]
        gi = θ[i] * (1 - θ[i]) # dθ/dη

        # Solve L * u = xi ⇒ u = L^{-1} xi
        copyto!(tmp, xi)
        LA.ldiv!(F1.L, tmp)
        se[i] = abs(gi) * LA.norm(tmp)
    end

    # Confidence intervals
    a = 1 - conf_level
    z = DT.quantile(DT.Normal(), 1 - a / 2)
    lo = @. estimate - z * se
    hi = @. estimate + z * se

    # Clamp to [0, 1] for probabilities
    lo = clamp.(lo, 0.0, 1.0)
    hi = clamp.(hi, 0.0, 1.0)

    # Terms names
    term = ["theta[$i]" for i in eachindex(θ)]

    # Output (broom-inspired column names)
    DF.DataFrame(
        term = term, 
        estimate = estimate, 
        conf_low = lo, 
        conf_high = hi
    )
end

"""
    summarize_lambda(model; conf_level = 0.95)

Return a DataFrame summarizing the estimated latent intensities λ.

Uncertainty is quantified using confidence intervals obtained via the
delta method and the Fisher precision block for β₂. No hypothesis tests
are reported for this derived quantity.
"""
function summarize_lambda(model::Model; conf_level::Float64 = 0.95)
    (; X2, scratch) = model
    (; λ, β2) = scratch

    # Fisher precision for β₂ and Cholesky factor
    P2 = model |> prec_22 |> LA.Symmetric
    F2 = LA.cholesky(P2)

    # Estimates
    estimate = copy(λ)

    # Delta-method standard errors
    se = similar(λ)
    tmp = similar(β2)
    @inbounds for i in eachindex(λ)
        xi = @view X2[i, :]
        gi = λ[i] # dλ/dη

        # Solve L * u = xi ⇒ u = L^{-1} xi
        copyto!(tmp, xi)
        LA.ldiv!(F2.L, tmp)
        se[i] = abs(gi) * LA.norm(tmp)
    end

    # Confidence intervals
    a = 1 - conf_level
    z = DT.quantile(DT.Normal(), 1 - a / 2)
    lo = @. estimate - z * se
    hi = @. estimate + z * se

    # Enforce positivity
    lo = max.(lo, 0.0)

    # Term names
    term = ["lambda[$i]" for i in eachindex(λ)]

    # Output (broom-inspired column names)
    DF.DataFrame(
        term = term,
        estimate = estimate,
        conf_low = lo,
        conf_high = hi
    )
end

"""
    summarize_nhat(model; conf_level = 0.95)

Return a DataFrame summarizing the estimated latent counts n̂.

Uncertainty is quantified using confidence intervals obtained via the
delta method and the full Fisher precision matrix for (β₁, β₂).
No hypothesis tests are reported for this derived quantity.
"""
function summarize_nhat(model::Model; conf_level::Float64 = 0.95)
    (; y, e, X1, X2, scratch) = model
    (; θ, λ, β1, β2) = scratch

    # Estimated n̂
    estimate = @. y + e * λ * (1 - θ)

    # Fisher precision and Cholesky factor
    F = prec_chol(model)

    # Workspace
    se = similar(estimate)
    g1 = similar(β1)
    g2 = similar(β2)
    g = vcat(g1, g2)
    
    # Delta-method standard errors
    @inbounds for i in eachindex(estimate)
        x1 = @view X1[i, :]
        x2 = @view X2[i, :]

        # Gradient w.r.t. (β₁, β₂)
        a1 = e[i] * λ[i] * θ[i] * (θ[i] - 1)
        a2 = e[i] * λ[i] * (1 - θ[i])
        @. g1 = a1 * x1
        @. g2 = a2 * x2
        vcat!(g, g1, g2)

        LA.ldiv!(F.L, g) # g <- L \ g
        se[i] = LA.norm(g)
    end

    # Confidence intervals
    a = 1 - conf_level
    z = DT.quantile(DT.Normal(), 1 - a / 2)
    lo = @. estimate - z * se
    hi = @. estimate + z * se

    # Enforce non-negativity
    lo = max.(lo, 0.0)

    # Term names
    term = ["nhat[$i]" for i in eachindex(estimate)]

    # Output (broom-inspired column names)
    DF.DataFrame(
        term = term,
        estimate = estimate,
        conf_low = lo,
        conf_high = hi
    )
end
