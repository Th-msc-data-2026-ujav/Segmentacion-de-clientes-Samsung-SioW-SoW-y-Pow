"""
    mstep!(model::Model)

Run the M-step of the EM algorithm.

Updates the regression coefficients by solving two weighted least-squares
problems using the sufficient statistics computed in the E-step.
"""
function mstep!(model::Model)
    (; h1, h2, Q1, Q2, β1, β2, β) = model.scratch

    @. β1 = h1
    S1 = LA.Symmetric(Q1)
    F1 = LA.cholesky!(S1)
    LA.ldiv!(F1, β1)

    @. β2 = h2
    S2 = LA.Symmetric(Q2)
    F2 = LA.cholesky!(S2)
    LA.ldiv!(F2, β2)

    vcat!(β, β1, β2)

    return nothing
end
