function prec_chol(model::Model)
    out = model |> prec |> LA.Symmetric |> LA.cholesky
    return out
end

"Fisher precision matrix for β"
function prec(model::Model)
    P11 = prec_11(model)
    P12 = prec_12(model)
    P22 = prec_22(model)

    out = [P11 P12; P12' P22]
    return out
end

"Fisher precision block for β₁ (reporting component)"
function prec_11(model::Model)
    (; X1, e, scratch) = model
    (; θ, λ) = scratch
    
    μ = @. e * λ * θ
    w = @. μ * (1 - θ)^2

    J1 = size(X1, 2)
    out = zeros(J1, J1)    
    turbo_wsyrk!(out, X1, w)
    return out
end

"Fisher precision block for β₂ (intensity component)"
function prec_22(model::Model)
    (; X2, e, scratch) = model
    (; θ, λ) = scratch

    μ = @. e * λ * θ

    J2 = size(X2, 2)
    out = zeros(J2, J2)
    turbo_wsyrk!(out, X2, μ)
    return out
end

"Fisher precision block between β₁ and β₂"
function prec_12(model::Model)
    (; X1, X2, e, scratch) = model
    (; θ, λ) = scratch
    
    μ = @. e * λ * θ
    w = @. μ * (1 - θ)

    J1 = size(X1, 2)
    J2 = size(X2, 2)
    out = zeros(J1, J2)    
    turbo_wgemmt!(out, X1, X2, w)
    return out
end
