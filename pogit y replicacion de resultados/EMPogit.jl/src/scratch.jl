struct Scratch
    θ::Vector{Float64}
    λ::Vector{Float64}
    n::Vector{Float64}
    η1::Vector{Float64}
    η2::Vector{Float64}
    w1::Vector{Float64}
    w2::Vector{Float64}
    κ1::Vector{Float64}
    κ2::Vector{Float64}
    h1::Vector{Float64}
    h2::Vector{Float64}
    Q1::Matrix{Float64}
    Q2::Matrix{Float64}
    β1::Vector{Float64}
    β2::Vector{Float64}
    β::Vector{Float64}
end

function Scratch(N::Int, J1::Int, J2::Int)
    zv(n) = zeros(n)
    zm(n) = zeros(n, n)    
    Scratch(
        ntuple(_ -> zv(N), 9)..., # θ λ n η1 η2 w1 w2 κ1 κ2
        zv(J1), zv(J2), # h1 h2 
        zm(J1), zm(J2), # Q1 Q2
        zv(J1), zv(J2), # β1 β2
        zv(J1 + J2), # β
    )
end
