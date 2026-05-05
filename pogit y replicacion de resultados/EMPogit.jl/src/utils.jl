# Inf distance / norm
# (too simple to import them from other packages)

"Max distance"
function max_dist(x::Vector{Float64}, y::Vector{Float64})
    out = 0.0
    @inbounds for i in eachindex(x, y)
        d = abs(x[i] - y[i])
        out = max(out, d)
    end
    out
end

"Max norm"
function max_norm(x::Vector{Float64})
    out = 0.0
    @inbounds for i in eachindex(x)
        v = abs(x[i])
        out = max(out, v)
    end
    out
end

# Numerically stable expit / tanhc
# (too simple to import them from other packages)

"Numerically stable expit: expit(x) = 1 / (1 + exp(-x))."
@inline function expit(x::Float64)
    if x ≥ 0
        z = exp(-x)
        return 1 / (1 + z)
    else
        z = exp(x)
        return z / (1 + z)
    end
end

"Numerically stable evaluation of tanhc(x) = tanh(x) / x"
@inline function tanhc(x::Float64)
    abs(x) < 1e-4 ? tanhc_taylor(x) : tanh(x) / x
end

"Taylor approximation of tanhc(x) = tanh(x) / x around x = 0"
@inline function tanhc_taylor(x::Float64)
    x2 = x^2
    1 - x2 / 3 + 2 * x2^2 / 15 - 17 * x2^3 / 315
end

# Turbo-based BLAS-like operators
# (to avoid unnecesary allocations)

"GEMV: c = A * b"
function turbo_gemv!(
    c::Vector{Float64},
    A::Matrix{Float64},
    b::Vector{Float64},
)
    LV.@turbo for i in axes(A, 1)
        acc = 0.0
        for j in eachindex(b)
            acc += A[i, j] * b[j]
        end
        c[i] = acc
    end
    return nothing
end

"Transposed GEMV: c = A' * b"
function turbo_gemvt!(
    c::Vector{Float64},
    A::Matrix{Float64},
    b::Vector{Float64},
)
    for j in axes(A, 2)
        acc = 0.0
        LV.@turbo for i in axes(A, 1)
            acc += A[i, j] * b[i]
        end
        c[j] = acc
    end
    return nothing
end

"Weighted SYRK: C = A' * diag(w) * A"
function turbo_wsyrk!(
    C::Matrix{Float64},
    A::Matrix{Float64},
    w::Vector{Float64},
)
    ncols = size(A, 2)
    ncells = ncols * (ncols + 1) ÷ 2
    for cell in 1:ncells
        j, k = ltriangular_cell_position(ncols, cell)
        val = 0.0
        LV.@turbo for i in axes(A, 1)
            val += w[i] * A[i, j] * A[i, k]
        end
        C[j, k] = val
        C[k, j] = val
    end
    return nothing
end

"Weighted transposed GEMM: C = A' * diag(w) * B"
function turbo_wgemmt!(
    C::Matrix{Float64},
    A::Matrix{Float64},
    B::Matrix{Float64},
    w::Vector{Float64},
)
    @inbounds for j2 in axes(B, 2)
        for j1 in axes(A, 2)
            val = 0.0
            LV.@turbo for i in axes(A, 1)
                val += A[i, j1] * (w[i] * B[i, j2])
            end
            C[j1, j2] = val
        end
    end
    return nothing
end

# Other helpers (miscellanea)

"Map a linear index to a (i, j) position in the lower triangular enumeration."
@inline function ltriangular_cell_position(
    ncols::Int,
    cell::Int
)
    col = ceil(Int, (2 * ncols + 1 - √((2 * ncols + 1)^2 - 8 * cell)) / 2)
    previous = col * (2 * ncols - col + 1) ÷ 2 - (ncols - col + 1)
    offset = cell - previous
    row = col - 1 + offset
    return row, col
end

"In-place vcat(): x = vcat(x1, x2)"
function vcat!(
    x::Vector{Float64},
    x1::Vector{Float64},
    x2::Vector{Float64}
)
    n1 = length(x1)
    n2 = length(x2)
    for i in 1:n1
        x[i] = x1[i]
    end
    for i in 1:n2
        x[i + n1] = x2[i]
    end
    return nothing
end
