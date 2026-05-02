############################################################
# CODIGO 4 JULIA: MATRICES X1 Y X2 CON RANGO COMPLETO
# - Ajusta en TRAIN (X1_train, X2_train, Y_train)
# - Exporta a carpeta outputs_empogit1
# - Crea Excel Report1.xlsx con: bhat/lambda/nhat/theta (TRAIN)
# - Luego predice en TEST usando betas TRAIN:
#     pi_hat     = σ(X1_test * beta1_hat)
#     lambda_hat = exp(X2_test * beta2_hat)
#     n_hat      = lambda_hat*(1 - pi_hat) + y_test
#   y agrega esas tablas a Report1.xlsx
############################################################

############################################################
# 1) ENTORNO + PAQUETES
############################################################
import Pkg

Pkg.activate(@__DIR__)
Pkg.instantiate()

include(joinpath(@__DIR__, "EMPogit.jl", "src", "EMPogit.jl"))
using .EMPogit

using CSV
using DataFrames
using Random

Random.seed!(12345)   # <<< SEMILLA FIJA


# Asegurar XLSX instalado dentro del entorno activo
try
    @eval using XLSX
catch
    Pkg.add("XLSX")
    @eval using XLSX
end

############################################################
# 2) RUTAS
############################################################
data_dir = raw"C:\Users\workw\Documents\Proyecto aplicado\Github\pogit\matrices"

# TRAIN
x1_train_path = joinpath(data_dir, "W_train.csv")
x2_train_path = joinpath(data_dir, "V_train.csv")
y_train_path  = joinpath(data_dir, "Y_train.csv")

# TEST
x1_test_path = joinpath(data_dir, "W_test.csv")
x2_test_path = joinpath(data_dir, "V_test.csv")
y_test_path  = joinpath(data_dir, "Y_test.csv")

# Salidas
out_dir = joinpath(data_dir, "outputs_empogit1")
isdir(out_dir) || mkpath(out_dir)

report_xlsx = joinpath(out_dir, "Report1.xlsx")

############################################################
# 3) UTILIDADES: parsing robusto a Float64 + matrices
############################################################
function to_float(x)
    if x === missing
        return missing
    elseif x isa Real
        return Float64(x)
    elseif x isa AbstractString
        s = strip(x)
        if isempty(s) || lowercase(s) in ("na", "nan", "null", "missing")
            return missing
        end
        s2 = replace(s, "," => "")
        v  = tryparse(Float64, s2)
        return v === nothing ? missing : v
    else
        return to_float(string(x))
    end
end

function coerce_numeric!(df::DataFrame)
    for nm in names(df)
        df[!, nm] = to_float.(df[!, nm])
    end
    return df
end

function df_to_matrix(df::DataFrame; impute_missing::Bool=true, impute_value::Float64=0.0)
    M = Matrix(df)
    if eltype(M) <: Union{Missing, Float64}
        return impute_missing ? Float64.(coalesce.(M, impute_value)) : Float64.(M)
    else
        return Float64.(M)
    end
end

function looks_like_intercept(col::AbstractVector{<:Real}; tol=1e-10)
    all(abs.(col .- 1.0) .<= tol)
end

# logística estable
σ(x) = 1.0 / (1.0 + exp(-clamp(x, -35.0, 35.0)))

############################################################
# 4) CARGA (TRAIN) + PREP
############################################################
X1_train_df = CSV.read(x1_train_path, DataFrame; normalizenames=true)
X2_train_df = CSV.read(x2_train_path, DataFrame; normalizenames=true)
Y_train_df  = CSV.read(y_train_path,  DataFrame; normalizenames=true)

@assert ncol(Y_train_df) >= 1 "Y_train.csv no tiene columnas."
y_train_col = names(Y_train_df)[1]
println("TRAIN: usando como y la primera columna de Y_train: ", y_train_col)

y_train_raw = to_float.(Y_train_df[!, y_train_col])
y_train     = Int.(round.(coalesce.(y_train_raw, 0.0)))

coerce_numeric!(X1_train_df)
coerce_numeric!(X2_train_df)

@assert nrow(X1_train_df) == nrow(X2_train_df) "TRAIN: X1 y X2 no tienen el mismo número de filas."
@assert nrow(X1_train_df) == length(y_train)   "TRAIN: X1/X2 y y no tienen el mismo número de observaciones."

X1_train = df_to_matrix(X1_train_df; impute_missing=true, impute_value=0.0)
X2_train = df_to_matrix(X2_train_df; impute_missing=true, impute_value=0.0)

if size(X1_train,2) >= 1 && !looks_like_intercept(view(X1_train, :, 1))
    @warn "TRAIN: la primera columna de X1 NO parece todo unos (intercepto)."
end
if size(X2_train,2) >= 1 && !looks_like_intercept(view(X2_train, :, 1))
    @warn "TRAIN: la primera columna de X2 NO parece todo unos (intercepto)."
end

e_train = ones(Float64, length(y_train))
r = 100.0

println("TRAIN dims: y=$(length(y_train)), X1=$(size(X1_train)), X2=$(size(X2_train))")

############################################################
# 5) FIT EN TRAIN + RESÚMENES TRAIN
############################################################
model_train = Model(y_train, e_train, X1_train, X2_train, r)
status = fit!(model_train)
println("TRAIN fit! status = ", status)

bhat_train  = summarize_bhat(model_train)
theta_train = summarize_theta(model_train)
lam_train   = summarize_lambda(model_train)
nhat_train  = summarize_nhat(model_train)

# CSVs (train)
CSV.write(joinpath(out_dir, "summarize_bhat_train.csv"),  bhat_train)
CSV.write(joinpath(out_dir, "summarize_theta_train.csv"), theta_train)
CSV.write(joinpath(out_dir, "summarize_lambda_train.csv"), lam_train)
CSV.write(joinpath(out_dir, "summarize_nhat_train.csv"),  nhat_train)

############################################################
# 6) EXTRAER beta1_hat y beta2_hat DESDE bhat_train
############################################################
function extract_beta_vector(bhat::DataFrame, prefix::AbstractString)
    rows = findall(t -> startswith(String(t), prefix), bhat.term)
    @assert !isempty(rows) "No encontré términos con prefijo $(prefix) en bhat."

    sub = bhat[rows, :]
    idx = parse.(Int, replace.(replace.(String.(sub.term), prefix => ""), "]" => ""))
    ord = sortperm(idx)
    return Float64.(sub.estimate[ord])
end

beta1_hat = extract_beta_vector(bhat_train, "beta1[")
beta2_hat = extract_beta_vector(bhat_train, "beta2[")

@assert length(beta1_hat) == size(X1_train,2) "beta1_hat no calza con columnas de X1_train."
@assert length(beta2_hat) == size(X2_train,2) "beta2_hat no calza con columnas de X2_train."

############################################################
# 7) CARGA (TEST) + PREDICCIÓN con betas TRAIN
############################################################
X1_test_df = CSV.read(x1_test_path, DataFrame; normalizenames=true)
X2_test_df = CSV.read(x2_test_path, DataFrame; normalizenames=true)
Y_test_df  = CSV.read(y_test_path,  DataFrame; normalizenames=true)

@assert ncol(Y_test_df) >= 1 "Y_test.csv no tiene columnas."
y_test_col = names(Y_test_df)[1]
println("TEST: usando como y la primera columna de Y_test: ", y_test_col)

y_test_raw = to_float.(Y_test_df[!, y_test_col])
y_test     = Int.(round.(coalesce.(y_test_raw, 0.0)))

coerce_numeric!(X1_test_df)
coerce_numeric!(X2_test_df)

@assert nrow(X1_test_df) == nrow(X2_test_df) "TEST: X1 y X2 no tienen el mismo número de filas."
@assert nrow(X1_test_df) == length(y_test)   "TEST: X1/X2 y y no tienen el mismo número de observaciones."

X1_test = df_to_matrix(X1_test_df; impute_missing=true, impute_value=0.0)
X2_test = df_to_matrix(X2_test_df; impute_missing=true, impute_value=0.0)

@assert size(X1_test,2) == length(beta1_hat) "TEST: columnas X1_test no coinciden con beta1_hat (train)."
@assert size(X2_test,2) == length(beta2_hat) "TEST: columnas X2_test no coinciden con beta2_hat (train)."

η1_test  = X1_test * beta1_hat
pi_test  = map(σ, η1_test)                  # theta = pi

η2_test  = X2_test * beta2_hat
lam_test = exp.(η2_test)                    # lambda

nhat_test = lam_test .* (1 .- pi_test) .+ Float64.(y_test)  # E[n|y]

theta_test_df  = DataFrame(term = ["theta[$i]" for i in 1:length(pi_test)],  estimate = pi_test)
lambda_test_df = DataFrame(term = ["lambda[$i]" for i in 1:length(lam_test)], estimate = lam_test)
nhat_test_df   = DataFrame(term = ["nhat[$i]" for i in 1:length(nhat_test)],  estimate = nhat_test)

CSV.write(joinpath(out_dir, "theta_test_pred.csv"),  theta_test_df)
CSV.write(joinpath(out_dir, "lambda_test_pred.csv"), lambda_test_df)
CSV.write(joinpath(out_dir, "nhat_test_pred.csv"),   nhat_test_df)

############################################################
# 8) EXCEL Report1.xlsx (sin Tables, sin anchor_cell)
#    -> escribimos DF a mano (encabezados + datos)
############################################################
function write_df_sheet!(xf::XLSX.XLSXFile, sheetname::AbstractString, df::DataFrame)
    # crear hoja
    XLSX.addsheet!(xf, sheetname)
    sh = xf[sheetname]

    # headers
    for (j, nm) in enumerate(names(df))
        sh[1, j] = String(nm)
    end

    # data
    n = nrow(df)
    p = ncol(df)
    for i in 1:n
        for j in 1:p
            v = df[i, j]
            sh[i+1, j] = v === missing ? "" : v
        end
    end
    return nothing
end

# Escribir workbook nuevo
XLSX.openxlsx(report_xlsx, mode="w") do xf
    # TRAIN
    write_df_sheet!(xf, "bhat_train",   bhat_train)
    write_df_sheet!(xf, "theta_train",  theta_train)
    write_df_sheet!(xf, "lambda_train", lam_train)
    write_df_sheet!(xf, "nhat_train",   nhat_train)

    # TEST (pred con betas train)
    write_df_sheet!(xf, "theta_test_pred",  theta_test_df)
    write_df_sheet!(xf, "lambda_test_pred", lambda_test_df)
    write_df_sheet!(xf, "nhat_test_pred",   nhat_test_df)
end

println("Listo.")
println("Carpeta outputs: ", out_dir)
println("Excel: ", report_xlsx)
