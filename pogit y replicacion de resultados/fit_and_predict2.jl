############################################################
# CODIGO 5 JULIA: MATRICES X1 Y X2 CON RECORTE POR NOMBRE
# - MISMO FORMATO DE SALIDA QUE CODE4
# - X1 (SoW): Intercept + RFM
# - X2 (SioW): Intercept + Life + Income + Use
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

Random.seed!(12345)

# Asegurar XLSX
try
    @eval using XLSX
catch
    Pkg.add("XLSX")
    @eval using XLSX
end

############################################################
# 2) RUTAS
############################################################
# Carpeta donde está este archivo .jl
ROOT = @__DIR__

# Carpeta matrices dentro de la carpeta actual
data_dir = joinpath(ROOT, "matrices")

@assert isdir(data_dir) "No existe la carpeta matrices en: $(data_dir)"

# TRAIN
x1_train_path = joinpath(data_dir, "W_train.csv")
x2_train_path = joinpath(data_dir, "V_train.csv")
y_train_path  = joinpath(data_dir, "Y_train.csv")

# TEST
x1_test_path = joinpath(data_dir, "W_test.csv")
x2_test_path = joinpath(data_dir, "V_test.csv")
y_test_path  = joinpath(data_dir, "Y_test.csv")

# Salidas
out_dir = joinpath(data_dir, "outputs_empogit2")
isdir(out_dir) || mkpath(out_dir)

report_xlsx = joinpath(out_dir, "Report2.xlsx")

############################################################
# 3) UTILIDADES (IGUAL QUE CODE4)
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

σ(x) = 1.0 / (1.0 + exp(-clamp(x, -35.0, 35.0)))

############################################################
# 3.1) SELECCIÓN DE VARIABLES (ÚNICO CAMBIO VS CODE4)
############################################################

X1_KEEP_VARS = [
    "Intercept",
    "recency_days_2018_Apple",
    "frequency_2018_Apple",
    "monetary_2018_Apple"
]

X2_KEEP_VARS = [
    "Intercept",

    # Life
    "life_lost_a_job",
    "life_moved_place_of_residence",
    "life_divorce",
    "life_had_a_child",
    "life_became_pregnant",

    # Income
    "q_demos_income_50_000_74_999",
    "q_demos_income_75_000_99_999",
    "q_demos_income_100_000_149_999",
    "q_demos_income_150_000_or_more",
    "q_demos_income_Less_than_25_000",
    "q_demos_income_Prefer_not_to_say",

    # Amazon use
    "q_amazon_use_howmany_2",
    "q_amazon_use_howmany_3",
    "q_amazon_use_howmany_4_"
]

function apply_covariate_selection!(
    X1_df::DataFrame,
    X2_df::DataFrame
)
    miss1 = setdiff(X1_KEEP_VARS, names(X1_df))
    miss2 = setdiff(X2_KEEP_VARS, names(X2_df))

    @assert isempty(miss1) "Faltan variables en X1: $(miss1)"
    @assert isempty(miss2) "Faltan variables en X2: $(miss2)"

    return X1_df[:, X1_KEEP_VARS], X2_df[:, X2_KEEP_VARS]
end

############################################################
# 4) CARGA (TRAIN) + PREP + RECORTE
############################################################
X1_train_df = CSV.read(x1_train_path, DataFrame; normalizenames=true)
X2_train_df = CSV.read(x2_train_path, DataFrame; normalizenames=true)
Y_train_df  = CSV.read(y_train_path,  DataFrame; normalizenames=true)

y_train_col = names(Y_train_df)[1]
y_train     = Int.(round.(coalesce.(to_float.(Y_train_df[!, y_train_col]), 0.0)))

coerce_numeric!(X1_train_df)
coerce_numeric!(X2_train_df)

X1_train_df, X2_train_df = apply_covariate_selection!(X1_train_df, X2_train_df)

X1_train = df_to_matrix(X1_train_df)
X2_train = df_to_matrix(X2_train_df)

@assert looks_like_intercept(view(X1_train, :, 1))
@assert looks_like_intercept(view(X2_train, :, 1))

e_train = ones(Float64, length(y_train))
r = 100.0

############################################################
# 5) FIT TRAIN + RESÚMENES (IGUAL QUE CODE4)
############################################################
model_train = EMPogit.Model(y_train, e_train, X1_train, X2_train, r)
EMPogit.fit!(model_train)

bhat_train  = EMPogit.summarize_bhat(model_train)
theta_train = EMPogit.summarize_theta(model_train)
lam_train   = EMPogit.summarize_lambda(model_train)
nhat_train  = EMPogit.summarize_nhat(model_train)

CSV.write(joinpath(out_dir, "summarize_bhat_train.csv"),  bhat_train)
CSV.write(joinpath(out_dir, "summarize_theta_train.csv"), theta_train)
CSV.write(joinpath(out_dir, "summarize_lambda_train.csv"), lam_train)
CSV.write(joinpath(out_dir, "summarize_nhat_train.csv"),  nhat_train)

############################################################
# 6) EXTRAER BETAS (IGUAL QUE CODE4)
############################################################
function extract_beta_vector(bhat::DataFrame, prefix::AbstractString)
    rows = findall(t -> startswith(String(t), prefix), bhat.term)
    sub  = bhat[rows, :]
    idx  = parse.(Int, replace.(replace.(String.(sub.term), prefix => ""), "]" => ""))
    sub.estimate[sortperm(idx)]
end

beta1_hat = extract_beta_vector(bhat_train, "beta1[")
beta2_hat = extract_beta_vector(bhat_train, "beta2[")

############################################################
# 7) TEST + PREDICCIÓN (IGUAL QUE CODE4)
############################################################
X1_test_df = CSV.read(x1_test_path, DataFrame; normalizenames=true)
X2_test_df = CSV.read(x2_test_path, DataFrame; normalizenames=true)
Y_test_df  = CSV.read(y_test_path,  DataFrame; normalizenames=true)

y_test_col = names(Y_test_df)[1]
y_test     = Int.(round.(coalesce.(to_float.(Y_test_df[!, y_test_col]), 0.0)))

coerce_numeric!(X1_test_df)
coerce_numeric!(X2_test_df)

X1_test_df, X2_test_df = apply_covariate_selection!(X1_test_df, X2_test_df)

X1_test = df_to_matrix(X1_test_df)
X2_test = df_to_matrix(X2_test_df)

η1_test  = X1_test * beta1_hat
pi_test  = map(σ, η1_test)

η2_test  = X2_test * beta2_hat
lam_test = exp.(η2_test)

nhat_test = lam_test .* (1 .- pi_test) .+ Float64.(y_test)

theta_test_df  = DataFrame(term = ["theta[$i]" for i in 1:length(pi_test)],  estimate = pi_test)
lambda_test_df = DataFrame(term = ["lambda[$i]" for i in 1:length(lam_test)], estimate = lam_test)
nhat_test_df   = DataFrame(term = ["nhat[$i]" for i in 1:length(nhat_test)],  estimate = nhat_test)

############################################################
# 8) EXCEL (FORMATO IDÉNTICO A CODE4)
############################################################
function write_df_sheet!(xf::XLSX.XLSXFile, sheetname::AbstractString, df::DataFrame)
    XLSX.addsheet!(xf, sheetname)
    sh = xf[sheetname]
    for (j, nm) in enumerate(names(df))
        sh[1, j] = String(nm)
    end
    for i in 1:nrow(df), j in 1:ncol(df)
        sh[i+1, j] = df[i, j]
    end
end

XLSX.openxlsx(report_xlsx, mode="w") do xf
    write_df_sheet!(xf, "bhat_train",   bhat_train)
    write_df_sheet!(xf, "theta_train",  theta_train)
    write_df_sheet!(xf, "lambda_train", lam_train)
    write_df_sheet!(xf, "nhat_train",   nhat_train)
    write_df_sheet!(xf, "theta_test_pred",  theta_test_df)
    write_df_sheet!(xf, "lambda_test_pred", lambda_test_df)
    write_df_sheet!(xf, "nhat_test_pred",   nhat_test_df)
end

println("Listo.")
println("Carpeta outputs: ", out_dir)
println("Excel: ", report_xlsx)
