using Pkg

# Carpeta donde está este archivo .jl
ROOT = @__DIR__

# Activar el proyecto ubicado en la carpeta actual
Pkg.activate(ROOT)

Pkg.add(["DataFrames", "CSV", "XLSX", "Distributions", "StatsBase", "LoopVectorization"])
Pkg.instantiate()

# Moverse a la carpeta actual
cd(ROOT)

println("Carpeta actual: ", pwd())

include(joinpath(ROOT, "fit_and_predict1.jl"))
include(joinpath(ROOT, "fit_and_predict2.jl"))