using Pkg
Pkg.activate(raw"C:\Users\workw\Documents\Proyecto aplicado\Github\pogit")
Pkg.add(["DataFrames", "CSV", "XLSX", "Distributions", "StatsBase", "LoopVectorization"])
Pkg.instantiate()

cd(@__DIR__)
println(pwd())

include("fit_and_predict1.jl")
include("fit_and_predict2.jl")