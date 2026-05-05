module EMPogit

using DataFrames: DataFrames
using Distributions: Distributions
using LinearAlgebra: LinearAlgebra
using LoopVectorization: LoopVectorization
using Random: Random

const DF = DataFrames
const DT = Distributions
const LA = LinearAlgebra
const LV = LoopVectorization
const RD = Random

include("utils.jl")
include("toy_data.jl")
include("scratch.jl")
include("model.jl")
include("estep.jl")
include("mstep.jl")
include("fit.jl")
include("prec.jl")
include("summarize.jl")

export toy_data
export Model
export fit!

export summarize_bhat
export summarize_theta
export summarize_lambda
export summarize_nhat

end
