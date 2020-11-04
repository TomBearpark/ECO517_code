# Code for assignment 9.
using LinearAlgebra, Statistics, Distributions
using VegaLite
using DataFrames
using GLM


# Problem 1
# yi = xi + ϵi 
# where ϵi = xi vi

# Set parameters
N = 200
β  = 1

# Take draws from uniform for x
x = rand(N)
# Check we have generated uniform distribution 

function plot_density(x, string_nm)
    DataFrame(x = x) |>
        @vlplot(
            :area,
            transform=[{density="x",bandwidth=0.1}],
            x={"value:q", title=string_nm},
            y="density:q"
        )
end
plot_density(x, "x")


# Draw v 
v = rand(Normal(), N)
plot_density(v, "v")

# Generate the ϵ values
ϵ = x .* v

# Generate the y values
y = β .* x + ϵ
plot_density(y, "y")

# store in a dataframe
df = DataFrame(y = y, x = x, epsilon = ϵ)

# test β = 1
reg = lm(@formula(y ~ x), df)

# lets calculate using matrix algebra...
X = [ones(N) x]
k = 2
b = (X'X)^(-1) * X'y
e = y - X*b
var = (X'X)^(-1) * e'e / (N-k)
sd = sqrt.(Diagonal(var))
t_stat = (b[2] - 1) / sd[2,2]

# Calculate using robust SEs... 
varR = (X'X)^(-1) * X' * Diagonal(e * e') * X * (X'X)^(-1)
sdR = sqrt.(Diagonal(varR))
t_statR = (b[2] - 1) / sdR[2,2]

