using GLM, RDatasets, Statistics, LinearAlgebra

iris

# Regression to replicate... 
lm1 = lm(@formula(SepalLength ~ PetalLength + PetalWidth), iris)
vcov(lm1)

# Matrix version
N = length(iris.SepalLength)
K = 3

X = [ones(N) iris.PetalLength iris.PetalWidth]
y = iris.SepalLength

β =  (X'X)^(-1)* (X'y) 
e = y - X* β

# Check we get zeros for these... 
X'e
mean(e)
# Calculate
σ = (X'X)^(-1) * e'e / (N-3)
std = sqrt(Diagonal(σ))

# robust version... 
σR = (X'X)^(-1) * (X'*(e'*e)* X ) / (N-3)* (X'X)^(-1) 
stdR = sqrt(Diagonal(σR))