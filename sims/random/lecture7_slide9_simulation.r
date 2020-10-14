library(ggplot2)
theme_set(theme_bw())

set.seed(123)

mu = 1
sigma = mu
X = rnorm(1000, mean = mu, sd = sigma)

df = as.data.frame(X)

ggplot(data = df) +
    geom_density(aes(x = X))

# Show the result from slide 10... 

n = 10000
draw = rnorm(n,mean = 0,sd = 1)
n1 = length(draw[draw > 1.64]) / n

draw_h1 = rnorm(n,mean = 2, sd = 1)
n2 = length(draw_h1[draw_h1 > 1.64]) / n

n1 / n2

ggplot(data = data.frame(draw, draw_h1)) +
    geom_density(aes(x = draw), color = "blue") +
    geom_density(aes(x = draw_h1), color = "red") +
    geom_vline(aes(xintercept = 1.64))