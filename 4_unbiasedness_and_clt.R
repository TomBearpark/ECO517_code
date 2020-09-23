# Code for exercise 3
rm(list = ls())
library(dplyr) # data manipulation, and piping
library(ggplot2) # plots
library(data.table) # fread command to easily read in this nasty asc file
library(testthat) # Assert statements
library(patchwork) # Combining ggplot objects
set.seed(123)
theme_set(theme_bw())
dir = "/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/4_week/"

##################################
# Question 1

# Load in the data from the URL
# note, fread from the URL means we don't need to clean on re-format 
df = fread("http://sims.princeton.edu/yftp/emet1_2020/mumuCLTex/newyork.asc", 
        skip = 6) %>% 
    as.data.frame()

# Want to generate 30 samples of 20 random draws, with replacement from the 
# full population of the 804 cities

# Check we have 804 cities
expect_that(length(df$population), equals(804))

# Save list of values we are interested in 
values = df$population

# Function for taking a sample, returning a dataframe, and the sample mean
get_sample = function(index, values, n =20){
    sample = sample(values, n, replace = TRUE, prob = NULL) %>% 
        as.data.frame() %>% 
        mutate(index = !!index)
    names(sample) = c("population", "index")
    return(sample)
}

# Run the function 30 times, returning a long dataframe
df_30_s = lapply(seq(1,30, 1), get_sample, values = values) %>% 
    bind_rows()

# Produce outputs... 
# 0. Histogram of the raw data
ggplot(data =df) +
    geom_histogram(aes(x = population), alpha = 0.5, fill = "blue") + 
    ggtitle("Histogram of full sample population values")
ggsave(file= paste0(dir, "/full_sample_raw_histogram.png"))

# 1. Display histogram of sample means
df_means = df_30_s %>% 
    group_by(index) %>%
    summarize(Sample_mean = mean(population))

ggplot(data =df_means) +
    geom_histogram(aes(x = Sample_mean), alpha = 0.5, fill = "blue") + 
    ggtitle("Histogram of means of 30 samples") +
    labs(caption = 
    "Means are from 30 randomly drawn samples, with replacement. Each sample has size 20.")

ggsave(file= paste0(dir, "/means_of_30_samples_histogram.png"))

# 2. Diplay a qqplot
df_means$len = 1:30
png(file=paste0(dir, "/qqplot_sample_means.png"))
    qqnorm(df_means$Sample_mean, col = "steelblue")
dev.off()


# Extention - can we make it seem more like a normal if we take more s
# samples, of bigger size?

# Lets try taking 1000 samples, each of size 200
plot_means = function(n_samples, size_sample){

    df_big_s = lapply(seq(1,n_samples, 1), get_sample, 
                    values = values, n = size_sample) %>% 
        bind_rows()

    df_means_big_s = df_big_s %>% 
        group_by(index) %>%
        summarize(Sample_mean = mean(population))

    p = ggplot(data =df_means_big_s) +
            geom_histogram(aes(x = Sample_mean), alpha = 0.5, fill = "blue") + 
            ggtitle(paste0(
                "No Samples ", n_samples))
    return(p)
}
# note - this is a big sample - takes a while to run! 
p = plot_means(n_samples = 100,size_sample = 30) + 
        plot_means(n_samples = 1000,size_sample = 30) + 
        plot_means(n_samples = 10000,size_sample = 30) 

q = plot_means(n_samples = 100,size_sample = 300) + 
        plot_means(n_samples = 1000,size_sample = 300) + 
        plot_means(n_samples = 10000,size_sample = 300) 

r = plot_means(n_samples = 1000,size_sample = 3000) + 
        plot_means(n_samples = 10000,size_sample = 3000) + 
        plot_means(n_samples = 100000,size_sample = 3000) 


##################################
# Question 2

# Consider model where iid data drawn from normal N(mu, mu^2)

# Set 
N <- 20
ndraw <- 30

lmpdf <- function(mu,xbar, s,N) -N * log(abs(mu)) - .5 * N *(s^2 + (xbar- mu)^2)/mu^2
## This is the kernel of the log posterior pdf for a sample of size N, sample
## mean xbar, and sample standard deviation s.
N <- 20
ndraw <- 30
mms <- matrix(rnorm(N * ndraw, mean=1, sd=1), N, ndraw)
mmean <- apply(mms, 2, mean)
mmsigsq <- apply(mms, 2, var) * (N-1)/N   #R uses N-1 in denominator
mnorm <- vector("numeric", ndraw)
mhat <- mnorm
mmax <- max(lmpdf(seq(-.5, 1.5, length=300), mmean[1], sqrt(msigsq[1]), N))
## we need a rough value of mmax to scale the log likelihood when exponentiating.
for (id in 1:ndraw) mnorm[id] <- integrate(function(mu)exp(lmpdf(mu, mmean[id], sqrt(msigsq[id]), N)-mmax), lower=.5, upper=1.5)$value
## above computes the scale factor of the likelihood in each sample that makes it
## integrate to one.
##
## This range of integration works for N=20. Might need adjustment for other N's.
##
for (id in 1:30) mhat[id] <- integrate(function(mu) mu * exp(lmpdf(mu, mmean[id], sqrt(msigsq[id]),N)-mmax), lower=.5, upper=1.5)$value/mnorm[id]
## mhat should be the posterior means.
## mmean should be the unbiased sample means
##

