# Code for Exercise 5, ECO517 at Princeton

# note: - make sure you have dplyr, ggplot2, data.table, testtha, patchwork
#       packages up to date and installed to run this code
#       - also, change the "dir" string to where you want to save outputs

# Contents: 
# 0. Set up environment and libraries 


##################################
# 0. Set up environment and libraries 

rm(list = ls())
library(dplyr) # data manipulation, and piping
library(ggplot2) # plots
library(tidyr) # pivot data from long to wide

# library(data.table) # fread command to easily read in this nasty asc file
# library(testthat) # Assert statements
# library(patchwork) # Combining ggplot objects
set.seed(1) # Make random numbers replicable
theme_set(theme_bw()) # ggplot theme

# Output location string
dir = "/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/5_week/"

# Load in the data
load(url("http://sims.princeton.edu/yftp/emet1_2020/kmeans/akdata.RData"))
df = akdataf 



# Check the data out...
# ggplot(data = df, aes(x = educ, y = logwage)) +
#     geom_point() + 
#     geom_smooth(method = "lm")



##################################
# Method 1
# Assume that we know the se of means is the sigma

# Find standard errors of conditional means
df_a = df %>% 
    group_by(educ) %>% 
    summarise(mu_s = mean(logwage), 
                n = n(), 
                sd = sd(logwage), 
                se = sd / sqrt(n))
# Plot output
ggplot(data = df_a) +
    geom_point(aes(x = educ, y = mu_s), color = "red") +
    geom_errorbar(aes(x = educ, 
                      ymin = mu_s - 1.96 * se, ymax = mu_s + 1.96 * se)) +
    ggtitle("Conditional means with +- 1.96 SE errorbars")

ggsave(paste0(dir, "conditional_means.pdf"))

# Check out the n per educ group
ggplot(data = df_a) + 
    geom_point(aes(x = educ, y = n))

# Set parameters for our draws
mu = df_a$mu_s
sigma = df_a$se

# Function for taking draws
draw_and_test = function(i, mu, sigma) {
    
    # Take draws. Note, we drop the zero educ draw to follow pset 
    draw = rnorm(length(mu), 
                 mean = mu, sd = sigma)[-1]
    
    # Check 1: monotone
    check1 = 1*all(diff(draw)>0)
    
    # Check 2: 
    check2 = 1*(draw[12] - draw[11] >  draw[11] - draw[10] )
    
    # Check 3: monotone 5 to 16
    check3 = 1*all(diff(draw[seq(5,16)])>0)
        
    # Check 4: 
    
    
    # Check 5: 
    
    
    df = data.frame(draw = rnorm(length(mu), 
                    mean = mu, sd = sigma), 
                    draw_num = i) %>% 
        mutate(educ = row_number())
    
    return(df)
}

# Take draws - reshape for easier manipulation
draws = lapply(1:2, draw, mu = mu, sigma = sigma) 
diff(c(1,2))



check1 = apply(
    diff(draws[[1:length(draws)]]$draw) < 0,
    1,
    all)


%>% 
    bind_rows() %>% 
    pivot_wider(id_cols = draw_num, 
                names_from = educ, names_prefix = "D_", 
                values_from = draw)

# Function 
all(diff(draws[[1]]$draw) <0)

check1=apply(diff(draws[,1:ncol(draws)])<0,2,all)


draws["D_1"] < draws["D_2"] < draws["D_3"] < draws["D_4"]

check_monotonicity_list = function(start_draw, end_draw, df){
    length(draws$draw_num)
    start_draw = 1
    end_draw = 3
    for (D in seq(start_draw, end_draw)){
        D = 1
        d2 = D+ 1
        as.numeric(df[,paste0("D_",D)]) - as.numeric(df[paste0("D_", D + 1)])
    }
}













x = test$draw
x = c(1,2,3)

1*all(diff(x)>0)

%>% 
    pivot_wider(id_cols = draw_num, 
                names_from = educ, names_prefix = "D_", values_from = draw)
draws %>% mutate()


df_t <- tibble(id = 40:45, w = 10:15, x = 20:25, y = 30:35, z = 40:45)
df_t
df_t %>% rowwise() %>% mutate(test = 1*all(diff(w:z)>0))

draws %>% mutate()

head(draws)

num_draws = 1000



draws = mapply(draw, MoreArgs = list(mu = mu, sigma = sigma))



d


d =draw(mu = mu, sigma = sigma)

plot(d)
unique(df$educ)


head(df)

