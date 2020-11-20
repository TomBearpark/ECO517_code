library(tidyverse)
library(ggplot2)
library(patchwork)

path = paste0("/Users/tombearpark/Documents/princeton/1st_year/",
              "ECO517/honore/assignments/pr5/")

# Check Quesiton 1 table
df = read_csv(paste0(path, "q1.csv"))

# Check Q 2 means

df = read_csv(paste0(path, "q2.csv"))

df %>% 
  ggplot() + 
  geom_density(aes(x = coef, color = type)) + 
  ggtitle("Coefficients, M = 3, n = 1000") + 
  facet_wrap(vars(m, estimate), ncol = 5)

df2 = read_csv(paste0(path, "q3.csv")) 
df2 %>% filter(estimate == "b2")%>%
  ggplot() + 
  geom_density(aes(x = coef, color = type)) + 
  ggtitle("Coefficients, M = 25, n = 1000") + 
  facet_wrap(vars(m))

df3 = read_csv(paste0(path, "q3_n250_M25.csv")) 
df3 %>% filter(estimate == "d0", m == 3)%>%
  ggplot() + 
  geom_density(aes(x = coef, color = type)) + 
  ggtitle("Coefficients, M = 25, n = 250") + 
  facet_wrap(vars(m))
