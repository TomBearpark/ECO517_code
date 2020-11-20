library(tidyverse)
library(ggplot2)
library(patchwork)

path = paste0("/Users/tombearpark/Documents/princeton/1st_year/",
              "ECO517/honore/assignments/pr5/")

# Check 
df = read_csv(paste0(path, "p2_500_iter_single_eqn_res_M3.csv"))

p = ggplot(data = df %>% filter(m == 1)) + 
  geom_density(aes(x = d0)) + ggtitle("Single Equation")
p

col = df %>% filter(m == 1)
mean(col$d0)

df_joint = read_csv(paste0(path, "p2_500_iter_joint_res_M3.csv"))
q = ggplot(data = df_joint %>% filter(param == "d0_m1")) + 
  geom_density(aes(x = res_mult)) + ggtitle("Joint")
q

col1 = df_joint %>% filter(m == 1, param == "d0_m1")
mean(col1$res_mult)

(p + q) 
