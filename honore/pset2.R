# Pset 1 for Honore - Problem set 2. Questions 1 and 2

rm(list = ls())
library(ggplot2)
library(dplyr)
library(patchwork)
library(stargazer)

theme_set(theme_bw())
dir = paste0("/Users/tombearpark/Documents/princeton/1st_year", 
             "/ECO517/honore/assignments/pr2/")

######################################################## 
# Question 1
######################################################## 

# Load and clean data for plotting correlations 
get_data = function(dir, name){
  df = fread(paste0(dir, name)) %>% 
    as.data.frame() %>% 
    rename(LNWAGE = V11, ED = V1) %>% 
    mutate(WAGE = exp(LNWAGE)) %>% 
    rename(FE = V5, UNION = V10, HISP = V4, NONWH = V3, EX = V8, EXSQ = V9) %>% 
    mutate(
      EX3 = EX^3, EX4 = EX^4, EX5 = EX^5
    ) %>% 
    select(!starts_with(c("V")))
  return(df)
}

# Calculate correlation, and plot a visualization 
plot_correlations = function(df){
  
  corr = round(cor(df$WAGE, df$ED), 4)
  corr_ln = round(cor(df$LNWAGE, df$ED), 4)
  
  ggplot(data = df) +
    geom_point(aes(x = ED, y = WAGE)) + 
    ggtitle(paste0("Corr: ", corr)) +
  ggplot(data = df) +
    geom_point(aes(x = ED, y = LNWAGE))+ 
    ggtitle(paste0("Corr: ", corr_ln))
}


# Run question 1: find correlations
df78 = get_data(dir = dir, name = "Cps78")
p1 = plot_correlations(df78)
ggsave(paste0(dir, "/correlations_78.png"), p1)


# 2- regression model
lm78 = lm(data = df78, 
   LNWAGE ~ FE + UNION + HISP + NONWH + ED + EX + EXSQ)
stargazer(lm78)

# 4 - at what level of experience is wage maximised
coefsm = coef(lm78)
exper = function(x) coefsm["(Intercept)"] + coefsm["EX"]* x + coefsm["EXSQ"]* x ^2

maxLNwage = as.numeric( -0.5 * (coefsm["EX"] / coefsm["EXSQ"]))

p2 = ggplot(data = df78) +
  stat_function(fun = exper) + xlim(0,50) +
  geom_vline(xintercept = maxLNwage, color = "red") +
  xlab("EX") + ylab("Effect on LNWAGE")
ggsave(paste0(dir, "/max_lnwage_by_ex_78.png"), p2)


# 5. 
# higher with more schooling
coefsm["ED"] * (12 - 16) + coefsm["EX"] *(12 - 8)  + coefsm["EXSQ"] * (12^2 - 8^2)

# 6. 
confint(lm78, 'FE', level=0.95)

# 7. Fail to reject since its in the interval
confint(lm78, 'HISP', level=0.9)

# 8. 
lm78_ex = lm(data = df78, 
          LNWAGE ~ FE + UNION + HISP + NONWH + ED + EX + EXSQ + EX3 + EX4 + EX5)

stargazer(lm78, lm78_ex)

# 9
summary(lm78_ex)


# 10. 
# Run restricted regression
lm78_noEX = lm(data = df78, 
               LNWAGE ~ FE + UNION + HISP + NONWH + ED)
# Calculate F-statistic
anova(lm78_ex,lm78_noEX)  

# 11. Re-run everything for 85 data
df85= get_data(dir = dir, name = "Cps85")
# Consider the correlations
plot_correlations(df85)
# Run regression
lm85 = lm(data = df85, 
          LNWAGE ~ FE + UNION + HISP + NONWH + ED + EX + EXSQ)
coef85 = coef(lm85)
# max ln wage by experience - 32
maxLNwage = as.numeric( -0.5 * (coef85["EX"] / coef85["EXSQ"]))
confint(lm85, 'FE', level=0.95)

# 7. Fail to reject since its in the interval
confint(lm85, 'HISP', level=0.9)

# 8. 
lm85_ex = lm(data = df78, 
             LNWAGE ~ FE + UNION + HISP + NONWH + 
               ED + EX + EXSQ + EX3 + EX4 + EX5)
summary(lm85_ex)
linearHypothesis(lm85_ex, c("EX3=0", "EX4=0", "EX5=0"))


# 12
df_long = bind_rows(df78 %>% mutate(year = 0), 
                    df85 %>% mutate(year = 1))

lm_joint = lm(data = df_long, 
              LNWAGE ~ 0+ FE + UNION + HISP + NONWH + ED + EX + EXSQ + 
                year * (FE + UNION + HISP + NONWH + ED + EX + EXSQ ))

linearHypothesis(lm_joint, c("FE:year=0", "UNION:year=0",   "HISP:year=0",  
                            "NONWH:year=0",  "ED:year=0",     "EX:year=0",
                            "EXSQ:year=0" ))
# 13
df_long =  df_long %>% 
  mutate(WAGE_inf = ifelse(year == 1, WAGE / 1.649, WAGE)) %>% 
  mutate(LNWAGE_inf = log(WAGE_inf))

# 14
lm85_def = lm(data = df_long %>% filter(year == 1), 
              LNWAGE_inf ~ FE + UNION + HISP + NONWH + ED + EX + EXSQ)

# 15 
lm_const = lm(data = df_long , 
              LNWAGE_inf ~ year +FE + UNION + HISP + NONWH + ED + EX + EXSQ)
summary(lm_const)


stargazer(lm78, lm78_ex, lm85, lm85_ex)



######################################
# Problem 2
######################################

x = rnorm(800)
y = x^2
summary(lm(y ~ x))











