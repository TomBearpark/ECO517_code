# Pset 1 for Honore - Question
rm(list = ls())
library(ggplot2)
library(dplyr)
library(data.table)

theme_set(theme_bw())
dir = paste0("/Users/tombearpark/Documents/princeton/",
  "1st_year/ECO517/honore/assignments/data/")

# Load in the data 
df = fread(paste0(dir, "NERC.txt")) %>% 
  as.data.frame()

# Create logs as needed
df = df %>% 
  mutate(ln_KWH = log(KWH), 
         ln_PRICE = log(PELEC), 
         ln_GNP = log(GNP), 
         t = YEAR - 1950)

# 1b - run the regressoin without anything de-trended 
lm_1b = lm(data = df, ln_KWH ~ t + ln_PRICE + ln_GNP)

# 1c Detrend ln_KNH, LN_PRICE, and ln_GNP
detrend = function(df, var) {
  return(lm(data = df, as.formula(paste(var, " ~ t")))$residuals)
}      
df$det_ln_KWH = detrend(df, "ln_KWH")
df$det_ln_PRICE = detrend(df, "ln_PRICE")
df$det_ln_GNP = detrend(df, "ln_GNP")

# 1d - use the detrended data for all vars
lm_1d = lm(data = df, det_ln_KWH ~ t + det_ln_PRICE + det_ln_GNP)

# 1e non-detrended dep var, detrended indepedent vars
lm_1e = lm(data = df, ln_KWH ~ t + det_ln_PRICE + det_ln_GNP)

# Look at results
coef(lm_1b)["ln_PRICE"]
coef(lm_1d)["det_ln_PRICE"]
coef(lm_1e)["det_ln_PRICE"]
