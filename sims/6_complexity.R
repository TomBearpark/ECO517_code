# Code for Exercise 6, ECO517 at Princeton

# note: - make sure you have all below
#       packages up to date and installed to run this code
#       - also, change the "dir" string to where you want to save outputs

# Contents: 
# 0. Set up environment and libraries 
# 1. Frequentist F tests
# 2. Bayesian BIC analysis
# 3. CV glmnet


##################################
# 0. Set up environment and libraries 
install.packages("glmnet")

rm(list = ls())
library(dplyr) # data manipulation, and piping
library(ggplot2) # plots
library(car) #Anova
library(ggrepel) # Making text on scatter plots nicely spaced
library(glmnet) # CV ML routine

set.seed(1) # Make random numbers replicable
theme_set(theme_bw()) # ggplot theme

# Output location string
dir = "/Users/tombearpark/Documents/princeton/1st_year/ECO517/exercises/6_week/"

# Load in the data
load(url("http://sims.princeton.edu/yftp/emet1_2020/kmeans/akdata.RData"))
df = akdataf 


####################################################################
# 1 Run regressions

# 1.0 - the one they actually use in their paper
lm.0 = lm(data = df, logwage ~ educ + as.factor(yob))
summary(lm.0)

# 1.1 - both as numeric
lm.1 = lm(data = df, logwage ~ educ + yob)
summary(lm.1)

# 1.2 both as factors
lm.2 = lm(data = df, logwage ~ as.factor(educ) + as.factor(yob))
summary(lm.2)

# 1.3 All dummies and their interactions
lm.3 = lm(data = df, logwage ~ as.factor(educ) * as.factor(yob))
summary(lm.3)
stopifnot(length(lm.3$coefficients) == 210)

# 1.4 educ and yob as numeric variables, 
# plus dummies for educ at the values 8, 12,16 and 20.
df = df %>% mutate(
    D8 = ifelse(educ == 8, 1, 0), 
    D12 = ifelse(educ == 12, 1, 0), 
    D16 = ifelse(educ == 16, 1, 0), 
    D20 = ifelse(educ == 20, 1, 0)
)
dum = paste("+ D8 + D12 + D16 + D20")

lm.4 = lm(data = df, as.formula(paste0("logwage ~ educ + yob",  dum)))
summary(lm.4)

# 1.5 educ and yob as numeric variables, plus dummies for 
# educ >= 8, educ >= 12, educ >= 16,and educ == 20.

df = df %>% mutate(
    D_geq_8 = ifelse(educ >= 8, 1, 0), 
    D_geq_12 = ifelse(educ >= 12, 1, 0), 
    D_geq_16 = ifelse(educ >= 16, 1, 0), 
    D_geq_20 = ifelse(educ >= 20, 1, 0)
)
geq = paste("+ D_geq_8 + D_geq_12 + D_geq_16 + D_geq_20")

lm.5 = lm(data = df, as.formula(paste0("logwage ~ educ + yob ", geq)))
summary(lm.5)

# 1.6 All the variables in items 4 and 5.
lm.6 = lm(data = df, as.formula(paste0("logwage ~ educ + yob ", geq, dum)))

# Can we visualise these? 
# Compare model 2 to the model used in AK, checking out the coefficients 

return_coefs = function(model){
    return(data.frame(var=names(coef(model)), coef=coef(model), row.names=NULL))
}
c2 = return_coefs(lm.2) %>% 
    filter(grepl("educ", var)) %>% 
    mutate(educ = row_number()) %>% 
    mutate(marginal = coef - lag(coef))

ggplot(data = c2) + 
    geom_point(aes(x = educ, y = marginal)) +
    geom_hline(yintercept = lm.0$coefficients["educ"], color = "red")
ggsave(file = paste0(dir, "/coefficients_plot.png"))

####################################################################
# 2 Frequentist tests

# F-test - R\beta= \gamma
# Model 0
anova.0 = Anova(lm.0)

# Model 1
anova.1 = Anova(lm.1)

# Model 2
anova.2 = Anova(lm.2)

# Model 3
anova.3 = Anova(lm.3)

# Model 4
# Calculate F-stat by comparing RSS to that of smaller model (ie in lm1)
# Helper function to calculate F stats
F_generate = function(lmR, lmU, N){
    
    RSSR = deviance(lmR)
    RSSU = deviance(lmU)
    k = length(lmU$coefficients) - length(lmR$coefficients) 
    
    return(
       ((RSSR - RSSU)/ RSSU) * ((N - k)/ k)
        )
}

N = length(df$educ)

# replicate 1 - make sure our function does the same thing as the cars package
lmVR = lm(data = df, logwage ~ educ)
all.equal(F_generate(lmVR, lm.1, N = N), anova.1$`F value`[2], 
          tolerance = 0.01)

# Calculate F stat on the dummies block for model 4
F_generate(lm.4, lm.1, N)

# Model 5
F_generate(lm.5, lm.1, N)

# Model 6
F_generate(lm.6, lm.5, N)
F_generate(lm.6, lm.4, N)


####################################################################
# 2 Bayesian tests
library(lme4)
# implement the BIC
bic_df = BIC(lm.0, lm.1, lm.2, lm.3, lm.4, lm.5, lm.6)
# Extract dataframe of results
bic_df = data.frame(model=rownames(bic_df), bic_df, row.names=NULL)

ggplot(data = bic_df) +
    geom_point(aes(x = df, y = BIC, color = model)) +
    theme(legend.position="none")+
    geom_text_repel(aes(x = df, y = BIC, label = model, color = model))

####################################################################
# 3 Cross validation - note, these take AGES to run, since its a pretty 
# huge optimisation problem
head(df)

# Get outcome as a matrix - for use in cv.glmnet function
y = df$logwage %>% as.matrix()

# Helped function for processing results
get_results = function(fit){
    tmp_coeffs <- coef(fit)
    myResults <- data.frame(
        feature = tmp_coeffs@Dimnames[[1]][ which(tmp_coeffs != 0 ) ], #intercept included
        coef    = tmp_coeffs              [ which(tmp_coeffs != 0 ) ]  #intercept included
    )
    return(myResults)
}

# Define coefs string...
eq_dum = paste0(" + educ:D_geq_8 + educ:D_geq_12 + educ:D_geq_16 + educ:D_geq_20 ", 
                " + D_geq_8 + D_geq_12 + D_geq_16 + D_geq_20 ")

formula = paste("~ educ + educ_f + educ:yob_f + educ:educ_f:yob_f + educ_f:yob_f", eq_dum, 
                collapse = " ") %>% 
    as.formula()
# Create design matrix
X_2 = model.matrix(formula, dd)

# Run the CV
fit2 = cv.glmnet(X_2, y)
fit2$lambda.min
fit2$lambda.1se

# Visualise them... 
png(file=paste0(dir, "/cv_fit.png"))
    plot(fit2)
dev.off()
    
 # Print results
g = get_results(fit2)


# See how this optimal solution performs
# Use these final results to see if we can do better on BIC than previously
df = df %>% 
    mutate(educ_f16 = 1*(educ == 16), 
           educ.D_geq_8 = educ * D_geq_8, 
           educ.D_geq_12 = educ * D_geq_12)

lm.cv = lm(logwage ~ educ + educ_f16 + D_geq_16 + educ.D_geq_8 + educ.D_geq_12, df)
summary(lm.cv)

bic_df = BIC(lm.0, lm.1, lm.2, lm.3, lm.4, lm.5, lm.6, lm.cv)

# Extract dataframe of results
bic_df = data.frame(model=rownames(bic_df), bic_df, row.names=NULL)

ggplot(data = bic_df) +
    geom_point(aes(x = df, y = BIC, color = model)) +
    theme(legend.position="none")+
    geom_text_repel(aes(x = df, y = BIC, label = model, color = model))

ggsave(paste0(dir, "/BIC_with_cv_results.png"))






