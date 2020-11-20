# Part 2, Bo Honore Metric Pset 5, GMM estimations
# Annoyingly, had to do this in two parts, this part generates the results.
# Separate script for plotting locally using ggplot2 (see pset5_plots.R)


rm(list = ls())
library(gmm) #- only works on the server for me :(
library(parallel) # loops take ages otherwise 

set.seed(1)

# Function to return a dataframe for given n amd M
gen_data = function(M, n){
  # Set parameters
  d0 = rep(1, M)
  d1 = rep(1, M)
  a1 = rep(1, M)
  a2 = rep(1, M)
  b = rep(0, M)
  a3 = rep(0, M)

  # Variables common across equations 
  df = data.frame(
                  u1 = rnorm(n), 
                  u2 = rnorm(n),
                  w1 = rnorm(n), 
                  x3_1 = runif(n, 0,3),
                  x3_2 = runif(n, 0,3),
                  x3_3 = runif(n, 0,3))

  # Variables that are specific to each equation, m
  for (m in 1:M) {
    for (i in 1:3) df[paste0("v",i,"_m",m)] = rnorm(n) 
    df[paste0("w2_m", m)] = rnorm(n)
    df[paste0("x1_m", m)] = rnorm(n)
    df[paste0("x2_m", m)] = df[paste0("w1")] + df[paste0("w2_m", m)]
  }
  # Specify alpha vectors

  # Generate the dependent variables
  for (m in 1:M) {
    # Generate z
    df[paste0("z_m", m)] = df[paste0("x1_m", m)] * a1[m] + 
      df[paste0("x2_m", m)] * a2[m] + 
      (df["x3_1"] + df["x3_1"] + df["x3_1"]) * a3[m] + 
      df[paste0("v1_m",m)] + df[paste0("v3_m",m)] + df["u2"]
    
    # Generate y
    df[paste0("y_m", m)] = d0[m] + df[paste0("z_m", m)] * d1[m] + 
      (df["x3_1"] + df["x3_1"] + df["x3_1"]) * b[m] + 
      df[paste0("v1_m",m)] + df[paste0("v2_m",m)] + df["u1"]
  }
  df$M = M
  return(df)
}
# Function for moment condition - very explicit version to make sure I know
# whats going on. Later function will be more general

moment_condition = function(par, df)
{
  m = df[1,8]
  d0 = par[1]
  d1 = par[2]
  b = c(par[3], par[4], par[5])
  
  print("dim X")
  print(dim(df))
  y   = df[,1]
  z   = df[,2]
  x1  = df[,3]
  x2  = df[,4]
  x31 = df[,5]
  x32 = df[,6]
  x33 = df[,7]
  
  m1 <-  y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])
  m2 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x1
  m3 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x2
  m4 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x31
  m5 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x32
  m6 <- (y - (d0 + z * d1 + x31* b[1] + x32 * b[2] + x33 * b[3])) * x33
  
  f = cbind(m1, m2, m3, m4, m5, m6) 
  return(f)
}

# Wrapper function to return relevant dataframe
run_single_eqn = function(m, df){
  df1 = df 
  df1$m = m
  mat = as.matrix(df1[c(paste0("y_m", m), paste0("z_m", m), 
          paste0("x1_m", m), paste0("x2_m", m), paste0("x3_1"), 
          paste0("x3_2"), paste0("x3_3"),"m")])
  coefs = coef(gmm(moment_condition, mat, c(1,1,1,1,1)))

  return(data.frame(m = m, d0 = coefs[1], d1 = coefs[2], 
    b1 = coefs[3], b2 = coefs[4], b3 = coefs[5]))
1}

# Run and produce output csv
df = gen_data(M = 3, n = 1000)
res = lapply(seq(1,3), run_single_eqn, df = df)
res_df = do.call(rbind.data.frame, res)
write.csv(res_df, '/u/bearpark/MACOSXFILES/single_eqn_res_M3.csv')

# Multiple equation version
run_multiple = function(par, df){
  M = df$M[1]
  mat= as.matrix(df)
  colnames(mat) <- names(df)
  f = c()
  for (m in 1:M){
    vars = mat[,paste0("y_m", m)] - (par[m] + mat[,paste0("z_m", m)] * par[m+1] + 
            mat[,paste0("x3_1")] * par[m+2] + mat[,paste0("x3_2")] * par[m+3] + 
            mat[,paste0("x3_3")] * par[m+4])
    f = c(f, 
        vars, 
        vars * mat[,paste0("x1_m", m)], 
        vars * mat[,paste0("x2_m", m)], 
        vars * mat[,paste0("x3_1")], 
        vars * mat[,paste0("x3_2")], 
        vars * mat[,paste0("x3_3")])
  }
  return(f)
}
# Wrapper function to run and format outputs
return_format_joint_df = function(M, df){

  par = rep(c(1,1,0,0,0), M)
  res_mult = coef(gmm(run_multiple, df, par))
  
  names_mult = c()
  for (m in 1:M){
    print(m)
    names_mult = c(names_mult, paste0("d0_m", m), paste0("d1_m", m), paste0("b1_m", m),
      paste0("b2_m", m), paste0("b3_m", m))
  }
  res_mult_df = data.frame(res_mult)
  res_mult_df$param = names_mult
  return(res_mult_df)
}
df = gen_data(M = 3, n = 1000)
res_df_joint = return_format_joint_df(3, df)
write.csv(res_df_joint, '/u/bearpark/MACOSXFILES/joint_res_M3.csv')

# Part 2
# Run function 500 times
# single equation version

run_single_wrapper = function(i, M = 3){
  print(i)
  df = gen_data(M = M, n = 1000)
  res = lapply(seq(1,M), run_single_eqn, df = df)
  res_df = do.call(rbind.data.frame, res)
  res_df$i = i
  return(res_df)
}
df2_1 = mclapply(seq(1,500), run_single_wrapper, mc.cores = 40)
res_df2_1 = do.call(rbind.data.frame, df2_1)
write.csv(res_df2_1, '/u/bearpark/MACOSXFILES/p2_500_iter_single_eqn_res_M3.csv')

# Multiple equatoin version 500 times
run_multiple_wrapper = function(i, M = 3){
  print(i)
  df = gen_data(M = M, n = 1000)
  res_df = return_format_joint_df(M = M, df)
  res_df$i = i
  return(res_df)
}

df2_2 = mclapply(seq(1,500), run_multiple_wrapper, mc.cores = 40)
res_df2_2 = do.call(rbind.data.frame, df2_2)
write.csv(res_df2_2, '/u/bearpark/MACOSXFILES/p2_500_iter_joint_res_M3.csv')

# Part 3
# Run function 500 times
# single equation version

df2_3_1 = mclapply(seq(1,500), run_single_wrapper, M = 25, mc.cores = 40)
res_df2_3_1 = do.call(rbind.data.frame, df2_3_1)
write.csv(res_df2_3_1, '/u/bearpark/MACOSXFILES/p2_500_iter_single_eqn_res_M25.csv')

# Joint equation version
df2_3_2 = mclapply(seq(1,500), run_multiple_wrapper, M = 25, mc.cores = 40)
res_df2_3_2 = do.call(rbind.data.frame, df2_3_2)
write.csv(res_df2_3_2, '/u/bearpark/MACOSXFILES/p2_500_iter_joint_res_M25.csv')






