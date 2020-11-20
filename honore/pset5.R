# Part 2, Bo Honore Metric Pset 5, GMM estimations
# Had to do this in two parts, this part generates the results.
# Separate script for plotting locally using ggplot2 (see pset5_plots.R)


rm(list = ls())
library(gmm) #- only works on the server for me - fortran compiler on laptop broken it seems
library(parallel) # to parallelise loops, otherwise very long computation time

set.seed(1)

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

single_moment_condition = function(par, df){
  m = df$M[1]
  mat= as.matrix(df)
  colnames(mat) <- names(df)

  vars = mat[,paste0("y_m", m)] - (par[1] + mat[,paste0("z_m", m)] * par[2] + 
            mat[,paste0("x3_1")] * par[3] + mat[,paste0("x3_2")] * par[4] + 
            mat[,paste0("x3_3")] * par[5])
  f = c( 
        vars, 
        vars * mat[,paste0("x1_m", m)], 
        vars * mat[,paste0("x2_m", m)], 
        vars * mat[,paste0("x3_1")], 
        vars * mat[,paste0("x3_2")], 
        vars * mat[,paste0("x3_3")])
  
  return(f)
}

joint_moment_condition = function(par, df){
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

run_single_and_joint = function(M, n, i){
  # parameter vector for joint
  par = rep(c(1,1,0,0,0), M)
  df = gen_data(M = M, n = n)
  
  res_mult = data.frame(coef = coef(gmm(joint_moment_condition, df, par)))
  res_mult$estimate = c("d0", "d1", "b1", "b2", "b3")
  res_mult$type = "joint"
  res_mult$m = rep(1:M, each=5)
  
  # Parameter vector per equation`
  par = c(1,1,0,0,0)
  for (m in 1:M){
    print(m)
    mat = df[c(paste0("y_m", m), paste0("z_m", m), 
            paste0("x1_m", m), paste0("x2_m", m), paste0("x3_1"), 
            paste0("x3_2"), paste0("x3_3"))]
    mat$M = m
    print("made data")
    res_single = data.frame(coef = coef(gmm(single_moment_condition, mat, par)))
    print("got coefs")
    res_single$m = m
    res_single$estimate = c("d0", "d1", "b1", "b2", "b3")
    res_single$type = "single"
    res_mult = rbind.data.frame(res_mult, res_single)
  }
  res_mult$i= i
  return(res_mult)
}
# Problem 4
df = run_single_and_joint(M = 3, n = 1000, i = 1)
write.csv(df, '/u/bearpark/MACOSXFILES/q1.csv')

# Problem 5
df2 = mclapply(seq(1,500), run_single_and_joint, 
    M = 3, n = 1000, mc.cores = 50)
res_df2 = do.call(rbind.data.frame, df2)
write.csv(res_df2, '/u/bearpark/MACOSXFILES/q2.csv')

# Problem 6
df3 = mclapply(seq(1,500), run_single_and_joint, 
    M = 25, n = 1000, mc.cores = 50)
res_df3 = do.call(rbind.data.frame, df3)
write.csv(res_df3, '/u/bearpark/MACOSXFILES/q3.csv')

df4 = mclapply(seq(1,500), run_single_and_joint, 
    M = 25, n = 250, mc.cores = 50)
res_df4 = do.call(rbind.data.frame, df4)
write.csv(res_df4, '/u/bearpark/MACOSXFILES/q3_n250_M25.csv')













